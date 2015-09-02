{-# LANGUAGE RankNTypes #-}
-- | Futhark Compiler Driver
module Main (main) where

import Data.Maybe
import Control.Category (id)
import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Exit
import System.Console.GetOpt

import Prelude hiding (id)

import Futhark.Pass
import Futhark.Actions
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Pipeline
import qualified Futhark.Representation.Basic as Basic
import Futhark.Representation.Basic (Basic)
import qualified Futhark.Representation.ExplicitMemory as ExplicitMemory
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Representation.AST (Prog)
import Futhark.TypeCheck (Checkable)
import Futhark.Util.Log

import Futhark.Pass.Untrace
import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.CSE
import Futhark.Optimise.Fusion
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.Simplify
import Futhark.Optimise.SuffCond
import Futhark.Optimise.SplitShapes
import Futhark.Optimise.InPlaceLowering
import Futhark.Pass.Flattening
import Futhark.Optimise.DoubleBuffer
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.ExpandArrays
import Futhark.Pass.ExtractKernels
import Futhark.Pass.ExpandAllocations
import Futhark.Pass.ExplicitAllocations

import Futhark.Passes (standardPipeline)

data Config = Config { futharkConfig :: FutharkConfig
                     , futharkPipeline :: [UntypedPass]
                     , futharkAction :: UntypedAction
                     }

data UntypedPassState = Basic Basic.Prog
                      | ExplicitMemory ExplicitMemory.Prog

data UntypedPass = UntypedPass (UntypedPassState
                                -> PipelineConfig
                                -> FutharkM UntypedPassState)

data UntypedAction = BasicAction (Action Basic)
                   | ExplicitMemoryAction (Action ExplicitMemory)
                   | PolyAction (Action Basic) (Action ExplicitMemory)

newConfig :: Config
newConfig = Config newFutharkConfig [] $ PolyAction printAction printAction

changeFutharkConfig :: (FutharkConfig -> FutharkConfig)
                    -> Config -> Config
changeFutharkConfig f cfg = cfg { futharkConfig = f $ futharkConfig cfg }

type FutharkOption = FunOptDescr Config

passOption :: String -> UntypedPass -> String -> [String] -> FutharkOption
passOption desc pass short long =
  Option short long
  (NoArg $ Right $ \cfg ->
   cfg { futharkPipeline = pass : futharkPipeline cfg })
  desc

explicitMemoryProg :: String -> UntypedPassState -> FutharkM ExplicitMemory.Prog
explicitMemoryProg name (Basic prog) =
  compileError (T.pack $
                "Pass " ++ name ++
                " expects ExplicitMemory representation, but got Basic")
  prog
explicitMemoryProg _ (ExplicitMemory prog) =
  return prog

basicProg :: String -> UntypedPassState -> FutharkM Basic.Prog
basicProg name (ExplicitMemory prog) =
  compileError (T.pack $
                "Pass " ++ name ++
                " expects Basic representation, but got ExplicitMemory")
  prog
basicProg _ (Basic prog) =
  return prog

typedPassOption :: Checkable tolore =>
                   (String -> UntypedPassState -> FutharkM (Prog fromlore))
                -> (Prog tolore -> UntypedPassState)
                -> Pass fromlore tolore
                -> String
                -> FutharkOption
typedPassOption getProg putProg pass short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where perform s config = do
          prog <- getProg (passName pass) s
          putProg <$> runPasses (onePass pass) config prog

        long = [passLongOption pass]

basicPassOption :: Pass Basic Basic -> String -> FutharkOption
basicPassOption =
  typedPassOption basicProg Basic

explicitMemoryPassOption :: Pass ExplicitMemory ExplicitMemory -> String -> FutharkOption
explicitMemoryPassOption =
  typedPassOption explicitMemoryProg ExplicitMemory

simplifyOption :: String -> FutharkOption
simplifyOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where perform (Basic prog) config =
          Basic <$> runPasses (onePass simplifyBasic) config prog
        perform (ExplicitMemory prog) config =
          ExplicitMemory <$> runPasses (onePass simplifyExplicitMemory) config prog

        long = [passLongOption pass]
        pass = simplifyBasic

cseOption :: String -> FutharkOption
cseOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where perform (Basic prog) config =
          Basic <$> runPasses (onePass performCSE) config prog
        perform (ExplicitMemory prog) config =
          ExplicitMemory <$> runPasses (onePass performCSE) config prog

        long = [passLongOption pass]
        pass = performCSE :: Pass Basic Basic

basicPipelineOption :: String -> Pipeline Basic Basic -> String -> [String]
                    -> FutharkOption
basicPipelineOption desc pipeline =
  passOption desc $ UntypedPass pipelinePass
  where pipelinePass (Basic prog) config =
          Basic <$> runPasses pipeline config prog
        pipelinePass (ExplicitMemory prog) _ =
          compileError (T.pack "Expected Basic representation, but got ExplicitMemory")
          prog

commandLineOptions :: [FutharkOption]
commandLineOptions =
  [ Option "V" ["verbose"]
    (OptArg (\file -> Right $ changeFutharkConfig $
                      \opts -> opts { futharkVerbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["no-bounds-checking"]
    (NoArg $ Right $ changeFutharkConfig $
     \opts -> opts { futharkBoundsCheck = False })
    "Do not perform bounds checking in the generated program."

  , Option [] ["compile-sequential"]
    (NoArg $ Right $ \opts ->
       opts { futharkAction = ExplicitMemoryAction seqCodeGenAction })
    "Translate program into sequential C and write it on standard output."
  , Option [] ["compile-imperative"]
    (NoArg $ Right $ \opts ->
       opts { futharkAction = ExplicitMemoryAction impCodeGenAction })
    "Translate program into the imperative IL and write it on standard output."
  , Option [] ["compile-imperative-kernels"]
    (NoArg $ Right $ \opts ->
       opts { futharkAction = ExplicitMemoryAction kernelImpCodeGenAction })
    "Translate program into the imperative IL with kernels and write it on standard output."
  , Option "i" ["interpret"]
    (NoArg $ Right $ \opts -> opts { futharkAction = BasicAction $
                                                     interpretAction' $
                                                      futharkRealConfiguration $
                                                      futharkConfig opts })
    "Run the program via an interpreter."
     , Option [] ["range-analysis"]
       (NoArg $ Right $ \opts -> opts { futharkAction = PolyAction rangeAction rangeAction })
       "Print the program with range annotations added."
  , Option "p" ["print"]
    (NoArg $ Right $ \opts -> opts { futharkAction = PolyAction printAction printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."

  , Option [] ["real-as-single"]
    (NoArg $ Right $ changeFutharkConfig $
     \config -> config { futharkRealConfiguration = RealAsFloat32 } )
    "Map 'real' to 32-bit floating point."
  , Option [] ["real-as-double"]
    (NoArg $ Right $ changeFutharkConfig $
     \config -> config { futharkRealConfiguration = RealAsFloat64 } )
    "Map 'real' to 64-bit floating point (the default)."

  , basicPassOption untraceProg "u"
  , basicPassOption firstOrderTransform "f"
  , basicPassOption fuseSOACs "o"
  , basicPassOption inlineAggressively []
  , basicPassOption removeDeadFunctions []
  , basicPassOption optimisePredicates []
  , basicPassOption splitShapes []
  , basicPassOption inPlaceLowering []
  , basicPassOption flattenProg []
  , basicPassOption babysitKernels []
  , basicPassOption expandArrays []
  , basicPassOption extractKernels []

  , typedPassOption basicProg ExplicitMemory explicitAllocations "a"

  , explicitMemoryPassOption doubleBuffer []
  , explicitMemoryPassOption expandAllocations []

  , cseOption []
  , simplifyOption "e"

  -- , passoption "Transform program to explicit memory representation" explicitMemory
  --   "a" ["explicit-allocations"]

  , basicPipelineOption "Run the default optimised pipeline"
    standardPipeline "s" ["standard"]
  ]

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: IO ()
main = mainWithOptions newConfig commandLineOptions compile
  where compile [file] config =
          Just $ do
            (res, msgs) <- runPipelineOnProgram (futharkConfig config) id file
            T.hPutStr stderr $ toText msgs
            case res of
              Left err -> do
                dumpError (futharkConfig config) err
                exitWith $ ExitFailure 2
              Right prog ->
                runPolyPasses config prog
        compile _      _      =
          Nothing

runPolyPasses :: Config -> Basic.Prog -> IO ()
runPolyPasses config prog = do
  (res, msgs) <- runFutharkM $ do
    prog' <- foldM (runPolyPass pipeline_config) (Basic prog) (futharkPipeline config)
    case (prog', futharkAction config) of
      (Basic basic_prog, BasicAction action) ->
        actionProcedure action basic_prog
      (ExplicitMemory mem_prog, BasicAction action) ->
        compileError (T.pack $ "Action " <>
                      actionName action <>
                      " expects Basic representation, but got ExplicitMemory.")
        mem_prog
      (ExplicitMemory mem_prog, ExplicitMemoryAction action) ->
        actionProcedure action mem_prog
      (Basic basic_prog, ExplicitMemoryAction action) ->
        compileError (T.pack $ "Action " <>
                      actionName action <>
                      " expects ExplicitMemory representation, but got Basic.")
        basic_prog
      (Basic basic_prog, PolyAction basic_action _) ->
        actionProcedure basic_action basic_prog
      (ExplicitMemory mem_prog, PolyAction _ poly_action) ->
        actionProcedure poly_action mem_prog

  T.hPutStr stderr $ toText msgs
  case res of
    Left err -> do
      dumpError (futharkConfig config) err
      exitWith $ ExitFailure 2
    Right () ->
      return ()
  where pipeline_config =
          PipelineConfig { pipelineVerbose = isJust $ futharkVerbose $ futharkConfig config
                         , pipelineValidate = True
                         }

runPolyPass :: PipelineConfig
            -> UntypedPassState -> UntypedPass -> FutharkM UntypedPassState
runPolyPass pipeline_config s (UntypedPass f) =
  f s pipeline_config
