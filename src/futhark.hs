{-# LANGUAGE RankNTypes #-}
-- | Futhark Compiler Driver
module Main (main) where

import Data.Maybe
import Control.Category (id)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
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
import qualified Futhark.Representation.SOACS as SOACS
import Futhark.Representation.SOACS (SOACS)
import qualified Futhark.Representation.Kernels as Kernels
import Futhark.Representation.Kernels (Kernels)
import qualified Futhark.Representation.ExplicitMemory as ExplicitMemory
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Representation.AST (Prog, pretty)
import Futhark.TypeCheck (Checkable)
import qualified Futhark.Util.Pretty as PP

import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.CSE
import Futhark.Optimise.Fusion
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.Simplify
import Futhark.Optimise.InPlaceLowering
import Futhark.Optimise.DoubleBuffer
import Futhark.Optimise.TileLoops
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.ExtractKernels
import Futhark.Pass.ExpandAllocations
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.CoalesceMemoryAccesses

import Futhark.Passes (standardPipeline)

data Config = Config { futharkConfig :: FutharkConfig
                     , futharkPipeline :: [UntypedPass]
                     , futharkAction :: UntypedAction
                     }

data UntypedPassState = SOACS (Prog SOACS.SOACS)
                      | Kernels (Prog Kernels.Kernels)
                      | ExplicitMemory (Prog ExplicitMemory.ExplicitMemory)

class Representation s where
  -- | A human-readable description of the representation expected or
  -- contained, usable for error messages.
  representation :: s -> String

instance Representation UntypedPassState where
  representation (SOACS _) = "SOACS"
  representation (Kernels _) = "Kernels"
  representation (ExplicitMemory _) = "ExplicitMemory"

instance PP.Pretty UntypedPassState where
  ppr (SOACS prog) = PP.ppr prog
  ppr (Kernels prog) = PP.ppr prog
  ppr (ExplicitMemory prog) = PP.ppr prog

data UntypedPass = UntypedPass (UntypedPassState
                                -> PipelineConfig
                                -> FutharkM UntypedPassState)

data UntypedAction = SOACSAction (Action SOACS)
                   | KernelsAction (Action Kernels)
                   | ExplicitMemoryAction (Action ExplicitMemory)
                   | PolyAction (Action SOACS) (Action Kernels) (Action ExplicitMemory)

untypedActionName :: UntypedAction -> String
untypedActionName (SOACSAction a) = actionName a
untypedActionName (KernelsAction a) = actionName a
untypedActionName (ExplicitMemoryAction a) = actionName a
untypedActionName (PolyAction a _ _) = actionName a

instance Representation UntypedAction where
  representation (SOACSAction _) = "SOACS"
  representation (KernelsAction _) = "Kernels"
  representation (ExplicitMemoryAction _) = "ExplicitMemory"
  representation PolyAction{} = "<any>"

newConfig :: Config
newConfig = Config newFutharkConfig [] $ PolyAction printAction printAction printAction

changeFutharkConfig :: (FutharkConfig -> FutharkConfig)
                    -> Config -> Config
changeFutharkConfig f cfg = cfg { futharkConfig = f $ futharkConfig cfg }

type FutharkOption = FunOptDescr Config

passOption :: String -> UntypedPass -> String -> [String] -> FutharkOption
passOption desc pass short long =
  Option short long
  (NoArg $ Right $ \cfg ->
   cfg { futharkPipeline = futharkPipeline cfg ++ [pass] })
  desc

explicitMemoryProg :: String -> UntypedPassState -> FutharkM (Prog ExplicitMemory.ExplicitMemory)
explicitMemoryProg _ (ExplicitMemory prog) =
  return prog
explicitMemoryProg name rep =
  compileError (T.pack $
                "Pass " ++ name ++
                " expects ExplicitMemory representation, but got " ++ representation rep) $
  pretty rep

soacsProg :: String -> UntypedPassState -> FutharkM (Prog SOACS.SOACS)
soacsProg _ (SOACS prog) =
  return prog
soacsProg name rep =
  compileError (T.pack $
                "Pass " ++ name ++
                " expects SOACS representation, but got " ++ representation rep) $
  pretty rep

kernelsProg :: String -> UntypedPassState -> FutharkM (Prog Kernels.Kernels)
kernelsProg _ (Kernels prog) =
  return prog
kernelsProg name rep =
  compileError (T.pack $
                "Pass " ++ name ++
                " expects Kernels representation, but got " ++ representation rep) $
  pretty rep

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

soacsPassOption :: Pass SOACS SOACS -> String -> FutharkOption
soacsPassOption =
  typedPassOption soacsProg SOACS

kernelsPassOption :: Pass Kernels Kernels -> String -> FutharkOption
kernelsPassOption =
  typedPassOption kernelsProg Kernels

explicitMemoryPassOption :: Pass ExplicitMemory ExplicitMemory -> String -> FutharkOption
explicitMemoryPassOption =
  typedPassOption explicitMemoryProg ExplicitMemory

simplifyOption :: String -> FutharkOption
simplifyOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where perform (SOACS prog) config =
          SOACS <$> runPasses (onePass simplifySOACS) config prog
        perform (Kernels prog) config =
          Kernels <$> runPasses (onePass simplifyKernels) config prog
        perform (ExplicitMemory prog) config =
          ExplicitMemory <$> runPasses (onePass simplifyExplicitMemory) config prog

        long = [passLongOption pass]
        pass = simplifySOACS

cseOption :: String -> FutharkOption
cseOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where perform (SOACS prog) config =
          SOACS <$> runPasses (onePass $ performCSE True) config prog
        perform (Kernels prog) config =
          Kernels <$> runPasses (onePass $ performCSE True) config prog
        perform (ExplicitMemory prog) config =
          ExplicitMemory <$> runPasses (onePass $ performCSE False) config prog

        long = [passLongOption pass]
        pass = performCSE True :: Pass SOACS SOACS

soacsPipelineOption :: String -> Pipeline SOACS SOACS -> String -> [String]
                    -> FutharkOption
soacsPipelineOption desc pipeline =
  passOption desc $ UntypedPass pipelinePass
  where pipelinePass (SOACS prog) config =
          SOACS <$> runPasses pipeline config prog
        pipelinePass rep _ =
          compileErrorS (T.pack $ "Expected SOACS representation, but got " ++
                         representation rep) $
          pretty rep

commandLineOptions :: [FutharkOption]
commandLineOptions =
  [ Option "V" ["verbose"]
    (OptArg (\file -> Right $ changeFutharkConfig $
                      \opts -> opts { futharkVerbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."

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
    (NoArg $ Right $ \opts -> opts { futharkAction = SOACSAction
                                                     interpretAction' })
    "Run the program via an interpreter."
     , Option [] ["range-analysis"]
       (NoArg $ Right $ \opts -> opts { futharkAction = PolyAction rangeAction rangeAction rangeAction })
       "Print the program with range annotations added."
  , Option "p" ["print"]
    (NoArg $ Right $ \opts -> opts { futharkAction = PolyAction printAction printAction printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."
  , typedPassOption soacsProg Kernels firstOrderTransform "f"
  , soacsPassOption fuseSOACs "o"
  , soacsPassOption inlineAggressively []
  , soacsPassOption removeDeadFunctions []
  , kernelsPassOption inPlaceLowering []
  , kernelsPassOption babysitKernels []
  , kernelsPassOption tileLoops []
  , typedPassOption soacsProg Kernels extractKernels []

  , typedPassOption kernelsProg ExplicitMemory explicitAllocations "a"

  , explicitMemoryPassOption doubleBuffer []
  , explicitMemoryPassOption expandAllocations []
  , explicitMemoryPassOption coalesceMemoryAccesses []

  , cseOption []
  , simplifyOption "e"

  , soacsPipelineOption "Run the default optimised pipeline"
    standardPipeline "s" ["standard"]
  ]

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: IO ()
main = mainWithOptions newConfig commandLineOptions compile
  where compile [file] config =
          Just $ do
            res <- runFutharkM (m file config) $
                   isJust $ futharkVerbose $ futharkConfig config
            case res of
              Left err -> do
                dumpError (futharkConfig config) err
                exitWith $ ExitFailure 2
              Right () -> return ()
        compile _      _      =
          Nothing
        m file config = do
          source <- liftIO $ T.readFile file
          prog <- runPipelineOnSource (futharkConfig config) id file source
          runPolyPasses config prog

runPolyPasses :: Config -> SOACS.Prog -> FutharkM ()
runPolyPasses config prog = do
    prog' <- foldM (runPolyPass pipeline_config) (SOACS prog) (futharkPipeline config)
    case (prog', futharkAction config) of
      (SOACS soacs_prog, SOACSAction action) ->
        actionProcedure action soacs_prog
      (Kernels kernels_prog, KernelsAction action) ->
        actionProcedure action kernels_prog
      (ExplicitMemory mem_prog, ExplicitMemoryAction action) ->
        actionProcedure action mem_prog

      (SOACS soacs_prog, PolyAction soacs_action _ _) ->
        actionProcedure soacs_action soacs_prog
      (Kernels kernels_prog, PolyAction _ kernels_action _) ->
        actionProcedure kernels_action kernels_prog
      (ExplicitMemory mem_prog, PolyAction _ _ mem_action) ->
        actionProcedure mem_action mem_prog

      (_, action) ->
        compileError (T.pack $ "Action " <>
                      untypedActionName action <>
                      " expects " ++ representation action ++ " representation, but got " ++
                     representation prog' ++ ".") $
        pretty prog'
  where pipeline_config =
          PipelineConfig { pipelineVerbose = isJust $ futharkVerbose $ futharkConfig config
                         , pipelineValidate = True
                         }

runPolyPass :: PipelineConfig
            -> UntypedPassState -> UntypedPass -> FutharkM UntypedPassState
runPolyPass pipeline_config s (UntypedPass f) =
  f s pipeline_config
