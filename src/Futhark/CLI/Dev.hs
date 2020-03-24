{-# LANGUAGE RankNTypes #-}
-- | Futhark Compiler Driver
module Futhark.CLI.Dev (main) where

import Data.Maybe
import Data.List
import Control.Category (id)
import Control.Monad
import Control.Monad.State
import qualified Data.Text.IO as T
import System.IO
import System.Exit
import System.Console.GetOpt

import Prelude hiding (id)

import Futhark.Pass
import Futhark.Actions
import Futhark.Compiler
import Language.Futhark.Parser (parseFuthark)
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

import Futhark.Internalise.Defunctorise as Defunctorise
import Futhark.Internalise.Monomorphise as Monomorphise
import Futhark.Internalise.Defunctionalise as Defunctionalise
import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.CSE
import Futhark.Optimise.Fusion
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.Simplify
import Futhark.Optimise.InPlaceLowering
import Futhark.Optimise.DoubleBuffer
import Futhark.Optimise.Sink
import Futhark.Optimise.TileLoops
import Futhark.Optimise.Unstream
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.ExtractKernels
import Futhark.Pass.ExpandAllocations
import Futhark.Pass.ExplicitAllocations
import Futhark.Passes

-- | What to do with the program after it has been read.
data FutharkPipeline = PrettyPrint
                     -- ^ Just print it.
                     | TypeCheck
                     -- ^ Run the type checker; print type errors.
                     | Pipeline [UntypedPass]
                     -- ^ Run this pipeline.
                     | Defunctorise
                     -- ^ Partially evaluate away the module language.
                     | Monomorphise
                     -- ^ Defunctorise and monomorphise.
                     | Defunctionalise
                     -- ^ Defunctorise, monomorphise, and defunctionalise.

data Config = Config { futharkConfig :: FutharkConfig
                     , futharkPipeline :: FutharkPipeline
                     -- ^ Nothing is distinct from a empty pipeline -
                     -- it means we don't even run the internaliser.
                     , futharkAction :: UntypedAction
                     , futharkPrintAST :: Bool
                     -- ^ If true, prints programs as raw ASTs instead
                     -- of their prettyprinted form.
                     }


-- | Get a Futhark pipeline from the configuration - an empty one if
-- none exists.
getFutharkPipeline :: Config -> [UntypedPass]
getFutharkPipeline = toPipeline . futharkPipeline
  where toPipeline (Pipeline p) = p
        toPipeline _            = []

data UntypedPassState = SOACS (Prog SOACS.SOACS)
                      | Kernels (Prog Kernels.Kernels)
                      | ExplicitMemory (Prog ExplicitMemory.ExplicitMemory)

getSOACSProg :: UntypedPassState -> Maybe (Prog SOACS.SOACS)
getSOACSProg (SOACS prog) = Just prog
getSOACSProg _            = Nothing

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

newtype UntypedPass = UntypedPass (UntypedPassState
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
newConfig = Config newFutharkConfig (Pipeline [])
            (PolyAction printAction printAction printAction) False

changeFutharkConfig :: (FutharkConfig -> FutharkConfig)
                    -> Config -> Config
changeFutharkConfig f cfg = cfg { futharkConfig = f $ futharkConfig cfg }

type FutharkOption = FunOptDescr Config

passOption :: String -> UntypedPass -> String -> [String] -> FutharkOption
passOption desc pass short long =
  Option short long
  (NoArg $ Right $ \cfg ->
   cfg { futharkPipeline = Pipeline $ getFutharkPipeline cfg ++ [pass] })
  desc

explicitMemoryProg :: String -> UntypedPassState -> FutharkM (Prog ExplicitMemory.ExplicitMemory)
explicitMemoryProg _ (ExplicitMemory prog) =
  return prog
explicitMemoryProg name rep =
  externalErrorS $ "Pass " ++ name ++
  " expects ExplicitMemory representation, but got " ++ representation rep

soacsProg :: String -> UntypedPassState -> FutharkM (Prog SOACS.SOACS)
soacsProg _ (SOACS prog) =
  return prog
soacsProg name rep =
  externalErrorS $ "Pass " ++ name ++
  " expects SOACS representation, but got " ++ representation rep

kernelsProg :: String -> UntypedPassState -> FutharkM (Prog Kernels.Kernels)
kernelsProg _ (Kernels prog) =
  return prog
kernelsProg name rep =
  externalErrorS $
  "Pass " ++ name ++" expects Kernels representation, but got " ++ representation rep

typedPassOption :: (Checkable fromlore, Checkable tolore) =>
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

pipelineOption :: (UntypedPassState -> Maybe (Prog fromlore))
               -> String
               -> (Prog tolore -> UntypedPassState)
               -> String
               -> Pipeline fromlore tolore
               -> String
               -> [String]
               -> FutharkOption
pipelineOption getprog repdesc repf desc pipeline =
  passOption desc $ UntypedPass pipelinePass
  where pipelinePass rep config =
          case getprog rep of
            Just prog ->
              repf <$> runPasses pipeline config prog
            Nothing   ->
              externalErrorS $ "Expected " ++ repdesc ++ " representation, but got " ++
              representation rep

soacsPipelineOption :: String -> Pipeline SOACS SOACS -> String -> [String]
                    -> FutharkOption
soacsPipelineOption = pipelineOption getSOACSProg "SOACS" SOACS

kernelsPipelineOption :: String -> Pipeline SOACS Kernels -> String -> [String]
                    -> FutharkOption
kernelsPipelineOption = pipelineOption getSOACSProg "Kernels" Kernels

explicitMemoryPipelineOption :: String -> Pipeline SOACS ExplicitMemory -> String -> [String]
                             -> FutharkOption
explicitMemoryPipelineOption = pipelineOption getSOACSProg "ExplicitMemory" ExplicitMemory

commandLineOptions :: [FutharkOption]
commandLineOptions =
  [ Option "v" ["verbose"]
    (OptArg (Right . changeFutharkConfig . incVerbosity) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["Werror"]
    (NoArg $ Right $ changeFutharkConfig $ \opts -> opts { futharkWerror = True })
    "Treat warnings as errors."

  , Option "t" ["type-check"]
    (NoArg $ Right $ \opts ->
        opts { futharkPipeline = TypeCheck })
    "Print on standard output the type-checked program."

  , Option [] ["pretty-print"]
    (NoArg $ Right $ \opts ->
        opts { futharkPipeline = PrettyPrint })
    "Parse and pretty-print the AST of the given program."

  , Option [] ["compile-imperative"]
    (NoArg $ Right $ \opts ->
       opts { futharkAction = ExplicitMemoryAction impCodeGenAction })
    "Translate program into the imperative IL and write it on standard output."
  , Option [] ["compile-imperative-kernels"]
    (NoArg $ Right $ \opts ->
       opts { futharkAction = ExplicitMemoryAction kernelImpCodeGenAction })
    "Translate program into the imperative IL with kernels and write it on standard output."
  , Option [] ["compile-imperative-multicore"]
    (NoArg $ Right $ \opts ->
       opts { futharkAction = ExplicitMemoryAction multicoreImpCodeGenAction })
    "Translate program into the imperative IL with kernels and write it on standard output."
  , Option [] ["range-analysis"]
       (NoArg $ Right $ \opts -> opts { futharkAction = PolyAction rangeAction rangeAction rangeAction })
       "Print the program with range annotations added."
  , Option "p" ["print"]
    (NoArg $ Right $ \opts -> opts { futharkAction = PolyAction printAction printAction printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."
  , Option "m" ["metrics"]
    (NoArg $ Right $ \opts -> opts { futharkAction = PolyAction metricsAction metricsAction metricsAction })
    "Print AST metrics of the resulting internal representation on standard output."
  , Option [] ["defunctorise"]
    (NoArg $ Right $ \opts -> opts { futharkPipeline = Defunctorise })
    "Partially evaluate all module constructs and print the residual program."
  , Option [] ["monomorphise"]
    (NoArg $ Right $ \opts -> opts { futharkPipeline = Monomorphise })
    "Monomorphise the program."
  , Option [] ["defunctionalise"]
    (NoArg $ Right $ \opts -> opts { futharkPipeline = Defunctionalise })
    "Defunctionalise the program."
  , Option [] ["ast"]
    (NoArg $ Right $ \opts -> opts { futharkPrintAST = True })
    "Output ASTs instead of prettyprinted programs."
  , Option [] ["safe"]
    (NoArg $ Right $ changeFutharkConfig $ \opts -> opts { futharkSafe = True })
    "Ignore 'unsafe'."
  , typedPassOption soacsProg Kernels firstOrderTransform "f"
  , soacsPassOption fuseSOACs "o"
  , soacsPassOption inlineFunctions []
  , soacsPassOption inlineConstants []
  , kernelsPassOption inPlaceLowering []
  , kernelsPassOption babysitKernels []
  , kernelsPassOption tileLoops []
  , kernelsPassOption unstream []
  , kernelsPassOption sink []
  , typedPassOption soacsProg Kernels extractKernels []

  , typedPassOption kernelsProg ExplicitMemory explicitAllocations "a"

  , explicitMemoryPassOption doubleBuffer []
  , explicitMemoryPassOption expandAllocations []

  , cseOption []
  , simplifyOption "e"

  , soacsPipelineOption "Run the default optimised pipeline"
    standardPipeline "s" ["standard"]
  , kernelsPipelineOption "Run the default optimised kernels pipeline"
    kernelsPipeline [] ["kernels"]
  , explicitMemoryPipelineOption "Run the full GPU compilation pipeline"
    gpuPipeline [] ["gpu"]
  , explicitMemoryPipelineOption "Run the sequential CPU compilation pipeline"
    sequentialCpuPipeline [] ["cpu"]
  , explicitMemoryPipelineOption "Run the multicore compilation pipeline"
    multicorePipeline [] ["multicore"]
  ]

incVerbosity :: Maybe FilePath -> FutharkConfig -> FutharkConfig
incVerbosity file cfg =
  cfg { futharkVerbose = (v, file `mplus` snd (futharkVerbose cfg)) }
  where v = case fst $ futharkVerbose cfg of
              NotVerbose -> Verbose
              Verbose -> VeryVerbose
              VeryVerbose -> VeryVerbose

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: String -> [String] -> IO ()
main = mainWithOptions newConfig commandLineOptions "options... program" compile
  where compile [file] config =
          Just $ do
            res <- runFutharkM (m file config) $
                   fst $ futharkVerbose $ futharkConfig config
            case res of
              Left err -> do
                dumpError (futharkConfig config) err
                exitWith $ ExitFailure 2
              Right () -> return ()
        compile _      _      =
          Nothing
        m file config = do
          let p :: (Show a, PP.Pretty a) => [a] -> IO ()
              p = mapM_ putStrLn .
                  intersperse "" .
                  map (if futharkPrintAST config then show else pretty)

          case futharkPipeline config of
            PrettyPrint -> liftIO $ do
              maybe_prog <- parseFuthark file <$> T.readFile file
              case maybe_prog of
                Left err  -> fail $ show err
                Right prog | futharkPrintAST config -> print prog
                           | otherwise -> putStrLn $ pretty prog
            TypeCheck -> do
              (_, imports, _) <- readProgram file
              liftIO $ forM_ (map snd imports) $ \fm ->
                putStrLn $ if futharkPrintAST config
                           then show $ fileProg fm
                           else pretty $ fileProg fm
            Defunctorise -> do
              (_, imports, src) <- readProgram file
              liftIO $ p $ evalState (Defunctorise.transformProg imports) src
            Monomorphise -> do
              (_, imports, src) <- readProgram file
              liftIO $ p $
                flip evalState src $
                Defunctorise.transformProg imports
                >>= Monomorphise.transformProg
            Defunctionalise -> do
              (_, imports, src) <- readProgram file
              liftIO $ p $
                flip evalState src $
                Defunctorise.transformProg imports
                >>= Monomorphise.transformProg
                >>= Defunctionalise.transformProg
            Pipeline{} -> do
              prog <- runPipelineOnProgram (futharkConfig config) id file
              runPolyPasses config prog

runPolyPasses :: Config -> Prog SOACS -> FutharkM ()
runPolyPasses config prog = do
    prog' <- foldM (runPolyPass pipeline_config) (SOACS prog) (getFutharkPipeline config)
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
        externalErrorS $ "Action " <>
        untypedActionName action <>
        " expects " ++ representation action ++ " representation, but got " ++
        representation prog' ++ "."
  where pipeline_config =
          PipelineConfig { pipelineVerbose = fst (futharkVerbose $ futharkConfig config) > NotVerbose
                         , pipelineValidate = True
                         }

runPolyPass :: PipelineConfig
            -> UntypedPassState -> UntypedPass -> FutharkM UntypedPassState
runPolyPass pipeline_config s (UntypedPass f) =
  f s pipeline_config
