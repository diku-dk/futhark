{-# LANGUAGE RankNTypes #-}
-- | Futhark Compiler Driver
module Futhark.CLI.Dev (main) where

import Data.Maybe
import Data.List (intersperse)
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
import qualified Futhark.Representation.Kernels as Kernels
import qualified Futhark.Representation.Seq as Seq
import qualified Futhark.Representation.KernelsMem as KernelsMem
import qualified Futhark.Representation.SeqMem as SeqMem
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
import qualified Futhark.Pass.ExplicitAllocations.Kernels as Kernels
import qualified Futhark.Pass.ExplicitAllocations.Seq as Seq
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
                      | Seq (Prog Seq.Seq)
                      | KernelsMem (Prog KernelsMem.KernelsMem)
                      | SeqMem (Prog SeqMem.SeqMem)

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
  representation (Seq _) = "Seq"
  representation (KernelsMem _) = "KernelsMem"
  representation (SeqMem _) = "SeqMEm"

instance PP.Pretty UntypedPassState where
  ppr (SOACS prog) = PP.ppr prog
  ppr (Kernels prog) = PP.ppr prog
  ppr (Seq prog) = PP.ppr prog
  ppr (SeqMem prog) = PP.ppr prog
  ppr (KernelsMem prog) = PP.ppr prog

newtype UntypedPass = UntypedPass (UntypedPassState
                                  -> PipelineConfig
                                  -> FutharkM UntypedPassState)

data AllActions = AllActions
  { actionSOACS :: Action SOACS.SOACS
  , actionKernels :: Action Kernels.Kernels
  , actionSeq :: Action Seq.Seq
  , actionKernelsMem :: Action KernelsMem.KernelsMem
  , actionSeqMem :: Action SeqMem.SeqMem
  }

data UntypedAction = SOACSAction (Action SOACS.SOACS)
                   | KernelsAction (Action Kernels.Kernels)
                   | KernelsMemAction (Action KernelsMem.KernelsMem)
                   | SeqMemAction (Action SeqMem.SeqMem)
                   | PolyAction AllActions

untypedActionName :: UntypedAction -> String
untypedActionName (SOACSAction a) = actionName a
untypedActionName (KernelsAction a) = actionName a
untypedActionName (SeqMemAction a) = actionName a
untypedActionName (KernelsMemAction a) = actionName a
untypedActionName (PolyAction a) = actionName (actionSOACS a)

instance Representation UntypedAction where
  representation (SOACSAction _) = "SOACS"
  representation (KernelsAction _) = "Kernels"
  representation (KernelsMemAction _) = "KernelsMem"
  representation (SeqMemAction _) = "SeqMem"
  representation PolyAction{} = "<any>"

newConfig :: Config
newConfig = Config newFutharkConfig (Pipeline []) action False
  where action = PolyAction $ AllActions printAction printAction printAction printAction printAction

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

kernelsMemProg :: String -> UntypedPassState
               -> FutharkM (Prog KernelsMem.KernelsMem)
kernelsMemProg _ (KernelsMem prog) =
  return prog
kernelsMemProg name rep =
  externalErrorS $ "Pass " ++ name ++
  " expects KernelsMem representation, but got " ++ representation rep

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

soacsPassOption :: Pass SOACS.SOACS SOACS.SOACS -> String -> FutharkOption
soacsPassOption =
  typedPassOption soacsProg SOACS

kernelsPassOption :: Pass Kernels.Kernels Kernels.Kernels
                  -> String -> FutharkOption
kernelsPassOption =
  typedPassOption kernelsProg Kernels

kernelsMemPassOption :: Pass KernelsMem.KernelsMem KernelsMem.KernelsMem
                     -> String -> FutharkOption
kernelsMemPassOption =
  typedPassOption kernelsMemProg KernelsMem

simplifyOption :: String -> FutharkOption
simplifyOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where perform (SOACS prog) config =
          SOACS <$> runPasses (onePass simplifySOACS) config prog
        perform (Kernels prog) config =
          Kernels <$> runPasses (onePass simplifyKernels) config prog
        perform (Seq prog) config =
          Seq <$> runPasses (onePass simplifySeq) config prog
        perform (SeqMem prog) config =
          SeqMem <$> runPasses (onePass simplifySeqMem) config prog
        perform (KernelsMem prog) config =
          KernelsMem <$> runPasses (onePass simplifyKernelsMem) config prog

        long = [passLongOption pass]
        pass = simplifySOACS

allocateOption :: String -> FutharkOption
allocateOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where perform (Kernels prog) config =
          KernelsMem <$>
          runPasses (onePass Kernels.explicitAllocations) config prog
        perform (Seq prog) config =
          SeqMem <$>
          runPasses (onePass Seq.explicitAllocations) config prog
        perform s _ =
          externalErrorS $
          "Pass '" ++ passDescription pass ++ "' cannot operate on " ++ representation s

        long = [passLongOption pass]
        pass = Seq.explicitAllocations

cseOption :: String -> FutharkOption
cseOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where perform (SOACS prog) config =
          SOACS <$> runPasses (onePass $ performCSE True) config prog
        perform (Kernels prog) config =
          Kernels <$> runPasses (onePass $ performCSE True) config prog
        perform (Seq prog) config =
          Seq <$> runPasses (onePass $ performCSE True) config prog
        perform (SeqMem prog) config =
          SeqMem <$> runPasses (onePass $ performCSE False) config prog
        perform (KernelsMem prog) config =
          KernelsMem <$> runPasses (onePass $ performCSE False) config prog

        long = [passLongOption pass]
        pass = performCSE True :: Pass SOACS.SOACS SOACS.SOACS

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

soacsPipelineOption :: String -> Pipeline SOACS.SOACS SOACS.SOACS -> String -> [String]
                    -> FutharkOption
soacsPipelineOption = pipelineOption getSOACSProg "SOACS" SOACS

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
       opts { futharkAction = SeqMemAction impCodeGenAction })
    "Translate program into the imperative IL and write it on standard output."
  , Option [] ["compile-imperative-kernels"]
    (NoArg $ Right $ \opts ->
       opts { futharkAction = KernelsMemAction kernelImpCodeGenAction })
    "Translate program into the imperative IL with kernels and write it on standard output."
  , Option [] ["compile-imperative-multicore"]
    (NoArg $ Right $ \opts ->
       opts { futharkAction = KernelsMemAction multicoreImpCodeGenAction })
    "Translate program into the imperative IL with kernels and write it on standard output."
  , Option [] ["range-analysis"]
    (NoArg $ Right $ \opts ->
        opts { futharkAction = PolyAction $ AllActions rangeAction rangeAction rangeAction rangeAction rangeAction })
    "Print the program with range annotations added."
  , Option "p" ["print"]
    (NoArg $ Right $ \opts ->
        opts { futharkAction = PolyAction $ AllActions printAction printAction printAction printAction printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."
  , Option "m" ["metrics"]
    (NoArg $ Right $ \opts ->
        opts { futharkAction = PolyAction $ AllActions metricsAction metricsAction metricsAction metricsAction metricsAction })
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
  , typedPassOption soacsProg Seq firstOrderTransform "f"
  , soacsPassOption fuseSOACs "o"
  , soacsPassOption inlineFunctions []
  , kernelsPassOption inPlaceLoweringKernels []
  , kernelsPassOption babysitKernels []
  , kernelsPassOption tileLoops []
  , kernelsPassOption unstream []
  , kernelsPassOption sink []
  , typedPassOption soacsProg Kernels extractKernels []

  , allocateOption "a"

  , kernelsMemPassOption doubleBuffer []
  , kernelsMemPassOption expandAllocations []

  , cseOption []
  , simplifyOption "e"

  , soacsPipelineOption "Run the default optimised pipeline"
    standardPipeline "s" ["standard"]
  , pipelineOption getSOACSProg "Kernels" Kernels
    "Run the default optimised kernels pipeline"
    kernelsPipeline [] ["kernels"]
  , pipelineOption getSOACSProg "KernelsMem" KernelsMem
    "Run the full GPU compilation pipeline"
    gpuPipeline [] ["gpu"]
  , pipelineOption getSOACSProg "KernelsMem" SeqMem
    "Run the sequential CPU compilation pipeline"
    sequentialCpuPipeline [] ["cpu"]
  , pipelineOption getSOACSProg "KernelsMem" KernelsMem
    "Run the multicore compilation pipeline"
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

runPolyPasses :: Config -> Prog SOACS.SOACS -> FutharkM ()
runPolyPasses config initial_prog = do
    end_prog <- foldM (runPolyPass pipeline_config) (SOACS initial_prog)
                (getFutharkPipeline config)
    case (end_prog, futharkAction config) of
      (SOACS prog, SOACSAction action) ->
        actionProcedure action prog
      (Kernels prog, KernelsAction action) ->
        actionProcedure action prog
      (SeqMem prog, SeqMemAction action) ->
        actionProcedure action prog
      (KernelsMem prog, KernelsMemAction action) ->
        actionProcedure action prog

      (SOACS soacs_prog, PolyAction acs) ->
        actionProcedure (actionSOACS acs) soacs_prog
      (Kernels kernels_prog, PolyAction acs) ->
        actionProcedure (actionKernels acs) kernels_prog
      (Seq seq_prog, PolyAction acs) ->
        actionProcedure (actionSeq acs) seq_prog
      (KernelsMem mem_prog, PolyAction acs) ->
        actionProcedure (actionKernelsMem acs) mem_prog
      (SeqMem mem_prog, PolyAction acs) ->
        actionProcedure (actionSeqMem acs) mem_prog

      (_, action) ->
        externalErrorS $ "Action " <>
        untypedActionName action <>
        " expects " ++ representation action ++ " representation, but got " ++
        representation end_prog ++ "."
  where pipeline_config =
          PipelineConfig { pipelineVerbose = fst (futharkVerbose $ futharkConfig config) > NotVerbose
                         , pipelineValidate = True
                         }

runPolyPass :: PipelineConfig
            -> UntypedPassState -> UntypedPass -> FutharkM UntypedPassState
runPolyPass pipeline_config s (UntypedPass f) =
  f s pipeline_config
