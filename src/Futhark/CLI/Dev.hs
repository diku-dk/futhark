{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Futhark Compiler Driver
module Futhark.CLI.Dev (main) where

import Control.Category (id)
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as ByteString
import Data.List (intersperse)
import Data.Maybe
import qualified Data.Text.IO as T
import Futhark.Actions
import Futhark.Analysis.Metrics (OpMetrics)
import Futhark.Compiler.CLI
import Futhark.IR (ASTLore, Op, Prog, pretty)
import qualified Futhark.IR.Kernels as Kernels
import qualified Futhark.IR.KernelsMem as KernelsMem
import qualified Futhark.IR.MC as MC
import qualified Futhark.IR.MCMem as MCMem
import Futhark.IR.Prop.Aliases (CanBeAliased)
import qualified Futhark.IR.SOACS as SOACS
import qualified Futhark.IR.Seq as Seq
import qualified Futhark.IR.SeqMem as SeqMem
import Futhark.Internalise.Defunctionalise as Defunctionalise
import Futhark.Internalise.Defunctorise as Defunctorise
import Futhark.Internalise.LiftLambdas as LiftLambdas
import Futhark.Internalise.Monomorphise as Monomorphise
import Futhark.Optimise.CSE
import Futhark.Optimise.DoubleBuffer
import Futhark.Optimise.Fusion
import Futhark.Optimise.InPlaceLowering
import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.Sink
import Futhark.Optimise.TileLoops
import Futhark.Optimise.Unstream
import Futhark.Pass
import Futhark.Pass.ExpandAllocations
import qualified Futhark.Pass.ExplicitAllocations.Kernels as Kernels
import qualified Futhark.Pass.ExplicitAllocations.Seq as Seq
import Futhark.Pass.ExtractKernels
import Futhark.Pass.ExtractMulticore
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.Simplify
import Futhark.Passes
import Futhark.TypeCheck (Checkable)
import Futhark.Util.Log
import Futhark.Util.Options
import qualified Futhark.Util.Pretty as PP
import Language.Futhark.Parser (parseFuthark)
import qualified Language.SexpGrammar as Sexp
import System.Exit
import System.FilePath
import System.IO
import Prelude hiding (id)

-- | What to do with the program after it has been read.
data FutharkPipeline
  = -- | Just print it.
    PrettyPrint
  | -- | Run the type checker; print type errors.
    TypeCheck
  | -- | Run this pipeline.
    Pipeline [UntypedPass]
  | -- | Partially evaluate away the module language.
    Defunctorise
  | -- | Defunctorise and monomorphise.
    Monomorphise
  | -- | Defunctorise, monomorphise, and lambda-lift.
    LiftLambdas
  | -- | Defunctorise, monomorphise, lambda-lift, and defunctionalise.
    Defunctionalise

data Config = Config
  { futharkConfig :: FutharkConfig,
    -- | Nothing is distinct from a empty pipeline -
    -- it means we don't even run the internaliser.
    futharkPipeline :: FutharkPipeline,
    futharkAction :: UntypedAction,
    -- | If true, prints programs as raw ASTs instead
    -- of their prettyprinted form.
    futharkPrintAST :: Bool
  }

-- | Get a Futhark pipeline from the configuration - an empty one if
-- none exists.
getFutharkPipeline :: Config -> [UntypedPass]
getFutharkPipeline = toPipeline . futharkPipeline
  where
    toPipeline (Pipeline p) = p
    toPipeline _ = []

data UntypedPassState
  = SOACS (Prog SOACS.SOACS)
  | Kernels (Prog Kernels.Kernels)
  | MC (Prog MC.MC)
  | Seq (Prog Seq.Seq)
  | KernelsMem (Prog KernelsMem.KernelsMem)
  | MCMem (Prog MCMem.MCMem)
  | SeqMem (Prog SeqMem.SeqMem)

getSOACSProg :: UntypedPassState -> Maybe (Prog SOACS.SOACS)
getSOACSProg (SOACS prog) = Just prog
getSOACSProg _ = Nothing

class Representation s where
  -- | A human-readable description of the representation expected or
  -- contained, usable for error messages.
  representation :: s -> String

instance Representation UntypedPassState where
  representation (SOACS _) = "SOACS"
  representation (Kernels _) = "Kernels"
  representation (MC _) = "MC"
  representation (Seq _) = "Seq"
  representation (KernelsMem _) = "KernelsMem"
  representation (MCMem _) = "MCMem"
  representation (SeqMem _) = "SeqMEm"

instance PP.Pretty UntypedPassState where
  ppr (SOACS prog) = PP.ppr prog
  ppr (Kernels prog) = PP.ppr prog
  ppr (MC prog) = PP.ppr prog
  ppr (Seq prog) = PP.ppr prog
  ppr (SeqMem prog) = PP.ppr prog
  ppr (MCMem prog) = PP.ppr prog
  ppr (KernelsMem prog) = PP.ppr prog

newtype UntypedPass
  = UntypedPass
      ( UntypedPassState ->
        PipelineConfig ->
        FutharkM UntypedPassState
      )

data UntypedAction
  = SOACSAction (Action SOACS.SOACS)
  | KernelsAction (Action Kernels.Kernels)
  | KernelsMemAction (FilePath -> Action KernelsMem.KernelsMem)
  | MCMemAction (FilePath -> Action MCMem.MCMem)
  | SeqMemAction (FilePath -> Action SeqMem.SeqMem)
  | PolyAction
      ( forall lore.
        ( ASTLore lore,
          (CanBeAliased (Op lore)),
          (OpMetrics (Op lore))
        ) =>
        Action lore
      )

untypedActionName :: UntypedAction -> String
untypedActionName (SOACSAction a) = actionName a
untypedActionName (KernelsAction a) = actionName a
untypedActionName (SeqMemAction a) = actionName $ a ""
untypedActionName (KernelsMemAction a) = actionName $ a ""
untypedActionName (MCMemAction a) = actionName $ a ""
untypedActionName (PolyAction a) = actionName (a :: Action SOACS.SOACS)

instance Representation UntypedAction where
  representation (SOACSAction _) = "SOACS"
  representation (KernelsAction _) = "Kernels"
  representation (KernelsMemAction _) = "KernelsMem"
  representation (MCMemAction _) = "MCMem"
  representation (SeqMemAction _) = "SeqMem"
  representation PolyAction {} = "<any>"

newConfig :: Config
newConfig = Config newFutharkConfig (Pipeline []) action False
  where
    action = PolyAction printAction

changeFutharkConfig ::
  (FutharkConfig -> FutharkConfig) ->
  Config ->
  Config
changeFutharkConfig f cfg = cfg {futharkConfig = f $ futharkConfig cfg}

type FutharkOption = FunOptDescr Config

passOption :: String -> UntypedPass -> String -> [String] -> FutharkOption
passOption desc pass short long =
  Option
    short
    long
    ( NoArg $
        Right $ \cfg ->
          cfg {futharkPipeline = Pipeline $ getFutharkPipeline cfg ++ [pass]}
    )
    desc

kernelsMemProg ::
  String ->
  UntypedPassState ->
  FutharkM (Prog KernelsMem.KernelsMem)
kernelsMemProg _ (KernelsMem prog) =
  return prog
kernelsMemProg name rep =
  externalErrorS $
    "Pass " ++ name
      ++ " expects KernelsMem representation, but got "
      ++ representation rep

soacsProg :: String -> UntypedPassState -> FutharkM (Prog SOACS.SOACS)
soacsProg _ (SOACS prog) =
  return prog
soacsProg name rep =
  externalErrorS $
    "Pass " ++ name
      ++ " expects SOACS representation, but got "
      ++ representation rep

kernelsProg :: String -> UntypedPassState -> FutharkM (Prog Kernels.Kernels)
kernelsProg _ (Kernels prog) =
  return prog
kernelsProg name rep =
  externalErrorS $
    "Pass " ++ name ++ " expects Kernels representation, but got " ++ representation rep

typedPassOption ::
  Checkable tolore =>
  (String -> UntypedPassState -> FutharkM (Prog fromlore)) ->
  (Prog tolore -> UntypedPassState) ->
  Pass fromlore tolore ->
  String ->
  FutharkOption
typedPassOption getProg putProg pass short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where
    perform s config = do
      prog <- getProg (passName pass) s
      putProg <$> runPipeline (onePass pass) config prog

    long = [passLongOption pass]

soacsPassOption :: Pass SOACS.SOACS SOACS.SOACS -> String -> FutharkOption
soacsPassOption =
  typedPassOption soacsProg SOACS

kernelsPassOption ::
  Pass Kernels.Kernels Kernels.Kernels ->
  String ->
  FutharkOption
kernelsPassOption =
  typedPassOption kernelsProg Kernels

kernelsMemPassOption ::
  Pass KernelsMem.KernelsMem KernelsMem.KernelsMem ->
  String ->
  FutharkOption
kernelsMemPassOption =
  typedPassOption kernelsMemProg KernelsMem

simplifyOption :: String -> FutharkOption
simplifyOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where
    perform (SOACS prog) config =
      SOACS <$> runPipeline (onePass simplifySOACS) config prog
    perform (Kernels prog) config =
      Kernels <$> runPipeline (onePass simplifyKernels) config prog
    perform (MC prog) config =
      MC <$> runPipeline (onePass simplifyMC) config prog
    perform (Seq prog) config =
      Seq <$> runPipeline (onePass simplifySeq) config prog
    perform (SeqMem prog) config =
      SeqMem <$> runPipeline (onePass simplifySeqMem) config prog
    perform (KernelsMem prog) config =
      KernelsMem <$> runPipeline (onePass simplifyKernelsMem) config prog
    perform (MCMem prog) config =
      MCMem <$> runPipeline (onePass simplifyMCMem) config prog

    long = [passLongOption pass]
    pass = simplifySOACS

allocateOption :: String -> FutharkOption
allocateOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where
    perform (Kernels prog) config =
      KernelsMem
        <$> runPipeline (onePass Kernels.explicitAllocations) config prog
    perform (Seq prog) config =
      SeqMem
        <$> runPipeline (onePass Seq.explicitAllocations) config prog
    perform s _ =
      externalErrorS $
        "Pass '" ++ passDescription pass ++ "' cannot operate on " ++ representation s

    long = [passLongOption pass]
    pass = Seq.explicitAllocations

iplOption :: String -> FutharkOption
iplOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where
    perform (Kernels prog) config =
      Kernels
        <$> runPipeline (onePass inPlaceLoweringKernels) config prog
    perform (Seq prog) config =
      Seq
        <$> runPipeline (onePass inPlaceLoweringSeq) config prog
    perform s _ =
      externalErrorS $
        "Pass '" ++ passDescription pass ++ "' cannot operate on " ++ representation s

    long = [passLongOption pass]
    pass = inPlaceLoweringSeq

cseOption :: String -> FutharkOption
cseOption short =
  passOption (passDescription pass) (UntypedPass perform) short long
  where
    perform (SOACS prog) config =
      SOACS <$> runPipeline (onePass $ performCSE True) config prog
    perform (Kernels prog) config =
      Kernels <$> runPipeline (onePass $ performCSE True) config prog
    perform (MC prog) config =
      MC <$> runPipeline (onePass $ performCSE True) config prog
    perform (Seq prog) config =
      Seq <$> runPipeline (onePass $ performCSE True) config prog
    perform (SeqMem prog) config =
      SeqMem <$> runPipeline (onePass $ performCSE False) config prog
    perform (KernelsMem prog) config =
      KernelsMem <$> runPipeline (onePass $ performCSE False) config prog
    perform (MCMem prog) config =
      MCMem <$> runPipeline (onePass $ performCSE False) config prog

    long = [passLongOption pass]
    pass = performCSE True :: Pass SOACS.SOACS SOACS.SOACS

pipelineOption ::
  (UntypedPassState -> Maybe (Prog fromlore)) ->
  String ->
  (Prog tolore -> UntypedPassState) ->
  String ->
  Pipeline fromlore tolore ->
  String ->
  [String] ->
  FutharkOption
pipelineOption getprog repdesc repf desc pipeline =
  passOption desc $ UntypedPass pipelinePass
  where
    pipelinePass rep config =
      case getprog rep of
        Just prog ->
          repf <$> runPipeline pipeline config prog
        Nothing ->
          externalErrorS $
            "Expected " ++ repdesc ++ " representation, but got "
              ++ representation rep

soacsPipelineOption ::
  String ->
  Pipeline SOACS.SOACS SOACS.SOACS ->
  String ->
  [String] ->
  FutharkOption
soacsPipelineOption = pipelineOption getSOACSProg "SOACS" SOACS

commandLineOptions :: [FutharkOption]
commandLineOptions =
  [ Option
      "v"
      ["verbose"]
      (OptArg (Right . changeFutharkConfig . incVerbosity) "FILE")
      "Print verbose output on standard error; wrong program to FILE.",
    Option
      []
      ["Werror"]
      (NoArg $ Right $ changeFutharkConfig $ \opts -> opts {futharkWerror = True})
      "Treat warnings as errors.",
    Option
      "t"
      ["type-check"]
      ( NoArg $
          Right $ \opts ->
            opts {futharkPipeline = TypeCheck}
      )
      "Print on standard output the type-checked program.",
    Option
      []
      ["pretty-print"]
      ( NoArg $
          Right $ \opts ->
            opts {futharkPipeline = PrettyPrint}
      )
      "Parse and pretty-print the AST of the given program.",
    Option
      []
      ["compile-imperative"]
      ( NoArg $
          Right $ \opts ->
            opts {futharkAction = SeqMemAction $ const impCodeGenAction}
      )
      "Translate program into the imperative IL and write it on standard output.",
    Option
      []
      ["compile-imperative-kernels"]
      ( NoArg $
          Right $ \opts ->
            opts {futharkAction = KernelsMemAction $ const kernelImpCodeGenAction}
      )
      "Translate program into the imperative IL with kernels and write it on standard output.",
    Option
      []
      ["compile-imperative-multicore"]
      ( NoArg $
          Right $ \opts ->
            opts {futharkAction = MCMemAction $ const multicoreImpCodeGenAction}
      )
      "Translate program into the imperative IL with kernels and write it on standard output.",
    Option
      []
      ["compile-opencl"]
      ( NoArg $
          Right $ \opts ->
            opts {futharkAction = KernelsMemAction $ compileOpenCLAction newFutharkConfig ToExecutable}
      )
      "Compile the program using the OpenCL backend.",
    Option
      []
      ["compile-c"]
      ( NoArg $
          Right $ \opts ->
            opts {futharkAction = SeqMemAction $ compileCAction newFutharkConfig ToExecutable}
      )
      "Compile the program using the C backend.",
    Option
      "p"
      ["print"]
      (NoArg $ Right $ \opts -> opts {futharkAction = PolyAction printAction})
      "Prettyprint the resulting internal representation on standard output (default action).",
    Option
      "m"
      ["metrics"]
      (NoArg $ Right $ \opts -> opts {futharkAction = PolyAction metricsAction})
      "Print AST metrics of the resulting internal representation on standard output.",
    Option
      []
      ["sexp"]
      (NoArg $ Right $ \opts -> opts {futharkAction = PolyAction sexpAction})
      "Print the resulting IR as S-expressions to standard output.",
    Option
      []
      ["defunctorise"]
      (NoArg $ Right $ \opts -> opts {futharkPipeline = Defunctorise})
      "Partially evaluate all module constructs and print the residual program.",
    Option
      []
      ["monomorphise"]
      (NoArg $ Right $ \opts -> opts {futharkPipeline = Monomorphise})
      "Monomorphise the program.",
    Option
      []
      ["lift-lambdas"]
      (NoArg $ Right $ \opts -> opts {futharkPipeline = LiftLambdas})
      "Lambda-lift the program.",
    Option
      []
      ["defunctionalise"]
      (NoArg $ Right $ \opts -> opts {futharkPipeline = Defunctionalise})
      "Defunctionalise the program.",
    Option
      []
      ["ast"]
      (NoArg $ Right $ \opts -> opts {futharkPrintAST = True})
      "Output ASTs instead of prettyprinted programs.",
    Option
      []
      ["safe"]
      (NoArg $ Right $ changeFutharkConfig $ \opts -> opts {futharkSafe = True})
      "Ignore 'unsafe'.",
    typedPassOption soacsProg Seq firstOrderTransform "f",
    soacsPassOption fuseSOACs "o",
    soacsPassOption inlineFunctions [],
    kernelsPassOption babysitKernels [],
    kernelsPassOption tileLoops [],
    kernelsPassOption unstreamKernels [],
    kernelsPassOption sinkKernels [],
    typedPassOption soacsProg Kernels extractKernels [],
    typedPassOption soacsProg MC extractMulticore [],
    iplOption [],
    allocateOption "a",
    kernelsMemPassOption doubleBufferKernels [],
    kernelsMemPassOption expandAllocations [],
    cseOption [],
    simplifyOption "e",
    soacsPipelineOption
      "Run the default optimised pipeline"
      standardPipeline
      "s"
      ["standard"],
    pipelineOption
      getSOACSProg
      "Kernels"
      Kernels
      "Run the default optimised kernels pipeline"
      kernelsPipeline
      []
      ["kernels"],
    pipelineOption
      getSOACSProg
      "KernelsMem"
      KernelsMem
      "Run the full GPU compilation pipeline"
      gpuPipeline
      []
      ["gpu"],
    pipelineOption
      getSOACSProg
      "KernelsMem"
      SeqMem
      "Run the sequential CPU compilation pipeline"
      sequentialCpuPipeline
      []
      ["cpu"],
    pipelineOption
      getSOACSProg
      "MCMem"
      MCMem
      "Run the multicore compilation pipeline"
      multicorePipeline
      []
      ["multicore"]
  ]

incVerbosity :: Maybe FilePath -> FutharkConfig -> FutharkConfig
incVerbosity file cfg =
  cfg {futharkVerbose = (v, file `mplus` snd (futharkVerbose cfg))}
  where
    v = case fst $ futharkVerbose cfg of
      NotVerbose -> Verbose
      Verbose -> VeryVerbose
      VeryVerbose -> VeryVerbose

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: String -> [String] -> IO ()
main = mainWithOptions newConfig commandLineOptions "options... program" compile
  where
    compile [file] config =
      Just $ do
        res <-
          runFutharkM (m file config) $
            fst $ futharkVerbose $ futharkConfig config
        case res of
          Left err -> do
            dumpError (futharkConfig config) err
            exitWith $ ExitFailure 2
          Right () -> return ()
    compile _ _ =
      Nothing
    m file config = do
      let p :: (Show a, PP.Pretty a) => [a] -> IO ()
          p =
            mapM_ putStrLn
              . intersperse ""
              . map (if futharkPrintAST config then show else pretty)

      case futharkPipeline config of
        PrettyPrint -> liftIO $ do
          maybe_prog <- parseFuthark file <$> T.readFile file
          case maybe_prog of
            Left err -> fail $ show err
            Right prog
              | futharkPrintAST config -> print prog
              | otherwise -> putStrLn $ pretty prog
        TypeCheck -> do
          (_, imports, _) <- readProgram file
          liftIO $
            forM_ (map snd imports) $ \fm ->
              putStrLn $
                if futharkPrintAST config
                  then show $ fileProg fm
                  else pretty $ fileProg fm
        Defunctorise -> do
          (_, imports, src) <- readProgram file
          liftIO $ p $ evalState (Defunctorise.transformProg imports) src
        Monomorphise -> do
          (_, imports, src) <- readProgram file
          liftIO $
            p $
              flip evalState src $
                Defunctorise.transformProg imports
                  >>= Monomorphise.transformProg
        LiftLambdas -> do
          (_, imports, src) <- readProgram file
          liftIO $
            p $
              flip evalState src $
                Defunctorise.transformProg imports
                  >>= Monomorphise.transformProg
                  >>= LiftLambdas.transformProg
        Defunctionalise -> do
          (_, imports, src) <- readProgram file
          liftIO $
            p $
              flip evalState src $
                Defunctorise.transformProg imports
                  >>= Monomorphise.transformProg
                  >>= LiftLambdas.transformProg
                  >>= Defunctionalise.transformProg
        Pipeline {} ->
          case splitExtensions file of
            (base, ".fut") -> do
              prog <- runPipelineOnProgram (futharkConfig config) id file
              runPolyPasses config base (SOACS prog)
            (base, ".sexp") -> do
              input <- liftIO $ ByteString.readFile file
              prog <- case Sexp.decode @(Prog SOACS.SOACS) input of
                Right prog' -> return $ SOACS prog'
                Left _ ->
                  case Sexp.decode @(Prog Kernels.Kernels) input of
                    Right prog' -> return $ Kernels prog'
                    Left _ ->
                      case Sexp.decode @(Prog Seq.Seq) input of
                        Right prog' -> return $ Seq prog'
                        Left _ ->
                          case Sexp.decode @(Prog KernelsMem.KernelsMem) input of
                            Right prog' -> return $ KernelsMem prog'
                            Left _ ->
                              case Sexp.decode @(Prog SeqMem.SeqMem) input of
                                Right prog' -> return $ SeqMem prog'
                                Left e -> externalErrorS $ "Couldn't parse sexp input: " ++ show e
              runPolyPasses config base prog
            (_, ext) ->
              externalErrorS $ unwords ["Unsupported extension", show ext, ". Supported extensions: sexp, fut"]

runPolyPasses :: Config -> FilePath -> UntypedPassState -> FutharkM ()
runPolyPasses config base initial_prog = do
  end_prog <-
    foldM
      (runPolyPass pipeline_config)
      initial_prog
      (getFutharkPipeline config)
  logMsg $ "Running action " ++ untypedActionName (futharkAction config)
  case (end_prog, futharkAction config) of
    (SOACS prog, SOACSAction action) ->
      actionProcedure action prog
    (Kernels prog, KernelsAction action) ->
      actionProcedure action prog
    (SeqMem prog, SeqMemAction action) ->
      actionProcedure (action base) prog
    (KernelsMem prog, KernelsMemAction action) ->
      actionProcedure (action base) prog
    (MCMem prog, MCMemAction action) ->
      actionProcedure (action base) prog
    (SOACS soacs_prog, PolyAction acs) ->
      actionProcedure acs soacs_prog
    (Kernels kernels_prog, PolyAction acs) ->
      actionProcedure acs kernels_prog
    (MC mc_prog, PolyAction acs) ->
      actionProcedure acs mc_prog
    (Seq seq_prog, PolyAction acs) ->
      actionProcedure acs seq_prog
    (KernelsMem mem_prog, PolyAction acs) ->
      actionProcedure acs mem_prog
    (SeqMem mem_prog, PolyAction acs) ->
      actionProcedure acs mem_prog
    (MCMem mem_prog, PolyAction acs) ->
      actionProcedure acs mem_prog
    (_, action) ->
      externalErrorS $
        "Action "
          <> untypedActionName action
          <> " expects "
          ++ representation action
          ++ " representation, but got "
          ++ representation end_prog
          ++ "."
  logMsg ("Done." :: String)
  where
    pipeline_config =
      PipelineConfig
        { pipelineVerbose = fst (futharkVerbose $ futharkConfig config) > NotVerbose,
          pipelineValidate = True
        }

runPolyPass ::
  PipelineConfig ->
  UntypedPassState ->
  UntypedPass ->
  FutharkM UntypedPassState
runPolyPass pipeline_config s (UntypedPass f) =
  f s pipeline_config
