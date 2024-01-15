{-# LANGUAGE Strict #-}

-- | High-level API for invoking the Futhark compiler.
module Futhark.Compiler
  ( runPipelineOnProgram,
    runCompilerOnProgram,
    dumpError,
    handleWarnings,
    prettyProgErrors,
    module Futhark.Compiler.Program,
    module Futhark.Compiler.Config,
    readProgramFile,
    readProgramFiles,
    readProgramOrDie,
    readUntypedProgram,
    readUntypedProgramOrDie,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Loc (Loc (..), posCoff, posFile)
import Data.Text.IO qualified as T
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Compiler.Config
import Futhark.Compiler.Program
import Futhark.IR
import Futhark.IR.SOACS qualified as I
import Futhark.IR.TypeCheck qualified as I
import Futhark.Internalise
import Futhark.MonadFreshNames
import Futhark.Pipeline
import Futhark.Util.Log
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Semantic (includeToString)
import Language.Futhark.Warnings
import System.Exit (ExitCode (..), exitWith)
import System.IO

-- | Print a compiler error to stdout.  The 'FutharkConfig' controls
-- to which degree auxiliary information (e.g. the failing program) is
-- also printed.
dumpError :: FutharkConfig -> CompilerError -> IO ()
dumpError config err =
  case err of
    ExternalError s -> do
      hPutDoc stderr s
      T.hPutStrLn stderr ""
      T.hPutStrLn stderr "If you find this error message confusing, uninformative, or wrong, please open an issue:"
      T.hPutStrLn stderr "  https://github.com/diku-dk/futhark/issues"
    InternalError s info CompilerBug -> do
      T.hPutStrLn stderr "Internal compiler error.  Please report this:"
      T.hPutStrLn stderr "  https://github.com/diku-dk/futhark/issues"
      report s info
    InternalError s info CompilerLimitation -> do
      T.hPutStrLn stderr "Known compiler limitation encountered.  Sorry."
      T.hPutStrLn stderr "Revise your program or try a different Futhark compiler."
      report s info
  where
    report s info = do
      T.hPutStrLn stderr s
      when (fst (futharkVerbose config) > NotVerbose)
        $ maybe
          (T.hPutStr stderr)
          T.writeFile
          (snd (futharkVerbose config))
        $ info <> "\n"

-- | Read a program from the given 'FilePath', run the given
-- 'Pipeline', and finish up with the given 'Action'.
runCompilerOnProgram ::
  FutharkConfig ->
  Pipeline I.SOACS rep ->
  Action rep ->
  FilePath ->
  IO ()
runCompilerOnProgram config pipeline action file = do
  res <- runFutharkM compile $ fst $ futharkVerbose config
  case res of
    Left err -> liftIO $ do
      dumpError config err
      exitWith $ ExitFailure 2
    Right () ->
      pure ()
  where
    compile = do
      prog <- runPipelineOnProgram config pipeline file
      when ((> NotVerbose) . fst $ futharkVerbose config) $
        logMsg $
          "Running action " ++ actionName action
      actionProcedure action prog
      when ((> NotVerbose) . fst $ futharkVerbose config) $
        logMsg ("Done." :: String)

-- | Read a program from the given 'FilePath', run the given
-- 'Pipeline', and return it.
runPipelineOnProgram ::
  FutharkConfig ->
  Pipeline I.SOACS torep ->
  FilePath ->
  FutharkM (Prog torep)
runPipelineOnProgram config pipeline file = do
  when (pipelineVerbose pipeline_config) $
    logMsg ("Reading and type-checking source program" :: String)
  (prog_imports, namesrc) <-
    handleWarnings config $
      (\(a, b, c) -> (a, (b, c)))
        <$> readProgramFile (futharkEntryPoints config) file

  putNameSource namesrc
  int_prog <- internaliseProg config prog_imports
  when (pipelineVerbose pipeline_config) $
    logMsg ("Type-checking internalised program" :: String)
  typeCheckInternalProgram int_prog
  runPipeline pipeline pipeline_config int_prog
  where
    pipeline_config =
      PipelineConfig
        { pipelineVerbose = fst (futharkVerbose config) > NotVerbose,
          pipelineValidate = futharkTypeCheck config
        }

typeCheckInternalProgram :: I.Prog I.SOACS -> FutharkM ()
typeCheckInternalProgram prog =
  case I.checkProg prog' of
    Left err -> internalErrorS ("After internalisation:\n" ++ show err) (pretty prog')
    Right () -> pure ()
  where
    prog' = Alias.aliasAnalysis prog

-- | Prettyprint program errors as suitable for showing on a text console.
prettyProgErrors :: NE.NonEmpty ProgError -> Doc AnsiStyle
prettyProgErrors = stack . punctuate line . map onError . sortOn (rep . locOf) . NE.toList
  where
    rep NoLoc = ("", 0)
    rep (Loc p _) = (posFile p, posCoff p)
    onError (ProgError NoLoc msg) =
      unAnnotate msg
    onError (ProgError loc msg) =
      annotate (color Red) ("Error at " <> pretty (locText (srclocOf loc))) <> ":" </> unAnnotate msg
    onError (ProgWarning NoLoc msg) =
      unAnnotate msg
    onError (ProgWarning loc msg) =
      annotate (color Yellow) $ "Warning at " <> pretty (locText (srclocOf loc)) <> ":" </> unAnnotate msg

-- | Throw an exception formatted with 'prettyProgErrors' if there's
-- an error.
throwOnProgError ::
  (MonadError CompilerError m) =>
  Either (NE.NonEmpty ProgError) a ->
  m a
throwOnProgError =
  either (externalError . prettyProgErrors) pure

-- | Read and type-check a Futhark program, comprising a single file,
-- including all imports.
readProgramFile ::
  (MonadError CompilerError m, MonadIO m) =>
  [I.Name] ->
  FilePath ->
  m (Warnings, Imports, VNameSource)
readProgramFile extra_eps =
  readProgramFiles extra_eps . pure

-- | Read and type-check a Futhark library, comprising multiple files,
-- including all imports.
readProgramFiles ::
  (MonadError CompilerError m, MonadIO m) =>
  [I.Name] ->
  [FilePath] ->
  m (Warnings, Imports, VNameSource)
readProgramFiles extra_eps =
  throwOnProgError <=< liftIO . readLibrary extra_eps

-- | Read and parse (but do not type-check) a Futhark program,
-- including all imports.
readUntypedProgram ::
  (MonadError CompilerError m, MonadIO m) =>
  FilePath ->
  m [(String, E.UncheckedProg)]
readUntypedProgram =
  fmap (map (first includeToString)) . throwOnProgError
    <=< liftIO . readUntypedLibrary . pure

orDie :: (MonadIO m) => FutharkM a -> m a
orDie m = liftIO $ do
  res <- runFutharkM m NotVerbose
  case res of
    Left err -> do
      dumpError newFutharkConfig err
      exitWith $ ExitFailure 2
    Right res' -> pure res'

-- | Not verbose, and terminates process on error.
readProgramOrDie :: (MonadIO m) => FilePath -> m (Warnings, Imports, VNameSource)
readProgramOrDie file = orDie $ readProgramFile mempty file

-- | Not verbose, and terminates process on error.
readUntypedProgramOrDie :: (MonadIO m) => FilePath -> m [(String, E.UncheckedProg)]
readUntypedProgramOrDie file = orDie $ readUntypedProgram file

-- | Run an operation that produces warnings, and handle them
-- appropriately, yielding the non-warning return value.  "Proper
-- handling" means e.g. to print them to the screen, as directed by
-- the compiler configuration.
handleWarnings :: FutharkConfig -> FutharkM (Warnings, a) -> FutharkM a
handleWarnings config m = do
  (ws, a) <- m

  when (futharkWarn config && anyWarnings ws) $ do
    liftIO $ hPutDoc stderr $ prettyWarnings ws
    when (futharkWerror config) $
      externalErrorS "Treating above warnings as errors due to --Werror."

  pure a
