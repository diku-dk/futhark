{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE Strict #-}

-- | High-level API for invoking the Futhark compiler.
module Futhark.Compiler
  ( runPipelineOnProgram,
    runCompilerOnProgram,
    FutharkConfig (..),
    newFutharkConfig,
    dumpError,
    handleWarnings,
    module Futhark.Compiler.Program,
    readProgram,
    readProgramOrDie,
  )
where

import Control.Monad
import Control.Monad.Except
import qualified Data.Text.IO as T
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Compiler.Program
import Futhark.IR
import qualified Futhark.IR.SOACS as I
import Futhark.Internalise
import Futhark.MonadFreshNames
import Futhark.Pipeline
import qualified Futhark.TypeCheck as I
import Futhark.Util.Log
import Futhark.Util.Pretty (ppr, prettyText)
import Language.Futhark.Warnings
import System.Exit (ExitCode (..), exitWith)
import System.IO

-- | The compiler configuration.  This only contains options related
-- to core compiler functionality, such as reading the initial program
-- and running passes.  Options related to code generation are handled
-- elsewhere.
data FutharkConfig = FutharkConfig
  { futharkVerbose :: (Verbosity, Maybe FilePath),
    -- | Warn if True.
    futharkWarn :: Bool,
    -- | If true, error on any warnings.
    futharkWerror :: Bool,
    -- | If True, ignore @unsafe@.
    futharkSafe :: Bool
  }

-- | The default compiler configuration.
newFutharkConfig :: FutharkConfig
newFutharkConfig =
  FutharkConfig
    { futharkVerbose = (NotVerbose, Nothing),
      futharkWarn = True,
      futharkWerror = False,
      futharkSafe = False
    }

-- | Print a compiler error to stdout.  The 'FutharkConfig' controls
-- to which degree auxiliary information (e.g. the failing program) is
-- also printed.
dumpError :: FutharkConfig -> CompilerError -> IO ()
dumpError config err =
  case err of
    ExternalError s -> do
      T.hPutStrLn stderr $ prettyText s
      T.hPutStrLn stderr ""
      T.hPutStrLn stderr "If you find this error message confusing, uninformative, or wrong, please open an issue at\nhttps://github.com/diku-dk/futhark/issues."
    InternalError s info CompilerBug -> do
      T.hPutStrLn stderr "Internal compiler error."
      T.hPutStrLn stderr "Please report this at https://github.com/diku-dk/futhark/issues."
      report s info
    InternalError s info CompilerLimitation -> do
      T.hPutStrLn stderr "Known compiler limitation encountered.  Sorry."
      T.hPutStrLn stderr "Revise your program or try a different Futhark compiler."
      report s info
  where
    report s info = do
      T.hPutStrLn stderr s
      when (fst (futharkVerbose config) > NotVerbose) $
        maybe
          (T.hPutStr stderr)
          T.writeFile
          (snd (futharkVerbose config))
          $ info <> "\n"

-- | Read a program from the given 'FilePath', run the given
-- 'Pipeline', and finish up with the given 'Action'.
runCompilerOnProgram ::
  FutharkConfig ->
  Pipeline I.SOACS lore ->
  Action lore ->
  FilePath ->
  IO ()
runCompilerOnProgram config pipeline action file = do
  res <- runFutharkM compile $ fst $ futharkVerbose config
  case res of
    Left err -> liftIO $ do
      dumpError config err
      exitWith $ ExitFailure 2
    Right () ->
      return ()
  where
    compile = do
      prog <- runPipelineOnProgram config pipeline file
      when ((> NotVerbose) . fst $ futharkVerbose config) $
        logMsg $ "Running action " ++ actionName action
      actionProcedure action prog
      when ((> NotVerbose) . fst $ futharkVerbose config) $
        logMsg ("Done." :: String)

-- | Read a program from the given 'FilePath', run the given
-- 'Pipeline', and return it.
runPipelineOnProgram ::
  FutharkConfig ->
  Pipeline I.SOACS tolore ->
  FilePath ->
  FutharkM (Prog tolore)
runPipelineOnProgram config pipeline file = do
  when (pipelineVerbose pipeline_config) $
    logMsg ("Reading and type-checking source program" :: String)
  (prog_imports, namesrc) <-
    handleWarnings config $ (\(a, b, c) -> (a, (b, c))) <$> readProgram file

  putNameSource namesrc
  when (pipelineVerbose pipeline_config) $
    logMsg ("Internalising program" :: String)
  int_prog <- internaliseProg (futharkSafe config) prog_imports
  when (pipelineVerbose pipeline_config) $
    logMsg ("Type-checking internalised program" :: String)
  typeCheckInternalProgram int_prog
  runPipeline pipeline pipeline_config int_prog
  where
    pipeline_config =
      PipelineConfig
        { pipelineVerbose = fst (futharkVerbose config) > NotVerbose,
          pipelineValidate = True
        }

typeCheckInternalProgram :: I.Prog I.SOACS -> FutharkM ()
typeCheckInternalProgram prog =
  case I.checkProg prog' of
    Left err -> internalErrorS ("After internalisation:\n" ++ show err) (ppr prog')
    Right () -> return ()
  where
    prog' = Alias.aliasAnalysis prog

-- | Read and type-check a Futhark program, including all imports.
readProgram ::
  (MonadError CompilerError m, MonadIO m) =>
  FilePath ->
  m (Warnings, Imports, VNameSource)
readProgram = readLibrary . pure

-- | Not verbose, and terminates process on error.
readProgramOrDie :: MonadIO m => FilePath -> m (Warnings, Imports, VNameSource)
readProgramOrDie file = liftIO $ do
  res <- runFutharkM (readProgram file) NotVerbose
  case res of
    Left err -> do
      dumpError newFutharkConfig err
      exitWith $ ExitFailure 2
    Right res' -> return res'

-- | Run an operation that produces warnings, and handle them
-- appropriately, yielding the non-warning return value.  "Proper
-- handling" means e.g. to print them to the screen, as directed by
-- the compiler configuration.
handleWarnings :: FutharkConfig -> FutharkM (Warnings, a) -> FutharkM a
handleWarnings config m = do
  (ws, a) <- m

  when (futharkWarn config && anyWarnings ws) $ do
    liftIO $ hPutStrLn stderr $ pretty ws
    when (futharkWerror config) $
      externalErrorS "Treating above warnings as errors due to --Werror."

  return a
