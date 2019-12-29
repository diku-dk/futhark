{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.Compiler
       (
         runPipelineOnProgram
       , runCompilerOnProgram

       , FutharkConfig (..)
       , newFutharkConfig
       , dumpError

       , module Futhark.Compiler.Program
       , readProgram
       , readLibrary
       , readProgramOrDie
       )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import System.Exit (exitWith, ExitCode(..))
import System.IO
import qualified Data.Text.IO as T

import qualified Futhark.Analysis.Alias as Alias
import Futhark.Internalise
import Futhark.Pipeline
import Futhark.MonadFreshNames
import Futhark.Representation.AST
import qualified Futhark.Representation.SOACS as I
import qualified Futhark.TypeCheck as I
import Futhark.Compiler.Program
import Futhark.Util.Log

data FutharkConfig = FutharkConfig
                     { futharkVerbose :: (Verbosity, Maybe FilePath)
                     , futharkWarn :: Bool -- ^ Warn if True.
                     , futharkWerror :: Bool -- ^ If true, error on any warnings.
                     , futharkSafe :: Bool -- ^ If True, ignore @unsafe@.
                     }

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig { futharkVerbose = (NotVerbose, Nothing)
                                 , futharkWarn = True
                                 , futharkWerror = False
                                 , futharkSafe = False
                                 }

dumpError :: FutharkConfig -> CompilerError -> IO ()
dumpError config err =
  case err of
    ExternalError s -> do
      T.hPutStrLn stderr s
      T.hPutStrLn stderr "If you find this error message confusing, uninformative, or wrong, please open an issue at https://github.com/diku-dk/futhark/issues."
    InternalError s info CompilerBug -> do
      T.hPutStrLn stderr "Internal compiler error."
      T.hPutStrLn stderr "Please report this at https://github.com/diku-dk/futhark/issues."
      report s info
    InternalError s info CompilerLimitation -> do
      T.hPutStrLn stderr "Known compiler limitation encountered.  Sorry."
      T.hPutStrLn stderr "Revise your program or try a different Futhark compiler."
      report s info
  where report s info = do
          T.hPutStrLn stderr s
          when (fst (futharkVerbose config) > NotVerbose) $
            maybe (T.hPutStr stderr) T.writeFile
            (snd (futharkVerbose config)) $ info <> "\n"

runCompilerOnProgram :: FutharkConfig
                     -> Pipeline I.SOACS lore
                     -> Action lore
                     -> FilePath
                     -> IO ()
runCompilerOnProgram config pipeline action file = do
  res <- runFutharkM compile $ fst $ futharkVerbose config
  case res of
    Left err -> liftIO $ do
      dumpError config err
      exitWith $ ExitFailure 2
    Right () ->
      return ()
  where compile = do
          prog <- runPipelineOnProgram config pipeline file
          when ((>NotVerbose) . fst $ futharkVerbose config) $
            logMsg $ "Running action " ++ actionName action
          actionProcedure action prog
          when ((>NotVerbose) . fst $ futharkVerbose config) $
            logMsg ("Done." :: String)

runPipelineOnProgram :: FutharkConfig
                     -> Pipeline I.SOACS tolore
                     -> FilePath
                     -> FutharkM (Prog tolore)
runPipelineOnProgram config pipeline file = do
  when (pipelineVerbose pipeline_config) $
    logMsg ("Reading and type-checking source program" :: String)
  (ws, prog_imports, namesrc) <- readProgram file

  when (futharkWarn config) $ do
    liftIO $ hPutStr stderr $ show ws
    when (futharkWerror config && ws /= mempty) $
      externalErrorS "Treating above warnings as errors due to --Werror."

  putNameSource namesrc
  when (pipelineVerbose pipeline_config) $
    logMsg ("Internalising program" :: String)
  int_prog <- internaliseProg (futharkSafe config) prog_imports
  when (pipelineVerbose pipeline_config) $
    logMsg ("Type-checking internalised program" :: String)
  typeCheckInternalProgram int_prog
  runPasses pipeline pipeline_config int_prog
  where pipeline_config =
          PipelineConfig { pipelineVerbose = fst (futharkVerbose config) > NotVerbose
                         , pipelineValidate = True
                         }

typeCheckInternalProgram :: I.Prog I.SOACS -> FutharkM ()
typeCheckInternalProgram prog =
  case I.checkProg prog' of
    Left err -> internalErrorS ("After internalisation:\n" ++ show err) (Just prog')
    Right () -> return ()
  where prog' = Alias.aliasAnalysis prog

-- | Read and type-check a Futhark program, including all imports.
readProgram :: (MonadError CompilerError m, MonadIO m) =>
               FilePath -> m (Warnings, Imports, VNameSource)
readProgram = readLibrary . pure

-- | Read and type-check a collection of Futhark files, including all
-- imports.
readLibrary :: (MonadError CompilerError m, MonadIO m) =>
               [FilePath] -> m (Warnings, Imports, VNameSource)
readLibrary = readLibraryWithBasis emptyBasis

-- | Not verbose, and terminates process on error.
readProgramOrDie :: MonadIO m => FilePath -> m (Warnings, Imports, VNameSource)
readProgramOrDie file = liftIO $ do
  res <- runFutharkM (readProgram file) NotVerbose
  case res of
    Left err -> do
      dumpError newFutharkConfig err
      exitWith $ ExitFailure 2
    Right res' -> return res'
