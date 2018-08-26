{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.Compiler
       (
         runPipelineOnProgram
       , runCompilerOnProgram

       , interpretAction'
       , FutharkConfig (..)
       , newFutharkConfig
       , dumpError
       , reportingIOErrors

       , module Futhark.Compiler.Program
       , readProgram
       , readLibrary
       )
where

import Data.Semigroup ((<>))
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import System.Exit (exitWith, ExitCode(..))
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Language.Futhark.Parser
import Futhark.Internalise
import Futhark.Pipeline
import Futhark.Actions
import Futhark.MonadFreshNames
import Futhark.Representation.AST
import qualified Futhark.Representation.SOACS as I
import qualified Futhark.TypeCheck as I
import Futhark.Compiler.Program
import qualified Language.Futhark as E
import Language.Futhark.Futlib.Builtin (builtinBasis)
import Futhark.Util.Log

data FutharkConfig = FutharkConfig
                     { futharkVerbose :: Maybe (Maybe FilePath)
                     , futharkWarn :: Bool -- ^ Warn if True.
                     , futharkWerror :: Bool -- ^ If true, error on any warnings.
                     , futharkSafe :: Bool -- ^ If True, ignore @unsafe@.
                     }

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig { futharkVerbose = Nothing
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
          case futharkVerbose config of
            Just outfile ->
              maybe (T.hPutStr stderr) T.writeFile outfile $ info <> "\n"
            _ -> return ()

-- | Catch all IO exceptions and print a better error message if they
-- happen.  Use this at the top-level of all Futhark compiler
-- frontends.
reportingIOErrors :: IO () -> IO ()
reportingIOErrors = flip catches [Handler onExit, Handler onError]
  where onExit :: ExitCode -> IO ()
        onExit = throwIO
        onError :: SomeException -> IO ()
        onError e
          | Just UserInterrupt <- asyncExceptionFromException e =
              return () -- This corresponds to CTRL-C, which is not an error.
          | otherwise = do
              T.hPutStrLn stderr "Internal compiler error (unhandled IO exception)."
              T.hPutStrLn stderr "Please report this at https://github.com/diku-dk/futhark/issues"
              T.hPutStrLn stderr $ T.pack $ show e
              exitWith $ ExitFailure 1

runCompilerOnProgram :: FutharkConfig
                     -> Pipeline I.SOACS lore
                     -> Action lore
                     -> FilePath
                     -> IO ()
runCompilerOnProgram config pipeline action file = do
  res <- runFutharkM compile $ case futharkVerbose config of
                                 Just _ -> Verbose
                                 Nothing -> NotVerbose
  case res of
    Left err -> liftIO $ do
      dumpError config err
      exitWith $ ExitFailure 2
    Right () ->
      return ()
  where compile = do
          prog <- runPipelineOnProgram config pipeline file
          when (isJust $ futharkVerbose config) $
            liftIO $ hPutStrLn stderr $ "Running action " ++ actionName action
          actionProcedure action prog

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
  res <- internaliseProg (futharkSafe config) prog_imports
  case res of
    Left err ->
      internalErrorS ("During internalisation: " <> pretty err) $ E.Prog Nothing $
      concatMap (E.progDecs . fileProg . snd) prog_imports
    Right int_prog -> do
      when (pipelineVerbose pipeline_config) $
        logMsg ("Type-checking internalised program" :: String)
      typeCheckInternalProgram int_prog
      runPasses pipeline pipeline_config int_prog
  where pipeline_config =
          PipelineConfig { pipelineVerbose = isJust $ futharkVerbose config
                         , pipelineValidate = True
                         }

typeCheckInternalProgram :: I.Prog -> FutharkM ()
typeCheckInternalProgram prog =
  case I.checkProg prog of
    Left err -> internalErrorS ("After internalisation:\n" ++ show err) (Just prog)
    Right () -> return ()

interpretAction' :: Name -> Action I.SOACS
interpretAction' =
  interpretAction parseValues'
  where parseValues' :: FilePath -> T.Text -> Either ParseError [I.Value]
        parseValues' path s =
          fmap concat $ mapM internalise =<< parseValues path s
        internalise v =
          maybe (Left $ ParseError $ "Invalid input value: " ++ I.pretty v) Right $
          internaliseValue v

-- | Read and type-check a Futhark program, including all imports.
readProgram :: (MonadError CompilerError m, MonadIO m) =>
               FilePath -> m (Warnings, Imports, VNameSource)
readProgram = readLibrary . pure

-- | Read and type-check a collection of Futhark files, including all
-- imports.
readLibrary :: (MonadError CompilerError m, MonadIO m) =>
               [FilePath] -> m (Warnings, Imports, VNameSource)
readLibrary = readLibraryWithBasis builtinBasis
