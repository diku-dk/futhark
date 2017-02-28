{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.Compiler
       (
         runPipelineOnProgram
       , runCompilerOnProgram
       , readProgram
       , interpretAction'
       , FutharkConfig (..)
       , newFutharkConfig
       , dumpError
       )
where

import Data.Monoid
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.List
import System.FilePath
import qualified System.FilePath.Posix as Posix
import System.Exit (exitWith, ExitCode(..))
import System.IO
import System.IO.Error
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude

import Language.Futhark.Parser
import Futhark.Internalise
import Futhark.Pipeline
import Futhark.Actions
import Futhark.MonadFreshNames
import Futhark.Representation.AST
import qualified Futhark.Representation.SOACS as I
import qualified Futhark.TypeCheck as I

import qualified Language.Futhark as E
import qualified Language.Futhark.TypeChecker as E
import Language.Futhark.Futlib

data FutharkConfig = FutharkConfig
                     { futharkVerbose :: Maybe (Maybe FilePath)
                     , futharkWarn :: Bool -- ^ Warn if True.
                     }

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig { futharkVerbose = Nothing
                                 , futharkWarn = True
                                 }

dumpError :: FutharkConfig -> CompileError -> IO ()
dumpError config err = do
  T.hPutStrLn stderr $ errorDesc err
  case (errorData err, futharkVerbose config) of
    (s, Just outfile) ->
      maybe (T.hPutStr stderr) T.writeFile outfile $ s <> "\n"
    _ -> return ()

runCompilerOnProgram :: FutharkConfig
                     -> Pipeline I.SOACS lore
                     -> Action lore
                     -> FilePath
                     -> IO ()
runCompilerOnProgram config pipeline action file = do
  res <- runFutharkM compile $ isJust $ futharkVerbose config
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
  (tagged_ext_prog, ws, _, namesrc) <- readProgram file

  when (futharkWarn config) $
    liftIO $ hPutStr stderr $ show ws
  putNameSource namesrc
  res <- internaliseProg tagged_ext_prog
  case res of
    Left err ->
      compileErrorS "During internalisation:" err
    Right int_prog -> do
      typeCheckInternalProgram int_prog
      runPasses pipeline pipeline_config int_prog
  where pipeline_config =
          PipelineConfig { pipelineVerbose = isJust $ futharkVerbose config
                         , pipelineValidate = True
                         }

-- | A little monad for reading and type-checking a Futhark program.
type CompilerM m = StateT ReaderState m

data ReaderState = ReaderState { alreadyImported :: E.Imports
                               , resultProgs :: [E.Prog]
                               , nameSource :: VNameSource
                               , warnings :: E.Warnings
                               }

newtype SearchPath = SearchPath FilePath -- Make this a list, eventually.

-- | Some bad operating systems do not use forward slash as directory
-- separator - this is where we convert Futhark includes (which always
-- use forward slash) to native paths.
includeToPath :: String -> FilePath
includeToPath = joinPath . Posix.splitPath

readImport :: (MonadError CompileError m, MonadIO m) =>
              SearchPath -> [FilePath] -> FilePath -> CompilerM m ()
readImport search_path steps name
  | name `elem` steps =
      throwError $ CompileError
      (T.pack $ "Import cycle: " ++ intercalate " -> " (reverse $ name:steps)) mempty
  | otherwise = do
      already_done <- gets $ HM.member name . alreadyImported

      unless already_done $ do
        (file_contents, file_name) <- readImportFile search_path name
        prog <- case parseFuthark file_name file_contents of
          Left err -> throwError $ CompileError (T.pack $ show err) mempty
          Right prog -> return prog

        mapM_ (readImport search_path (name:steps) . includeToPath) $ E.progImports prog

        -- It is important to not read these before the above calls to
        -- readImport.
        imports <- gets alreadyImported
        src <- gets nameSource

        case E.checkProg imports src prog of
          Left err ->
            throwError $ CompileError (T.pack $ show err) mempty
          Right ((progmod, prog'), ws, src') ->
            modify $ \s ->
              s { alreadyImported = HM.insert name progmod imports
                , nameSource      = src'
                , warnings        = warnings s <> ws
                , resultProgs     = prog' : resultProgs s
                }

readImportFile :: (MonadError CompileError m, MonadIO m) =>
                  SearchPath -> FilePath -> m (T.Text, FilePath)
readImportFile (SearchPath dir) name = do
  -- First we try to find a file of the given name in the search path,
  -- then we look at the builtin library if we have to.
  r <- liftIO $ (Right <$> T.readFile (dir </> name_with_ext)) `catch` couldNotRead
  case (r, lookup name_with_ext futlib) of
    (Right s, _)            -> return (s, dir </> name_with_ext)
    (Left Nothing, Just t)  -> return (t, "[builtin]" </> name_with_ext)
    (Left Nothing, Nothing) -> throwError $ CompileError not_found mempty
    (Left (Just e), _)      -> throwError e
  where name_with_ext = name <.> "fut"

        couldNotRead e
          | isDoesNotExistError e =
              return $ Left Nothing
          | otherwise             =
              return $ Left $ Just $
              CompileError (T.pack $ "Could not import " ++ show name ++ ": "
                            ++ show e) mempty

        not_found =
          T.pack $ "Could not find import " ++ show name ++ " in path " ++ show dir ++ "."

-- | Read and type-check a Futhark program, including all imports.
readProgram :: (MonadError CompileError m, MonadIO m) =>
               FilePath -> m (E.Prog,
                              E.Warnings,
                              E.Imports,
                              VNameSource)
readProgram fp = do
  unless (ext == ".fut") $
    throwError $ CompileError
    ("File does not have a .fut extension: " <> T.pack fp) mempty
  s' <- execStateT (readImport (SearchPath dir) [] file_root) s
  return (E.Prog $ concatMap E.progDecs $ reverse $ resultProgs s',
          warnings s',
          alreadyImported s',
          nameSource s')
  where s = ReaderState mempty mempty newNameSourceForCompiler mempty
        (dir, file) = splitFileName fp
        (file_root, ext) = splitExtension file

newNameSourceForCompiler :: VNameSource
newNameSourceForCompiler = newNameSource $ succ $ maximum $ map baseTag $
                           HM.keys E.intrinsics

typeCheckInternalProgram :: I.Prog -> FutharkM ()
typeCheckInternalProgram prog =
  case I.checkProg prog of
    Left err -> compileError (T.pack $ "After internalisation:\n" ++ show err) prog
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
