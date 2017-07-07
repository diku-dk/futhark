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
       , reportingIOErrors
       )
where

import Data.Monoid
import Data.Loc
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M
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
import Futhark.Util.Log
import Language.Futhark.Futlib

data FutharkConfig = FutharkConfig
                     { futharkVerbose :: Maybe (Maybe FilePath)
                     , futharkWarn :: Bool -- ^ Warn if True.
                     }

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig { futharkVerbose = Nothing
                                 , futharkWarn = True
                                 }

dumpError :: FutharkConfig -> CompilerError -> IO ()
dumpError config err =
  case err of
    ExternalError s -> do
      T.hPutStrLn stderr s
      T.hPutStrLn stderr "If you find this error message confusing, uninformative, or wrong, please open an issue at https://github.com/HIPERFIT/futhark/issues."
    InternalError s info CompilerBug -> do
      T.hPutStrLn stderr "Internal compiler error."
      T.hPutStrLn stderr "Please report this at https://github.com/HIPERFIT/futhark/issues."
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
              T.hPutStrLn stderr "Please report this at https://github.com/HIPERFIT/futhark/issues."
              T.hPutStrLn stderr $ T.pack $ show e
              exitWith $ ExitFailure 1

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
  when (pipelineVerbose pipeline_config) $
    logMsg ("Reading and type-checking source program" :: String)
  (tagged_ext_prog, ws, _, namesrc) <- readProgram [file]

  when (futharkWarn config) $
    liftIO $ hPutStr stderr $ show ws
  putNameSource namesrc
  when (pipelineVerbose pipeline_config) $
    logMsg ("Internalising program" :: String)
  res <- internaliseProg tagged_ext_prog
  case res of
    Left err ->
      internalErrorS ("During internalisation: " <> pretty err) tagged_ext_prog
    Right int_prog -> do
      when (pipelineVerbose pipeline_config) $
        logMsg ("Type-checking internalised program" :: String)
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

-- | Absolute reference to a Futhark code file.  Does not include the
-- @.fut@ extension.  The 'FilePath' must be absolute.
data FutharkInclude = FutharkInclude Posix.FilePath SrcLoc
  deriving (Eq, Show)

instance Located FutharkInclude where
  locOf (FutharkInclude _ loc) = locOf loc

mkInitialInclude :: String -> FutharkInclude
mkInitialInclude s = FutharkInclude ("/" Posix.</> toPOSIX s) noLoc
  where
    toPOSIX :: FilePath -> Posix.FilePath
    toPOSIX = Posix.joinPath . splitDirectories

mkInclude :: FutharkInclude -> String -> SrcLoc -> FutharkInclude
mkInclude (FutharkInclude includer _) includee =
  FutharkInclude (takeDirectory includer Posix.</> includee)

includeToFilePath :: FilePath -> FutharkInclude -> FilePath
includeToFilePath dir (FutharkInclude fp _) =
  dir </> fromPOSIX (Posix.makeRelative "/" fp) <.> "fut"
  where
    -- | Some bad operating systems do not use forward slash as
    -- directory separator - this is where we convert Futhark includes
    -- (which always use forward slash) to native paths.
    fromPOSIX :: Posix.FilePath -> FilePath
    fromPOSIX = joinPath . Posix.splitDirectories

includeToString :: FutharkInclude -> String
includeToString (FutharkInclude s _) = s

includePath :: FutharkInclude -> String
includePath (FutharkInclude s _) = Posix.takeDirectory s

readImport :: (MonadError CompilerError m, MonadIO m) =>
              SearchPath -> [FutharkInclude] -> FutharkInclude -> CompilerM m ()
readImport search_path steps include
  | include `elem` steps =
      throwError $ ExternalError $ T.pack $
      "Import cycle: " ++ intercalate " -> "
      (map includeToString $ reverse $ include:steps)
  | otherwise = do
      already_done <- gets $ isJust . lookup (includeToString include) . alreadyImported

      unless already_done $ do
        (file_contents, file_name) <- readImportFile search_path include
        prog <- case parseFuthark file_name file_contents of
          Left err -> externalErrorS $ show err
          Right prog -> return prog

        mapM_ (readImport search_path (include:steps) . uncurry (mkInclude include)) $ E.progImports prog

        -- It is important to not read these before the above calls to
        -- readImport.
        imports <- gets alreadyImported
        src <- gets nameSource

        case E.checkProg imports src (includePath include) prog of
          Left err ->
            externalError $ T.pack $ show err
          Right ((progmod, prog'), ws, src') ->
            modify $ \s ->
              s { alreadyImported = (includeToString include,progmod) : imports
                , nameSource      = src'
                , warnings        = warnings s <> ws
                , resultProgs     = prog' : resultProgs s
                }

readImportFile :: (MonadError CompilerError m, MonadIO m) =>
                  SearchPath -> FutharkInclude -> m (T.Text, FilePath)
readImportFile (SearchPath dir) include = do
  -- First we try to find a file of the given name in the search path,
  -- then we look at the builtin library if we have to.  For the
  -- builtins, we don't use the search path.
  r <- liftIO $ (Right <$> T.readFile filepath) `catch` couldNotRead
  case (r, lookup abs_filepath futlib) of
    (Right s, _)            -> return (s, dir </> filepath)
    (Left Nothing, Just t)  -> return (t, "[builtin]" </> filepath)
    (Left Nothing, Nothing) -> externalErrorS not_found
    (Left (Just e), _)      -> externalErrorS e
  where filepath = includeToFilePath dir include
        abs_filepath = includeToFilePath "/" include

        couldNotRead e
          | isDoesNotExistError e =
              return $ Left Nothing
          | otherwise             =
              return $ Left $ Just $
              "Could not import " ++ includeToString include ++ ": " ++ show e

        not_found =
          "Could not find import '" ++ includeToString include ++ "' at " ++
          locStr (srclocOf include) ++ " in path '" ++ dir ++ "'."

-- | Read and type-check a Futhark program, including all imports.
readProgram :: (MonadError CompilerError m, MonadIO m) =>
               [FilePath] -> m (E.Prog,
                                E.Warnings,
                                E.Imports,
                                VNameSource)
readProgram fps = do
  let s = ReaderState mempty mempty newNameSourceForCompiler mempty
  s' <- execStateT (mapM onFile fps) s
  return (E.Prog $ concatMap E.progDecs $ reverse $ resultProgs s',
          warnings s',
          reverse $ alreadyImported s',
          nameSource s')
  where onFile fp =  do
          unless (ext == ".fut") $
            externalErrorS $ "File does not have a .fut extension: " <> fp
          readImport (SearchPath ".") [] $ mkInitialInclude name
            where (name, ext) = splitExtension fp

newNameSourceForCompiler :: VNameSource
newNameSourceForCompiler = newNameSource $ succ $ maximum $ map baseTag $
                           M.keys E.intrinsics

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
