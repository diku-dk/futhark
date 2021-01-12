{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Low-level compilation parts.  Look at "Futhark.Compiler" for a
-- more high-level API.
module Futhark.Compiler.Program
  ( readLibrary,
    readImports,
    Imports,
    FileModule (..),
    E.Warnings,
    Basis (..),
    emptyBasis,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate, isPrefixOf)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Futhark.Error
import Futhark.FreshNames
import Futhark.Util.Pretty (line, ppr, (</>))
import qualified Language.Futhark as E
import Language.Futhark.Parser
import Language.Futhark.Prelude
import Language.Futhark.Semantic
import qualified Language.Futhark.TypeChecker as E
import Language.Futhark.Warnings
import qualified System.FilePath.Posix as Posix
import System.IO.Error

readFileSafely :: String -> IO (Maybe (Either String (String, T.Text)))
readFileSafely filepath =
  (Just . Right . (filepath,) <$> T.readFile filepath) `catch` couldNotRead
  where
    couldNotRead e
      | isDoesNotExistError e =
        return Nothing
      | otherwise =
        return $ Just $ Left $ show e

newtype ReaderState = ReaderState
  {alreadyRead :: [(ImportName, E.UncheckedProg)]}

-- | A little monad for parsing a Futhark program.
type ReaderM m = StateT ReaderState m

runReaderM ::
  (MonadError CompilerError m) =>
  ReaderM m a ->
  m [(ImportName, E.UncheckedProg)]
runReaderM m = reverse . alreadyRead <$> execStateT m (ReaderState mempty)

readImportFile ::
  (MonadError CompilerError m, MonadIO m) =>
  ImportName ->
  ReaderM m (T.Text, FilePath)
readImportFile include = do
  -- First we try to find a file of the given name in the search path,
  -- then we look at the builtin library if we have to.  For the
  -- builtins, we don't use the search path.
  r <- liftIO $ readFileSafely $ includeToFilePath include
  case (r, lookup prelude_str prelude) of
    (Just (Right (filepath, s)), _) -> return (s, filepath)
    (Just (Left e), _) -> externalErrorS e
    (Nothing, Just t) -> return (t, prelude_str)
    (Nothing, Nothing) -> externalErrorS not_found
  where
    prelude_str = "/" Posix.</> includeToString include Posix.<.> "fut"

    not_found =
      "Error at " ++ E.locStr (E.srclocOf include)
        ++ ": could not find import '"
        ++ includeToString include
        ++ "'."

readImport ::
  (MonadError CompilerError m, MonadIO m) =>
  [ImportName] ->
  ImportName ->
  ReaderM m ()
readImport steps include
  | include `elem` steps =
    externalErrorS $
      "Import cycle: "
        ++ intercalate
          " -> "
          (map includeToString $ reverse $ include : steps)
  | otherwise = do
    already_done <- gets $ isJust . lookup include . alreadyRead

    unless already_done $
      uncurry (handleFile steps include) =<< readImportFile include

handleFile ::
  (MonadIO m, MonadError CompilerError m) =>
  [ImportName] ->
  ImportName ->
  T.Text ->
  FilePath ->
  ReaderM m ()
handleFile steps import_name file_contents file_name = do
  prog <- case parseFuthark file_name file_contents of
    Left err -> externalErrorS $ show err
    Right prog -> return prog

  let steps' = import_name : steps
  mapM_ (readImport steps' . uncurry (mkImportFrom import_name)) $
    E.progImports prog

  modify $ \s ->
    s {alreadyRead = (import_name, prog) : alreadyRead s}

-- | Pre-typechecked imports, including a starting point for the name source.
data Basis = Basis
  { basisImports :: Imports,
    basisNameSource :: VNameSource,
    -- | Files that should be implicitly opened.
    basisRoots :: [String]
  }

-- | A basis that contains no imports, and has a properly initialised
-- name source.
emptyBasis :: Basis
emptyBasis =
  Basis
    { basisImports = mempty,
      basisNameSource = src,
      basisRoots = mempty
    }
  where
    src = newNameSource $ E.maxIntrinsicTag + 1

typeCheckProgram ::
  MonadError CompilerError m =>
  Basis ->
  [(ImportName, E.UncheckedProg)] ->
  m (E.Warnings, Imports, VNameSource)
typeCheckProgram basis =
  foldM f (mempty, basisImports basis, basisNameSource basis)
  where
    roots = ["/prelude/prelude"]

    f (ws, imports, src) (import_name, prog) = do
      let prog'
            | "/prelude" `isPrefixOf` includeToFilePath import_name = prog
            | otherwise = prependRoots roots prog
      case E.checkProg imports src import_name prog' of
        (prog_ws, Left err) -> do
          let ws' = ws <> prog_ws
          externalError $
            if anyWarnings ws'
              then ppr ws' </> line <> ppr err
              else ppr err
        (prog_ws, Right (m, src')) ->
          pure
            ( ws <> prog_ws,
              imports ++ [(includeToString import_name, m)],
              src'
            )

-- | Read Futhark files from some basis, and printing log messages if
-- the first parameter is True.
readLibrary ::
  (MonadError CompilerError m, MonadIO m) =>
  [FilePath] ->
  m (E.Warnings, Imports, VNameSource)
readLibrary fps = typeCheckProgram emptyBasis <=< runReaderM $ do
  readImport [] (mkInitialImport "/prelude/prelude")
  mapM_ onFile fps
  where
    onFile fp = do
      r <- liftIO $ readFileSafely fp
      case r of
        Just (Right (_, fs)) ->
          handleFile [] (mkInitialImport fp_name) fs fp
        Just (Left e) -> externalErrorS e
        Nothing -> externalErrorS $ fp ++ ": file not found."
      where
        (fp_name, _) = Posix.splitExtension fp

-- | Read and type-check Futhark imports (no @.fut@ extension; may
-- refer to baked-in prelude).  This is an exotic operation that
-- probably only makes sense in an interactive environment.
readImports ::
  (MonadError CompilerError m, MonadIO m) =>
  Basis ->
  [ImportName] ->
  m
    ( E.Warnings,
      Imports,
      VNameSource
    )
readImports basis imps = do
  files <- runReaderM $ mapM (readImport []) imps
  typeCheckProgram basis files

prependRoots :: [FilePath] -> E.UncheckedProg -> E.UncheckedProg
prependRoots roots (E.Prog doc ds) =
  E.Prog doc $ map mkImport roots ++ ds
  where
    mkImport fp =
      -- We do not use ImportDec here, because we do not want the
      -- type checker to issue a warning about a redundant import.
      E.LocalDec (E.OpenDec (E.ModImport fp E.NoInfo mempty) mempty) mempty
