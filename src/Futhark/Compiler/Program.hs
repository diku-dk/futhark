{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Low-level compilation parts.  Look at "Futhark.Compiler" for a
-- more high-level API.
module Futhark.Compiler.Program
  ( readLibrary,
    readUntypedLibrary,
    readImports,
    Imports,
    FileModule (..),
    E.Warnings,
    Basis (..),
    emptyBasis,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( MVar,
    modifyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
  )
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (execStateT, gets, modify)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import qualified Data.Text as T
import Futhark.Error
import Futhark.FreshNames
import Futhark.Util (readFileSafely)
import Futhark.Util.Pretty (line, ppr, text, (</>))
import qualified Language.Futhark as E
import Language.Futhark.Parser
import Language.Futhark.Prelude
import Language.Futhark.Semantic
import qualified Language.Futhark.TypeChecker as E
import Language.Futhark.Warnings
import System.FilePath (normalise)
import qualified System.FilePath.Posix as Posix

newtype UncheckedImport = UncheckedImport
  { unChecked ::
      Either CompilerError (E.UncheckedProg, [(ImportName, MVar UncheckedImport)])
  }

type ReaderState = MVar (M.Map ImportName (MVar UncheckedImport))

newState :: IO ReaderState
newState = newMVar mempty

orderedImports ::
  (MonadError CompilerError m, MonadIO m) =>
  [(ImportName, MVar UncheckedImport)] ->
  m [(ImportName, E.UncheckedProg)]
orderedImports = fmap reverse . flip execStateT [] . mapM_ (spelunk [])
  where
    spelunk steps (include, mvar)
      | include `elem` steps =
        externalErrorS $
          "Import cycle: "
            ++ intercalate
              " -> "
              (map includeToString $ reverse $ include : steps)
      | otherwise = do
        prev <- gets $ lookup include
        case prev of
          Just _ -> pure ()
          Nothing -> do
            (prog, more_imports) <-
              either throwError pure . unChecked =<< liftIO (readMVar mvar)
            mapM_ (spelunk (include : steps)) more_imports
            modify ((include, prog) :)

newImportMVar :: IO UncheckedImport -> IO (MVar UncheckedImport)
newImportMVar m = do
  mvar <- newEmptyMVar
  void $ forkIO $ putMVar mvar =<< m
  pure mvar

readImportFile :: ImportName -> IO (Either CompilerError (T.Text, FilePath))
readImportFile include = do
  -- First we try to find a file of the given name in the search path,
  -- then we look at the builtin library if we have to.  For the
  -- builtins, we don't use the search path.
  let filepath = includeToFilePath include
  r <- readFileSafely filepath
  case (r, lookup prelude_str prelude) of
    (Just (Right s), _) -> pure $ Right (s, filepath)
    (Just (Left e), _) -> pure $ Left $ ExternalError $ text e
    (Nothing, Just t) -> pure $ Right (t, prelude_str)
    (Nothing, Nothing) -> pure $ Left $ ExternalError $ text not_found
  where
    prelude_str = "/" Posix.</> includeToString include Posix.<.> "fut"

    not_found =
      "Error at " ++ E.locStr (E.srclocOf include)
        ++ ": could not find import '"
        ++ includeToString include
        ++ "'."

handleFile ::
  ReaderState -> [ImportName] -> ImportName -> T.Text -> FilePath -> IO UncheckedImport
handleFile state_mvar steps import_name file_contents file_name = do
  case parseFuthark file_name file_contents of
    Left err -> pure $ UncheckedImport $ Left $ ExternalError $ text $ show err
    Right prog -> do
      let steps' = import_name : steps
          imports = map (uncurry (mkImportFrom import_name)) $ E.progImports prog
      mvars <-
        mapM (readImport state_mvar steps') imports
      pure $ UncheckedImport $ Right (prog, zip imports mvars)

readImport :: ReaderState -> [ImportName] -> ImportName -> IO (MVar UncheckedImport)
readImport state_mvar steps include =
  modifyMVar state_mvar $ \state ->
    case M.lookup include state of
      Just prog_mvar -> pure (state, prog_mvar)
      Nothing -> do
        prog_mvar <- newImportMVar $ do
          readImportFile include >>= \case
            Left e -> pure $ UncheckedImport $ Left e
            Right (x, y) -> handleFile state_mvar steps include x y
        pure (M.insert include prog_mvar state, prog_mvar)

-- | Read (and parse) all source files (including the builtin prelude)
-- corresponding to a set of root files.
readUntypedLibrary ::
  (MonadIO m, MonadError CompilerError m) =>
  [FilePath] ->
  m [(ImportName, E.UncheckedProg)]
readUntypedLibrary fps = do
  state_mvar <- liftIO newState
  let prelude_import = mkInitialImport "/prelude/prelude"
  prelude_mvar <- liftIO $ readImport state_mvar [] prelude_import
  fps_mvars <- liftIO (mapM (onFile state_mvar) fps)
  orderedImports $ (prelude_import, prelude_mvar) : fps_mvars
  where
    onFile state_mvar fp =
      modifyMVar state_mvar $ \state -> do
        case M.lookup include state of
          Just prog_mvar -> pure (state, (include, prog_mvar))
          Nothing -> do
            prog_mvar <- newImportMVar $ do
              r <- readFileSafely fp
              case r of
                Just (Right fs) -> do
                  handleFile state_mvar [] include fs fp
                Just (Left e) ->
                  pure $ UncheckedImport $ Left $ ExternalError $ text $ show e
                Nothing ->
                  pure $ UncheckedImport $ Left $ ExternalError $ text $ fp <> ": file not found."
            pure (M.insert include prog_mvar state, (include, prog_mvar))
      where
        include = mkInitialImport fp_name
        (fp_name, _) = Posix.splitExtension fp

-- | Pre-typechecked imports, including a starting point for the name source.
data Basis = Basis
  { basisImports :: Imports,
    basisNameSource :: VNameSource
  }

-- | A basis that contains no imports, and has a properly initialised
-- name source.
emptyBasis :: Basis
emptyBasis =
  Basis
    { basisImports = mempty,
      basisNameSource = src
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

setEntryPoints ::
  [E.Name] ->
  [FilePath] ->
  [(ImportName, E.UncheckedProg)] ->
  [(ImportName, E.UncheckedProg)]
setEntryPoints extra_eps fps = map onProg
  where
    fps' = map normalise fps
    onProg (name, prog)
      | includeToFilePath name `elem` fps' =
        (name, prog {E.progDecs = map onDec (E.progDecs prog)})
      | otherwise =
        (name, prog)

    onDec (E.ValDec vb)
      | E.valBindName vb `elem` extra_eps =
        E.ValDec vb {E.valBindEntryPoint = Just E.NoInfo}
    onDec dec = dec

-- | Read and type-check some Futhark files.
readLibrary ::
  (MonadError CompilerError m, MonadIO m) =>
  -- | Extra functions that should be marked as entry points; only
  -- applies to the immediate files, not any imports imported.
  [E.Name] ->
  -- | The files to read.
  [FilePath] ->
  m (E.Warnings, Imports, VNameSource)
readLibrary extra_eps fps =
  typeCheckProgram emptyBasis . setEntryPoints (E.defaultEntryPoint : extra_eps) fps
    =<< readUntypedLibrary fps

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
  state_mvar <- liftIO newState
  files <- orderedImports . zip imps =<< liftIO (mapM (readImport state_mvar []) imps)
  typeCheckProgram basis files

prependRoots :: [FilePath] -> E.UncheckedProg -> E.UncheckedProg
prependRoots roots (E.Prog doc ds) =
  E.Prog doc $ map mkImport roots ++ ds
  where
    mkImport fp =
      -- We do not use ImportDec here, because we do not want the
      -- type checker to issue a warning about a redundant import.
      E.LocalDec (E.OpenDec (E.ModImport fp E.NoInfo mempty) mempty) mempty
