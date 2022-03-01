{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Low-level compilation parts.  Look at "Futhark.Compiler" for a
-- more high-level API.
module Futhark.Compiler.Program
  ( readLibrary,
    readUntypedLibrary,
    Imports,
    FileModule (..),
    E.Warnings,
    ProgramError (..),
    LoadedProg (lpNameSource),
    noLoadedProg,
    lpImports,
    reloadProg,
    extendProg,
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
import Data.Bifunctor (first)
import Data.List (intercalate, isPrefixOf, sort)
import qualified Data.List.NonEmpty as NE
import Data.Loc (Loc (..), locOf)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime)
import Futhark.FreshNames
import Futhark.Util (interactWithFileSafely, nubOrd, startupTime)
import Futhark.Util.Pretty (Doc, line, ppr, text, (</>))
import qualified Language.Futhark as E
import Language.Futhark.Parser
import Language.Futhark.Prelude
import Language.Futhark.Semantic
import qualified Language.Futhark.TypeChecker as E
import Language.Futhark.Warnings
import System.Directory (getModificationTime)
import System.FilePath (normalise)
import qualified System.FilePath.Posix as Posix

data LoadedFile fm = LoadedFile
  { lfPath :: FilePath,
    lfImportName :: ImportName,
    lfMod :: fm,
    -- | Modification time of the underlying file.
    lfModTime :: UTCTime
  }
  deriving (Eq, Ord, Show)

-- | Note that the location may be 'NoLoc'.  This essentially only
-- happens when the problem is that a root file cannot be found.
data ProgramError = ProgramError Loc Doc

type WithErrors = Either (NE.NonEmpty ProgramError)

newtype UncheckedImport = UncheckedImport
  { unChecked ::
      WithErrors (LoadedFile E.UncheckedProg, [(ImportName, MVar UncheckedImport)])
  }

-- | If mapped to Nothing, treat it as present.  This is used when
-- reloading programs.
type ReaderState = MVar (M.Map ImportName (Maybe (MVar UncheckedImport)))

newState :: [ImportName] -> IO ReaderState
newState known = newMVar $ M.fromList $ zip known $ repeat Nothing

-- Since we need to work with base 4.14 that does not have NE.singleton.
singleError :: ProgramError -> NE.NonEmpty ProgramError
singleError = (NE.:| [])

orderedImports ::
  [(ImportName, MVar UncheckedImport)] ->
  IO [(ImportName, WithErrors (LoadedFile E.UncheckedProg))]
orderedImports = fmap reverse . flip execStateT [] . mapM_ (spelunk [])
  where
    spelunk steps (include, mvar)
      | include `elem` steps = do
        let problem =
              ProgramError (locOf include) . text $
                "Import cycle: "
                  <> intercalate
                    " -> "
                    (map includeToString $ reverse $ include : steps)
        modify ((include, Left (singleError problem)) :)
      | otherwise = do
        prev <- gets $ lookup include
        case prev of
          Just _ -> pure ()
          Nothing -> do
            res <- unChecked <$> liftIO (readMVar mvar)
            case res of
              Left errors ->
                modify ((include, Left errors) :)
              Right (file, more_imports) -> do
                mapM_ (spelunk (include : steps)) more_imports
                modify ((include, Right file) :)

errorsToTop ::
  [(ImportName, WithErrors (LoadedFile E.UncheckedProg))] ->
  WithErrors [(ImportName, LoadedFile E.UncheckedProg)]
errorsToTop [] = Right []
errorsToTop ((_, Left x) : rest) =
  either (Left . (x <>)) (const (Left x)) (errorsToTop rest)
errorsToTop ((name, Right x) : rest) =
  fmap ((name, x) :) (errorsToTop rest)

newImportMVar :: IO UncheckedImport -> IO (Maybe (MVar UncheckedImport))
newImportMVar m = do
  mvar <- newEmptyMVar
  void $ forkIO $ putMVar mvar =<< m
  pure $ Just mvar

contentsAndModTime :: FilePath -> IO (Maybe (Either String (T.Text, UTCTime)))
contentsAndModTime filepath =
  interactWithFileSafely $
    (,) <$> T.readFile filepath <*> getModificationTime filepath

readImportFile :: ImportName -> IO (Either ProgramError (LoadedFile T.Text))
readImportFile include = do
  -- First we try to find a file of the given name in the search path,
  -- then we look at the builtin library if we have to.  For the
  -- builtins, we don't use the search path.
  let filepath = includeToFilePath include
  r <- contentsAndModTime filepath
  case (r, lookup prelude_str prelude) of
    (Just (Right (s, mod_time)), _) ->
      pure $ Right $ loaded filepath s mod_time
    (Just (Left e), _) ->
      pure $ Left $ ProgramError (locOf include) $ text e
    (Nothing, Just s) ->
      pure $ Right $ loaded prelude_str s startupTime
    (Nothing, Nothing) ->
      pure $ Left $ ProgramError (locOf include) $ text not_found
  where
    prelude_str = "/" Posix.</> includeToString include Posix.<.> "fut"

    loaded path s mod_time =
      LoadedFile
        { lfImportName = include,
          lfPath = path,
          lfMod = s,
          lfModTime = mod_time
        }

    not_found =
      "Could not find import " <> E.quote (includeToString include) <> "."

handleFile ::
  ReaderState -> LoadedFile T.Text -> IO UncheckedImport
handleFile state_mvar (LoadedFile file_name import_name file_contents mod_time) = do
  case parseFuthark file_name file_contents of
    Left err ->
      pure . UncheckedImport . Left . singleError $
        ProgramError (locOf import_name) $ text $ show err
    Right prog -> do
      let imports = map (uncurry (mkImportFrom import_name)) $ E.progImports prog
      mvars <-
        mapMaybe sequenceA . zip imports
          <$> mapM (readImport state_mvar) imports
      let file =
            LoadedFile
              { lfPath = file_name,
                lfImportName = import_name,
                lfModTime = mod_time,
                lfMod = prog
              }
      pure $ UncheckedImport $ Right (file, mvars)

readImport :: ReaderState -> ImportName -> IO (Maybe (MVar UncheckedImport))
readImport state_mvar include =
  modifyMVar state_mvar $ \state ->
    case M.lookup include state of
      Just x -> pure (state, x)
      Nothing -> do
        prog_mvar <- newImportMVar $ do
          readImportFile include >>= \case
            Left e -> pure $ UncheckedImport $ Left $ singleError e
            Right file -> handleFile state_mvar file
        pure (M.insert include prog_mvar state, prog_mvar)

readUntypedLibraryExceptKnown ::
  [ImportName] ->
  [FilePath] ->
  IO (Either (NE.NonEmpty ProgramError) [LoadedFile E.UncheckedProg])
readUntypedLibraryExceptKnown known fps = do
  state_mvar <- liftIO $ newState known
  let prelude_import = mkInitialImport "/prelude/prelude"
  prelude_mvar <- liftIO $ readImport state_mvar prelude_import
  fps_mvars <- liftIO (mapM (onFile state_mvar) fps)
  let unknown_mvars = onlyUnknown ((prelude_import, prelude_mvar) : fps_mvars)
  fmap (map snd) . errorsToTop <$> orderedImports unknown_mvars
  where
    onlyUnknown = mapMaybe sequenceA
    onFile state_mvar fp =
      modifyMVar state_mvar $ \state -> do
        case M.lookup include state of
          Just prog_mvar -> pure (state, (include, prog_mvar))
          Nothing -> do
            prog_mvar <- newImportMVar $ do
              r <- contentsAndModTime fp
              case r of
                Just (Right (fs, mod_time)) -> do
                  handleFile state_mvar $
                    LoadedFile
                      { lfImportName = include,
                        lfMod = fs,
                        lfModTime = mod_time,
                        lfPath = fp
                      }
                Just (Left e) ->
                  pure . UncheckedImport . Left . singleError $
                    ProgramError NoLoc $ text $ show e
                Nothing ->
                  pure . UncheckedImport . Left . singleError $
                    ProgramError NoLoc $ text $ fp <> ": file not found."
            pure (M.insert include prog_mvar state, (include, prog_mvar))
      where
        include = mkInitialImport fp_name
        (fp_name, _) = Posix.splitExtension fp

asImports :: [LoadedFile (VNameSource, FileModule)] -> Imports
asImports = map f
  where
    f lf = (includeToString (lfImportName lf), snd $ lfMod lf)

typeCheckProg ::
  [LoadedFile (VNameSource, FileModule)] ->
  VNameSource ->
  [LoadedFile E.UncheckedProg] ->
  WithErrors (E.Warnings, [LoadedFile (VNameSource, FileModule)], VNameSource)
typeCheckProg orig_imports orig_src =
  foldM f (mempty, orig_imports, orig_src)
  where
    roots = ["/prelude/prelude"]

    f (ws, imports, src) (LoadedFile path import_name prog mod_time) = do
      let prog'
            | "/prelude" `isPrefixOf` includeToFilePath import_name = prog
            | otherwise = prependRoots roots prog
      case E.checkProg (asImports imports) src import_name prog' of
        (prog_ws, Left (E.TypeError loc notes msg)) -> do
          let ws' = ws <> prog_ws
              err' = msg <> ppr notes
          Left . singleError . ProgramError (locOf loc) $
            if anyWarnings ws'
              then ppr ws' </> line <> ppr err'
              else ppr err'
        (prog_ws, Right (m, src')) ->
          Right
            ( ws <> prog_ws,
              imports ++ [LoadedFile path import_name (src, m) mod_time],
              src'
            )

setEntryPoints ::
  [E.Name] ->
  [FilePath] ->
  [LoadedFile E.UncheckedProg] ->
  [LoadedFile E.UncheckedProg]
setEntryPoints extra_eps fps = map onFile
  where
    fps' = map normalise fps
    onFile lf
      | includeToFilePath (lfImportName lf) `elem` fps' =
        lf {lfMod = prog {E.progDecs = map onDec (E.progDecs prog)}}
      | otherwise =
        lf
      where
        prog = lfMod lf

    onDec (E.ValDec vb)
      | E.valBindName vb `elem` extra_eps =
        E.ValDec vb {E.valBindEntryPoint = Just E.NoInfo}
    onDec dec = dec

prependRoots :: [FilePath] -> E.UncheckedProg -> E.UncheckedProg
prependRoots roots (E.Prog doc ds) =
  E.Prog doc $ map mkImport roots ++ ds
  where
    mkImport fp =
      -- We do not use ImportDec here, because we do not want the
      -- type checker to issue a warning about a redundant import.
      E.LocalDec (E.OpenDec (E.ModImport fp E.NoInfo mempty) mempty) mempty

-- | A loaded, type-checked program.  This can be used to extract
-- information about the program, but also to speed up subsequent
-- reloads.
data LoadedProg = LoadedProg
  { lpRoots :: [FilePath],
    -- | The 'VNameSource' is the name source just *before* the module
    -- was type checked.
    lpFiles :: [LoadedFile (VNameSource, FileModule)],
    -- | Final name source.
    lpNameSource :: VNameSource
  }

-- | The 'Imports' of a 'LoadedProg', as expected by e.g. type
-- checking functions.
lpImports :: LoadedProg -> Imports
lpImports = map f . lpFiles
  where
    f lf = (includeToString (lfImportName lf), snd $ lfMod lf)

unchangedImports ::
  MonadIO m =>
  VNameSource ->
  [LoadedFile (VNameSource, FileModule)] ->
  m ([LoadedFile (VNameSource, FileModule)], VNameSource)
unchangedImports src [] = pure ([], src)
unchangedImports src (f : fs)
  | "/prelude" `isPrefixOf` includeToFilePath (lfImportName f) =
    first (f :) <$> unchangedImports src fs
  | otherwise = do
    changed <-
      maybe True (either (const True) (> lfModTime f))
        <$> liftIO (interactWithFileSafely (getModificationTime $ lfPath f))
    if changed
      then pure ([], fst $ lfMod f)
      else first (f :) <$> unchangedImports src fs

-- | A "loaded program" containing no actual files.  Use this as a
-- starting point for 'reloadProg'
noLoadedProg :: LoadedProg
noLoadedProg =
  LoadedProg
    { lpRoots = [],
      lpFiles = mempty,
      lpNameSource = newNameSource $ E.maxIntrinsicTag + 1
    }

-- | Find out how many of the old imports can be used.  Here we are
-- forced to be overly conservative, because our type checker
-- enforces a linear ordering.
usableLoadedProg :: MonadIO m => LoadedProg -> [FilePath] -> m LoadedProg
usableLoadedProg (LoadedProg roots imports src) new_roots
  | sort roots == sort new_roots = do
    (imports', src') <- unchangedImports src imports
    pure $ LoadedProg [] imports' src'
  | otherwise =
    pure noLoadedProg

-- | Extend a loaded program with (possibly new) files.
extendProg ::
  LoadedProg ->
  [FilePath] ->
  IO (Either (NE.NonEmpty ProgramError) (E.Warnings, LoadedProg))
extendProg lp new_roots = do
  new_imports_untyped <-
    readUntypedLibraryExceptKnown (map lfImportName $ lpFiles lp) new_roots
  pure $ do
    (ws, imports, src') <-
      typeCheckProg (lpFiles lp) (lpNameSource lp) =<< new_imports_untyped
    Right (ws, LoadedProg (nubOrd (lpRoots lp ++ new_roots)) imports src')

-- | Load some new files, reusing as much of the previously loaded
-- program as possible.  This does not *extend* the currently loaded
-- program the way 'extendProg' does it, so it is always correct (if
-- less efficient) to pass 'noLoadedProg'.
reloadProg ::
  LoadedProg ->
  [FilePath] ->
  IO (Either (NE.NonEmpty ProgramError) (E.Warnings, LoadedProg))
reloadProg lp new_roots = do
  lp' <- usableLoadedProg lp new_roots
  extendProg lp' new_roots

-- | Read and type-check some Futhark files.
readLibrary ::
  -- | Extra functions that should be marked as entry points; only
  -- applies to the immediate files, not any imports imported.
  [E.Name] ->
  -- | The files to read.
  [FilePath] ->
  IO (Either (NE.NonEmpty ProgramError) (E.Warnings, Imports, VNameSource))
readLibrary extra_eps fps =
  ( fmap frob
      . typeCheckProg mempty (lpNameSource noLoadedProg)
      <=< fmap (setEntryPoints (E.defaultEntryPoint : extra_eps) fps)
  )
    <$> readUntypedLibraryExceptKnown [] fps
  where
    frob (x, y, z) = (x, asImports y, z)

-- | Read (and parse) all source files (including the builtin prelude)
-- corresponding to a set of root files.
readUntypedLibrary ::
  [FilePath] ->
  IO (Either (NE.NonEmpty ProgramError) [(ImportName, E.UncheckedProg)])
readUntypedLibrary = fmap (fmap (map f)) . readUntypedLibraryExceptKnown []
  where
    f lf = (lfImportName lf, lfMod lf)
