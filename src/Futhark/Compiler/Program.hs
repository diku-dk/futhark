{-# LANGUAGE LambdaCase #-}

-- | Low-level compilation parts.  Look at "Futhark.Compiler" for a
-- more high-level API.
module Futhark.Compiler.Program
  ( readLibrary,
    readLibraryExceptKnown,
    readUntypedLibrary,
    Imports,
    FileModule (..),
    E.Warnings,
    prettyWarnings,
    ProgError (..),
    LoadedProg (lpNameSource),
    noLoadedProg,
    lpImports,
    lpWarnings,
    lpFilePaths,
    reloadProg,
    extendProg,
    VFS,
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (execStateT, gets, modify)
import Data.Bifunctor (first)
import Data.List (intercalate, sort)
import Data.List.NonEmpty qualified as NE
import Data.Loc (Loc (..), Located, locOf)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Futhark.FreshNames
import Futhark.Util (interactWithFileSafely, nubOrd, startupTime)
import Futhark.Util.Pretty (Doc, align, pretty)
import Language.Futhark qualified as E
import Language.Futhark.Core (isBuiltin)
import Language.Futhark.Parser (SyntaxError (..), parseFuthark)
import Language.Futhark.Prelude
import Language.Futhark.Semantic
import Language.Futhark.TypeChecker qualified as E
import Language.Futhark.Warnings
import System.Directory (getModificationTime)
import System.FilePath (normalise, takeExtension)
import System.FilePath.Posix qualified as Posix

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
data ProgError
  = ProgError Loc (Doc ())
  | -- | Not actually an error, but we want them reported
    -- with errors.
    ProgWarning Loc (Doc ())

type WithErrors = Either (NE.NonEmpty ProgError)

instance Located ProgError where
  locOf (ProgError l _) = l
  locOf (ProgWarning l _) = l

-- | A mapping from absolute pathnames to pretty representing a virtual
-- file system.  Before loading a file from the file system, this
-- mapping is consulted.  If the desired pathname has an entry here,
-- the corresponding pretty is used instead of loading the file from
-- disk.
type VFS = M.Map FilePath T.Text

newtype UncheckedImport = UncheckedImport
  { unChecked ::
      WithErrors (LoadedFile E.UncheckedProg, [((ImportName, Loc), MVar UncheckedImport)])
  }

-- | If mapped to Nothing, treat it as present.  This is used when
-- reloading programs.
type ReaderState = MVar (M.Map ImportName (Maybe (MVar UncheckedImport)))

newState :: [ImportName] -> IO ReaderState
newState known = newMVar $ M.fromList $ map (,Nothing) known

orderedImports ::
  [((ImportName, Loc), MVar UncheckedImport)] ->
  IO [(ImportName, WithErrors (LoadedFile E.UncheckedProg))]
orderedImports = fmap reverse . flip execStateT [] . mapM_ (spelunk [])
  where
    spelunk steps ((include, loc), mvar)
      | include `elem` steps = do
          let problem =
                ProgError loc . pretty $
                  "Import cycle: "
                    <> intercalate
                      " -> "
                      (map includeToString $ reverse $ include : steps)
          modify ((include, Left (NE.singleton problem)) :)
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

-- | Read the content and modification time of a file.
-- Check if the file exits in VFS before interact with file system directly.
contentsAndModTime :: FilePath -> VFS -> IO (Maybe (Either String (T.Text, UTCTime)))
contentsAndModTime filepath vfs = do
  case M.lookup filepath vfs of
    Nothing -> interactWithFileSafely $ (,) <$> T.readFile filepath <*> getModificationTime filepath
    Just file_contents -> do
      now <- getCurrentTime
      pure $ Just $ Right (file_contents, now)

readImportFile :: ImportName -> Loc -> VFS -> IO (Either ProgError (LoadedFile T.Text))
readImportFile include loc vfs = do
  -- First we try to find a file of the given name in the search path,
  -- then we look at the builtin library if we have to.  For the
  -- builtins, we don't use the search path.
  r <- contentsAndModTime filepath vfs
  case (r, lookup prelude_str prelude) of
    (Just (Right (s, mod_time)), _) ->
      pure $ Right $ loaded filepath s mod_time
    (Just (Left e), _) ->
      pure $ Left $ ProgError loc $ pretty e
    (Nothing, Just s) ->
      pure $ Right $ loaded prelude_str s startupTime
    (Nothing, Nothing) ->
      pure $ Left $ ProgError loc $ pretty not_found
  where
    filepath = includeToFilePath include

    prelude_str = "/" Posix.</> includeToString include Posix.<.> "fut"

    loaded path s mod_time =
      LoadedFile
        { lfImportName = include,
          lfPath = path,
          lfMod = s,
          lfModTime = mod_time
        }

    not_found =
      "Could not read file " <> E.quote (T.pack filepath) <> "."

handleFile :: ReaderState -> VFS -> LoadedFile T.Text -> IO UncheckedImport
handleFile state_mvar vfs (LoadedFile file_name import_name file_contents mod_time) = do
  case parseFuthark file_name file_contents of
    Left (SyntaxError loc err) ->
      pure . UncheckedImport . Left . NE.singleton $ ProgError loc $ pretty err
    Right prog -> do
      let imports = map (first $ mkImportFrom import_name) $ E.progImports prog
      mvars <-
        mapMaybe sequenceA . zip imports
          <$> mapM (uncurry $ readImport state_mvar vfs) imports
      let file =
            LoadedFile
              { lfPath = file_name,
                lfImportName = import_name,
                lfModTime = mod_time,
                lfMod = prog
              }
      pure $ UncheckedImport $ Right (file, mvars)

readImport :: ReaderState -> VFS -> ImportName -> Loc -> IO (Maybe (MVar UncheckedImport))
readImport state_mvar vfs include loc =
  modifyMVar state_mvar $ \state ->
    case M.lookup include state of
      Just x -> pure (state, x)
      Nothing -> do
        prog_mvar <- newImportMVar $ do
          readImportFile include loc vfs >>= \case
            Left e -> pure $ UncheckedImport $ Left $ NE.singleton e
            Right file -> handleFile state_mvar vfs file
        pure (M.insert include prog_mvar state, prog_mvar)

readUntypedLibraryExceptKnown ::
  [ImportName] ->
  VFS ->
  [FilePath] ->
  IO (Either (NE.NonEmpty ProgError) [LoadedFile E.UncheckedProg])
readUntypedLibraryExceptKnown known vfs fps = do
  state_mvar <- liftIO $ newState known
  let prelude_import = mkInitialImport "/prelude/prelude"
  prelude_mvar <- liftIO $ readImport state_mvar vfs prelude_import mempty
  fps_mvars <- liftIO (mapM (onFile state_mvar) fps)
  let unknown_mvars = onlyUnknown (((prelude_import, mempty), prelude_mvar) : fps_mvars)
  fmap (map snd) . errorsToTop <$> orderedImports unknown_mvars
  where
    onlyUnknown = mapMaybe sequenceA
    onFile state_mvar fp =
      modifyMVar state_mvar $ \state -> do
        case M.lookup include state of
          Just prog_mvar -> pure (state, ((include, mempty), prog_mvar))
          Nothing -> do
            prog_mvar <- newImportMVar $ do
              if takeExtension fp /= ".fut"
                then
                  pure . UncheckedImport . Left . NE.singleton $
                    ProgError NoLoc $
                      pretty fp <> ": source files must have a .fut extension."
                else do
                  r <- contentsAndModTime fp vfs
                  case r of
                    Just (Right (fs, mod_time)) -> do
                      handleFile state_mvar vfs $
                        LoadedFile
                          { lfImportName = include,
                            lfMod = fs,
                            lfModTime = mod_time,
                            lfPath = fp
                          }
                    Just (Left e) ->
                      pure . UncheckedImport . Left . NE.singleton $
                        ProgError NoLoc $
                          pretty $
                            show e
                    Nothing ->
                      pure . UncheckedImport . Left . NE.singleton $
                        ProgError NoLoc $
                          pretty fp <> ": file not found."
            pure (M.insert include prog_mvar state, ((include, mempty), prog_mvar))
      where
        include = mkInitialImport fp_name
        (fp_name, _) = Posix.splitExtension fp

-- | A type-checked file.
data CheckedFile = CheckedFile
  { -- | The name generation state after checking this file.
    cfNameSource :: VNameSource,
    -- | The warnings that were issued from checking this file.
    cfWarnings :: Warnings,
    -- | The type-checked file.
    cfMod :: FileModule
  }

asImports :: [LoadedFile CheckedFile] -> Imports
asImports = map f
  where
    f lf = (lfImportName lf, cfMod $ lfMod lf)

typeCheckProg ::
  [LoadedFile CheckedFile] ->
  VNameSource ->
  [LoadedFile E.UncheckedProg] ->
  WithErrors ([LoadedFile CheckedFile], VNameSource)
typeCheckProg orig_imports orig_src =
  foldM f (orig_imports, orig_src)
  where
    roots = ["/prelude/prelude"]

    f (imports, src) (LoadedFile path import_name prog mod_time) = do
      let prog'
            | isBuiltin (includeToFilePath import_name) = prog
            | otherwise = prependRoots roots prog
      case E.checkProg (asImports imports) src import_name prog' of
        (prog_ws, Left (E.TypeError loc notes msg)) -> do
          let err' = msg <> pretty notes
              warningToError (wloc, wmsg) = ProgWarning (locOf wloc) wmsg
          Left $
            ProgError (locOf loc) err'
              NE.:| map warningToError (listWarnings prog_ws)
        (prog_ws, Right (m, src')) ->
          let warnHole (loc, t) =
                singleWarning (E.locOf loc) $ "Hole of type: " <> align (pretty t)
              prog_ws' = prog_ws <> foldMap warnHole (E.progHoles (fileProg m))
           in Right
                ( imports ++ [LoadedFile path import_name (CheckedFile src prog_ws' m) mod_time],
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
    lpFiles :: [LoadedFile CheckedFile],
    -- | Final name source.
    lpNameSource :: VNameSource
  }

-- | The 'Imports' of a 'LoadedProg', as expected by e.g. type
-- checking functions.
lpImports :: LoadedProg -> Imports
lpImports = map f . lpFiles
  where
    f lf = (lfImportName lf, cfMod $ lfMod lf)

-- | All warnings of a 'LoadedProg'.
lpWarnings :: LoadedProg -> Warnings
lpWarnings = foldMap (cfWarnings . lfMod) . lpFiles

-- | The absolute paths of the files that are part of this program.
lpFilePaths :: LoadedProg -> [FilePath]
lpFilePaths = map lfPath . lpFiles

unchangedImports ::
  (MonadIO m) =>
  VNameSource ->
  VFS ->
  [LoadedFile CheckedFile] ->
  m ([LoadedFile CheckedFile], VNameSource)
unchangedImports src _ [] = pure ([], src)
unchangedImports src vfs (f : fs)
  | isBuiltin (includeToFilePath (lfImportName f)) =
      first (f :) <$> unchangedImports src vfs fs
  | otherwise = do
      let file_path = lfPath f
      if M.member file_path vfs
        then pure ([], cfNameSource $ lfMod f)
        else do
          changed <-
            maybe True (either (const True) (> lfModTime f))
              <$> liftIO (interactWithFileSafely (getModificationTime file_path))
          if changed
            then pure ([], cfNameSource $ lfMod f)
            else first (f :) <$> unchangedImports src vfs fs

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
usableLoadedProg :: (MonadIO m) => LoadedProg -> VFS -> [FilePath] -> m LoadedProg
usableLoadedProg (LoadedProg roots imports src) vfs new_roots
  | sort roots == sort new_roots = do
      (imports', src') <- unchangedImports src vfs imports
      pure $ LoadedProg [] imports' src'
  | otherwise =
      pure noLoadedProg

-- | Extend a loaded program with (possibly new) files.
extendProg ::
  LoadedProg ->
  [FilePath] ->
  VFS ->
  IO (Either (NE.NonEmpty ProgError) LoadedProg)
extendProg lp new_roots vfs = do
  new_imports_untyped <-
    readUntypedLibraryExceptKnown (map lfImportName $ lpFiles lp) vfs new_roots
  pure $ do
    (imports, src') <-
      typeCheckProg (lpFiles lp) (lpNameSource lp) =<< new_imports_untyped
    Right (LoadedProg (nubOrd (lpRoots lp ++ new_roots)) imports src')

-- | Load some new files, reusing as much of the previously loaded
-- program as possible.  This does not *extend* the currently loaded
-- program the way 'extendProg' does it, so it is always correct (if
-- less efficient) to pass 'noLoadedProg'.
reloadProg ::
  LoadedProg ->
  [FilePath] ->
  VFS ->
  IO (Either (NE.NonEmpty ProgError) LoadedProg)
reloadProg lp new_roots vfs = do
  lp' <- usableLoadedProg lp vfs new_roots
  extendProg lp' new_roots vfs

-- | Read and type-check some Futhark files.
readLibrary ::
  -- | Extra functions that should be marked as entry points; only
  -- applies to the immediate files, not any imports imported.
  [E.Name] ->
  -- | The files to read.
  [FilePath] ->
  IO (Either (NE.NonEmpty ProgError) (Warnings, Imports, VNameSource))
readLibrary extra_eps fps = readLibraryExceptKnown extra_eps fps M.empty

-- | Read and type-check some Futhark files.
readLibraryExceptKnown ::
  -- | Extra functions that should be marked as entry points; only
  -- applies to the immediate files, not any imports imported.
  [E.Name] ->
  -- | The files to read.
  [FilePath] ->
  -- | Files which are already cached
  VFS ->
  IO (Either (NE.NonEmpty ProgError) (E.Warnings, Imports, VNameSource))
readLibraryExceptKnown extra_eps fps vfs =
  ( fmap frob
      . typeCheckProg mempty (lpNameSource noLoadedProg)
      <=< fmap (setEntryPoints (E.defaultEntryPoint : extra_eps) fps)
  )
    <$> readUntypedLibraryExceptKnown [] vfs fps
  where
    frob (y, z) = (foldMap (cfWarnings . lfMod) y, asImports y, z)

-- | Read (and parse) all source files (including the builtin prelude)
-- corresponding to a set of root files.
readUntypedLibrary ::
  [FilePath] ->
  IO (Either (NE.NonEmpty ProgError) [(ImportName, E.UncheckedProg)])
readUntypedLibrary = fmap (fmap (map f)) . readUntypedLibraryExceptKnown [] M.empty
  where
    f lf = (lfImportName lf, lfMod lf)
