-- | Obtaining information about packages over THE INTERNET!
module Futhark.Pkg.Info
  ( -- * Package info
    PkgInfo (..),
    lookupPkgRev,
    pkgInfo,
    PkgRevInfo (..),
    GetManifest (getManifest),
    GetFiles (getFiles),
    CacheDir (..),

    -- * Package registry
    PkgRegistry,
    MonadPkgRegistry (..),
    lookupPackage,
    lookupPackageRev,
    lookupNewestRev,
  )
where

import Control.Monad (unless, void)
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.IORef
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (zonedTimeToUTC)
import Futhark.Pkg.Types
import Futhark.Util (directoryContents, showText, zEncodeText)
import Futhark.Util.Log
import System.Directory (doesDirectoryExist)
import System.Exit
import System.FilePath (makeRelative, (</>))
import System.Process.ByteString (readProcessWithExitCode)

-- | The manifest is stored as a monadic action, because we want to
-- fetch them on-demand.  It would be a waste to fetch it information
-- for every version of every package if we only actually need a small
-- subset of them.
newtype GetManifest m = GetManifest {getManifest :: m PkgManifest}

instance Show (GetManifest m) where
  show _ = "#<GetManifest>"

instance Eq (GetManifest m) where
  _ == _ = True

-- | Get the absolute path to a package directory on disk, as well as
-- /relative/ paths to files that should be installed from this
-- package.  Composing the package directory and one of these paths
-- refers to a local file (pointing into the cache) and is valid at
-- least until the next cache operation.
newtype GetFiles m = GetFiles {getFiles :: m (FilePath, [FilePath])}

instance Show (GetFiles m) where
  show _ = "#<GetFiles>"

instance Eq (GetFiles m) where
  _ == _ = True

-- | Information about a version of a single package.  The version
-- number is stored separately.
data PkgRevInfo m = PkgRevInfo
  { pkgGetFiles :: GetFiles m,
    -- | The commit ID can be used for verification ("freezing"), by
    -- storing what it was at the time this version was last selected.
    pkgRevCommit :: T.Text,
    pkgRevGetManifest :: GetManifest m,
    -- | Timestamp for when the revision was made (rarely used).
    pkgRevTime :: UTCTime
  }
  deriving (Eq, Show)

-- | Create memoisation around a 'GetManifest' action to ensure that
-- multiple inspections of the same revisions will not result in
-- potentially expensive IO operations.
memoiseGetManifest :: (MonadIO m) => GetManifest m -> m (GetManifest m)
memoiseGetManifest (GetManifest m) = do
  ref <- liftIO $ newIORef Nothing
  pure $
    GetManifest $ do
      v <- liftIO $ readIORef ref
      case v of
        Just v' -> pure v'
        Nothing -> do
          v' <- m
          liftIO $ writeIORef ref $ Just v'
          pure v'

-- | Information about a package.  The name of the package is stored
-- separately.
data PkgInfo m = PkgInfo
  { pkgVersions :: M.Map SemVer (PkgRevInfo m),
    -- | Look up information about a specific
    -- commit, or HEAD in case of Nothing.
    pkgLookupCommit :: Maybe T.Text -> m (PkgRevInfo m)
  }

-- | Lookup information about a given version of a package.
lookupPkgRev :: SemVer -> PkgInfo m -> Maybe (PkgRevInfo m)
lookupPkgRev v = M.lookup v . pkgVersions

majorRevOfPkg :: PkgPath -> (T.Text, [Word])
majorRevOfPkg p =
  case T.splitOn "@" p of
    [p', v] | [(v', "")] <- reads $ T.unpack v -> (p', [v'])
    _ -> (p, [0, 1])

gitCmd :: (MonadIO m, MonadLogger m, MonadFail m) => [String] -> m BS.ByteString
gitCmd opts = do
  logMsg $ "Running command: " <> T.unwords ("git" : map T.pack opts)
  (code, out, err) <- liftIO $ readProcessWithExitCode "git" opts mempty
  unless (err == mempty) $ logMsg $ T.decodeUtf8 err
  case code of
    ExitFailure 127 -> fail $ "'" <> unwords ("git" : opts) <> "' failed (program not found?)."
    ExitFailure _ -> fail $ "'" <> unwords ("git" : opts) <> "' failed."
    ExitSuccess -> pure out

gitCmd_ :: (MonadIO m, MonadLogger m, MonadFail m) => [String] -> m ()
gitCmd_ = void . gitCmd

gitCmdLines :: (MonadIO m, MonadLogger m, MonadFail m) => [String] -> m [T.Text]
gitCmdLines = fmap (T.lines . T.decodeUtf8) . gitCmd

-- | A temporary directory in which we store Git checkouts while
-- running.  This is to avoid constantly re-cloning.  Will be deleted
-- when @futhark pkg@ terminates.  In principle we could keep this
-- around for longer, but then we would have to 'git pull' now and
-- then also.  Note that the cache is stateful - we are going to use
-- @git checkout@ to move around the history.  It is generally not
-- safe to have multiple operations running concurrently.
newtype CacheDir = CacheDir FilePath

ensureGit ::
  (MonadIO m, MonadLogger m, MonadFail m) =>
  CacheDir ->
  T.Text ->
  m FilePath
ensureGit (CacheDir cachedir) url = do
  exists <- liftIO $ doesDirectoryExist gitdir
  unless exists $
    gitCmd_ ["-C", cachedir, "clone", "https://" <> T.unpack url, url']
  pure gitdir
  where
    url' = T.unpack $ zEncodeText url
    gitdir = cachedir </> url'

-- A git reference (tag, commit, HEAD, etc).
type Ref = String

versionRef :: SemVer -> Ref
versionRef v = T.unpack $ "v" <> prettySemVer v

revInfo ::
  (MonadIO m, MonadLogger m, MonadFail m) =>
  FilePath ->
  PkgPath ->
  Ref ->
  m (PkgRevInfo m)
revInfo gitdir path ref = do
  gitCmd_ ["-C", gitdir, "rev-parse", ref, "--"]
  [sha] <- gitCmdLines ["-C", gitdir, "rev-list", "-n1", ref]
  [time] <- gitCmdLines ["-C", gitdir, "show", "-s", "--format=%cI", ref]
  utc <- zonedTimeToUTC <$> iso8601ParseM (T.unpack time)
  gm <- memoiseGetManifest getManifest'
  pure $
    PkgRevInfo
      { pkgGetFiles = getFiles gm,
        pkgRevCommit = sha,
        pkgRevGetManifest = gm,
        pkgRevTime = utc
      }
  where
    noPkgDir pdir =
      fail $
        T.unpack path
          <> "-"
          <> ref
          <> " does not contain a directory "
          <> pdir

    noPkgPath =
      fail $
        "futhark.pkg for "
          <> T.unpack path
          <> "-"
          <> ref
          <> " does not define a package path."

    getFiles gm = GetFiles $ do
      gitCmd_ ["-C", gitdir, "checkout", ref, "--"]
      pdir <- maybe noPkgPath pure . pkgDir =<< getManifest gm
      let pdir_abs = gitdir </> pdir
      exists <- liftIO $ doesDirectoryExist pdir_abs
      unless exists $ noPkgDir pdir
      fs <- liftIO $ directoryContents pdir_abs
      pure (pdir_abs, map (makeRelative pdir_abs) fs)

    getManifest' = GetManifest $ do
      gitCmd_ ["-C", gitdir, "checkout", ref, "--"]
      let f = gitdir </> futharkPkg
      s <- liftIO $ T.readFile f
      let msg =
            "When reading package manifest for "
              <> T.unpack path
              <> " "
              <> ref
              <> ":\n"
      case parsePkgManifest f s of
        Left e -> fail $ msg <> errorBundlePretty e
        Right pm -> pure pm

-- | Retrieve information about a package based on its package path.
-- This uses Semantic Import Versioning when interacting with
-- repositories.  For example, a package @github.com/user/repo@ will
-- match version 0.* or 1.* tags only, a package
-- @github.com/user/repo/v2@ will match 2.* tags, and so forth..
pkgInfo ::
  (MonadIO m, MonadLogger m, MonadFail m) =>
  CacheDir ->
  PkgPath ->
  m (PkgInfo m)
pkgInfo cachedir path = do
  gitdir <- ensureGit cachedir url
  versions <- mapMaybe isVersionRef <$> gitCmdLines ["-C", gitdir, "tag"]
  versions' <-
    M.fromList . zip versions
      <$> mapM (revInfo gitdir path . versionRef) versions
  pure $ PkgInfo versions' $ lookupCommit gitdir
  where
    (url, path_versions) = majorRevOfPkg path
    isVersionRef l
      | "v" `T.isPrefixOf` l,
        Right v <- parseVersion $ T.drop 1 l,
        _svMajor v `elem` path_versions =
          Just v
      | otherwise = Nothing

    lookupCommit gitdir = revInfo gitdir path . maybe "HEAD" T.unpack

-- | A package registry is a mapping from package paths to information
-- about the package.  It is unlikely that any given registry is
-- global; rather small registries are constructed on-demand based on
-- the package paths referenced by the user, and may also be combined
-- monoidically.  In essence, the PkgRegistry is just a cache.
newtype PkgRegistry m = PkgRegistry (M.Map PkgPath (PkgInfo m))

instance Semigroup (PkgRegistry m) where
  PkgRegistry x <> PkgRegistry y = PkgRegistry $ x <> y

instance Monoid (PkgRegistry m) where
  mempty = PkgRegistry mempty

lookupKnownPackage :: PkgPath -> PkgRegistry m -> Maybe (PkgInfo m)
lookupKnownPackage p (PkgRegistry m) = M.lookup p m

-- | Monads that support a stateful package registry.  These are also
-- required to be instances of 'MonadIO' because most package registry
-- operations involve network operations.
class (MonadIO m, MonadLogger m, MonadFail m) => MonadPkgRegistry m where
  getPkgRegistry :: m (PkgRegistry m)
  putPkgRegistry :: PkgRegistry m -> m ()
  modifyPkgRegistry :: (PkgRegistry m -> PkgRegistry m) -> m ()
  modifyPkgRegistry f = putPkgRegistry . f =<< getPkgRegistry

-- | Given a package path, look up information about that package.
lookupPackage ::
  (MonadPkgRegistry m) =>
  CacheDir ->
  PkgPath ->
  m (PkgInfo m)
lookupPackage cachedir p = do
  r@(PkgRegistry m) <- getPkgRegistry
  case lookupKnownPackage p r of
    Just info ->
      pure info
    Nothing -> do
      pinfo <- pkgInfo cachedir p
      putPkgRegistry $ PkgRegistry $ M.insert p pinfo m
      pure pinfo

lookupPackageCommit ::
  (MonadPkgRegistry m) =>
  CacheDir ->
  PkgPath ->
  Maybe T.Text ->
  m (SemVer, PkgRevInfo m)
lookupPackageCommit cachedir p ref = do
  pinfo <- lookupPackage cachedir p
  rev_info <- pkgLookupCommit pinfo ref
  let timestamp =
        T.pack $
          formatTime defaultTimeLocale "%Y%m%d%H%M%S" $
            pkgRevTime rev_info
      v = commitVersion timestamp $ pkgRevCommit rev_info
      pinfo' = pinfo {pkgVersions = M.insert v rev_info $ pkgVersions pinfo}
  modifyPkgRegistry $ \(PkgRegistry m) ->
    PkgRegistry $ M.insert p pinfo' m
  pure (v, rev_info)

-- | Look up information about a specific version of a package.
lookupPackageRev ::
  (MonadPkgRegistry m) =>
  CacheDir ->
  PkgPath ->
  SemVer ->
  m (PkgRevInfo m)
lookupPackageRev cachedir p v
  | Just commit <- isCommitVersion v =
      snd <$> lookupPackageCommit cachedir p (Just commit)
  | otherwise = do
      pinfo <- lookupPackage cachedir p
      case lookupPkgRev v pinfo of
        Nothing ->
          let versions = case M.keys $ pkgVersions pinfo of
                [] -> "Package " <> p <> " has no versions.  Invalid package path?"
                ks ->
                  "Known versions: "
                    <> T.concat (L.intersperse ", " $ map prettySemVer ks)
              major
                | (_, vs) <- majorRevOfPkg p,
                  _svMajor v `notElem` vs =
                    "\nFor major version "
                      <> showText (_svMajor v)
                      <> ", use package path "
                      <> p
                      <> "@"
                      <> showText (_svMajor v)
                | otherwise = mempty
           in fail $
                T.unpack $
                  "package "
                    <> p
                    <> " does not have a version "
                    <> prettySemVer v
                    <> ".\n"
                    <> versions
                    <> major
        Just v' -> pure v'

-- | Find the newest version of a package.
lookupNewestRev ::
  (MonadPkgRegistry m) =>
  CacheDir ->
  PkgPath ->
  m SemVer
lookupNewestRev cachedir p = do
  pinfo <- lookupPackage cachedir p
  case M.keys $ pkgVersions pinfo of
    [] -> do
      logMsg $ "Package " <> p <> " has no released versions.  Using HEAD."
      fst <$> lookupPackageCommit cachedir p Nothing
    v : vs -> pure $ L.foldl' max v vs
