{-# LANGUAGE OverloadedStrings #-}
-- | Obtaining information about packages over THE INTERNET!
module Futhark.Pkg.Info
  ( -- * Package info
    PkgInfo(..)
  , lookupPkgRev
  , pkgInfo
  , PkgRevInfo (..)
  , GetManifest (getManifest)
  , downloadZipball

    -- * Package registry
  , PkgRegistry
  , MonadPkgRegistry(..)
  , lookupPackage
  , lookupPackageRev
  , lookupNewestRev
  )
  where

import Control.Monad.IO.Class
import Data.Maybe
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Data.List
import qualified System.FilePath.Posix as Posix
import System.Exit
import System.IO

import qualified Codec.Archive.Zip as Zip
import Data.Time (UTCTime, UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.Process.ByteString (readProcessWithExitCode)
import Network.HTTP.Client hiding (path)
import Network.HTTP.Simple

import Futhark.Pkg.Types
import Futhark.Util.Log
import Futhark.Util (maybeHead)

-- | The manifest is stored as a monadic action, because we want to
-- fetch them on-demand.  It would be a waste to fetch it information
-- for every version of every package if we only actually need a small
-- subset of them.
newtype GetManifest m = GetManifest { getManifest :: m PkgManifest }

instance Show (GetManifest m) where
  show _ = "#<revdeps>"

instance Eq (GetManifest m) where
  _ == _ = True

-- | Information about a version of a single package.  The version
-- number is stored separately.
data PkgRevInfo m = PkgRevInfo { pkgRevZipballUrl :: T.Text
                               , pkgRevZipballDir :: FilePath
                                 -- ^ The directory inside the zipball
                                 -- containing the 'lib' directory, in
                                 -- which the package files themselves
                                 -- are stored (Based on the package
                                 -- path).
                               , pkgRevCommit :: T.Text
                                 -- ^ The commit ID can be used for
                                 -- verification ("freezing"), by
                                 -- storing what it was at the time this
                                 -- version was last selected.
                               , pkgRevGetManifest :: GetManifest m
                               , pkgRevTime :: UTCTime
                                 -- ^ Timestamp for when the revision
                                 -- was made (rarely used).
                               }
                  deriving (Eq, Show)

-- | Create memoisation around a 'GetManifest' action to ensure that
-- multiple inspections of the same revisions will not result in
-- potentially expensive network round trips.
memoiseGetManifest :: MonadIO m => GetManifest m -> m (GetManifest m)
memoiseGetManifest (GetManifest m) = do
  ref <- liftIO $ newIORef Nothing
  return $ GetManifest $ do
    v <- liftIO $ readIORef ref
    case v of Just v' -> return v'
              Nothing -> do
                v' <- m
                liftIO $ writeIORef ref $ Just v'
                return v'

downloadZipball :: (MonadLogger m, MonadIO m, MonadFail m) =>
                   T.Text -> m Zip.Archive
downloadZipball url = do
  logMsg $ "Downloading " <> T.unpack url
  r <- liftIO $ parseRequest $ T.unpack url

  r' <- liftIO $ httpLBS r
  let bad = fail . (("When downloading " <> T.unpack url <> ": ")<>)
  case getResponseStatusCode r' of
    200 ->
      case Zip.toArchiveOrFail $ getResponseBody r' of
        Left e -> bad $ show e
        Right a -> return a
    x -> bad $ "got HTTP status " ++ show x

-- | Information about a package.  The name of the package is stored
-- separately.
data PkgInfo m = PkgInfo { pkgVersions :: M.Map SemVer (PkgRevInfo m)
                         , pkgLookupCommit :: Maybe T.Text -> m (PkgRevInfo m)
                           -- ^ Look up information about a specific
                           -- commit, or HEAD in case of Nothing.
                         }

lookupPkgRev :: SemVer -> PkgInfo m -> Maybe (PkgRevInfo m)
lookupPkgRev v = M.lookup v . pkgVersions

majorRevOfPkg :: PkgPath -> (PkgPath, [Word])
majorRevOfPkg p =
  case T.splitOn "@" p of
    [p', v] | [(v', "")] <- reads $ T.unpack v -> (p', [v'])
    _                                          -> (p, [0, 1])

-- | Retrieve information about a package based on its package path.
-- This uses Semantic Import Versioning when interacting with
-- repositories.  For example, a package @github.com/user/repo@ will
-- match version 0.* or 1.* tags only, a package
-- @github.com/user/repo/v2@ will match 2.* tags, and so forth..
pkgInfo :: (MonadIO m, MonadLogger m, MonadFail m) =>
           PkgPath -> m (Either T.Text (PkgInfo m))
pkgInfo path
  | ["github.com", owner, repo] <- T.splitOn "/" path =
      let (repo', vs) = majorRevOfPkg repo
      in ghPkgInfo owner repo' vs
  | "github.com": owner : repo : _ <- T.splitOn "/" path =
      return $ Left $ T.intercalate "\n"
      [nope, "Do you perhaps mean 'github.com/" <> owner <> "/" <> repo <> "'?"]
  | ["gitlab.com", owner, repo] <- T.splitOn "/" path =
      let (repo', vs) = majorRevOfPkg repo
      in glPkgInfo owner repo' vs
  | "gitlab.com": owner : repo : _ <- T.splitOn "/" path =
      return $ Left $ T.intercalate "\n"
      [nope, "Do you perhaps mean 'gitlab.com/" <> owner <> "/" <> repo <> "'?"]
  | otherwise =
      return $ Left nope
  where nope = "Unable to handle package paths of the form '" <> path <> "'"

-- For GitHub, we unfortunately cannot use the (otherwise very nice)
-- GitHub web API, because it is rate-limited to 60 requests per hour
-- for non-authenticated users.  Instead we fall back to a combination
-- of calling 'git' directly and retrieving things from the GitHub
-- webserver, which is not rate-limited.  This approach is also used
-- by other systems (Go most notably), so we should not be stepping on
-- any toes.

gitCmd :: (MonadIO m, MonadFail m) => [String] -> m BS.ByteString
gitCmd opts = do
  (code, out, err) <- liftIO $ readProcessWithExitCode "git" opts mempty
  liftIO $ BS.hPutStr stderr err
  case code of
    ExitFailure 127 -> fail $ "'" <> unwords ("git" : opts) <> "' failed (program not found?)."
    ExitFailure _ -> fail $ "'" <> unwords ("git" : opts) <> "' failed."
    ExitSuccess -> return out

-- The GitLab and GitHub interactions are very similar, so we define a
-- couple of generic functions that are used to implement support for
-- both.

ghglRevGetManifest :: (MonadIO m, MonadLogger m, MonadFail m) =>
                      T.Text -> T.Text -> T.Text -> T.Text -> GetManifest m
ghglRevGetManifest url owner repo tag = GetManifest $ do
  logMsg $ "Downloading package manifest from " <> url
  r <- liftIO $ parseRequest $ T.unpack url

  r' <- liftIO $ httpBS r
  let path = T.unpack $ owner <> "/" <> repo <> "@" <>
             tag <> "/" <> T.pack futharkPkg
      msg = (("When reading " <> path <> ": ")<>)
  case getResponseStatusCode r' of
    200 ->
      case T.decodeUtf8' $ getResponseBody r' of
        Left e -> fail $ msg $ show e
        Right s ->
          case parsePkgManifest path s of
            Left e -> fail $ msg $ errorBundlePretty e
            Right pm -> return pm
    x -> fail $ msg $ "got HTTP status " ++ show x

ghglLookupCommit :: (MonadIO m, MonadLogger m, MonadFail m) =>
                    T.Text -> T.Text
                 -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> m (PkgRevInfo m)
ghglLookupCommit archive_url manifest_url owner repo d ref hash = do
  gd <- memoiseGetManifest $ ghglRevGetManifest manifest_url owner repo ref
  let dir = Posix.addTrailingPathSeparator $ T.unpack repo <> "-" <> T.unpack d
  time <- liftIO getCurrentTime -- FIXME
  return $ PkgRevInfo archive_url dir hash gd time

ghglPkgInfo :: (MonadIO m, MonadLogger m, MonadFail m) =>
               T.Text -> (T.Text -> T.Text) -> (T.Text -> T.Text)
            -> T.Text -> T.Text -> [Word] -> m (Either T.Text (PkgInfo m))
ghglPkgInfo repo_url mk_archive_url mk_manifest_url owner repo versions = do
  logMsg $ "Retrieving list of tags from " <> repo_url
  remote_lines <- T.lines . T.decodeUtf8 <$> gitCmd ["ls-remote", T.unpack repo_url]

  head_ref <- maybe (fail $ "Cannot find HEAD ref for " <> T.unpack repo_url) return $
              maybeHead $ mapMaybe isHeadRef remote_lines
  let def = fromMaybe head_ref

  rev_info <- M.fromList . catMaybes <$> mapM revInfo remote_lines

  return $ Right $ PkgInfo rev_info $ \r ->
    ghglLookupCommit (mk_archive_url (def r)) (mk_manifest_url (def r))
    owner repo (def r) (def r) (def r)
  where isHeadRef l
          | [hash, "HEAD"] <- T.words l = Just hash
          | otherwise                   = Nothing

        revInfo l
          | [hash, ref] <- T.words l,
            ["refs", "tags", t] <- T.splitOn "/" ref,
            "v" `T.isPrefixOf` t,
            Right v <- semver $ T.drop 1 t,
            _svMajor v `elem` versions = do
              pinfo <- ghglLookupCommit (mk_archive_url t) (mk_manifest_url t)
                       owner repo (prettySemVer v) t hash
              return $ Just (v, pinfo)
          | otherwise = return Nothing

ghPkgInfo :: (MonadIO m, MonadLogger m, MonadFail m) =>
             T.Text -> T.Text -> [Word] -> m (Either T.Text (PkgInfo m))
ghPkgInfo owner repo versions =
  ghglPkgInfo repo_url mk_archive_url mk_manifest_url owner repo versions
  where repo_url = "https://github.com/" <> owner <> "/" <> repo
        mk_archive_url r = repo_url <> "/archive/" <> r <> ".zip"
        mk_manifest_url r = "https://raw.githubusercontent.com/" <>
                            owner <> "/" <> repo <> "/" <>
                            r <> "/" <> T.pack futharkPkg

glPkgInfo :: (MonadIO m, MonadLogger m, MonadFail m) =>
             T.Text -> T.Text -> [Word] -> m (Either T.Text (PkgInfo m))
glPkgInfo owner repo versions =
  ghglPkgInfo repo_url mk_archive_url mk_manifest_url owner repo versions
  where base_url = "https://gitlab.com/" <> owner <> "/" <> repo
        repo_url = base_url <> ".git"
        mk_archive_url r = base_url <> "/-/archive/" <> r <>
                           "/" <> repo <> "-" <> r <> ".zip"
        mk_manifest_url r = base_url <> "/raw/" <>
                            r <> "/" <> T.pack futharkPkg

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

lookupPackage :: MonadPkgRegistry m =>
                 PkgPath -> m (PkgInfo m)
lookupPackage p = do
  r@(PkgRegistry m) <- getPkgRegistry
  case lookupKnownPackage p r of
    Just info ->
      return info
    Nothing -> do
      e <- pkgInfo p
      case e of
        Left e' -> fail $ T.unpack e'
        Right pinfo -> do
          putPkgRegistry $ PkgRegistry $ M.insert p pinfo m
          return pinfo

lookupPackageCommit :: MonadPkgRegistry m =>
                       PkgPath -> Maybe T.Text -> m (SemVer, PkgRevInfo m)
lookupPackageCommit p ref = do
  pinfo <- lookupPackage p
  rev_info <- pkgLookupCommit pinfo ref
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M%S" $
                  pkgRevTime rev_info
      v = commitVersion timestamp $ pkgRevCommit rev_info
      pinfo' = pinfo { pkgVersions = M.insert v rev_info $ pkgVersions pinfo }
  modifyPkgRegistry $ \(PkgRegistry m) ->
    PkgRegistry $ M.insert p pinfo' m
  return (v, rev_info)

-- | Look up information about a specific version of a package.
lookupPackageRev :: MonadPkgRegistry m =>
                    PkgPath -> SemVer -> m (PkgRevInfo m)
lookupPackageRev p v
  | Just commit <- isCommitVersion v =
      snd <$> lookupPackageCommit p (Just commit)
  | otherwise = do
  pinfo <- lookupPackage p
  case lookupPkgRev v pinfo of
    Nothing ->
      let versions = case M.keys $ pkgVersions pinfo of
                       [] -> "Package " <> p <> " has no versions.  Invalid package path?"
                       ks -> "Known versions: " <>
                             T.concat (intersperse ", " $ map prettySemVer ks)
          major | (_, vs) <- majorRevOfPkg p,
                  _svMajor v `notElem` vs =
                    "\nFor major version " <> T.pack (show (_svMajor v)) <>
                    ", use package path " <> p <> "@" <> T.pack (show (_svMajor v))
                | otherwise = mempty
      in fail $ T.unpack $
         "package " <> p <> " does not have a version " <> prettySemVer v <> ".\n" <>
         versions <> major
    Just v' -> return v'

-- | Find the newest version of a package.
lookupNewestRev :: MonadPkgRegistry m =>
                   PkgPath -> m SemVer
lookupNewestRev p = do
  pinfo <- lookupPackage p
  case M.keys $ pkgVersions pinfo of
    [] -> do
      logMsg $ "Package " <> p <> " has no released versions.  Using HEAD."
      fst <$> lookupPackageCommit p Nothing
    v:vs -> return $ foldl' max v vs
