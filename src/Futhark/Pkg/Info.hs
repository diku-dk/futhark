{-# LANGUAGE OverloadedStrings #-}
-- | Obtaining information about packages over THE INTERNET!
module Futhark.Pkg.Info
  ( -- * Package info
    PkgInfo(..)
  , lookupPkgRev
  , pkgInfo
  , PkgRevInfo (..)
  , GetDeps (..)
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
import qualified Data.Semigroup as Sem
import Data.List
import Data.Monoid ((<>))
import qualified System.FilePath.Posix as Posix
import System.Environment
import System.Exit
import System.IO

import qualified Codec.Archive.Zip as Zip
import Data.Versions (SemVer(..), semver, prettySemVer, parseErrorPretty)
import System.Process.ByteString (readProcessWithExitCode)
import Network.HTTP.Client hiding (path)
import Network.HTTP.Simple

import Futhark.Pkg.Types
import Futhark.Util.Log

-- | Revision dependencies are stored as a monadic action, because we
-- want to fetch them on-demand.  It would be a waste to fetch
-- dependency information for every version of every package if we
-- only actually need a small subset of them.
newtype GetDeps m = GetDeps (m PkgRevDeps)

instance Show (GetDeps m) where
  show _ = "#<revdeps>"

instance Eq (GetDeps m) where
  _ == _ = True

-- | Information about a version of a single package.  The version
-- number is stored separately.
data PkgRevInfo m = PkgRevInfo { pkgRevZipballUrl :: T.Text
                               , pkgRevPkgDir :: FilePath
                                 -- ^ The directory inside the tarball
                                 -- containing the package source code.
                               , pkgRevCommit :: T.Text
                                 -- ^ The commit ID can be used for
                                 -- verification ("freezing"), by
                                 -- storing what it was at the time this
                                 -- version was last selected.
                               , pkgRevGetDeps :: GetDeps m
                               }
                  deriving (Eq, Show)

-- | Create memoisation around a 'GetDeps' action to ensure that
-- multiple inspections of the same revisions will not result in
-- potentially expensive network round trips.
memoiseGetDeps :: MonadIO m => GetDeps m -> m (GetDeps m)
memoiseGetDeps (GetDeps m) = do
  ref <- liftIO $ newIORef Nothing
  return $ GetDeps $ do
    v <- liftIO $ readIORef ref
    case v of Just v' -> return v'
              Nothing -> do
                v' <- m
                liftIO $ writeIORef ref $ Just v'
                return v'

downloadZipball :: (MonadLogger m, MonadIO m) =>
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
newtype PkgInfo m = PkgInfo { pkgVersions :: M.Map SemVer (PkgRevInfo m) }
  deriving (Show)

lookupPkgRev :: SemVer -> PkgInfo m -> Maybe (PkgRevInfo m)
lookupPkgRev v (PkgInfo m) = M.lookup v m

majorRevOfPkg :: PkgPath -> (PkgPath, [Word])
majorRevOfPkg p =
  case T.splitOn "@" p of
    [p', v] | [(v', "")] <- reads $ T.unpack v -> (p', [v'])
    _                                          -> (p, [0, 1])

-- | Retrieve information about a package based on its package path.
-- This uses Semantic Import Versioning when interacting with GitHub
-- repositories.  Specifically, a package @github.com/user/repo@ will
-- match version 0.* or 1.* tags only, a package
-- @github.com/user/repo/v2@ will match 2.* tags, and so forth..
pkgInfo :: (MonadIO m, MonadLogger m) =>
           PkgPath -> m (Either T.Text (PkgInfo m))
pkgInfo path
  | ["github.com", owner, repo] <- T.splitOn "/" path =
      let (repo', vs) = majorRevOfPkg repo
      in ghPkgInfo path owner repo' vs
pkgInfo path =
  return $ Left $ "Unable to handle package paths of the form '" <> path <> "'"

-- For Github, we unfortunately cannot use the (otherwise very nice)
-- GitHub web API, because it is rate-limited to 60 requests per hour
-- for non-authenticated users.  Instead we fall back to a combination
-- of calling 'git' directly and retrieving things from the GitHub
-- webserver, which is not rate-limited.  This approach is also used
-- by other systems (Go most notably), so we should not be stepping on
-- any toes.

ghPkgInfo :: (MonadIO m, MonadLogger m) =>
             PkgPath -> T.Text -> T.Text -> [Word] -> m (Either T.Text (PkgInfo m))
ghPkgInfo path owner repo versions = do
  logMsg $ "Retrieving list of tags from " <> repo_url
  let prog = "git"
      prog_opts :: [String]
      prog_opts = ["ls-remote", "--tags", repo_url]
  -- Avoid Git asking for credentials.  We prefer failure.
  liftIO $ setEnv "GIT_TERMINAL_PROMPT" "0"
  (code, out, err) <- liftIO $ readProcessWithExitCode prog prog_opts mempty
  liftIO $ BS.hPutStr stderr err

  case code of
    ExitFailure 127 -> fail $ "'" <> unwords (prog : prog_opts) <> "' failed (program not found?)."
    ExitFailure _ -> fail $ "'" <> unwords (prog : prog_opts) <> "' failed."
    ExitSuccess -> return ()

  Right . PkgInfo . M.fromList . catMaybes <$>
    mapM revInfo (T.lines $ T.decodeUtf8 out)
  where repo_url = "https://github.com/" <> T.unpack owner <> "/" <> T.unpack repo

        revInfo l
          | [hash, ref] <- T.words l,
            ["refs", "tags", t] <- T.splitOn "/" ref,
            "v" `T.isPrefixOf` t,
            Right v <- semver $ T.drop 1 t,
            _svMajor v `elem` versions = do
              gd <- memoiseGetDeps $ ghRevGetDeps owner repo t
              let dir = Posix.addTrailingPathSeparator $
                        T.unpack repo <> "-" <> T.unpack (prettySemVer v) Posix.</>
                        "lib" Posix.</> T.unpack path
              return $ Just (v, PkgRevInfo
                                (T.pack repo_url <> "/archive/" <> t <> ".zip")
                                dir
                                hash
                                gd)
          | otherwise = return Nothing

ghRevGetDeps :: (MonadIO m, MonadLogger m) =>
                T.Text -> T.Text -> T.Text -> GetDeps m
ghRevGetDeps owner repo tag = GetDeps $ do
  let url = "https://raw.githubusercontent.com/" <>
            owner <> "/" <> repo <> "/" <>
            tag <> "/" <> T.pack futharkPkg
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
            Left e -> fail $ msg $ parseErrorPretty e
            Right pm -> return $ pkgRevDeps pm
    x -> fail $ msg $ "got HTTP status " ++ show x

-- | A package registry is a mapping from package paths to information
-- about the package.  It is unlikely that any given registry is
-- global; rather small registries are constructed on-demand based on
-- the package paths referenced by the user, and may also be combined
-- monoidically.  In essence, the PkgRegistry is just a cache.
newtype PkgRegistry m = PkgRegistry (M.Map PkgPath (PkgInfo m))

instance Sem.Semigroup (PkgRegistry m) where
  PkgRegistry x <> PkgRegistry y = PkgRegistry $ x <> y

instance Monoid (PkgRegistry m) where
  mempty = PkgRegistry mempty
  mappend = (Sem.<>)

lookupKnownPackage :: PkgPath -> PkgRegistry m -> Maybe (PkgInfo m)
lookupKnownPackage p (PkgRegistry m) = M.lookup p m

-- | Monads that support a stateful package registry.  These are also
-- required to be instances of 'MonadIO' because most package registry
-- operations involve network operations.
class (MonadIO m, MonadLogger m) => MonadPkgRegistry m where
  getPkgRegistry :: m (PkgRegistry m)
  putPkgRegistry :: PkgRegistry m -> m ()

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
        Left e' -> fail $ show e'
        Right pinfo -> do
          putPkgRegistry $ PkgRegistry $ M.insert p pinfo m
          return pinfo

lookupPackageRev :: MonadPkgRegistry m =>
                    PkgPath -> SemVer -> m (PkgRevInfo m)
lookupPackageRev p v = do
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
    [] -> fail $ T.unpack $
          "Package " <> p <> " has no versions.  Invalid package path?"
    v:vs -> return $ foldl' max v vs
