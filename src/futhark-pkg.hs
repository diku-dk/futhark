{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Monoid
import System.Directory
import System.FilePath
import qualified System.FilePath.Posix as Posix
import System.Environment
import System.Exit
import System.IO

import qualified Codec.Archive.Zip as Zip
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Prelude

import Futhark.Util.Options
import Futhark.Pkg.Types
import Futhark.Pkg.Info
import Futhark.Pkg.Solve
import Futhark.Util (directoryContents)
import Futhark.Util.Log

--- Installing packages

libDir, libNewDir, libOldDir :: FilePath
(libDir, libNewDir, libOldDir) = ("lib", "lib~new", "lib~old")

mkLibNewDir :: IO ()
mkLibNewDir = do
  removePathForcibly libNewDir
  createDirectoryIfMissing False "lib~new"

installInDir :: BuildList -> FilePath -> PkgM ()
installInDir (BuildList bl) dir = do
  let putEntry pdir info entry
        | not (isInPkgDir info $ Zip.eRelativePath entry)
          || hasTrailingPathSeparator (Zip.eRelativePath entry) = return ()
        | otherwise = do
        -- Since we are writing to paths indicated in a zipfile we
        -- downloaded from the wild Internet, we are going to be a
        -- little bit paranoid.  Specifically, we want to avoid
        -- writing outside of the 'lib/' directory.  We do this by
        -- bailing out if the path contains any '..' components.  We
        -- have to use System.FilePath.Posix, because the zip library
        -- claims to encode filepaths with '/' directory seperators no
        -- matter the host OS.
        when (".." `elem` Posix.splitPath (Zip.eRelativePath entry)) $
          fail $ "Zip archive for " <> pdir <> " contains suspicuous path: " <>
          Zip.eRelativePath entry
        let f = pdir </> makeRelative (pkgRevPkgDir info) (Zip.eRelativePath entry)
        createDirectoryIfMissing True $ takeDirectory f
        LBS.writeFile f $ Zip.fromEntry entry

      isInPkgDir info f =
        splitPath (pkgRevPkgDir info) `isPrefixOf` Posix.splitPath f

  forM_ (M.toList bl) $ \(p, v) -> do
    info <- lookupPackageRev p v
    a <- downloadZipball $ pkgRevZipballUrl info

    -- The directory that will contain the package.
    let pdir = dir </> T.unpack p
    -- Remove any existing directory for this package.  This is a bit
    -- inefficient, as the likelihood that the old ``lib`` directory
    -- already contains the correct version is rather high.  We should
    -- have a way to recognise this situation, and not download the
    -- zipball in that case.
    liftIO $ removePathForcibly pdir
    liftIO $ createDirectoryIfMissing True pdir

    liftIO $ mapM_ (putEntry pdir info) $ Zip.zEntries a

-- | Install the packages listed in the build list in the 'lib'
-- directory of the current working directory.  Since we are touching
-- the file system, we are going to be very paranoid.  In particular,
-- we want to avoid corrupting the 'lib' directory if something fails
-- along the way.
--
-- The procedure is as follows:
--
-- 1) Create a directory 'lib~new'.  Delete an existing 'lib~new' if
-- necessary.
--
-- 2) Populate 'lib~new' based on the build list.
--
-- 3) Rename 'lib' to 'lib~old'.  Delete an existing 'lib~old' if
-- necessary.
--
-- 4) Rename 'lib~new' to 'lib'
--
-- 5) Delete 'lib~old'.
--
-- Since POSIX at least guarantees atomic renames, the only place this
-- can fail is between steps 3 and 4.  In that case, at least the
-- 'lib~old' will still exist and can be put back by the user.
installBuildList :: BuildList -> PkgM ()
installBuildList bl = do
  libdir_exists <- liftIO $ doesDirectoryExist libDir

  liftIO mkLibNewDir
  installInDir bl libNewDir
  when libdir_exists $ liftIO $ renameDirectory libDir libOldDir
  liftIO $ renameDirectory libNewDir libDir
  when libdir_exists $ liftIO $ removePathForcibly libOldDir

getPkgManifest :: PkgM PkgManifest
getPkgManifest = do
  file_exists <- liftIO $ doesFileExist futharkPkg
  dir_exists <- liftIO $ doesDirectoryExist futharkPkg

  case (file_exists, dir_exists) of
    (True, _) -> liftIO $ parsePkgManifestFromFile futharkPkg
    (_, True) -> fail $ futharkPkg <>
                 " exists, but it is a directory!  What in Odin's beard..."
    _         -> liftIO $ do T.putStrLn $ T.pack futharkPkg <> " not found - pretending it's empty."
                             return $ newPkgManifest Nothing

putPkgManifest :: PkgManifest -> PkgM ()
putPkgManifest = liftIO . T.writeFile futharkPkg . prettyPkgManifest

--- The CLI

-- | The monad in which futhark-pkg runs.
newtype PkgM a = PkgM (StateT (PkgRegistry PkgM) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadPkgRegistry PkgM where
  putPkgRegistry = PkgM . put
  getPkgRegistry = PkgM get

instance MonadLogger PkgM where
  addLog = liftIO . T.hPutStr stderr . toText

runPkgM :: PkgM a -> IO a
runPkgM (PkgM m) = evalStateT m mempty

doFmt :: IO ()
doFmt = mainWithOptions () [] $ \args () ->
  case args of
    [] -> Just $ do
      m <- parsePkgManifestFromFile futharkPkg
      T.writeFile futharkPkg $ prettyPkgManifest m
    _ -> Nothing

doCheck :: IO ()
doCheck = runPkgM $ do
  m <- getPkgManifest
  bl <- solveDeps $ pkgRevDeps m

  liftIO $ T.putStrLn "Dependencies chosen:"
  liftIO $ T.putStr $ prettyBuildList bl

  case commented $ manifestPkgPath m of
    Nothing -> return ()
    Just p -> do
      let pdir = "lib" </> T.unpack p

      pdir_exists <- liftIO $ doesDirectoryExist pdir

      unless pdir_exists $ liftIO $ do
        T.putStrLn $ "Problem: the directory " <> T.pack pdir <> " does not exist."
        exitFailure

      anything <- liftIO $ any ((==".fut") . takeExtension) <$>
                  directoryContents ("lib" </> T.unpack p)
      unless anything $ liftIO $ do
        T.putStrLn $ "Problem: the directory " <> T.pack pdir <> " does not contain any .fut files."
        exitFailure

doSync :: IO ()
doSync = runPkgM $ do
  m <- getPkgManifest
  bl <- solveDeps $ pkgRevDeps m
  installBuildList bl

doAdd :: IO ()
doAdd = mainWithOptions () [] $ \args () ->
  case args of
    [p, v] | Right v' <- semver $ T.pack v -> Just $ runPkgM $ doAdd' (T.pack p) v'
    [p] -> Just $ runPkgM $
      -- Look up the newest revision of the package.
      doAdd' (T.pack p) =<< lookupNewestRev (T.pack p)
    _ -> Nothing

  where
    doAdd' p v = do
      m <- getPkgManifest

      -- See if this package (and its dependencies) even exists.  We
      -- do this by running the solver with the dependencies already
      -- in the manifest, plus this new one.  The Monoid instance for
      -- PkgRevDeps is left-biased, so we are careful to use the new
      -- version for this package.
      _ <- solveDeps $ PkgRevDeps (M.singleton p (v, Nothing)) <> pkgRevDeps m

      -- We either replace any existing occurence of package 'p', or
      -- we add a new one.
      p_info <- lookupPackageRev p v
      let req = Required p v $ Just $ pkgRevCommit p_info
          (m', prev_r) = addRequiredToManifest req m

      case prev_r of
        Just prev_r' ->
          liftIO $ T.putStrLn $ "Replaced " <> p <> " " <>
          prettySemVer (requiredPkgRev prev_r') <> " => " <> prettySemVer v <> "."
        Nothing ->
          liftIO $ T.putStrLn $ "Added new required package " <> p <> " " <> prettySemVer v <> "."
      putPkgManifest m'
      liftIO $ T.putStrLn "Remember to run 'futhark-pkg sync'."

doRemove :: IO ()
doRemove = mainWithOptions () [] $ \args () ->
  case args of
    [p] -> Just $ doRemove' $ T.pack p
    _ -> Nothing
  where
    doRemove' p = runPkgM $ do
      m <- getPkgManifest
      case removeRequiredFromManifest p m of
        Nothing -> liftIO $ do
          T.putStrLn $ "No package " <> p <> " found in " <> T.pack futharkPkg <> "."
          exitFailure
        Just (m', r) -> do
          putPkgManifest m'
          liftIO $ T.putStrLn $ "Removed " <> p <> " " <> prettySemVer (requiredPkgRev r) <> "."

doCreate :: IO ()
doCreate = mainWithOptions () [] $ \args () ->
  case args of
    [p] -> Just $ doCreate' $ T.pack p
    _ -> Nothing
  where
    doCreate' p = runPkgM $ do
      exists <- liftIO $ (||) <$> doesFileExist futharkPkg <*> doesDirectoryExist futharkPkg
      when exists $ liftIO $ do
        T.putStrLn $ T.pack futharkPkg <> " already exists."
        exitFailure

      liftIO $ createDirectoryIfMissing True $ "lib" </> T.unpack p
      liftIO $ T.putStrLn $ "Created directory " <> T.pack ("lib" </> T.unpack p) <> "."

      putPkgManifest $ newPkgManifest $ Just p
      liftIO $ T.putStrLn $ "Wrote " <> T.pack futharkPkg <> "."

doUpgrade :: IO ()
doUpgrade = runPkgM $ do
  m <- getPkgManifest
  rs <- traverse (mapM (traverse upgrade)) $ manifestRequire m
  putPkgManifest m { manifestRequire = rs }
  where upgrade req = do
          v <- lookupNewestRev $ requiredPkg req
          h <- pkgRevCommit <$> lookupPackageRev (requiredPkg req) v

          when (v /= requiredPkgRev req) $
            liftIO $ T.putStrLn $ "Upgraded " <> requiredPkg req <> " " <>
            prettySemVer (requiredPkgRev req) <> " => " <> prettySemVer v <> "."

          return req { requiredPkgRev = v
                     , requiredHash = Just h }

main :: IO ()
main = do
  -- Ensure that we can make HTTPS requests.
  setGlobalManager =<< newManager tlsManagerSettings

  args <- getArgs
  let commands = [ ("add",
                    (doAdd, "Add another required package to futhark.pkg."))
                 , ("check",
                    (doCheck, "Check that futhark.pkg is satisfiable."))
                 , ("create",
                    (doCreate, "Create a new futhark.pkg and a lib/ skeleton."))
                 , ("fmt",
                    (doFmt, "Reformat futhark.pkg."))
                 , ("sync",
                    (doSync, "Populate lib/ as specified by futhark.pkg."))
                 , ("remove",
                    (doRemove, "Remove a required package from futhark.pkg."))
                 , ("upgrade",
                    (doUpgrade, "Upgrade all packages to newest versions."))
                 ]
  case args of
    cmd : args' | Just (m, _) <- lookup cmd commands -> withArgs args' m
    _ -> mainWithOptions () [] $ \_ () -> Just $ do
      T.putStrLn "Usage: futhark-pkg [--version] [--help] <command> ...:"
      T.putStrLn ""
      T.putStrLn "Commands:"
      let k = maximum (map (length . fst) commands) + 3
      forM_ commands $ \(cmd, (_, desc)) ->
        T.putStrLn $ "   " <> T.pack cmd <>
        T.pack (replicate (k - length cmd) ' ') <> desc
      exitFailure
