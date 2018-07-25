{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Exception (throw)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Monoid
import System.Directory
import System.FilePath
import System.Environment
import System.Exit

import qualified Codec.Archive.Zip as Zip
import qualified System.Directory.Tree as DirTree

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Prelude

import Futhark.Util.Options
import Futhark.Pkg.Types
import Futhark.Pkg.Info
import Futhark.Pkg.Solve
import Futhark.Util (directoryContents)

--- Installing packages

-- From https://stackoverflow.com/questions/6807025/what-is-the-haskell-way-to-copy-a-directory
--
-- Why is a simple function for this not on Hackage?
copyDirectory ::  FilePath -> FilePath -> IO ()
copyDirectory src dst =
  void $ do
    r <- DirTree.readDirectoryWithL (const $ return ()) src >>=
         DirTree.writeDirectoryWith copy
    case DirTree.failures $ DirTree.dirTree r of
      DirTree.Failed _ e : _ -> throw e
      _                      -> return ()
  where copy f () = do
          let f' = dst </> joinPath (drop 1 $ splitPath f)
          createDirectoryIfMissing True $ takeDirectory f'
          copyFile f f'

libDir, libNewDir, libOldDir :: FilePath
(libDir, libNewDir, libOldDir) = ("lib", "lib~new", "lib~old")

mkLibNewDir :: IO ()
mkLibNewDir = do
  removePathForcibly libNewDir
  exists <- doesPathExist libDir
  if exists
    then copyDirectory libDir libNewDir
    else createDirectoryIfMissing False "lib~new"

installInDir :: MonadPkgRegistry m => BuildList -> FilePath -> m ()
installInDir (BuildList bl) dir = do
  let putFile pdir info entry = do
        let f = pdir </> makeRelative (pkgRevPkgDir info) (Zip.eRelativePath entry)
        createDirectoryIfMissing True $ takeDirectory f
        LBS.writeFile f $ Zip.fromEntry entry

  forM_ (M.toList bl) $ \(p, v) -> do
    info <- lookupPackageRev p v
    a <- liftIO $ downloadZipball $ pkgRevZipballUrl info

    -- The directory that will contain the package.
    let pdir = dir </> T.unpack p
    -- Remove any existing directory for this package.  This is a bit
    -- inefficient, as the likelihood that the existing directory
    -- already contains the correct version is rather high.  We should
    -- have a way to recognise this situation, and not download the
    -- zipball in that case.
    liftIO $ removePathForcibly pdir
    liftIO $ createDirectoryIfMissing True pdir

    liftIO $ mapM_ (putFile pdir info) $
      mapMaybe (`Zip.findEntryByPath` a) $
      filter (not . hasTrailingPathSeparator) $
      filter (pkgRevPkgDir info `isPrefixOf`) $ Zip.filesInArchive a

-- | Install the packages listed in the build list in the 'lib'
-- directory of the current working directory.  Since we are touching
-- the file system, we are going to be very paranoid.  In particular,
-- we want to avoid corrupting the 'lib' directory if something fails
-- along the way.  Also, we want to maintain anything in the existing
-- 'lib' directory that we do not have an opinion about (such as
-- manually copied-in files and directories).  We will, however,
-- clobber any directories corresponding to the packages we are
-- installing.
--
-- The procedure is as follows:
--
-- 1) Copy 'lib' (if it exists) to 'lib~new'.  Delete an existing
-- 'lib~new' if necessary.
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
installBuildList :: MonadPkgRegistry m => BuildList -> m ()
installBuildList bl = do
  liftIO mkLibNewDir
  installInDir bl libNewDir
  liftIO $ renameDirectory libDir libOldDir
  liftIO $ renameDirectory libNewDir libDir
  liftIO $ removePathForcibly libOldDir

--- The CLI

-- | The monad in which futhark-pkg runs.
newtype PkgM a = PkgM (StateT PkgRegistry IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadPkgRegistry PkgM where
  putPkgRegistry = PkgM . put
  getPkgRegistry = PkgM get

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
doCheck = do
  m <- parsePkgManifestFromFile futharkPkg
  bl <- runPkgM $ solveDeps $ pkgRevDeps m

  T.putStrLn "Dependencies chosen:"
  T.putStr $ prettyBuildList bl

  case commented $ manifestPkgPath m of
    Nothing -> return ()
    Just p -> do
      let pdir = "lib" </> T.unpack p

      pdir_exists <- doesDirectoryExist pdir

      unless pdir_exists $ do
        T.putStrLn $ "Problem: the directory " <> T.pack pdir <> " does not exist."
        exitFailure

      anything <- any ((==".fut") . takeExtension) <$>
                  directoryContents ("lib" </> T.unpack p)
      unless anything $ do
        T.putStrLn $ "Problem: the directory " <> T.pack pdir <> " does not contain any .fut files."
        exitFailure

doGet :: IO ()
doGet = runPkgM $ do
  m <- liftIO $ parsePkgManifestFromFile futharkPkg
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
      m <- liftIO $ parsePkgManifestFromFile futharkPkg

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
      liftIO $ T.writeFile futharkPkg $ prettyPkgManifest m'
      liftIO $ T.putStrLn "Remember to run 'futhark-pkg get'."

doRemove :: IO ()
doRemove = mainWithOptions () [] $ \args () ->
  case args of
    [p] -> Just $ doRemove' $ T.pack p
    _ -> Nothing
  where
    doRemove' p = do
      m <- parsePkgManifestFromFile futharkPkg

      case removeRequiredFromManifest p m of
        Nothing -> do
          T.putStrLn $ "No package " <> p <> " found in " <> T.pack futharkPkg <> "."
          exitFailure
        Just (m', r) -> do
          T.writeFile futharkPkg $ prettyPkgManifest m'
          T.putStrLn $ "Removed " <> p <> " " <> prettySemVer (requiredPkgRev r) <> "."

doCreate :: IO ()
doCreate = mainWithOptions () [] $ \args () ->
  case args of
    [p] -> Just $ doCreate' $ T.pack p
    _ -> Nothing
  where
    doCreate' p = do
      exists <- (||) <$> doesFileExist futharkPkg <*> doesDirectoryExist futharkPkg
      when exists $ do
        T.putStrLn $ T.pack futharkPkg <> " already exists."
        exitFailure

      createDirectoryIfMissing True $ "lib" </> T.unpack p
      T.putStrLn $ "Created directory " <> T.pack ("lib" </> T.unpack p) <> "."

      T.writeFile futharkPkg $ prettyPkgManifest $ newPkgManifest p
      T.putStrLn $ "Wrote " <> T.pack futharkPkg <> "."

doUpgrade :: IO ()
doUpgrade = runPkgM $ do
  m <- liftIO $ parsePkgManifestFromFile futharkPkg

  rs <- traverse (mapM (traverse upgrade)) $ manifestRequire m
  liftIO $ T.writeFile futharkPkg $ prettyPkgManifest m { manifestRequire = rs }
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
                 , ("get",
                    (doGet, "Download packages listed in futhark.pkg to lib/."))
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
