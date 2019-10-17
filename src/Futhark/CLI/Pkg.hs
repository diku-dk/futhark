{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.CLI.Pkg (main) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
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
import System.Console.GetOpt

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

installInDir :: BuildList -> FilePath -> PkgM ()
installInDir (BuildList bl) dir = do
  let putEntry from_dir pdir entry
        -- The archive may contain all kinds of other stuff that we don't want.
        | not (isInPkgDir from_dir $ Zip.eRelativePath entry)
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
          fail $ "Zip archive for " <> pdir <> " contains suspicious path: " <>
          Zip.eRelativePath entry
        let f = pdir </> makeRelative from_dir (Zip.eRelativePath entry)
        createDirectoryIfMissing True $ takeDirectory f
        LBS.writeFile f $ Zip.fromEntry entry

      isInPkgDir from_dir f =
        Posix.splitPath from_dir `isPrefixOf` Posix.splitPath f

  forM_ (M.toList bl) $ \(p, v) -> do
    info <- lookupPackageRev p v
    a <- downloadZipball $ pkgRevZipballUrl info
    m <- getManifest $ pkgRevGetManifest info

    -- Compute the directory in the zipball that should contain the
    -- package files.
    let noPkgDir = fail $ "futhark.pkg for " ++ T.unpack p ++ "-" ++
                   T.unpack (prettySemVer v) ++ " does not define a package path."
    from_dir <- maybe noPkgDir (return . (pkgRevZipballDir info <>)) $ pkgDir m

    -- The directory in the local file system that will contain the
    -- package files.
    let pdir = dir </> T.unpack p
    -- Remove any existing directory for this package.  This is a bit
    -- inefficient, as the likelihood that the old ``lib`` directory
    -- already contains the correct version is rather high.  We should
    -- have a way to recognise this situation, and not download the
    -- zipball in that case.
    liftIO $ removePathForcibly pdir
    liftIO $ createDirectoryIfMissing True pdir

    liftIO $ mapM_ (putEntry from_dir pdir) $ Zip.zEntries a

libDir, libNewDir, libOldDir :: FilePath
(libDir, libNewDir, libOldDir) = ("lib", "lib~new", "lib~old")

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
-- 5) If the current package has package path 'p', move 'lib~old/p' to
-- 'lib~new/p'.
--
-- 6) Delete 'lib~old'.
--
-- Since POSIX at least guarantees atomic renames, the only place this
-- can fail is between steps 3, 4, and 5.  In that case, at least the
-- 'lib~old' will still exist and can be put back by the user.
installBuildList :: Maybe PkgPath -> BuildList -> PkgM ()
installBuildList p bl = do
  libdir_exists <- liftIO $ doesDirectoryExist libDir

  -- 1
  liftIO $ do removePathForcibly libNewDir
              createDirectoryIfMissing False libNewDir

  -- 2
  installInDir bl libNewDir

  -- 3
  when libdir_exists $ liftIO $ do
    removePathForcibly libOldDir
    renameDirectory libDir libOldDir

  -- 4
  liftIO $ renameDirectory libNewDir libDir

  -- 5
  case pkgPathFilePath <$> p of
    Just pfp | libdir_exists -> liftIO $ do
      pkgdir_exists <- doesDirectoryExist $ libOldDir </> pfp
      when pkgdir_exists $ do
        -- Ensure the parent directories exist so that we can move the
        -- package directory directly.
        createDirectoryIfMissing True $ takeDirectory $ libDir </> pfp
        renameDirectory (libOldDir </> pfp) (libDir </> pfp)
    _ -> return ()

  -- 6
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

newtype PkgConfig = PkgConfig { pkgVerbose :: Bool }

-- | The monad in which futhark-pkg runs.
newtype PkgM a = PkgM { unPkgM :: ReaderT PkgConfig (StateT (PkgRegistry PkgM) IO) a }
  deriving (Functor, Applicative, MonadIO, MonadReader PkgConfig)

instance Monad PkgM where
  PkgM m >>= f = PkgM $ m >>= unPkgM . f
  return = PkgM . return

instance MonadFail PkgM where
  fail s = liftIO $ do
    prog <- getProgName
    putStrLn $ prog ++ ": " ++ s
    exitFailure

instance MonadPkgRegistry PkgM where
  putPkgRegistry = PkgM . put
  getPkgRegistry = PkgM get

instance MonadLogger PkgM where
  addLog l = do
    verbose <- asks pkgVerbose
    when verbose $ liftIO $ T.hPutStr stderr $ toText l

runPkgM :: PkgConfig -> PkgM a -> IO a
runPkgM cfg (PkgM m) = evalStateT (runReaderT m cfg) mempty

cmdMain :: String -> ([String] -> PkgConfig -> Maybe (IO ()))
        -> String -> [String] -> IO ()
cmdMain = mainWithOptions (PkgConfig False) options
  where options = [ Option "v" ["verbose"]
                    (NoArg $ Right $ \cfg -> cfg { pkgVerbose = True })
                    "Write running diagnostics to stderr."]

doFmt :: String -> [String] -> IO ()
doFmt = mainWithOptions () [] "" $ \args () ->
  case args of
    [] -> Just $ do
      m <- parsePkgManifestFromFile futharkPkg
      T.writeFile futharkPkg $ prettyPkgManifest m
    _ -> Nothing

doCheck :: String -> [String] -> IO ()
doCheck = cmdMain "check" $ \args cfg ->
  case args of
    [] -> Just $ runPkgM cfg $ do
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
    _ -> Nothing

doSync :: String -> [String] -> IO ()
doSync = cmdMain "" $ \args cfg ->
  case args of
    [] -> Just $ runPkgM cfg $ do
      m <- getPkgManifest
      bl <- solveDeps $ pkgRevDeps m
      installBuildList (commented $ manifestPkgPath m) bl
    _ -> Nothing

doAdd :: String -> [String] -> IO ()
doAdd = cmdMain "PKGPATH" $ \args cfg ->
  case args of
    [p, v] | Right v' <- parseVersion $ T.pack v -> Just $ runPkgM cfg $ doAdd' (T.pack p) v'
    [p] -> Just $ runPkgM cfg $
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
      let hash = case (_svMajor v, _svMinor v, _svPatch v) of
                   -- We do not perform hash-pinning for
                   -- (0,0,0)-versions, because these already embed a
                   -- specific revision ID into their version number.
                   (0, 0, 0) -> Nothing
                   _ -> Just $ pkgRevCommit p_info
          req = Required p v hash
          (m', prev_r) = addRequiredToManifest req m

      case prev_r of
        Just prev_r'
          | requiredPkgRev prev_r' == v ->
            liftIO $ T.putStrLn $ "Package already at version " <> prettySemVer v <> "; nothing to do."
          | otherwise ->
            liftIO $ T.putStrLn $ "Replaced " <> p <> " " <>
            prettySemVer (requiredPkgRev prev_r') <> " => " <> prettySemVer v <> "."
        Nothing ->
          liftIO $ T.putStrLn $ "Added new required package " <> p <> " " <> prettySemVer v <> "."
      putPkgManifest m'
      liftIO $ T.putStrLn "Remember to run 'futhark pkg sync'."

doRemove :: String -> [String] -> IO ()
doRemove = cmdMain "PKGPATH" $ \args cfg ->
  case args of
    [p] -> Just $ runPkgM cfg $ doRemove' $ T.pack p
    _ -> Nothing
  where
    doRemove' p = do
      m <- getPkgManifest
      case removeRequiredFromManifest p m of
        Nothing -> liftIO $ do
          T.putStrLn $ "No package " <> p <> " found in " <> T.pack futharkPkg <> "."
          exitFailure
        Just (m', r) -> do
          putPkgManifest m'
          liftIO $ T.putStrLn $ "Removed " <> p <> " " <> prettySemVer (requiredPkgRev r) <> "."

doInit :: String -> [String] -> IO ()
doInit = cmdMain "PKGPATH" $ \args cfg ->
  case args of
    [p] -> Just $ runPkgM cfg $ doCreate' $ T.pack p
    _ -> Nothing
  where
    doCreate' p = do
      exists <- liftIO $ (||) <$> doesFileExist futharkPkg <*> doesDirectoryExist futharkPkg
      when exists $ liftIO $ do
        T.putStrLn $ T.pack futharkPkg <> " already exists."
        exitFailure

      liftIO $ createDirectoryIfMissing True $ "lib" </> T.unpack p
      liftIO $ T.putStrLn $ "Created directory " <> T.pack ("lib" </> T.unpack p) <> "."

      putPkgManifest $ newPkgManifest $ Just p
      liftIO $ T.putStrLn $ "Wrote " <> T.pack futharkPkg <> "."

doUpgrade :: String -> [String] -> IO ()
doUpgrade = cmdMain "" $ \args cfg ->
  case args of
    [] -> Just $ runPkgM cfg $ do
      m <- getPkgManifest
      rs <- traverse (mapM (traverse upgrade)) $ manifestRequire m
      putPkgManifest m { manifestRequire = rs }
      if rs == manifestRequire m
        then liftIO $ T.putStrLn "Nothing to upgrade."
        else liftIO $ T.putStrLn "Remember to run 'futhark pkg sync'."
    _ -> Nothing
  where upgrade req = do
          v <- lookupNewestRev $ requiredPkg req
          h <- pkgRevCommit <$> lookupPackageRev (requiredPkg req) v

          when (v /= requiredPkgRev req) $
            liftIO $ T.putStrLn $ "Upgraded " <> requiredPkg req <> " " <>
            prettySemVer (requiredPkgRev req) <> " => " <> prettySemVer v <> "."

          return req { requiredPkgRev = v
                     , requiredHash = Just h }

doVersions :: String -> [String] -> IO ()
doVersions = cmdMain "PKGPATH" $ \args cfg ->
  case args of
    [p] -> Just $ runPkgM cfg $ doVersions' $ T.pack p
    _ -> Nothing
  where doVersions' =
          mapM_ (liftIO . T.putStrLn . prettySemVer) . M.keys . pkgVersions
          <=< lookupPackage

main :: String -> [String] -> IO ()
main prog args = do
  -- Ensure that we can make HTTPS requests.
  setGlobalManager =<< newManager tlsManagerSettings

  -- Avoid Git asking for credentials.  We prefer failure.
  liftIO $ setEnv "GIT_TERMINAL_PROMPT" "0"

  let commands = [ ("add",
                    (doAdd, "Add another required package to futhark.pkg."))
                 , ("check",
                    (doCheck, "Check that futhark.pkg is satisfiable."))
                 , ("init",
                    (doInit, "Create a new futhark.pkg and a lib/ skeleton."))
                 , ("fmt",
                    (doFmt, "Reformat futhark.pkg."))
                 , ("sync",
                    (doSync, "Populate lib/ as specified by futhark.pkg."))
                 , ("remove",
                    (doRemove, "Remove a required package from futhark.pkg."))
                 , ("upgrade",
                    (doUpgrade, "Upgrade all packages to newest versions."))
                 , ("versions",
                    (doVersions, "List available versions for a package."))
                 ]
      usage = "options... <" <> intercalate "|" (map fst commands) <> ">"
  case args of
    cmd : args' | Just (m, _) <- lookup cmd commands ->
                    m (unwords [prog, cmd]) args'
    _ -> do
      let bad _ () = Just $ do
            let k = maximum (map (length . fst) commands) + 3
            usageMsg $ T.unlines $
              ["<command> ...:", "", "Commands:"] ++
              [ "   " <> T.pack cmd <> T.pack (replicate (k - length cmd) ' ') <> desc
              | (cmd, (_, desc)) <- commands ]

      mainWithOptions () [] usage bad prog args

  where usageMsg s = do
          T.putStrLn $ "Usage: " <> T.pack prog <> " [--version] [--help] " <> s
          exitFailure
