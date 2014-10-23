#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

-- Adapted from
-- http://www.hyperedsoftware.com/blog/entries/build-info-gen.html by
-- Võ Minh Thu.

main :: IO ()
main = defaultMainWithHooks myHooks
  where myHooks = simpleUserHooks { postConf = generateBuildInfoModule }

generateBuildInfoModule :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildInfoModule _ _ _ buildinfo = do
  let build_futhark_filename = autogenModulesDir buildinfo ++ "/Build_futhark.hs"
  putStrLn $ "Generating " ++ build_futhark_filename
  createDirectoryIfMissing True "dist/build/autogen/"

  desc <- readProcess "git" ["describe", "--dirty=-modified", "--always"] ""

  writeFile build_futhark_filename $ unlines
    [ "module Build_futhark where"
    , "gitCommit :: String"
    , "gitCommit = " ++ show (init desc)
    ]
