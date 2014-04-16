#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

-- Adapted from
-- http://www.hyperedsoftware.com/blog/entries/build-info-gen.html by
-- Võ Minh Thu.

main :: IO ()
main = defaultMainWithHooks myHooks
  where myHooks = simpleUserHooks { preBuild = myPreBuild }

myPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
myPreBuild _ _ = do
  putStrLn "Generating dist/build/autogen/Build_futhark..."
  createDirectoryIfMissing True "dist/build/autogen/"

  desc <- readProcess "git" ["describe", "--dirty=-modified", "--always"] ""

  writeFile "dist/build/autogen/Build_futhark.hs" $ unlines
    [ "module Build_futhark where"
    , "gitCommit :: String"
    , "gitCommit = " ++ show (init desc)
    ]
  return emptyHookedBuildInfo
