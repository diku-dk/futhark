{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | @futhark doc@
module Futhark.CLI.Unused (main) where

import Language.Futhark
import Control.Monad.State
import Futhark.Compiler (dumpError, newFutharkConfig, readProgramFiles)
import Futhark.Pipeline (Verbosity (..), runFutharkM)
import Futhark.Util.Options
import Language.Futhark.Unused
import System.Exit
import Data.Map.Strict qualified as M

main :: String -> [String] -> IO ()
main = mainWithOptions initialCheckConfig [] "files..." find
  where
    find [] _ = Nothing
    find files _ = Just $ printUnused files

printUnused :: [FilePath] -> IO ()
printUnused files = do
  res <- runFutharkM (readProgramFiles [] files) Verbose
  case res of
    Left err -> liftIO $ do
      dumpError newFutharkConfig err
      exitWith $ ExitFailure 2
    Right (_, imp, _) -> do
      -- let decs = getDecs fm
      -- print $ length decs
      -- print $ head decs
      -- print $ map fst imp
      -- putStrLn $ unlines $ map (\(x,VName y _,z) -> x <> ": " <> nameToString y <> " -> " <> locStr z) $ findUnused files imp
      putStrLn "did some stuff"
      -- print $ partDefFuncs files imp
      -- let u2 = concatMap (\(x,y) -> map (\(z,u) -> (x,z,u)) y ) $ M.toList $ findUnused files imp
      let un = fu files imp
      putStrLn $ unlines $ map (\(x,VName y _,z) -> x <> ": " <> nameToString y <> " -> " <> locStr z) $ concatMap (\(x,y) -> map (\(z,u) -> (x,z,u)) y ) $ M.toList un


data CheckConfig = CheckConfig Bool

initialCheckConfig :: CheckConfig
initialCheckConfig = CheckConfig False
