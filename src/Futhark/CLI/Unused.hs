{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | @futhark doc@
module Futhark.CLI.Unused (main) where

import Language.Futhark
import Data.Set qualified as S

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
      putStrLn $ unlines $ map (\(x,VName y _,z) -> x <> ": " <> nameToString y <> " -> " <> locStr z) $
        concatMap (\(x,y) -> map (\(z,u) -> (x,z,u)) y ) $ M.toList $ findUnused files imp


data CheckConfig = CheckConfig Bool

initialCheckConfig :: CheckConfig
initialCheckConfig = CheckConfig False
