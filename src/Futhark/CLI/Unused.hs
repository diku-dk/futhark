{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
-- | @futhark doc@
module Futhark.CLI.Unused (main) where

import Control.Monad.State
import Futhark.Pipeline ( Verbosity (..), runFutharkM)
import Futhark.Util.Options

import System.Exit

import Language.Futhark.Unused
import Futhark.Compiler (readProgramFiles, dumpError, newFutharkConfig)

-- | Run @futhark doc@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialCheckConfig [] "options... files..." find
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
    Right (_,imp,_) -> do
      print $ findUnused imp

data CheckConfig = CheckConfig Bool

initialCheckConfig :: CheckConfig
initialCheckConfig = CheckConfig False
