{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | @futhark doc@
module Futhark.CLI.Unused (main) where

import Control.Monad.State
import Data.Map.Strict qualified as M
import Futhark.Compiler (dumpError, newFutharkConfig, readProgramFiles)
import Futhark.Pipeline (Verbosity (..), runFutharkM)
import Futhark.Util.Options
import Language.Futhark
import Language.Futhark.Unused
import System.Exit
import Data.List ( elemIndices )
import Futhark.Util.Loc

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
      putStr $
        unlines $
          map (\(VName y _, z) -> simpleLocStr z <> ": " <> nameToString y) $
            concatMap snd $
              M.toList $
                findUnused files imp

data CheckConfig = CheckConfig Bool

initialCheckConfig :: CheckConfig
initialCheckConfig = CheckConfig False

simpleLocStr :: Located a => a -> String
simpleLocStr a =
  case locOf a of
    NoLoc -> "unknown location"
    Loc (Pos file line1 _col1 _) (Pos _ _line2 _col2 _) -> -- ignore second position, we only care about file and line number 1.
      file ++ ":" ++ show line1
