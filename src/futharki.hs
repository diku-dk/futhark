module Main (main) where

import System.Environment
import System.Process
import System.IO
import System.Exit

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  let suffix = case args of
                 [] -> "repl"
                 _ -> "run"
  hPutStrLn stderr $
    "'" ++ prog ++ "' is deprecated.  Use '" ++
    unwords ["futhark", suffix] ++ "' instead."
  (_, _, _, h) <- createProcess $ proc "futhark" $ suffix:args
  exitWith =<< waitForProcess h
