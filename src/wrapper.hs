-- Wrapper program that translates @futhark-foo@ to @futhark foo@;
-- using whichever @futhark@ binary is in the user's search path.

module Main (main) where

import Data.Maybe

import System.Environment
import System.Process
import System.IO
import System.Exit

nameChanges :: [(String, String)]
nameChanges = [ ("py", "python")
              , ("cs", "csharp")
              ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  let suffix = drop 1 $ dropWhile (/='-') prog
      suffix' = fromMaybe suffix $ lookup suffix nameChanges

  hPutStrLn stderr $
    prog ++ ": this command is deprecated.  Use '" ++
    unwords ["futhark", suffix'] ++ "' instead."
  (_, _, _, h) <- createProcess $ proc "futhark" $ suffix':args
  exitWith =<< waitForProcess h
