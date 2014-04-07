module Main where

import Language.Futhark.Parser


main :: IO ()
main = do
  print =<< parseExpIncrIO "input" ""
  main
