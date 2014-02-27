module Main where

import Language.L0.Parser


main :: IO ()
main = do
  print =<< parseExpIncrIO "input" ""
  main
