module Main (main) where

import Criterion.Main
import Language.Futhark.TypeChecker.TySolveBenchmarks qualified

main :: IO ()
main =
  defaultMain
    [ Language.Futhark.TypeChecker.TySolveBenchmarks.benchmarks
    ]
