module Main (main) where

import Criterion.Main
import Language.Futhark.ParserBenchmarks qualified
import Language.Futhark.TypeChecker.TySolveBenchmarks qualified

main :: IO ()
main =
  defaultMain
    [ Language.Futhark.ParserBenchmarks.benchmarks,
      Language.Futhark.TypeChecker.TySolveBenchmarks.benchmarks
    ]
