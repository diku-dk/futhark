module Main (main) where

import Criterion.Main
import Language.Futhark.ParserBenchmarks qualified

main :: IO ()
main =
  defaultMain
    [ Language.Futhark.ParserBenchmarks.benchmarks
    ]
