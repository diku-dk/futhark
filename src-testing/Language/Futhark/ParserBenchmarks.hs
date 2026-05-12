module Language.Futhark.ParserBenchmarks (benchmarks) where

import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Either (fromRight)
import Data.Text qualified as T
import Futhark.Util (showText)
import Language.Futhark.Parser (parseExp)

bigArray :: Int -> T.Text
bigArray n = "[" <> T.intercalate "," (map ((<> "i32") . showText) [0 .. n - 1]) <> "]"

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Language.Futhark.Parser"
    [ benchIntArray 10000,
      benchIntArray 100000,
      benchIntArray 1000000
    ]
  where
    benchIntArray n =
      bench ("[" <> show n <> "]i32") $
        whnf (fromRight (error "parse error") . parseExp "") (bigArray n)
