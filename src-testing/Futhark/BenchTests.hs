{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.BenchTests (tests) where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Bench
import Futhark.ProfileTests ()
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary RunResult where
  arbitrary = RunResult . getPositive <$> arbitrary

printable :: Gen String
printable = getASCIIString <$> arbitrary

instance Arbitrary DataResult where
  arbitrary =
    DataResult
      <$> (T.pack <$> printable)
      <*> oneof
        [ Left <$> arbText,
          Right
            <$> ( Result
                    <$> arbitrary
                    <*> arbMap
                    <*> oneof [pure Nothing, Just <$> arbText]
                    <*> arbitrary
                )
        ]
    where
      arbText = T.pack <$> printable
      arbMap = M.fromList <$> listOf ((,) <$> arbText <*> arbitrary)

-- XXX: we restrict this generator to single datasets to we don't have
-- to worry about duplicates.
instance Arbitrary BenchResult where
  arbitrary = BenchResult <$> printable <*> (pure <$> arbitrary)

encodeDecodeJSON :: TestTree
encodeDecodeJSON = testProperty "encoding and decoding are inverse" prop
  where
    prop :: BenchResult -> Bool
    prop brs = decodeBenchResults (encodeBenchResults [brs]) == Right [brs]

metadataJSON :: TestTree
metadataJSON = testProperty "metadata preserves benchmark decoding" prop
  where
    prop :: BenchResult -> Bool
    prop brs =
      decodeBenchResults (encodeBenchResultsWithMetadata metadata [brs])
        == Right [brs]
    metadata =
      BenchMetadata
        { benchStartTime = read "2026-09-09 01:02:03 UTC",
          benchEndTime = read "2026-09-09 01:02:04 UTC",
          benchHostname = Just "benchmark-host",
          benchCompilerVersion = Just "Futhark test compiler",
          benchBackend = "c",
          benchCompilerOptions = ["--safe"],
          benchRuntimeOptions = ["--debugging"]
        }

tests :: TestTree
tests = testGroup "Futhark.BenchTests" [encodeDecodeJSON, metadataJSON]
