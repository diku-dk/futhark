{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.BenchTests (tests) where

import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.QuickCheck

import Futhark.Bench

instance Arbitrary RunResult where
  arbitrary = RunResult . getPositive <$> arbitrary

printable :: Gen String
printable = getPrintableString <$> arbitrary

instance Arbitrary DataResult where
  arbitrary = DataResult
              <$> printable
              <*> oneof [Left <$> arbText,
                         Right <$> ((,) <$> arbitrary <*> arbText)]
    where arbText = T.pack <$> printable

-- XXX: we restrict this generator to single datasets to we don't have
-- to worry about duplicates.
instance Arbitrary BenchResult where
  arbitrary = BenchResult <$> printable <*> (pure <$> arbitrary)

encodeDecodeJSON :: TestTree
encodeDecodeJSON = testProperty "encoding and decoding are inverse" prop
  where prop :: BenchResult -> Bool
        prop brs = decodeBenchResults (encodeBenchResults [brs]) == Right [brs]

tests :: TestTree
tests = testGroup "Futhark.BenchTests" [encodeDecodeJSON]
