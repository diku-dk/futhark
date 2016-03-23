{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Representation.PrimitiveTests
       ( tests
       , arbitraryPrimValOfType
       )
       where

import Control.Applicative

import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude

import Futhark.Representation.Primitive

tests :: [Test]
tests = propPrimValuesHaveRightType

propPrimValuesHaveRightType :: [Test]
propPrimValuesHaveRightType = [ testCase (show t ++ " has blank of right type") $
                                primValueType (blankPrimValue t) @?= t
                              | t <- [minBound..maxBound]
                              ]

instance Arbitrary IntType where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary FloatType where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary PrimType where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary IntValue where
  arbitrary = oneof [ Int8Value <$> arbitrary
                    , Int16Value <$> arbitrary
                    , Int32Value <$> arbitrary
                    , Int64Value <$> arbitrary ]

instance Arbitrary FloatValue where
  arbitrary = oneof [ Float32Value <$> arbitrary
                    , Float64Value <$> arbitrary ]

instance Arbitrary PrimValue where
  arbitrary = oneof [ IntValue <$> arbitrary
                    , FloatValue <$> arbitrary
                    , BoolValue <$> arbitrary
                    , pure Checked
                    ]

arbitraryPrimValOfType :: PrimType -> Gen PrimValue
arbitraryPrimValOfType (IntType Int8) = IntValue <$> Int8Value <$> arbitrary
arbitraryPrimValOfType (IntType Int16) = IntValue <$> Int16Value <$> arbitrary
arbitraryPrimValOfType (IntType Int32) = IntValue <$> Int32Value <$> arbitrary
arbitraryPrimValOfType (IntType Int64) = IntValue <$> Int64Value <$> arbitrary
arbitraryPrimValOfType (FloatType Float32) = FloatValue <$> Float32Value <$> arbitrary
arbitraryPrimValOfType (FloatType Float64) = FloatValue <$> Float32Value <$> arbitrary
arbitraryPrimValOfType Bool = BoolValue <$> arbitrary
arbitraryPrimValOfType Cert = return Checked
