{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Futhark.PrimitiveTests
  ( tests,
    arbitraryPrimValOfType,
  )
where

import Control.Applicative
import Futhark.Util (convFloat)
import Language.Futhark.Primitive
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests = testGroup "PrimitiveTests" [propPrimValuesHaveRightType]

propPrimValuesHaveRightType :: TestTree
propPrimValuesHaveRightType =
  testGroup
    "propPrimValuesHaveRightTypes"
    [ testCase (show t ++ " has blank of right type") $
        primValueType (blankPrimValue t) @?= t
    | t <- [minBound .. maxBound]
    ]

instance Arbitrary IntType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary FloatType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary PrimType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary IntValue where
  arbitrary =
    oneof
      [ Int8Value <$> arbitrary,
        Int16Value <$> arbitrary,
        Int32Value <$> arbitrary,
        Int64Value <$> arbitrary
      ]

instance Arbitrary Half where
  arbitrary = (convFloat :: Float -> Half) <$> arbitrary

instance Arbitrary FloatValue where
  arbitrary =
    oneof
      [ Float16Value <$> arbitrary,
        Float32Value <$> arbitrary,
        Float64Value <$> arbitrary
      ]

instance Arbitrary PrimValue where
  arbitrary =
    oneof
      [ IntValue <$> arbitrary,
        FloatValue <$> arbitrary,
        BoolValue <$> arbitrary,
        pure UnitValue
      ]

arbitraryPrimValOfType :: PrimType -> Gen PrimValue
arbitraryPrimValOfType (IntType Int8) = IntValue . Int8Value <$> arbitrary
arbitraryPrimValOfType (IntType Int16) = IntValue . Int16Value <$> arbitrary
arbitraryPrimValOfType (IntType Int32) = IntValue . Int32Value <$> arbitrary
arbitraryPrimValOfType (IntType Int64) = IntValue . Int64Value <$> arbitrary
arbitraryPrimValOfType (FloatType Float16) = FloatValue . Float16Value <$> arbitrary
arbitraryPrimValOfType (FloatType Float32) = FloatValue . Float32Value <$> arbitrary
arbitraryPrimValOfType (FloatType Float64) = FloatValue . Float32Value <$> arbitrary
arbitraryPrimValOfType Bool = BoolValue <$> arbitrary
arbitraryPrimValOfType Unit = pure UnitValue
