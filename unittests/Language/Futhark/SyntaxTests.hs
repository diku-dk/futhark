{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Futhark.SyntaxTests (tests) where

import Control.Applicative
import Futhark.IR.PrimitiveTests ()
import Language.Futhark.Syntax
import Test.QuickCheck
import Test.Tasty
import Prelude

tests :: TestTree
tests = testGroup "Source SyntaxTests" []

instance Arbitrary BinOp where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Uniqueness where
  arbitrary = elements [Unique, Nonunique]

instance Arbitrary PrimType where
  arbitrary =
    oneof
      [ Signed <$> arbitrary,
        Unsigned <$> arbitrary,
        FloatType <$> arbitrary,
        pure Bool
      ]

instance Arbitrary PrimValue where
  arbitrary =
    oneof
      [ SignedValue <$> arbitrary,
        UnsignedValue <$> arbitrary,
        FloatValue <$> arbitrary,
        BoolValue <$> arbitrary
      ]
