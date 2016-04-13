{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Futhark.SyntaxTests (tests)
where

import Control.Applicative

import Prelude

import Test.QuickCheck
import Test.Framework

import Language.Futhark.Syntax

import Futhark.Representation.PrimitiveTests()

tests :: [Test]
tests = []

instance Arbitrary BinOp where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Uniqueness where
  arbitrary = elements [Unique, Nonunique]

instance Arbitrary PrimType where
  arbitrary = oneof [ Signed <$> arbitrary
                    , Unsigned <$> arbitrary
                    , FloatType <$> arbitrary
                    , pure Bool
                    ]

instance Arbitrary PrimValue where
  arbitrary = oneof [ SignedValue <$> arbitrary
                    , UnsignedValue <$> arbitrary
                    , FloatValue <$> arbitrary
                    , BoolValue <$> arbitrary
                    ]
