{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Futhark.SyntaxTests (tests)
where

import Control.Applicative

import Prelude

import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.HashMap.Lazy as HM

import Language.Futhark.Syntax
import Language.Futhark.Attributes

import Futhark.Representation.PrimitiveTests()

tests :: [Test]
tests = testBuiltins

testBuiltins :: [Test]
testBuiltins = [ testCase (nameToString f ++ " is builtin") $
                 isBuiltInFunction f @?= True
                 | f <- HM.keys builtInFunctions ]

instance Arbitrary BinOp where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Uniqueness where
  arbitrary = elements [Unique, Nonunique]

instance Arbitrary PrimType where
  arbitrary = oneof [ IntType <$> arbitrary
                    , FloatType <$> arbitrary
                    , pure Bool
                    , pure Char
                    ]

instance Arbitrary PrimValue where
  arbitrary = oneof [ IntValue <$> arbitrary
                    , FloatValue <$> arbitrary
                    , BoolValue <$> arbitrary
                    , CharValue <$> arbitrary
                    ]
