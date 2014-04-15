{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Futhark.CoreTests
  ( tests
  , arbitraryBasicValOfType
  )
where

import Control.Applicative

import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.HashMap.Lazy as HM

import Language.Futhark.Core

tests :: [Test]
tests = testBuiltins ++
        propBasicValuesHaveRightType

propBasicValuesHaveRightType :: [Test]
propBasicValuesHaveRightType = [ testCase (show t ++ " has blank of right type") $
                                 basicValueType (blankBasicValue t) @?= t
                                 | t <- [minBound..maxBound]
                               ]

testBuiltins :: [Test]
testBuiltins = [ testCase (nameToString f ++ " is builtin") $
                 isBuiltInFunction f @?= True
                 | f <- HM.keys builtInFunctions ]

instance Arbitrary BinOp where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Uniqueness where
  arbitrary = elements [Unique, Nonunique]

instance Arbitrary BasicType where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary BasicValue where
  arbitrary = oneof [ IntVal <$> arbitrary
                    , RealVal <$> arbitrary
                    , LogVal <$> arbitrary
                    , CharVal <$> arbitrary
                    , pure Checked]

instance Arbitrary Name where
  arbitrary = nameFromString <$> listOf1 (elements ['a'..'z'])

instance Arbitrary VName where
  arbitrary = curry ID <$> arbitrary <*> arbitrary

arbitraryBasicValOfType :: BasicType -> Gen BasicValue
arbitraryBasicValOfType Int  = IntVal <$> arbitrary
arbitraryBasicValOfType Real = RealVal <$> arbitrary
arbitraryBasicValOfType Bool = LogVal <$> arbitrary
arbitraryBasicValOfType Char = CharVal <$> arbitrary
arbitraryBasicValOfType Cert = return Checked
