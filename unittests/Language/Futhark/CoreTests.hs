{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Futhark.CoreTests ()
where

import Test.QuickCheck

import Language.Futhark.Core
import Futhark.Representation.PrimitiveTests()

instance Arbitrary Name where
  arbitrary = nameFromString <$> listOf1 (elements ['a'..'z'])

instance Arbitrary VName where
  arbitrary = VName <$> arbitrary <*> arbitrary
