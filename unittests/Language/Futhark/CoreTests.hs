{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Futhark.CoreTests () where

import Futhark.IR.PrimitiveTests ()
import Language.Futhark.Core
import Test.QuickCheck

instance Arbitrary Name where
  arbitrary = nameFromString <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary VName where
  arbitrary = VName <$> arbitrary <*> arbitrary
