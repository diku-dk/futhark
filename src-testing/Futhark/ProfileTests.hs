{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.ProfileTests () where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Profile
import Test.Tasty.QuickCheck

printable :: Gen String
printable = getPrintableString <$> arbitrary

arbText :: Gen T.Text
arbText = T.pack <$> printable

instance Arbitrary ProfilingEvent where
  arbitrary = ProfilingEvent <$> arbText <*> arbitrary <*> listOf arbText <*> arbitrary

instance Arbitrary ProfilingReport where
  arbitrary =
    ProfilingReport
      <$> arbitrary
      <*> (M.fromList <$> listOf ((,) <$> arbText <*> arbitrary))
