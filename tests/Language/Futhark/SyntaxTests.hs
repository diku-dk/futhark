{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Futhark.SyntaxTests()
where

import Test.QuickCheck

import Language.Futhark.Syntax

instance Arbitrary BinOp where
  arbitrary = elements [minBound..maxBound]
