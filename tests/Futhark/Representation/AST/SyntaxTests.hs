{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Representation.AST.SyntaxTests
  ()
where

-- There isn't anything to test in this module, so instead we just
-- define Arbitrary instances.

import Control.Applicative

import Test.QuickCheck

import Language.Futhark.CoreTests ()
import Futhark.Representation.AST.Syntax

instance Arbitrary Rank where
  arbitrary = Rank <$> elements [1..9]

instance Arbitrary Shape where
  arbitrary = Shape <$> map intconst <$> listOf1 (elements [1..9])
    where intconst x = Constant $ IntVal x

instance Arbitrary shape => Arbitrary (TypeBase shape) where
  arbitrary =
    oneof [ Basic <$> arbitrary
          , Array <$> arbitrary <*> arbitrary <*> arbitrary
          ]

instance Arbitrary Value where
  arbitrary = BasicVal <$> arbitrary

instance Arbitrary (IdentBase Shape) where
  arbitrary = Ident <$> arbitrary <*> arbitrary
