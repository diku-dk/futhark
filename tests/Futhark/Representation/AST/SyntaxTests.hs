{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Representation.AST.SyntaxTests
  ()
where

-- There isn't anything to test in this module, so instead we just
-- define Arbitrary instances.

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Loc

import Test.QuickCheck

import Language.Futhark.CoreTests (arbitraryBasicValOfType)
import Futhark.Representation.AST.Syntax

instance Arbitrary Rank where
  arbitrary = Rank <$> elements [1..9]

instance Arbitrary Shape where
  arbitrary = Shape <$> map intconst <$> listOf1 (elements [1..9])
    where intconst x = Constant (BasicVal $ IntVal x) noLoc

instance Arbitrary shape => Arbitrary (TypeBase shape) where
  arbitrary =
    oneof [ Basic <$> arbitrary
          , Array <$> arbitrary <*> arbitrary <*> arbitrary
          ]

instance Arbitrary Value where
  arbitrary =
    oneof [ BasicVal <$> arbitrary
          , do et <- arbitrary -- Compute element type.
               shape <- arbitrary
               arbitraryArray shape et
          ]

arbitraryArray :: [Int] -> BasicType -> Gen Value
arbitraryArray [] t = BasicVal <$> arbitraryBasicValOfType t
arbitraryArray (n:ns) t = do
  arr <- listArray (0,n-1) <$> replicateM n (arbitraryArray ns t)
  return $ ArrayVal arr $ Array t (Rank $ length ns) Nonunique

instance Arbitrary (IdentBase Shape) where
  arbitrary = Ident <$> arbitrary <*> arbitrary <*> pure noLoc
