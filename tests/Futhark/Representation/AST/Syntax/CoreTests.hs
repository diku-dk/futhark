{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Representation.AST.Syntax.CoreTests
       ( tests )
       where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.QuickCheck

import Language.Futhark.CoreTests ()
import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.Pretty

tests :: [Test]
tests = subShapeTests

subShapeTests :: [Test]
subShapeTests =
  [ shape [free 1, free 2] `isSubShapeOf` shape [free 1, free 2]
  , shape [free 1, free 3] `isNotSubShapeOf` shape [free 1, free 2]
  , shape [free 1] `isNotSubShapeOf` shape [free 1, free 2]
  , shape [free 1, free 2] `isSubShapeOf` shape [free 1, Ext 3]
  , shape [Ext 1, Ext 2] `isNotSubShapeOf` shape [Ext 1, Ext 1]
  , shape [Ext 1, Ext 1] `isSubShapeOf` shape [Ext 1, Ext 2]
  ]
  where shape :: [ExtDimSize] -> ExtShape
        shape = ExtShape

        free :: Int -> ExtDimSize
        free = Free . Constant . IntVal

        isSubShapeOf shape1 shape2 =
          subShapeTest shape1 shape2 True
        isNotSubShapeOf shape1 shape2 =
          subShapeTest shape1 shape2 False

        subShapeTest :: ExtShape -> ExtShape -> Bool -> Test
        subShapeTest shape1 shape2 expected =
          testCase ("subshapeOf " ++ pretty shape1 ++ " " ++
                    pretty shape2 ++ " == " ++
                    show expected) $
          shape1 `subShapeOf` shape2 @?= expected


instance Arbitrary shape => Arbitrary (TypeBase shape) where
  arbitrary =
    oneof [ Basic <$> arbitrary
          , Array <$> arbitrary <*> arbitrary <*> arbitrary
          ]

instance Arbitrary Value where
  arbitrary = BasicVal <$> arbitrary

instance Arbitrary (IdentBase Shape) where
  arbitrary = Ident <$> arbitrary <*> arbitrary

instance Arbitrary Rank where
  arbitrary = Rank <$> elements [1..9]

instance Arbitrary Shape where
  arbitrary = Shape <$> map intconst <$> listOf1 (elements [1..9])
    where intconst x = Constant $ IntVal x
