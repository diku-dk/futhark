{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.Syntax.CoreTests (tests) where

import Control.Applicative
import Futhark.IR.Pretty (prettyString)
import Futhark.IR.Syntax.Core
import Language.Futhark.CoreTests ()
import Language.Futhark.PrimitiveTests ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests = testGroup "Internal CoreTests" subShapeTests

subShapeTests :: [TestTree]
subShapeTests =
  [ shape [free 1, free 2] `isSubShapeOf` shape [free 1, free 2],
    shape [free 1, free 3] `isNotSubShapeOf` shape [free 1, free 2],
    shape [free 1] `isNotSubShapeOf` shape [free 1, free 2],
    shape [free 1, free 2] `isSubShapeOf` shape [free 1, Ext 3],
    shape [Ext 1, Ext 2] `isNotSubShapeOf` shape [Ext 1, Ext 1],
    shape [Ext 1, Ext 1] `isSubShapeOf` shape [Ext 1, Ext 2]
  ]
  where
    shape :: [ExtSize] -> ExtShape
    shape = Shape

    free :: Int -> ExtSize
    free = Free . Constant . IntValue . Int32Value . fromIntegral

    isSubShapeOf shape1 shape2 =
      subShapeTest shape1 shape2 True
    isNotSubShapeOf shape1 shape2 =
      subShapeTest shape1 shape2 False

    subShapeTest :: ExtShape -> ExtShape -> Bool -> TestTree
    subShapeTest shape1 shape2 expected =
      testCase
        ( "subshapeOf "
            ++ prettyString shape1
            ++ " "
            ++ prettyString shape2
            ++ " == "
            ++ show expected
        )
        $ shape1 `subShapeOf` shape2 @?= expected

instance Arbitrary NoUniqueness where
  arbitrary = pure NoUniqueness

instance (Arbitrary shape, Arbitrary u) => Arbitrary (TypeBase shape u) where
  arbitrary =
    oneof
      [ Prim <$> arbitrary,
        Array <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Arbitrary Ident where
  arbitrary = Ident <$> arbitrary <*> arbitrary

instance Arbitrary Rank where
  arbitrary = Rank <$> elements [1 .. 9]

instance Arbitrary Shape where
  arbitrary = Shape . map intconst <$> listOf1 (elements [1 .. 9])
    where
      intconst = Constant . IntValue . Int32Value
