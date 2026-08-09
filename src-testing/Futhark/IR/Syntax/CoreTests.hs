{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.Syntax.CoreTests (tests) where

import Control.Applicative
import Data.Loc (Loc (..), Pos (..))
import Futhark.IR.Syntax.Core
import Language.Futhark.CoreTests ()
import Language.Futhark.PrimitiveTests ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

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

provenanceTests :: [TestTree]
provenanceTests =
  [ testGroup
      "<>"
      [ testCase "simple" $
          (Provenance [] line1 <> Provenance [] line0) @?= Provenance [] lines01,
        testCase "mempty left" $
          (Provenance [] mempty <> Provenance [] line0) @?= Provenance [] line0,
        testCase "mempty right" $
          (Provenance [] line1 <> Provenance [] mempty) @?= Provenance [] line1
      ],
    testGroup
      "stackProvenance"
      [ testCase "encloses" $
          (Provenance [] line0 `stackProvenance` Provenance [] line0_sub)
            @?= Provenance [] line0_sub
      ]
  ]
  where
    line0 = Loc (Pos "" 0 1 0) (Pos "" 0 10 10)
    line0_sub = Loc (Pos "" 0 2 1) (Pos "" 0 9 9)
    line1 = Loc (Pos "" 1 1 0) (Pos "" 1 10 20)
    lines01 = Loc (Pos "" 0 1 0) (Pos "" 1 10 20)

tests :: TestTree
tests =
  testGroup
    "Internal CoreTests"
    [ testGroup "Provenance" provenanceTests
    ]
