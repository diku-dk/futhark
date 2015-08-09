{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Representation.AST.Attributes.ReshapeTests
       ( tests
       )
       where

import Control.Applicative

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Prelude

import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Syntax

tests :: [Test]
tests = fuseReshapeTests ++ informReshapeTests ++
        [ fuseReshapeProp
        , informReshapeProp
        ]

fuseReshapeTests :: [Test]
fuseReshapeTests =
  [ testCase (unwords ["fuseReshape ", show d1, show d2]) $
    fuseReshape (d1 :: ShapeChange Int) d2 @?= dres -- type signature to avoid warning
  | (d1, d2, dres) <- [ ([DimCoercion 1], [DimNew 1], [DimCoercion 1])
                      , ([DimNew 1], [DimCoercion 1], [DimNew 1])
                      , ([DimCoercion 1, DimNew 2], [DimNew 1, DimNew 2], [DimCoercion 1, DimNew 2])
                      , ([DimNew 1, DimNew 2], [DimCoercion 1, DimNew 2], [DimNew 1, DimNew 2])
                      ]
  ]

informReshapeTests :: [Test]
informReshapeTests =
  [ testCase (unwords ["informReshape ", show shape, show sc, show sc_res]) $
    informReshape (shape :: [Int]) sc @?= sc_res -- type signature to avoid warning
  | (shape, sc, sc_res) <-
    [ ([1, 2], [DimNew 1, DimNew 3], [DimCoercion 1, DimNew 3])
    , ([2, 2], [DimNew 1, DimNew 3], [DimNew 1, DimNew 3])
    ]
  ]

fuseReshapeProp :: Test
fuseReshapeProp = testProperty "fuseReshape result matches second argument" prop
  where prop :: ShapeChange Int -> ShapeChange Int -> Bool
        prop sc1 sc2 = map newDim (fuseReshape sc1 sc2) == map newDim sc2

informReshapeProp :: Test
informReshapeProp = testProperty "informReshape result matches second argument" prop
  where prop :: [Int] -> ShapeChange Int -> Bool
        prop sc1 sc2 = map newDim (informReshape sc1 sc2) == map newDim sc2


instance Arbitrary d => Arbitrary (DimChange d) where
  arbitrary = oneof [ DimNew <$> arbitrary
                    , DimCoercion <$> arbitrary
                    ]
