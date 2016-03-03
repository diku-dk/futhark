module Futhark.Representation.AST.Attributes.RearrangeTests
       ( tests )
       where

import Control.Applicative

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Prelude

import Futhark.Representation.AST.Attributes.Rearrange

tests :: [Test]
tests = isMapTransposeTests ++
        [isMapTransposeProp]

isMapTransposeTests :: [Test]
isMapTransposeTests =
  [ testCase (unwords ["isMapTranspose", show perm, "==", show dres]) $
    isMapTranspose perm @?= dres
  | (perm, dres) <- [ ([0,1,4,5,2,3], Just (2,2,2))
                    , ([1,0,4,5,2,3], Nothing)
                    , ([1,0], Just (0, 1, 1))
                    , ([0,2,1], Just (1, 1, 1))
                    , ([0,1,2], Nothing)
                    , ([1,0,2], Nothing)
                    ]
  ]

newtype Permutation = Permutation [Int]
                    deriving (Eq, Ord, Show)

instance Arbitrary Permutation where
  arbitrary = do
    Positive n <- arbitrary
    Permutation <$> shuffle [0..n-1]

isMapTransposeProp :: Test
isMapTransposeProp = testProperty "isMapTranspose corresponds to a map of transpose" prop
  where prop :: Permutation -> Bool
        prop (Permutation perm) =
          case isMapTranspose perm of
            Nothing -> True
            Just (r1, r2, r3) ->
              and [r1 >= 0,
                   r2 > 0,
                   r3 > 0,
                   r1 + r2 + r3 == length perm,
                   let (mapped, notmapped) =splitAt r1 perm
                       (pretrans, posttrans) = splitAt r2 notmapped
                   in mapped ++ posttrans ++ pretrans == [0..length perm-1]
                  ]
