module Futhark.IR.Prop.RearrangeTests (tests) where

import Control.Applicative
import Futhark.IR.Prop.Rearrange
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude

tests :: TestTree
tests =
  testGroup "RearrangeTests" $
    isMapTransposeTests
      ++ [isMapTransposeProp]

isMapTransposeTests :: [TestTree]
isMapTransposeTests =
  [ testCase (unwords ["isMapTranspose", show perm, "==", show dres]) $
      isMapTranspose perm @?= dres
    | (perm, dres) <-
        [ ([0, 1, 4, 5, 2, 3], Just (2, 2, 2)),
          ([1, 0, 4, 5, 2, 3], Nothing),
          ([1, 0], Just (0, 1, 1)),
          ([0, 2, 1], Just (1, 1, 1)),
          ([0, 1, 2], Nothing),
          ([1, 0, 2], Nothing)
        ]
  ]

newtype Permutation = Permutation [Int]
  deriving (Eq, Ord, Show)

instance Arbitrary Permutation where
  arbitrary = do
    Positive n <- arbitrary
    Permutation <$> shuffle [0 .. n - 1]

isMapTransposeProp :: TestTree
isMapTransposeProp = testProperty "isMapTranspose corresponds to a map of transpose" prop
  where
    prop :: Permutation -> Bool
    prop (Permutation perm) =
      case isMapTranspose perm of
        Nothing -> True
        Just (r1, r2, r3) ->
          and
            [ r1 >= 0,
              r2 > 0,
              r3 > 0,
              r1 + r2 + r3 == length perm,
              let (mapped, notmapped) = splitAt r1 perm
                  (pretrans, posttrans) = splitAt r2 notmapped
               in mapped ++ posttrans ++ pretrans == [0 .. length perm - 1]
            ]
