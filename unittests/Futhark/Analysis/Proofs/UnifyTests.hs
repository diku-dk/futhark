module Futhark.Analysis.Proofs.UnifyTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  []
  -- [ testCase "List comparison (different length)" $
  --     [1 :: Integer, 2, 3] `compare` [1,2] @?= GT

  -- -- the following test does not hold
  -- , testCase "List comparison (same length)" $
  --     [1 :: Integer, 2, 3] `compare` [1,2,2] @?= LT
  -- ]
