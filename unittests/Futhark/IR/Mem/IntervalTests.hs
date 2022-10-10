module Futhark.IR.Mem.IntervalTests
  ( tests,
  )
where

import Futhark.Analysis.AlgSimplify
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.Interval
import Futhark.IR.Syntax
import Futhark.IR.Syntax.Core ()
import Test.Tasty
import Test.Tasty.HUnit

-- Actual tests.
tests :: TestTree
tests =
  testGroup
    "IntervalTests"
    testDistributeOffset

name :: String -> Int -> VName
name s = VName (nameFromString s)

testDistributeOffset :: [TestTree]
testDistributeOffset =
  [ testCase "Stride is (nb-b)" $ do
      let n = TPrimExp $ LeafExp (name "n" 1) $ IntType Int64
          b = TPrimExp $ LeafExp (name "b" 2) $ IntType Int64
      res <-
        distributeOffset
          [Prod False [untyped (n * b - b :: TPrimExp Int64 VName)]]
          [ Interval 0 1 (n * b - b),
            Interval 0 b b,
            Interval 0 b 1
          ]
      res == [Interval 1 1 (n * b - b), Interval 0 b b, Interval 0 b 1] @? "Failed",
    testCase "Stride is 1024r" $ do
      let r = TPrimExp $ LeafExp (name "r" 1) $ IntType Int64
      res <-
        distributeOffset
          [Prod False [untyped (1024 :: TPrimExp Int64 VName), untyped r]]
          [ Interval 0 1 (1024 * r),
            Interval 0 32 32,
            Interval 0 32 1
          ]
      res == [Interval 1 1 (1024 * r), Interval 0 32 32, Interval 0 32 1] @? "Failed. Got " <> show res,
    testCase "Stride is 32, offsets are multples of 32" $ do
      let n = TPrimExp $ LeafExp (name "n" 0) $ IntType Int64
      let g1 = TPrimExp $ LeafExp (name "g" 1) $ IntType Int64
      let g2 = TPrimExp $ LeafExp (name "g" 2) $ IntType Int64
      res <-
        distributeOffset
          [ Prod False [untyped (1024 :: TPrimExp Int64 VName)],
            Prod False [untyped (1024 :: TPrimExp Int64 VName), untyped g1],
            Prod False [untyped (32 :: TPrimExp Int64 VName), untyped g2]
          ]
          [ Interval 0 1 (1024 * n),
            Interval 0 1 32,
            Interval 0 32 1
          ]
      res
        == [ Interval 0 1 (1024 * n),
             Interval (32 + 32 * g1 + g2) 1 32,
             Interval 0 32 1
           ]
        @? "Failed. Got "
          <> show res
  ]
