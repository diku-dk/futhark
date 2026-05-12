module Language.Futhark.TypeChecker.ConsumptionTests
  ( tests,
  )
where

import Data.Bifunctor
import Data.Set qualified as S
import Language.Futhark
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker.Consumption
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ConsumptionTests"
    [ testGroup
        "inferReturnUniqueness"
        [ testCase "*[]i32" $
            inferReturnUniqueness
              [Id "x_1" (Info "[2]i32") mempty]
              "[2]i32"
              (second (const mempty) ("[2]i32" :: StructType))
              @?= "*[2]i32",
          --
          testCase "[]i32" $
            inferReturnUniqueness
              [Id "x_1" (Info "[2]i32") mempty]
              "[2]i32"
              ( second
                  (const (S.singleton (AliasBound "x_1")))
                  ("[2]i32" :: StructType)
              )
              @?= "[2]i32",
          --
          testCase "([]i32,[]i32)" $
            inferReturnUniqueness
              [Id "x_1" (Info "[2]i32") mempty]
              "([2]i32, [2]i32)"
              ( second
                  (const (S.singleton (AliasFree "y_2")))
                  ("([2]i32,[2]i32)" :: StructType)
              )
              @?= "([2]i32, [2]i32)",
          --
          testCase "opaque" $
            let t = Scalar (TypeVar Nonunique (qualName "t_2") [])
             in inferReturnUniqueness
                  [Id "n_1" (Info "i64") mempty]
                  t
                  (second (const (S.singleton (AliasFree "y_3"))) t)
                  @?= (t `setUniqueness` Nonunique),
          --
          testCase "*opaque" $
            let t = Scalar (TypeVar Nonunique (qualName "t_2") [])
             in inferReturnUniqueness
                  [Id "n_1" (Info "i64") mempty]
                  t
                  (second (const mempty) t)
                  @?= (t `setUniqueness` Unique)
        ]
    ]
