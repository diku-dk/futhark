module Futhark.Analysis.Proofs.QueryTests (tests) where

import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Query
import Futhark.Analysis.Proofs.Properties
import Futhark.Analysis.Proofs.Symbol
import Futhark.MonadFreshNames
import Futhark.SoP.SoP (sym2SoP, (~-~), int2SoP, (.-.), (.+.))
import Test.Tasty
import Test.Tasty.HUnit

runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests =
  testGroup
    "Proofs.Query"
    [ testCase "Monotonically increasing" $
        run
          ( \(i, _, _, n, _, _, _) -> do
              let fn =
                    IndexFn
                      { iterator = Forall i (Iota (sVar n)),
                        body =
                          cases [(Bool True, sVar i)]
                      }
              askQ (CaseIsMonotonic Inc) fn 0
          )
          @?= Yes,
      testCase "Monotonically decreasing" $
        run
          ( \(i, _, _, n, _, _, _) -> do
              let fn =
                    IndexFn
                      { iterator = Forall i (Iota (sVar n)),
                        body =
                          cases [(Bool True, Var n ~-~ Var i)]
                      }
              askQ (CaseIsMonotonic Dec) fn 0
          )
          @?= Yes,
      testCase "Monotonicity unknown 1" $
        run
          ( \(i, _, _, n, x, _, _) -> do
              let fn =
                    IndexFn
                      { iterator = Forall i (Iota (sVar n)),
                        body =
                          cases [(Bool True, sym2SoP $ Idx (Var x) (sVar i))]
                      }
              askQ (CaseIsMonotonic Inc) fn 0
          )
          @?= Unknown,
      testCase "Monotonicity unknown 2" $
        run
          ( \(i, _, _, n, x, _, _) -> do
              let fn =
                    IndexFn
                      { iterator = Forall i (Iota (sVar n)),
                        body =
                          cases [(Bool True, Var x ~-~ Var i)]
                      }
              askQ (CaseIsMonotonic Inc) fn 0
          )
          @?= Unknown,
      testCase "Monotonic constant" $
        run
          ( \(i, _, _, n, x, _, _) -> do
              let fn =
                    IndexFn
                      { iterator = Forall i (Iota (sVar n)),
                        body =
                          cases [(Bool True, sVar x)]
                      }
              askQ (CaseIsMonotonic Inc) fn 0
          )
          @?= Yes,
      testCase "Permutation of domain" $
        run
          ( \(i, j, _, n, x, _, _) -> do
              let xs_i = Idx (Var x) (sVar i)
              let xs_j = Idx (Var x) (sVar j)
              let fn = IndexFn
                    { iterator = Forall i (Iota (sVar n)),
                      body =
                        cases
                          [ ( xs_i,
                              int2SoP (-1) .+. sym2SoP (Sum j (int2SoP 0) (sVar i) xs_j)
                            ),
                            ( neg xs_i,
                              sVar i .+. sym2SoP (Sum j (sVar i .+. int2SoP 1) (sVar n .-. int2SoP 1) xs_j)
                            )
                          ]
                    }
              prove (PermutationOfZeroTo $ sVar n .-. int2SoP 1) fn
          )
          @?= Yes
    ]
  where
    sVar = sym2SoP . Var

    varsM =
      (,,,,,,)
        <$> newVName "i"
        <*> newVName "j"
        <*> newVName "k"
        <*> newVName "n"
        <*> newVName "x"
        <*> newVName "y"
        <*> newVName "z"

    run f = runTest (varsM >>= f)
