module Futhark.Analysis.Properties.QueryTests (tests) where

import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Prove
import Futhark.Analysis.Properties.Query
import Futhark.Analysis.Properties.Symbol
import Futhark.MonadFreshNames
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.+.), (.-.), (~-~))
import Test.Tasty
import Test.Tasty.HUnit

runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests =
  testGroup
    "Properties.Query"
    $ [ testCase "Monotonically increasing" $
          run
            ( \(i, _, _, n, _, _, _) -> do
                let fn =
                      IndexFn
                        { iterator = Forall i (Iota (sVar n)),
                          body =
                            cases [(Bool True, sVar i)]
                        }
                queryCase (CaseIsMonotonic Inc) fn 0
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
                queryCase (CaseIsMonotonic Dec) fn 0
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
                queryCase (CaseIsMonotonic Inc) fn 0
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
                queryCase (CaseIsMonotonic Inc) fn 0
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
                queryCase (CaseIsMonotonic Inc) fn 0
            )
            @?= Yes,
        testCase "Permutation of domain" $
          run
            ( \(i, j, _, n, x, _, _) -> do
                let xs_i = Idx (Var x) (sVar i)
                let xs_j = Idx (Var x) (sVar j)
                let fn =
                      IndexFn
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
      <> [ testCase ("bijectiveRCD" <> "." <> show a <> "." <> show c) $
             run
               ( \(i, _, _, n, _, _, _) -> do
                   let fn =
                         IndexFn
                           { iterator = Forall i (Iota (sVar n)),
                             body =
                               cases
                                 [(Bool True, sVar i .+. int2SoP 1)]
                           }
                   prove (PBijectiveRCD (int2SoP a, f (sVar n)) (int2SoP c, g (sVar n))) fn
               )
               @?= answer
           | (a, f, c, g, answer) <-
               [ (1, id, 1, id, Yes),
                 (0, id, 1, id, Yes), -- RCD too big is OK.
                 (0, id, 0, id, Unknown), -- ImgRCD too big.
                 (2, id, 1, id, Unknown) -- RCD smaller than ImgRCD.
               ]
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
