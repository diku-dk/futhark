{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.RewriteTests (tests) where

import Control.Monad (unless)
import Futhark.Analysis.Proofs.AlgebraBridge (toAlgebra)
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Rewrite
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify
import Futhark.MonadFreshNames
import Futhark.SoP.Monad (addEquiv, addRange, mkRange, mkRangeLB, mkRangeUB, addProperty)
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.*.), (.+.), (.-.))
import Futhark.Util.Pretty (docString, line, pretty, (<+>))
import Test.Tasty
import Test.Tasty.HUnit

runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests =
  testGroup
    "Proofs.Rewrite"
    [ testCase "Add" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite (sVar x .+. sVar y .+. int 1)
          )
          @??= (sVar x .+. sVar y .+. int 1),
      testCase "Extend sum lower bound (1)" $
        run
          ( \(x, y, z, w, _, _, _, _) -> do
              let lb = sVar y .+. int 1
              z `lowerBoundedBy` lb
              rewrite (Idx (Var x) (sVar y) ~+~ Sum w lb (sVar z) (Idx (Var x) (sVar w)))
          )
          @??= sym2SoP (Sum w (sVar y) (sVar z) (Idx (Var x) (sVar w))),
      testCase "Extend sum lower bound (2)" $
        run
          ( \(x, y, z, w, _, _, _, _) -> do
              let lb = sVar y
              z `lowerBoundedBy` lb
              rewrite (Idx (Var x) (lb .-. int 1) ~+~ Sum w lb (sVar z) (Idx (Var x) (sVar w)))
          )
          @??= sym2SoP (Sum w (sVar y .-. int 1) (sVar z) (Idx (Var x) (sVar w))),
      testCase "Extend sum lower bound (3)" $
        run
          ( \(x, y, z, w, _, _, _, _) -> do
              let lb = sVar y .-. int 1336
              z `lowerBoundedBy` lb
              rewrite (Idx (Var x) (sVar y .-. int 1337) ~+~ Sum w lb (sVar z) (Idx (Var x) (sVar w)))
          )
          @??= sym2SoP (Sum w (sVar y .-. int 1337) (sVar z) (Idx (Var x) (sVar w))),
      testCase "Extend sum lower bound (4)" $
        run
          ( \(x, _, z, w, _, _, _, _) -> do
              let lb = int 1
              z `lowerBoundedBy` lb
              rewrite (Idx (Var x) (int 0) ~+~ Sum w lb (sVar z) (Idx (Var x) (sVar w)))
          )
          @??= sym2SoP (Sum w (int 0) (sVar z) (Idx (Var x) (sVar w))),
      testCase "Extend sum lower bound twice" $
        run
          ( \(x, y, z, w, _, _, _, _) -> do
              let lb = sVar y .+. int 1
              z `lowerBoundedBy` lb
              rewrite (Idx (Var x) (sVar y .-. int 1) ~+~ Idx (Var x) (sVar y) .+. sym2SoP (Sum w lb (sVar z) (Idx (Var x) (sVar w))))
          )
          @??= sym2SoP (Sum w (sVar y .-. int 1) (sVar z) (Idx (Var x) (sVar w))),
      testCase "Extend sum upper bound (1)" $
        run
          ( \(x, y, z, w, _, _, _, _) -> do
              let ub = sVar z .-. int 1
              y `upperBoundedBy` ub
              rewrite (Idx (Var x) (sVar z) ~+~ Sum w (sVar y) ub (Idx (Var x) (sVar w)))
          )
          @??= sym2SoP (Sum w (sVar y) (sVar z) (Idx (Var x) (sVar w))),
      testCase "Extend sum upper bound (2)" $
        run
          ( \(x, y, z, w, _, _, _, _) -> do
              let ub = sVar z
              y `upperBoundedBy` ub
              rewrite (Idx (Var x) (sVar z .+. int 1) ~+~ Sum w (sVar y) (sVar z) (Idx (Var x) (sVar w)))
          )
          @??= sym2SoP (Sum w (sVar y) (sVar z .+. int 1) (Idx (Var x) (sVar w))),
      testCase "Merge sum-subtraction (no match)" $
        -- Should fail because we cannot show b <= c without bounds on these variables general.
        run
          ( \(x, _, z, w, a, b, c, _) ->
              rewrite
                ( Sum w (sVar a) (sVar c) (Idx (Var x) (sVar w))
                    ~-~ Sum z (sVar a) (sVar b) (Idx (Var x) (sVar z))
                )
          )
          @??= (Sum w (sVar a) (sVar c) (Idx (Var x) (sVar w)) ~-~ Sum z (sVar a) (sVar b) (Idx (Var x) (sVar z))),
      testCase "Merge sum-subtraction (match)" $
        run
          ( \(x, _, z, w, a, b, c, _) -> do
              addAlgRange b (sVar a) (sVar c)
              rewrite
                ( Sum w (sVar a) (sVar c) (Idx (Var x) (sVar w))
                    ~-~ Sum z (sVar a) (sVar b) (Idx (Var x) (sVar z))
                )
          )
          @??= sym2SoP (Sum w (sVar b .+. int 1) (sVar c) (Idx (Var x) (sVar w))),
      testCase "Merge sum-subtraction (match 2)" $
        run
          ( \(x, _, z, w, a, b, _, _) -> do
              addAlgRange b (int2SoP 0) (sVar a .-. int 1)
              rewrite
                ( Sum w (int 0) (sVar a .-. int 1) (Idx (Var x) (sVar w))
                    ~-~ Sum z (int 0) (sVar b) (Idx (Var x) (sVar z))
                )
          )
          @??= sym2SoP (Sum w (sVar b .+. int 1) (sVar a .-. int 1) (Idx (Var x) (sVar w))),
      testCase "Merge sum-subtraction (match 3)" $
        run
          ( \(x, _, z, w, a, b, _, _) -> do
              addAlgRange b (int2SoP 0) (sVar a)
              rewrite
                ( Sum w (int 0) (sVar a) (Idx (Var x) (sVar w))
                    ~-~ Sum z (int 0) (sVar b) (Idx (Var x) (sVar z))
                )
          )
          @??= sym2SoP (Sum w (sVar b .+. int 1) (sVar a) (Idx (Var x) (sVar w))),
      testCase "Rule matches on subterms" $
        run
          ( \(x, y, z, w, _, _, _, _) -> do
              let lb = sVar y .+. int 1
              z `lowerBoundedBy` lb
              rewrite (int 1 .+. Idx (Var x) (sVar y) ~+~ Sum w lb (sVar z) (Idx (Var x) (sVar w)))
          )
          @??= (int 1 .+. sym2SoP (Sum w (sVar y) (sVar z) (Idx (Var x) (sVar w)))),
      testCase "Rule matches on all relevant subterms" $
        run
          ( \(x, y, z, w, a, b, c, d) -> do
              let lb = sVar y .+. int 1
              z `lowerBoundedBy` lb
              let lb2 = sVar b .+. int 1
              c `lowerBoundedBy` lb2
              rewrite (int 1 .+. Idx (Var x) (sVar y) ~+~ Sum w lb (sVar z) (Idx (Var x) (sVar w)) .+. Idx (Var a) (sVar b) ~+~ Sum d lb2 (sVar c) (Idx (Var a) (sVar d)))
          )
          @??= (int 1 .+. Sum w (sVar y) (sVar z) (Idx (Var x) (sVar w)) ~+~ Sum d (sVar b) (sVar c) (Idx (Var a) (sVar d))),
      testCase "Match symbols in SVar" $
        run
          ( \(x, y, z, _, _, _, _, _) ->
              rewrite ((Bool True :&& (sVar x :<= sVar y)) ~+~ Var z)
          )
          @??= ((sVar x :<= sVar y) ~+~ Var z),
      testCase "Match SVars in symbols in SVar" $
        run
          ( \(x, _, _, _, _, _, _, _) ->
              rewrite (sym2SoP $ Idx (Var x) (sym2SoP $ (neg (Var x))))
          )
          @??= sym2SoP (Idx (Var x) (int 1 .-. sym2SoP ((Var x)))),
      testCase "[[¬x]] => 1 - [[x]]" $
        run
          ( \(x, _, _, _, _, _, _, _) ->
              rewrite (sym2SoP $ (neg (Var x)))
          )
          @??= (int 1 .-. sym2SoP ((Var x))),
      -- Symbol tests.
      testCase ":&& identity (1)" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite (Bool True :&& (sVar x :<= sVar y))
          )
          @??= (sVar x :<= sVar y),
      testCase ":&& identity (2)" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite ((sVar x :<= sVar y) :&& Bool True)
          )
          @??= (sVar x :<= sVar y),
      testCase ":&& annhilation (1)" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite (Bool False :&& (sVar x :<= sVar y))
          )
          @??= Bool False,
      testCase ":&& annhilation (2)" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite ((sVar x :<= sVar y) :&& Bool False)
          )
          @??= Bool False,
      testCase ":|| identity (1)" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite (Bool False :|| (sVar x :<= sVar y))
          )
          @??= (sVar x :<= sVar y),
      testCase ":|| identity (2)" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite ((sVar x :<= sVar y) :|| Bool False)
          )
          @??= (sVar x :<= sVar y),
      testCase ":|| annihilation (1)" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite (Bool True :|| (sVar x :<= sVar y))
          )
          @??= Bool True,
      testCase ":|| annihilation (2)" $
        run
          ( \(x, y, _, _, _, _, _, _) ->
              rewrite ((sVar x :<= sVar y) :|| Bool True)
          )
          @??= Bool True,
      -- Refine tests.
      testCase "Equivalence (1)" $
        run
          ( \(x, _, _, _, _, _, _, _) -> do
              let x' = Algebra.Var x
              addEquiv x' (int2SoP 1)
              rewrite (sVar x .+. int 1)
          )
          @??= int 2,
      testCase "Tautology" $
        run
          ( \(_, _, _, _, _, _, _, _) ->
              rewrite (int 1 :<= int 2)
          )
          @??= Bool True,
      testCase "Tautology (negated contradiction)" $
        run
          ( \(_, _, _, _, _, _, _, _) ->
              rewrite (neg $ int 1 :>= int 2)
          )
          @??= Bool True,
      testCase "Tautology (variable)" $
        run
          ( \(x, _, _, _, _, _, _, _) -> do
              let x' = Algebra.Var x
              addEquiv x' (int2SoP 1)
              rewrite (sVar x :<= int 2)
          )
          @??= Bool True,
      testCase "Match subsymbol" $
        run
          ( \(x, y, _, _, _, _, _, _) -> do
              addProperty (Algebra.Var x) Algebra.Boolean
              addProperty (Algebra.Var y) Algebra.Boolean
              rewrite (Var x :&& (Var y :&& neg (int 1 :>= int 2)))
          )
          @??= (Var x :&& Var y),
      testCase "Replace sum over one element sequence by element (1)" $
        run
          ( \(x, y, _, w, _, _, _, _) ->
              rewrite (sym2SoP $ Sum w (sVar y) (sVar y) (Idx (Var x) (sVar w)))
          )
          @??= sym2SoP (Idx (Var x) (sVar y)),
      testCase "Replace sum over one element sequence by element (2)" $
        run
          ( \(x, _, _, w, _, _, _, _) ->
              rewrite (sym2SoP $ Sum w (int 0) (int 0) (Idx (Var x) (sVar w)))
          )
          @??= sym2SoP (Idx (Var x) (int 0)),
      testCase "Replace sum over empty sequence by zero (1)" $
        run
          ( \(x, y, _, w, _, _, _, _) ->
              rewrite (sym2SoP $ Sum w (sVar y) (sVar y .-. int 1) (Idx (Var x) (sVar w)))
          )
          @??= int2SoP 0,
      testCase "Replace sum over empty sequence by zero (2)" $
        run
          ( \(x, _, _, w, _, _, _, _) ->
              rewrite (sym2SoP $ Sum w (int 1) (int 0) (Idx (Var x) (sVar w)))
          )
          @??= int2SoP 0,
      -- Index functions.
      testCase "Rule 5 (carry) (match 1)" $
        run
          ( \(x, _, _, _, a, b, _, _) ->
              rewrite
                ( IndexFn
                    { iterator = Forall x (Iota (sVar a)),
                      body =
                        cases
                          [ (sVar x :== int 0, sVar b),
                            (sVar x :/= int 0, sym2SoP Recurrence)
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Iota (sVar a)),
                     body = cases [(Bool True, sVar b)]
                   }
               ),
      testCase "Rule 5 (carry) (match 2)" $
        run
          ( \(x, _, _, _, a, _, _, _) ->
              rewrite
                ( IndexFn
                    { iterator = Forall x (Iota (sVar a)),
                      body =
                        cases
                          [ (sVar x :== int 0, sVar x),
                            (sVar x :/= int 0, sym2SoP Recurrence)
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Iota (sVar a)),
                     body = cases [(Bool True, int 0)]
                   }
               ),
      testCase "Rule 5 (carry) (don't match 1)" $
        run
          ( \(x, _, _, _, _, _, _, _) ->
              rewrite
                ( IndexFn
                    { iterator = Forall x (Iota (sVar a)),
                      body =
                        cases
                          [ (sVar b :== int 0, int 0),
                            (sVar b :/= int 0, sym2SoP Recurrence)
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Iota (sVar a)),
                     body =
                       cases
                         [ (sVar b :== int 0, int 0),
                           (sVar b :/= int 0, sym2SoP Recurrence)
                         ]
                   }
               ),
      testCase "Rule 5 (carry) (match 3)" $
        run
          ( \(x, y, _, _, a, b, _, _) ->
              rewrite
                ( IndexFn
                    { iterator = Forall x (Cat y (sVar a) (sym2SoP $ Idx (Var b) $ sVar y)),
                      body =
                        cases
                          [ (sVar x :== sym2SoP (Idx (Var b) $ sVar y), sVar y),
                            (sVar x :/= sym2SoP (Idx (Var b) $ sVar y), sym2SoP Recurrence)
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Cat y (sVar a) (sym2SoP $ Idx (Var b) $ sVar y)),
                     body = cases [(Bool True, sVar y)]
                   }
               ),
      testCase "Rule 4 (prefix sum) (match 1)" $
        run
          ( \(x, y, _, _, a, b, _, _) -> do
              rewrite
                ( IndexFn
                    { iterator = Forall x (Iota (sVar y)),
                      body =
                        cases
                          [ (sVar x :== int 0, sVar a),
                            (sVar x :/= int 0, Recurrence ~+~ Idx (Var b) (sVar x))
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Iota (sVar y)),
                     body = cases [(Bool True, Var a ~+~ Sum c (int 1) (sVar x) (Idx (Var b) (sVar c)))]
                   }
               ),
      testCase "Match scan (1)" $
        run
          ( \(x, y, _, _, _, b, _, _) ->
              rewrite
                ( IndexFn
                    { iterator = Forall x (Iota (sVar y)),
                      body =
                        cases
                          [ (sVar x :== int 0, sym2SoP $ Idx (Var b) (sVar x)),
                            (sVar x :/= int 0, Recurrence ~+~ Idx (Var b) (sVar x))
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Iota (sVar y)),
                     body = cases [(Bool True, sym2SoP $ Sum c (int 0) (sVar x) (Idx (Var b) (sVar c)))]
                   }
               ),
      testCase "Match scan (2)" $
        run
          ( \(x, y, _, _, _, b, _, _) ->
              rewrite
                ( IndexFn
                    { iterator = Forall x (Iota (sVar y)),
                      body =
                        cases
                          [ (sVar x :== int 0, int2SoP (-1) .*. sym2SoP (Idx (Var b) (sVar x))),
                            (sVar x :/= int 0, Recurrence ~+~ Idx (Var b) (sVar x))
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Iota (sVar y)),
                     body = cases [(Bool True, int2SoP (-1) .*. sym2SoP (Idx (Var b) (int 0)) .+. sym2SoP (Sum c (int 1) (sVar x) (Idx (Var b) (sVar c))))]
                   }
               ),
      testCase "Match scan (3)" $
        run
          ( \(x, y, _, _, _, b, _, _) ->
              rewrite
                ( IndexFn
                    { iterator = Forall x (Iota (sVar y)),
                      body =
                        cases
                          [ (sVar x :== int 0, int2SoP (-1) .*. sym2SoP (Idx (Var b) (sVar x))),
                            (sVar x :/= int 0, sym2SoP Recurrence .+. int2SoP (-1) .*. sym2SoP (Idx (Var b) (sVar x)))
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Iota (sVar y)),
                     body = cases [(Bool True, int2SoP (-1) .*. sym2SoP (Sum c (int 0) (sVar x) (Idx (Var b) (sVar c))))]
                   }
               ),
      testCase "Match scan (4)" $
        run
          ( \(x, y, _, _, _, b, _, _) ->
              rewrite
                ( IndexFn
                    { iterator = Forall x (Iota (sVar y)),
                      body =
                        cases
                          [ (sVar x :== int 0, sym2SoP $ Idx (Var b) (sVar x)),
                            (sVar x :/= int 0, sym2SoP Recurrence .+. int2SoP (-1) .*. sym2SoP (Idx (Var b) (sVar x)))
                          ]
                    }
                )
          )
          @??= ( IndexFn
                   { iterator = Forall x (Iota (sVar y)),
                     body = cases [(Bool True, sym2SoP (Idx (Var b) (int 0)) .+. int2SoP (-1) .*. sym2SoP (Sum c (int 1) (sVar x) (Idx (Var b) (sVar c))))]
                   }
               )
    ]
  where
    int = int2SoP
    sVar = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b
    a ~-~ b = sym2SoP a .-. sym2SoP b

    varsM =
      (,,,,,,,)
        <$> newVName "x"
        <*> newVName "y"
        <*> newVName "z"
        <*> newVName "w"
        <*> newVName "a"
        <*> newVName "b"
        <*> newVName "c"
        <*> newVName "d"
    (x, y, z, w, a, b, c, d) = runTest varsM

    run f = runTest (varsM >>= f)

    -- Less fragile renaming.
    e @??= e' = do
      let (actual, expected) = runTest $ varsM >> renameSame e e'
      unless (actual == expected) (assertFailure $ msg actual expected)
      where
        msg actual expected =
          docString $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual

    vn `lowerBoundedBy` x = do
      a <- toAlgebra x
      addRange (Algebra.Var vn) (mkRangeLB a)

    vn `upperBoundedBy` x = do
      a <- toAlgebra x
      addRange (Algebra.Var vn) (mkRangeUB a)

    addAlgRange vn x y = do
      a <- toAlgebra x
      b <- toAlgebra y
      addRange (Algebra.Var vn) (mkRange a b)