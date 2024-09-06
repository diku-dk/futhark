{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Proofs.RewriteTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify
import Futhark.Analysis.Proofs.Rewrite
import Futhark.SoP.SoP (sym2SoP, (.+.), int2SoP, (.-.))
import Futhark.SoP.Monad (addEquiv, addRange)
import qualified Data.Set as S
import qualified Futhark.SoP.SoP as SoP
import Futhark.Analysis.Proofs.IndexFn


runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests = testGroup "Proofs.Rewrite"
  [ testCase "Add" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (sVar x .+. sVar y)
      ) @??= (sVar x .+. sVar y)
  , testCase "Extend sum lower bound (1)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sVar y) ~+~ LinComb w (sVar y .+. int 1) (sVar z) (Var x))
      ) @??= sym2SoP (LinComb w (sVar y) (sVar z) (Var x))
  , testCase "Extend sum lower bound (2)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sVar y .-. int 1) ~+~ LinComb w (sVar y) (sVar z) (Var x))
      ) @??= sym2SoP (LinComb w (sVar y .-. int 1) (sVar z) (Var x))
  , testCase "Extend sum upper bound (1)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sVar z) ~+~ LinComb w (sVar y) (sVar z .-. int 1) (Var x))
      ) @??= sym2SoP (LinComb w (sVar y) (sVar z) (Var x))
  , testCase "Extend sum upper bound (2)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sVar z .+. int 1) ~+~ LinComb w (sVar y) (sVar z) (Var x))
      ) @??= sym2SoP (LinComb w (sVar y) (sVar z .+. int 1) (Var x))
  , testCase "Merge sum-subtraction (no match)" $
      -- Should fail because we cannot show b <= c without bounds on these variables general.
      run (\(x,_,z,w,a,b,c,_) ->
        rewrite (LinComb w (sVar a) (sVar c) (Var x) ~-~ LinComb z (sVar a) (sVar b) (Var x))
      ) @??= (LinComb w (sVar a) (sVar c) (Var x) ~-~ LinComb z (sVar a) (sVar b) (Var x))
  , testCase "Merge sum-subtraction (match)" $
      run (\(x,_,z,w,a,b,c,_) -> do
        addRange (Var b) (SoP.Range mempty 1 (S.singleton (sVar c)))
        rewrite (LinComb w (sVar a) (sVar c) (Var x) ~-~ LinComb z (sVar a) (sVar b) (Var x))
      ) @??= sym2SoP (LinComb w (sVar b .+. int 1) (sVar c) (Var x))
  , testCase "Rule matches on subterms" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (int 1 .+. Idx (Var x) (sVar y) ~+~ LinComb w (sVar y .+. int 1) (sVar z) (Var x))
      ) @??= (int 1 .+. sym2SoP (LinComb w (sVar y) (sVar z) (Var x)))
  , testCase "Rule matches on all relevant subterms" $
      run (\(x,y,z,w,a,b,c,d) ->
        rewrite (int 1 .+. Idx (Var x) (sVar y) ~+~ LinComb w (sVar y .+. int 1) (sVar z) (Var x) .+. Idx (Var a) (sVar b) ~+~ LinComb d (sVar b .+. int 1) (sVar c) (Var a))
      ) @??= (int 1 .+. LinComb w (sVar y) (sVar z) (Var x) ~+~ LinComb d (sVar b) (sVar c) (Var a))
  , testCase "Match symbols in SVar" $
      run (\(x,y,z,_,_,_,_,_) ->
        rewrite (Indicator (Bool True :&& (sVar x :<= sVar y)) ~+~ Var z)
      ) @??= (Indicator (sVar x :<= sVar y) ~+~ Var z)
  , testCase "Match SVars in symbols in SVar" $
      run (\(x,_,_,_,_,_,_,_) ->
        rewrite (sym2SoP $ Idx (Var x) (sym2SoP $ Indicator (Not (Var x))))
      ) @??= sym2SoP (Idx (Var x) (int 1 .-. sym2SoP (Indicator (Var x))))
  , testCase "[[¬x]] => 1 - [[x]]" $
      run (\(x,_,_,_,_,_,_,_) ->
        rewrite (sym2SoP $ Indicator (Not (Var x)))
      ) @??= (int 1 .-. sym2SoP (Indicator (Var x)))
  -- Symbol tests.
  , testCase ":&& identity (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Bool True :&& (sVar x :<= sVar y))
      ) @??= (sVar x :<= sVar y)
  , testCase ":&& identity (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite ((sVar x :<= sVar y) :&& Bool True)
      ) @??= (sVar x :<= sVar y)
  , testCase ":&& annhilation (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Bool False :&& (sVar x :<= sVar y))
      ) @??= Bool False
  , testCase ":&& annhilation (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite ((sVar x :<= sVar y) :&& Bool False)
      ) @??= Bool False
  , testCase ":|| identity (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Bool False :|| (sVar x :<= sVar y))
      ) @??= (sVar x :<= sVar y)
  , testCase ":|| identity (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite ((sVar x :<= sVar y) :|| Bool False)
      ) @??= (sVar x :<= sVar y)
  , testCase ":|| annihilation (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Bool True :|| (sVar x :<= sVar y))
      ) @??= Bool True
  , testCase ":|| annihilation (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite ((sVar x :<= sVar y) :|| Bool True)
      ) @??= Bool True
  , testCase "Match subsymbols" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Indicator (Bool True :&& (sVar x :<= sVar y)))
      ) @??= Indicator (sVar x :<= sVar y)
  -- Refine tests.
  , testCase "Equivalence (1)" $
      run (\(x,_,_,_,_,_,_,_) -> do
        addEquiv (Var x) (int 1)
        rewrite (sVar x .+. int 1)
      ) @??= int 2
  , testCase "Tautology" $
      run (\(_,_,_,_,_,_,_,_) ->
        rewrite (int 1 :<= int 2)
      ) @??= Bool True
  , testCase "Tautology (negated contradiction)" $
      run (\(_,_,_,_,_,_,_,_) ->
        rewrite (Not $ int 1 :>= int 2)
      ) @??= Bool True
  , testCase "Tautology (variable)" $
      run (\(x,_,_,_,_,_,_,_) -> do
        addEquiv (Var x) (int 1)
        rewrite (sVar x :<= int 2)
      ) @??= Bool True
  , testCase "Match subsymbol" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Var x :&& (Var y :&& Not (int 1 :>= int 2)))
      ) @??= (Var x :&& Var y)
  -- Index functions.
  , testCase "Rule 5 (carry) (match 1)" $
      run (\(x,_,_,_,a,b,_,_) ->
        rewrite (IndexFn {
            iterator = Forall x (Iota (sVar a)),
            body = cases [(sVar x :== int 0, sVar b),
                          (sVar x :/= int 0, sym2SoP Recurrence)]
          })
      ) @??= (IndexFn {
            iterator = Forall x (Iota (sVar a)),
            body = cases [(Bool True, sVar b)]
          })
  , testCase "Rule 5 (carry) (match 2)" $
      run (\(x,_,_,_,a,_,_,_) ->
        rewrite (IndexFn {
            iterator = Forall x (Iota (sVar a)),
            body = cases [(sVar x :== int 0, sVar x),
                          (sVar x :/= int 0, sym2SoP Recurrence)]
          })
      ) @??= (IndexFn {
            iterator = Forall x (Iota (sVar a)),
            body = cases [(Bool True, int 0)]
          })
  , testCase "Rule 5 (carry) (don't match 1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (IndexFn {
            iterator = Forall x (Iota (sVar a)),
            body = cases [(sVar b :== int 0, sVar b),
                          (sVar b :/= int 0, sym2SoP Recurrence)]
          })
      ) @??= (IndexFn {
            iterator = Forall x (Iota (sVar a)),
            body = cases [(sVar b :== int 0, sVar b),
                          (sVar b :/= int 0, sym2SoP Recurrence)]
          })
  , testCase "Rule 5 (carry) (match 3)" $
      run (\(x,y,_,_,a,b,_,_) ->
        rewrite (IndexFn {
            iterator = Forall x (Cat y (sVar a) (sVar b)),
            body = cases [(sVar x :== int 0, sVar y),
                          (sVar x :/= int 0, sym2SoP Recurrence)]
          })
      ) @??= (IndexFn {
            iterator = Forall x (Cat y (sVar a) (sVar b)),
            body = cases [(Bool True, sVar y)]
          })
  , testCase "Rule 4 (prefix sum) (match 1)" $
      run (\(x,_,_,_,a,b,c,_) ->
        rewrite (IndexFn {
            iterator = Forall x (Iota (sVar a)),
            body = cases [(sVar x :== int 0, sVar b),
                          (sVar x :/= int 0, Recurrence ~+~ Idx (Var b) (sVar x))]
          })
      ) @??= (IndexFn {
            iterator = Forall x (Iota (sVar a)),
            body = cases [(Bool True, Var b ~+~ LinComb c (int 0) (sVar x) (Idx (Var b) (sVar c)))]
          })
  ]
  where
    int = int2SoP
    sVar = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b
    a ~-~ b = sym2SoP a .-. sym2SoP b

    varsM =
      (,,,,,,,) <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
                <*> newVName "a" <*> newVName "b" <*> newVName "c" <*> newVName "d"
    (x,y,z,w,a,b,c,d) = runTest varsM

    run f = runTest (varsM >>= f)

    -- Less fragile renaming.
    e @??= e' = renamed e @?= renamed e'
    renamed x = runTest $ do
          putNameSource (newNameSource (-10000))
          rename x
