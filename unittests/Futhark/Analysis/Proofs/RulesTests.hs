{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Proofs.RulesTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify
import Futhark.Analysis.Proofs.Rules
import Futhark.SoP.SoP (sym2SoP, (.+.), int2SoP, (.-.))
import Futhark.SoP.Monad (addRange)
import qualified Data.Set as S
import qualified Futhark.SoP.SoP as SoP
import Futhark.Analysis.Proofs.IndexFn
import Futhark.SoP.Monad (addEquiv)


runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests = testGroup "Proofs.Rules"
  [ testCase "Add" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (sop x .+. sop y)
      ) @??= (sop x .+. sop y)
  , testCase "Extend sum lower bound (1)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sop y) ~+~ LinComb w (sop y .+. int 1) (sop z) (Var x))
      ) @??= sym2SoP (LinComb w (sop y) (sop z) (Var x))
  , testCase "Extend sum lower bound (2)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sop y .-. int 1) ~+~ LinComb w (sop y) (sop z) (Var x))
      ) @??= sym2SoP (LinComb w (sop y .-. int 1) (sop z) (Var x))
  , testCase "Extend sum upper bound (1)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sop z) ~+~ LinComb w (sop y) (sop z .-. int 1) (Var x))
      ) @??= sym2SoP (LinComb w (sop y) (sop z) (Var x))
  , testCase "Extend sum upper bound (2)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sop z .+. int 1) ~+~ LinComb w (sop y) (sop z) (Var x))
      ) @??= sym2SoP (LinComb w (sop y) (sop z .+. int 1) (Var x))
  , testCase "Merge sum-subtraction (no match)" $
      -- Should fail because we cannot show b <= c without bounds on these variables general.
      run (\(x,_,z,w,a,b,c,_) ->
        rewrite (LinComb w (sop a) (sop c) (Var x) ~-~ LinComb z (sop a) (sop b) (Var x))
      ) @??= (LinComb w (sop a) (sop c) (Var x) ~-~ LinComb z (sop a) (sop b) (Var x))
  , testCase "Merge sum-subtraction (match)" $
      run (\(x,_,z,w,a,b,c,_) -> do
        addRange (Var b) (SoP.Range mempty 1 (S.singleton (sop c)))
        rewrite (LinComb w (sop a) (sop c) (Var x) ~-~ LinComb z (sop a) (sop b) (Var x))
      ) @??= sym2SoP (LinComb w (sop b .+. int 1) (sop c) (Var x))
  , testCase "Rule matches on subterms" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (int 1 .+. Idx (Var x) (sop y) ~+~ LinComb w (sop y .+. int 1) (sop z) (Var x))
      ) @??= (int 1 .+. sym2SoP (LinComb w (sop y) (sop z) (Var x)))
  , testCase "Rule matches on all relevant subterms" $
      run (\(x,y,z,w,a,b,c,d) ->
        rewrite (int 1 .+. Idx (Var x) (sop y) ~+~ LinComb w (sop y .+. int 1) (sop z) (Var x) .+. Idx (Var a) (sop b) ~+~ LinComb d (sop b .+. int 1) (sop c) (Var a))
      ) @??= (int 1 .+. sym2SoP (LinComb w (sop y) (sop z) (Var x)) .+. sym2SoP (LinComb d (sop b) (sop c) (Var a)))
  , testCase "Match symbols in SoP" $
      run (\(x,y,z,_,_,_,_,_) ->
        rewrite (Indicator (Bool True :&& (sop x :<= sop y)) ~+~ Var z)
      ) @??= (Indicator (sop x :<= sop y) ~+~ Var z)
  , testCase "Match SoPs in symbols in SoP" $
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
        rewrite (Bool True :&& (sop x :<= sop y))
      ) @??= (sop x :<= sop y)
  , testCase ":&& identity (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite ((sop x :<= sop y) :&& Bool True)
      ) @??= (sop x :<= sop y)
  , testCase ":&& annhilation (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Bool False :&& (sop x :<= sop y))
      ) @??= Bool False
  , testCase ":&& annhilation (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite ((sop x :<= sop y) :&& Bool False)
      ) @??= Bool False
  , testCase ":|| identity (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Bool False :|| (sop x :<= sop y))
      ) @??= (sop x :<= sop y)
  , testCase ":|| identity (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite ((sop x :<= sop y) :|| Bool False)
      ) @??= (sop x :<= sop y)
  , testCase ":|| annihilation (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Bool True :|| (sop x :<= sop y))
      ) @??= Bool True
  , testCase ":|| annihilation (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite ((sop x :<= sop y) :|| Bool True)
      ) @??= Bool True
  , testCase "Match subsymbols" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Indicator (Bool True :&& (sop x :<= sop y)))
      ) @??= Indicator (sop x :<= sop y)
  -- Refine tests.
 -- , testCase "Equivalence (1)" $
 --      run (\(x,_,_,_,_,_,_,_) -> do
 --        addEquiv (Var x) (int 1)
 --        rewrite (Var x)
 --      ) @??= int 1
  , testCase "Equivalence (2)" $
      run (\(x,_,_,_,_,_,_,_) -> do
        addEquiv (Var x) (int 1)
        rewrite (sop x .+. int 1)
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
        rewrite (sop x :<= int 2)
      ) @??= Bool True
  , testCase "Match subsymbol" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (Var x :&& (Var y :&& Not (int 1 :>= int 2)))
      ) @??= (Var x :&& Var y)
  ]
  where
    int = int2SoP
    sop = sym2SoP . Var
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
