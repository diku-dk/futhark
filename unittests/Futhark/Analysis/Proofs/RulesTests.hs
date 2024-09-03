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


runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests = testGroup "Proofs.Rules"
  [ testCase "Add" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sop x .+. sop y)
      ) @??= (sop x .+. sop y)
  , testCase "Extend sum lower bound (1)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite sopRules (Idx (Var x) (sop y) ~+~ LinComb w (sop y .+. int 1) (sop z) (Var x))
      ) @??= sym2SoP (LinComb w (sop y) (sop z) (Var x))
  , testCase "Extend sum lower bound (2)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite sopRules (Idx (Var x) (sop y .-. int 1) ~+~ LinComb w (sop y) (sop z) (Var x))
      ) @??= sym2SoP (LinComb w (sop y .-. int 1) (sop z) (Var x))
  , testCase "Extend sum upper bound (1)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite sopRules (Idx (Var x) (sop z) ~+~ LinComb w (sop y) (sop z .-. int 1) (Var x))
      ) @??= sym2SoP (LinComb w (sop y) (sop z) (Var x))
  , testCase "Extend sum upper bound (2)" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite sopRules (Idx (Var x) (sop z .+. int 1) ~+~ LinComb w (sop y) (sop z) (Var x))
      ) @??= sym2SoP (LinComb w (sop y) (sop z .+. int 1) (Var x))
  , testCase "Merge sum-subtraction" $
      -- Should fail because we cannot show b <= c without bounds on these variables general.
      run (\(x,_,z,w,a,b,c,_) ->
        rewrite sopRules (LinComb w (sop a) (sop c) (Var x) ~-~ LinComb z (sop a) (sop b) (Var x))
      ) @??= (LinComb w (sop a) (sop c) (Var x) ~-~ LinComb z (sop a) (sop b) (Var x))
  , testCase "Merge sum-subtraction" $
      run (\(x,_,z,w,a,b,c,_) -> do
        addRange (Var b) (SoP.Range mempty 1 (S.singleton (sop c)))
        rewrite sopRules (LinComb w (sop a) (sop c) (Var x) ~-~ LinComb z (sop a) (sop b) (Var x))
      ) @??= sym2SoP (LinComb w (sop b .+. int 1) (sop c) (Var x))
  , testCase "and (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sym2SoP $ Bool True :&& (sop x :<= sop y))
      ) @??= sym2SoP (sop x :<= sop y)
  , testCase "and (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sym2SoP $ (sop x :<= sop y) :&& Bool True)
      ) @??= sym2SoP (sop x :<= sop y)
  , testCase "and (3)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sym2SoP $ Bool False :&& (sop x :<= sop y))
      ) @??= sym2SoP (Bool False)
  , testCase "and (4)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sym2SoP $ (sop x :<= sop y) :&& Bool False)
      ) @??= sym2SoP (Bool False)
  , testCase "or (1)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sym2SoP $ Bool True :|| (sop x :<= sop y))
      ) @??= sym2SoP (Bool True)
  , testCase "or (2)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sym2SoP $ (sop x :<= sop y) :|| Bool True)
      ) @??= sym2SoP (Bool True)
  , testCase "or (3)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sym2SoP $ Bool False :|| (sop x :<= sop y))
      ) @??= sym2SoP (sop x :<= sop y)
  , testCase "or (4)" $
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite sopRules (sym2SoP $ (sop x :<= sop y) :|| Bool False)
      ) @??= sym2SoP (sop x :<= sop y)
  ]
  where
    int = int2SoP
    sop = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b
    a ~-~ b = sym2SoP a .-. sym2SoP b

    varsM =
      (,,,,,,,) <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
                <*> newVName "a" <*> newVName "b" <*> newVName "c" <*> newVName "d"
    (x,y,z,w,a,b,c,_) = runTest varsM

    run f = runTest (varsM >>= f)

    -- Less fragile renaming.
    e @??= e' = renamed e @?= renamed e'
    renamed x = runTest $ do
          putNameSource (newNameSource (-10000))
          rename x
