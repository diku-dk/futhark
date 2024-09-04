{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Futhark.Analysis.Proofs.RefineTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (sym2SoP, int2SoP, SoP, (.+.))
import Futhark.SoP.Monad (addEquiv)
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Refine

runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests = testGroup "Proofs.Refine"
  [ testCase "Equivalence (1)" $
      run (\(x,_,_,_,_,_,_,_) -> do
        addEquiv (Var x) (int 1)
        refine (Var x) :: IndexFnM (SoP Symbol)
      ) @??= int 1
  , testCase "Equivalence (2)" $
      run (\(x,_,_,_,_,_,_,_) -> do
        addEquiv (Var x) (int 1)
        refine (sop x .+. int 1)
      ) @??= int 2
  , testCase "Tautology" $
      run (\(_,_,_,_,_,_,_,_) ->
        refine (int 1 :<= int 2) :: IndexFnM Symbol
      ) @??= Bool True
  , testCase "Tautology (negated contradiction)" $
      run (\(_,_,_,_,_,_,_,_) ->
        refine (Not $ int 1 :>= int 2) :: IndexFnM Symbol
      ) @??= Bool True
  , testCase "Tautology (variable)" $
      run (\(x,_,_,_,_,_,_,_) -> do
        addEquiv (Var x) (int 1)
        refine (sop x :<= int 2) :: IndexFnM Symbol
      ) @??= Bool True
  , testCase "Match subsymbol" $
      run (\(x,y,_,_,_,_,_,_) ->
        refine (Var x :&& (Var y :&& Not (int 1 :>= int 2))) :: IndexFnM Symbol
      ) @??= (Var x :&& Var y)
  ]
  where
    int = int2SoP
    sop = sym2SoP . Var

    varsM =
      (,,,,,,,) <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
                <*> newVName "a" <*> newVName "b" <*> newVName "c" <*> newVName "d"
    (x,y,_,_,_,_,_,_) = runTest varsM

    run f = runTest (varsM >>= f)

    -- Less fragile renaming.
    e @??= e' = renamed e @?= renamed e'
    renamed x = runTest $ do
          putNameSource (newNameSource (-10000))
          rename x

