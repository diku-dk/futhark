{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Futhark.Analysis.Proofs.UnifyTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import qualified Futhark.SoP.SoP as SoP
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (sym2SoP, (.+.), scaleSoP, (.*.), int2SoP)
import qualified Data.Map as M
import Futhark.Analysis.Proofs.IndexFn


type Exp = SoP.SoP Symbol
type Sub = Substitution Exp

runTest :: IndexFnM (Maybe Sub) -> Maybe Sub
runTest test = fst $ runIndexFnM test blankNameSource

getValue :: IndexFnM a -> a
getValue test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests = testGroup "Proofs.Unify"
  [ testCase "Add" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (name2SoP x .+. name2SoP y) (name2SoP z .+. name2SoP w)
      ) @?= x2z_y2w
  , testCase "Multiply" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (name2SoP x .*. name2SoP y) (name2SoP z .*. name2SoP w)
      ) @?= x2z_y2w
  , testCase "First is scaled" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (scaleSoP 2 (name2SoP x) .+. name2SoP y) (name2SoP z .+. name2SoP w)
      ) @?= Nothing
  , testCase "Second is scaled" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (name2SoP x .+. name2SoP y) (name2SoP z .+. scaleSoP 2 (name2SoP w))
      ) @?= Nothing
  , testCase "Both scaled, but permuted" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (scaleSoP 2 (name2SoP x) .+. name2SoP y) (name2SoP z .+. scaleSoP 2 (name2SoP w))
      ) @?= x2w_y2z
  , testCase "Wrong operator" $
      run (\(x,y,_,_,_,_,_,_) ->
        unify (name2SoP x .*. name2SoP y) (name2SoP x .+. name2SoP y)
      ) @?= Nothing
  , testCase "One has constant" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (name2SoP x .+. name2SoP y .+. int2SoP 2) (name2SoP z .+. name2SoP w)
      ) @?= Nothing
  , testCase "Different constants" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (name2SoP x .+. name2SoP y .+. int2SoP 1) (name2SoP z .+. name2SoP w .+. int2SoP 2)
      ) @?= Nothing
  , testCase "Same constant" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (name2SoP x .+. name2SoP y .+. int2SoP 2) (name2SoP z .+. name2SoP w .+. int2SoP 2)
      ) @?= x2z_y2w
  -- Empty substitutions (e.g., simply permuting symbols or terms).
  , testCase "Permuted terms" $
      run (\(x,y,_,_,_,_,_,_) ->
        unify (name2SoP x .+. name2SoP y) (name2SoP y .+. name2SoP x)
      ) @?= Just mempty
  , testCase "Permuted symbols" $
      run (\(x,y,z,_,_,_,_,_) ->
        unify (name2SoP x .*. name2SoP y .*. name2SoP z) (name2SoP z .*. name2SoP y .*. name2SoP x)
      ) @?= Just mempty
  -- Indexing.
  , testCase "Indexing" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (Idx (Var x) (name2SoP y .+. int2SoP 1)) (Idx (Var z) (name2SoP w .+. int2SoP 1))
      ) @?= x2z_y2w
  , testCase "Indexing different constant" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (Idx (Var x) (name2SoP y .+. int2SoP 1)) (Idx (Var z) (name2SoP w .+. int2SoP 2))
      ) @?= Nothing
  -- Substituting with quantifiers.
  , testCase "LinComb empty" $
      run (\(x,y,z,w,_,_,_,_) ->
        unify (LinComb x (name2SoP y) (name2SoP z) (Var w)) (LinComb x (name2SoP y) (name2SoP z) (Var w))
      ) @?= Just mempty
  , testCase "Bound names are not substituted" $
      run (\(x,y,z,w,a,b,c,d) ->
        unify (LinComb x (name2SoP y) (name2SoP z) (Var w)) (LinComb a (name2SoP b) (name2SoP c) (Var d))
      ) @?= y2b_z2c_w2d
  , testCase "Bound names are renamed" $
      run (\(x,_,_,_,a,b,c,d) ->
        unify (Var x) (LinComb a (name2SoP b) (name2SoP c) (Var d))
      ) @?= let renamed_lin_comb = getValue $ do
                  (_,_,_,_,a,b,c,d) <- varsM
                  _ <- newVName "k" -- Simulate "k" introduced by Unify.
                  rename $ sym2SoP (LinComb a (name2SoP b) (name2SoP c) (Var d))
            in Just (M.singleton x renamed_lin_comb)
  -- TODO This test shouldn't be allowed since we assume VNames in first argument are holes?
  -- , testCase "Substitute only some symbols" $
  --     run_xyzw (\(x,y,z,_) ->
  --       unify (name2SoP x .*. name2SoP y .*. name2SoP w) (name2SoP z .*. name2SoP w .*. name2SoP w)
  --     ) @?= x2z_y2w
  ]
  where
    name2SoP = sym2SoP . Var

    varsM =
      (,,,,,,,)
        <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
        <*> newVName "a" <*> newVName "b" <*> newVName "c" <*> newVName "d"
    (x,y,z,w,_,b,c,d) = getValue varsM

    x2z_y2w = Just $ M.fromList [(x, name2SoP z), (y, name2SoP w)]
    x2w_y2z = Just $ M.fromList [(x, name2SoP w), (y, name2SoP z)]
    y2b_z2c_w2d =
      Just $ M.fromList [(y, name2SoP b), (z, name2SoP c), (w, name2SoP d)]

    run f = runTest (varsM >>= f)
