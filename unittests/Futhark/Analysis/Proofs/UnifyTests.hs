{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Futhark.Analysis.Proofs.UnifyTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import qualified Futhark.SoP.SoP as SoP
import Futhark.Analysis.Proofs.Term
import Futhark.Analysis.Proofs.Unify
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import Futhark.SoP.SoP (sym2SoP, (.+.), scaleSoP, (.*.), int2SoP)
import qualified Data.Map as M
import Data.Maybe (fromJust)


newtype VEnv = VEnv { vnamesource :: VNameSource }

type Exp = SoP.SoP Term
type Sub = Substitution Exp

newtype TestM a = TestM (RWS () () VEnv a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames
    )

instance (Monoid w) => MonadFreshNames (RWS r w VEnv) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

evalTestM :: TestM a -> VNameSource -> a
evalTestM (TestM m) vns = fst $ evalRWS m () s
  where
    s = VEnv vns

runTest :: TestM (Maybe Sub) -> Maybe Sub
runTest test = evalTestM test blankNameSource

getValue :: TestM a -> a
getValue m = evalTestM m blankNameSource


tests :: TestTree
tests = testGroup "Proofs.Unify"
  [ testCase "Add" $
      run_xyzw (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y) (name2SoP z .+. name2SoP w)
      ) @?= x2z_y2w
  , testCase "Multiply" $
      run_xyzw (\(x,y,z,w) ->
        unify (name2SoP x .*. name2SoP y) (name2SoP z .*. name2SoP w)
      ) @?= x2z_y2w
  , testCase "First is scaled" $
      run_xyzw (\(x,y,z,w) ->
        unify (scaleSoP 2 (name2SoP x) .+. name2SoP y) (name2SoP z .+. name2SoP w)
      ) @?= Nothing
  , testCase "Second is scaled" $
      run_xyzw (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y) (name2SoP z .+. scaleSoP 2 (name2SoP w))
      ) @?= Nothing
  , testCase "Both scaled, but permuted" $
      run_xyzw (\(x,y,z,w) ->
        unify (scaleSoP 2 (name2SoP x) .+. name2SoP y) (name2SoP z .+. scaleSoP 2 (name2SoP w))
      ) @?= x2w_y2z
  , testCase "Wrong operator" $
      run_xyzw (\(x,y,_,_) ->
        unify (name2SoP x .*. name2SoP y) (name2SoP x .+. name2SoP y)
      ) @?= Nothing
  , testCase "One has constant" $
      run_xyzw (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y .+. int2SoP 2) (name2SoP z .+. name2SoP w)
      ) @?= Nothing
  , testCase "Different constants" $
      run_xyzw (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y .+. int2SoP 1) (name2SoP z .+. name2SoP w .+. int2SoP 2)
      ) @?= Nothing
  , testCase "Same constant" $
      run_xyzw (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y .+. int2SoP 2) (name2SoP z .+. name2SoP w .+. int2SoP 2)
      ) @?= x2z_y2w
  -- Empty substitutions (e.g., simply permuting symbols or terms).
  , testCase "Permuted terms" $
      run_xyzw (\(x,y,_,_) ->
        unify (name2SoP x .+. name2SoP y) (name2SoP y .+. name2SoP x)
      ) @?= Just mempty
  , testCase "Permuted symbols" $
      run_xyzw (\(x,y,z,_) ->
        unify (name2SoP x .*. name2SoP y .*. name2SoP z) (name2SoP z .*. name2SoP y .*. name2SoP x)
      ) @?= Just mempty
  -- Indexing.
  , testCase "Indexing" $
      run_xyzw (\(x,y,z,w) ->
        unify (Idx (Var x) (name2SoP y .+. int2SoP 1)) (Idx (Var z) (name2SoP w .+. int2SoP 1))
      ) @?= x2z_y2w
  , testCase "Indexing different constant" $
      run_xyzw (\(x,y,z,w) ->
        unify (Idx (Var x) (name2SoP y .+. int2SoP 1)) (Idx (Var z) (name2SoP w .+. int2SoP 2))
      ) @?= Nothing
  -- Substituting with quantifiers.
  , testCase "LinComb empty" $
      run_xyzw (\(x,y,z,w) ->
        unify (LinComb x (name2SoP y) (name2SoP z) (Var w)) (LinComb x (name2SoP y) (name2SoP z) (Var w))
      ) @?= Just mempty
  , testCase "Bound names are not substituted" $
      run_xyzw_abcd (\((x,y,z,w), (a,b,c,d)) ->
        unify (LinComb x (name2SoP y) (name2SoP z) (Var w)) (LinComb a (name2SoP b) (name2SoP c) (Var d))
      ) @?= yzw2bcd
  , testCase "Bound names are renamed" $
      run_xyzw_abcd (\((x,_,_,_), (a,b,c,d)) ->
        unify (Var x) (LinComb a (name2SoP b) (name2SoP c) (Var d))
      ) @?= Just (s x renamed_lin_comb)
  -- TODO This test shouldn't be allowed since we assume VNames in first argument are holes?
  -- , testCase "Substitute only some symbols" $
  --     run_xyzw (\(x,y,z,_) ->
  --       unify (name2SoP x .*. name2SoP y .*. name2SoP w) (name2SoP z .*. name2SoP w .*. name2SoP w)
  --     ) @?= x2z_y2w
  ]
  where
    name2SoP = sym2SoP . Var
    s = M.singleton

    xyzwM =
      (,,,) <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
    (x,y,z,w) = getValue xyzwM
    abcdM =
      (,,,) <$> newVName "a" <*> newVName "b" <*> newVName "c" <*> newVName "d"
    (_,b,c,d) = getValue (xyzwM >> abcdM)

    x2z_y2w = Just (s x (name2SoP z) <> s y (name2SoP w))
    x2w_y2z = Just (s x (name2SoP w) <> s y (name2SoP z))
    yzw2bcd = Just (s y (name2SoP b) <> s z (name2SoP c) <> s w (name2SoP d))

    renamed_lin_comb = getValue $ do
          _ <- xyzwM
          (a,b,c,d) <- abcdM
          _ <- newVName "k" -- Simulate "k" introduced by Unify.
          rename $ sym2SoP (LinComb a (name2SoP b) (name2SoP c) (Var d))

    run_xyzw f = runTest (xyzwM >>= f)
    run_xyzw_abcd f = runTest ((,) <$> xyzwM <*> abcdM >>= f)
