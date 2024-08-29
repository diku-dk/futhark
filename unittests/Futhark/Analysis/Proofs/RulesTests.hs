{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Futhark.Analysis.Proofs.RulesTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import qualified Futhark.SoP.SoP as SoP
import Futhark.Analysis.Proofs.Term
import Futhark.Analysis.Proofs.Unify
import Futhark.Analysis.Proofs.Rules
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

runTest :: TestM a -> a
runTest test = evalTestM test blankNameSource

getValue :: TestM a -> a
getValue m = evalTestM m blankNameSource


tests :: TestTree
tests = testGroup "Proofs.Rules"
  [ testCase "Add" $
      run_xyzw (\(x,y,z,w) ->
        rewrite sopRules (name2SoP x .+. name2SoP y)
      ) @?= (name2SoP x .+. name2SoP y)
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
