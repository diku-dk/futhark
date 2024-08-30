{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Proofs.RulesTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify
import Futhark.Analysis.Proofs.Rules
import Control.Monad.RWS.Strict
import Futhark.SoP.SoP (sym2SoP, (.+.), int2SoP, (.-.))
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (..))
import qualified Language.Futhark as E
import Debug.Trace (traceM)
import Futhark.Util.Pretty
import qualified Data.Set as S
import qualified Futhark.SoP.SoP as SoP
import Futhark.SoP.Monad (addRange)


data VEnv = VEnv {
    vnamesource :: VNameSource,
    algenv :: AlgEnv Symbol E.Exp
  }

newtype TestM a = TestM (RWS () () VEnv a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames,
      MonadState VEnv
    )

instance (Monoid w) => MonadFreshNames (RWS r w VEnv) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

instance Nameable Symbol where
  mkName (VNameSource i) = (Var $ E.VName "x" i, VNameSource $ i + 1)

instance MonadSoP Symbol E.Exp TestM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

evalTestM :: TestM a -> VNameSource -> a
evalTestM (TestM m) vns = fst $ evalRWS m () s
  where
    s = VEnv vns mempty

runTest :: TestM a -> a
runTest test = evalTestM test blankNameSource

getValue :: TestM a -> a
getValue m = evalTestM m blankNameSource


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
  ]
  where
    int = int2SoP
    sop = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b
    a ~-~ b = sym2SoP a .-. sym2SoP b

    varsM =
      (,,,,,,,) <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
                <*> newVName "a" <*> newVName "b" <*> newVName "c" <*> newVName "d"
    (x,y,z,w,a,b,c,_) = getValue varsM

    run f = runTest (varsM >>= f)

    -- Less fragile renaming.
    e @??= e' = renamed e @?= renamed e'
    renamed x = getValue $ do
          putNameSource (newNameSource (-10000))
          rename x
