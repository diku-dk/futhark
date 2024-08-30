{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Futhark.Analysis.Proofs.RulesTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import qualified Futhark.SoP.SoP as SoP
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify
import Futhark.Analysis.Proofs.Rules
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import Futhark.SoP.SoP (sym2SoP, (.+.), scaleSoP, (.*.), int2SoP)
import qualified Data.Map as M
import Data.Maybe (fromJust)


newtype VEnv = VEnv { vnamesource :: VNameSource }

type Exp = SoP.SoP Symbol
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
      run (\(x,y,_,_,_,_,_,_) ->
        rewrite (sop x .+. sop y)
      ) @?= (sop x .+. sop y)
  , testCase "Extend sum lower bound" $
      run (\(x,y,z,w,_,_,_,_) ->
        rewrite (Idx (Var x) (sop y) ~+~ LinComb w (sop y .+. int 1) (sop z) (Var x))
      ) @?= (renamed $ LinComb w (sop y) (sop z) (Var x))
  ]
  where
    int = int2SoP
    sop = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b

    varsM =
      (,,,,,,,) <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
                <*> newVName "a" <*> newVName "b" <*> newVName "c" <*> newVName "d"
    (x,y,z,w,a,b,c,_) = getValue varsM

    renamed x = getValue $ do
          _ <- varsM
          rename . sym2SoP $ x
    run f = runTest (varsM >>= f)
