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

runTest :: MaybeT TestM Sub -> Maybe Sub
runTest test = evalTestM (runMaybeT test) blankNameSource

getNames :: MaybeT TestM a -> a
getNames test = fromJust $ evalTestM (runMaybeT test) blankNameSource


tests :: TestTree
tests = testGroup "Proofs.UnifyTests"
  [ testCase "Unify (x + y) (z + w)" $
      run4 (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y) (name2SoP z .+. name2SoP w)
      ) @?= x2z_y2w
  , testCase "Unify (2x + y) (z + w)" $
      run4 (\(x,y,z,w) ->
        unify (scaleSoP 2 (name2SoP x) .+. name2SoP y) (name2SoP z .+. name2SoP w)
      ) @?= Nothing
  , testCase "Unify (x + y) (z + 2w)" $
      run4 (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y) (name2SoP z .+. scaleSoP 2 (name2SoP w))
      ) @?= Nothing
  , testCase "Unify (2x + y) (z + 2w)" $
      run4 (\(x,y,z,w) ->
        unify (scaleSoP 2 (name2SoP x) .+. name2SoP y) (scaleSoP 2 (name2SoP z) .+. name2SoP w)
      ) @?= x2z_y2w
  , testCase "Unify (2x + y) (z + 2w)" $
      run4 (\(x,y,z,w) ->
        unify (scaleSoP 2 (name2SoP x) .+. name2SoP y) (name2SoP z .+. scaleSoP 2 (name2SoP w))
      ) @?= x2w_y2z
  , testCase "Unify (x * y) (z + w)" $
      run4 (\(x,y,z,w) ->
        unify (name2SoP x .*. name2SoP y) (name2SoP z .+. name2SoP w)
      ) @?= Nothing
  , testCase "Unify (x * y) (z * w)" $
      run4 (\(x,y,z,w) ->
        unify (name2SoP x .*. name2SoP y) (name2SoP z .*. name2SoP w)
      ) @?= x2z_y2w
  , testCase "Unify (x + y + 2) (z + w)" $
      run4 (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y .+. int2SoP 2) (name2SoP z .+. name2SoP w)
      ) @?= Nothing
  , testCase "Unify (x + y + 2) (z + w + 2)" $
      run4 (\(x,y,z,w) ->
        unify (name2SoP x .+. name2SoP y .+. int2SoP 2) (name2SoP z .+. name2SoP w .+. int2SoP 2)
      ) @?= x2z_y2w
  ]
  where
    name2SoP = sym2SoP . Var
    s = M.singleton
    xyzwM =
      (,,,) <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
    (x,y,z,w) = getNames xyzwM
    x2z_y2w = Just (s x (name2SoP z) <> s y (name2SoP w)) 
    x2w_y2z = Just (s x (name2SoP w) <> s y (name2SoP z)) 
    run4 f = runTest (xyzwM >>= f)
