module Futhark.Analysis.Proofs.AlgebraPC.SolveTests (tests) where

import Control.Monad (unless, forM)
import Data.Set qualified as S
import Data.Map qualified as M
import Futhark.MonadFreshNames
import Futhark.SoP.Monad (addEquiv, addRange, mkRange, mkRangeLB, AlgEnv (..), UntransEnv (..), mkRangeUB)
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.*.), (.+.), (.-.), Range(..))
import Futhark.Util.Pretty (docString, line, pretty, (<+>))
import Futhark.Analysis.Proofs.AlgebraPC.Symbol
import Futhark.Analysis.Proofs.AlgebraPC.Solve
import Test.Tasty
import Test.Tasty.HUnit
import qualified Futhark.Analysis.Proofs.Symbol as IxFn
import Futhark.Analysis.Proofs.Monad (IndexFnM(..), VEnv(..))
import Control.Monad.RWS.Strict hiding (Sum)
import Futhark.SoP.FourierMotzkin qualified as FM

import Futhark.Util.Pretty
import Debug.Trace
-------------------------------------
-- Run with:
--  $ cabal test --test-show-details=always  --test-option="--pattern=Proofs.AlgebraPC.SolveTests"
-------------------------------------

runAlgM :: IndexFnM a -> AlgEnv Symbol IxFn.Symbol Property -> VNameSource -> (a, VEnv)
runAlgM (IndexFnM m) env vns = getRes $ runRWS m () s
  where
    getRes (x, env1, _) = (x, env1)
    s = VEnv vns env mempty mempty False

runTest :: IndexFnM a -> AlgEnv Symbol IxFn.Symbol Property -> a
runTest test env = fst $ runAlgM test env blankNameSource

num_tests_pow :: Integer
num_tests_pow = 1 -- 100000

num_tests_sum_sub :: Integer
num_tests_sum_sub = 1 -- 1000


tests :: TestTree
tests =
  testGroup
    "Proofs.AlgebraPC.SolveTests"
    [
      testCase "Pow Exact Normaliation" $
        run
          ( forM [1..num_tests_pow] $ \ kk -> do
              let xy_p_3x= sVar x .*. (sVar y .+. int 3)
                  pow_xy = int 2 .*. sVar i1 .*. pow2 xy_p_3x
                  pow_z  = int 8 .*. sVar i2 .*. pow2 (sVar z .*. int 2)
              simplifyLevel $ pow_xy .*. pow_z .+. int kk
          )
          @??= map (\ kk ->
                       let expo = sVar x .*. sVar y .+. int 3 .*. sVar x .+. int 2 .*. sVar z .+. int 4
                       in  sVar i1 .*. sVar i2 .*. pow2 expo .+. int kk
                   ) [1..num_tests_pow],
      testCase "Pow2 precise simplification from FFT" $
        run
          ( do
              let sop_tmp = (pow2 (sVar n .-. sVar q) .-. int 1)  .*. pow2 (sVar q) .+. pow2 (sVar q .-. int 1) .-. pow2 (sVar n)
              simplify sop_tmp
          )
          @??= ( pow2 (sVar q .-. int 1) .-. pow2 (sVar q)
               ),
      testCase "FFT bounds-check queries" $
        run
          ( do
              addRange (Var q) $ mkRange (int 1) (sVar n)
              addRange (Var j) $ mkRange (int 0) (pow2 (sVar q .-. int  1) .-. int 1)
              addRange (Var k) $ mkRange (int 0) (pow2 (sVar n .-. sVar q) .-. int 1)
              let kLj = sVar k .*. pow2 (sVar q) .+. sVar j
              succ_lb <- (int 0) FM.$<=$ (kLj)
              succ_ub1 <- (kLj .+. pow2 (sVar q .-. int 1)) FM.$<$ pow2 (sVar n)
              succ_ub2 <- kLj FM.$<$ pow2 (sVar n)
              let rj = sVar j .*. pow2 (sVar n .-. sVar q)
              succ_rj_lb <- rj FM.$>=$ (int 0)
              succ_rj_ub <- rj FM.$<$ pow2 (sVar n .-. int 1)
              pure [succ_lb, succ_ub1, succ_ub2, succ_rj_lb, succ_rj_ub]
          )
          @??= [True, True, True, True, True],
      --
      testCase "Sum subtraction and Peel off (from partition2)" $
        run
          ( do
              addRange (Var n) $ mkRangeLB (int 0)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              let sum1 = sym2SoP $ Sum a (int 0) $ sVar i1
                  sum2 = sym2SoP $ Sum a (sVar i2 .+. int 1) $ sVar n .+. int (-1)
              simplifyLevel $ sum2 .+. sVar i2 .+. int 1 .-. sum1
          )
          @??= ( let sum1 = sym2SoP $ Sum a (int 0) $ sVar i2 .-. int 1
                     sum2 = sym2SoP $ Sum a (sVar i1 .+. int 1) $ sVar n .+. int (-1)
                 in  sum2 .+. sVar i2 .+. int 1 .-. sum1
               ),
      testCase "Unite sums (fromNikolaj)" $
        run
          ( do
              addRange (Var n) $ mkRangeLB (int 0)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              -- addRange (Var z) $ mkRange (int 0) (sVar n)
              addRange (Var y) $ mkRange (int 0) (sVar z)
              let sum1 = sym2SoP $ Sum b (sVar y .+. int 1) $ sVar z
                  idx1 = sym2SoP $ Idx b $ sVar y
                  sum2 = sym2SoP $ Sum a (sVar i2 .+. int 1) $ sVar i1 .-. int 1
                  idx2 = sym2SoP $ Idx a $ sVar i1
              simplifyLevel $ int 1 .+. idx1 .+. sum1 .+. sum2 .+. idx2
          )
          @??= ( let sum1 = sym2SoP $ Sum b (sVar y) $ sVar z
                     sum2 = sym2SoP $ Sum a (sVar i2 .+. int 1) $ sVar i1
                 in  int 1 .+. sum1 .+. sum2
               ),
      testCase "Complex sum subtraction" $
        run
          ( do
              addRange (Var n) $ mkRangeLB (int 1)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              addRange (Var i3) $ Range (S.singleton $ int 2 .*. sVar i1 .-. int 1) 3 mempty
              forM [1..num_tests_sum_sub] $ \i -> do
                let sum1 = sym2SoP $ Sum a (int 2 .*. sVar i2) $ int 3 .*. sVar i3
                    idx1 = sym2SoP $ Idx a $ int 3 .*. sVar i3 .+. int 1
                    sum2 = sym2SoP $ Sum a (int 2 .*. sVar i1) $ int 3 .*. sVar i3 .+. sVar n .-. int 1
                    idx2 = sym2SoP $ Idx a $ int 3 .*. sVar i3 .+. sVar n
                simplifyLevel $ (int 0 .-. idx2) .+. sum1 .-. sum2 .+. idx1 .+. int i
          )
          @??= ( map (\ i->
                       let sum1 = sym2SoP $ Sum a (int 2 .*. sVar i2) $ int 2 .*. sVar i1 .-. int 1
                           sum2 = sym2SoP $ Sum a (int 3 .*. sVar i3 .+. int 2) $ int 3 .*. sVar i3 .+. sVar n
                       in  sum1 .-. sum2 .+. int i
                     ) [1..num_tests_sum_sub]
               ),

      testCase "Parition2: I1 Intersect I2 = empty (querry 1)" $
        run
          ( do
              addRange (Var  n) $ mkRangeLB (int 0)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              addEquiv (Idx c (sVar i1)) (int 1)
              addEquiv (Idx c (sVar i2)) (int 0)
              let sum1 = sym2SoP $ Sum c (sVar i2 .+. int 1) $ sVar n .-. int 1
                  sum2 = sym2SoP $ Sum c (int 0) $ sVar i1
              (sum2 .-. int 1) FM.$<$ (sVar i2 .+. sum1)
          )
          @??= True,
      --
      testCase "Parition2: I1 Intersect I2 = empty (querry 2)" $
        run
          ( do
              addRange (Var  n) $ mkRangeLB (int 0)
              addRange (Var i2) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i1) $ mkRange (int 0) (sVar i2 .-. int 1)
              addEquiv (Idx c (sVar i1)) (int 1)
              addEquiv (Idx c (sVar i2)) (int 0)
              let sum1 = sym2SoP $ Sum c (sVar i2 .+. int 1) $ sVar n .-. int 1
                  sum2 = sym2SoP $ Sum c (int 0) $ sVar i1
              (sum2 .-. int 1) FM.$<$ (sVar i2 .+. sum1)
          )
          @??= True,
      testCase "FME1" $
        run
          ( do
              let idx1 = Idx c (sVar i1)
              let idx2 = Idx c (sVar i2)
              addRange idx1 $ mkRangeUB (int2SoP (-1))
              addRange idx2 $ mkRangeLB (int2SoP 0)
              sym2SoP idx1 FM.$<$ sym2SoP idx2
          )
          @??= True
    ]
  where
    int = int2SoP
    sVar = sym2SoP . Var
    pow2 some_sop = sym2SoP $ Pow (2, some_sop)

    env_empty = AlgEnv { untrans = Unknowns M.empty M.empty
                       , equivs  = M.empty
                       , ranges  = M.empty
                       , properties = M.empty
                       }
    varsM =
      (,,,,,,,,,)
        <$> newVName "x"
        <*> newVName "y"
        <*> newVName "z"
        <*> newVName "i1"
        <*> newVName "i2"
        <*> newVName "i3"
        <*> newVName "n"
        <*> newVName "A"
        <*> newVName "B"
        <*> newVName "C"
    (x, y, z, i1, i2, i3, n, a0, b0, c0) = runTest varsM env_empty
    (a, b, c) = (One a0, One b0, POR (S.singleton c0))

    varsFFT =
      (,,)
        <$> newVName "q"
        <*> newVName "k"
        <*> newVName "j"
    (q,k,j) = runTest varsFFT env_empty

    env = env_empty { equivs = M.singleton (Idx a (sVar i2)) (int 0)
                    , ranges = M.empty
                    }

    run f = runTest f env

    -- Less fragile renaming.
    e @??= e' = do
      let (actual, expected) = (e, e') -- runTest $ renameSame e e'
      unless (actual == expected) (assertFailure $ msg actual expected)
      where
        msg actual expected =
          docString $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual
