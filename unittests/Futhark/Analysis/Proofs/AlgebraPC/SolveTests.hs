module Futhark.Analysis.Proofs.AlgebraPC.SolveTests (tests) where

import Control.Monad (unless, forM)
import Data.Set qualified as S
import Data.Map qualified as M
import Futhark.MonadFreshNames
import Futhark.SoP.Monad (addEquiv, addRange, mkRange, mkRangeLB, AlgEnv (..), UntransEnv (..), mkRangeUB, addProperty)
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.*.), (.+.), (.-.), Range(..), (~+~), negSoP)
import Futhark.Util.Pretty (docString, line, pretty, (<+>))
import Futhark.Analysis.Proofs.AlgebraPC.Symbol
import Futhark.Analysis.Proofs.AlgebraPC.Solve
import Test.Tasty
import Test.Tasty.HUnit
import qualified Futhark.Analysis.Proofs.Symbol as IxFn
import Futhark.Analysis.Proofs.Monad (IndexFnM(..), VEnv(..), debugPrintAlgEnv, debugOn, debugPrettyM, clearAlgEnv)
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
      testCase "Partition2 variant" $
        run
          ( do
              addRange (Var  n) $ mkRangeLB (int 0)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              addEquiv (Idx c (sVar i1)) (int 1)
              addEquiv (Idx c (sVar i2)) (int 0)
              let sum1 = sym2SoP $ Sum c (sVar i2 .+. int 1) $ sVar n .-. int 1
                  sum2 = sym2SoP $ Sum c (int 0) $ sVar i1 .-. int 1
              sum2 FM.$<$ (sVar i2 .+. sum1)
          )
          @??= True,
      testCase "Partition3 within bounds: case2 < n" $
        run
          ( do
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              let pc = POR $ S.singleton c0 -- conds[i] == 1
              let pd = POR $ S.singleton d0 -- conds[i] == 2
              -- We are in the case for conds[i] == 2.
              addEquiv (Idx pd (sVar i1)) (int 1)
              -- Add: idx1 <=> not idx2
              addProperty (Var c0) (Disjoint set_de)
              addProperty (Var d0) (Disjoint set_ce)
              let sum1 = Sum pc (int 0) (sVar n .-. int 1)
                  sum2 = Sum pd (int 0) (sVar i1 .-. int 1)
              (sum1 ~+~ sum2) FM.$<$ sVar n
          )
          @??= True,
      testCase "Partition3 within bounds: case3 < n" $
        run
          ( do
              -- n >  i₁₈₀₉₀ + ∑j₁₈₀₀₂∈(i₁₈₀₉₀ .. -1 + n₄₅₄₃) (conds₄₅₄₅[j₁₈₀₀₂] = 1) + ∑j₁₈₀₀₂∈(i₁₈₀₉₀ .. -1 + n₄₅₄₃) (conds₄₅₄₅[j₁₈₀₀₂] = 2)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              let pc = POR $ S.singleton c0 -- conds[i] == 1
              let pd = POR $ S.singleton d0 -- conds[i] == 2
              -- We are in the case for conds[i] /= 1 ^ conds[i] /= 2.
              addEquiv (Idx pd (sVar i1)) (int 0)
              addEquiv (Idx pc (sVar i1)) (int 0)
              addProperty (Var c0) (Disjoint set_de)
              addProperty (Var d0) (Disjoint set_ce)
              let sum1 = Sum pc (sVar i1) (sVar n .-. int 1)
                  sum2 = Sum pd (sVar i1) (sVar n .-. int 1)
              (sVar i1 .+. (sum1 ~+~ sum2)) FM.$<$ sVar n
          )
          @??= True,
      testCase "Partition3 within bounds: case >= 0" $
        run
          ( do
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              let pc = POR $ S.singleton c0 -- conds[i] == 1
              let pd = POR $ S.singleton d0 -- conds[i] == 2
              -- We are in the case for conds[i] == 2.
              addEquiv (Idx pd (sVar i1)) (int 1)
              -- Add: idx1 <=> not idx2
              addProperty (Var c0) (Disjoint set_de)
              addProperty (Var d0) (Disjoint set_ce)
              let sum1 = Sum pc (int 0) (sVar n .-. int 1)
                  sum2 = Sum pd (int 0) (sVar i1 .-. int 1)
              int 0 FM.$<=$ (sum1 ~+~ sum2)
          )
          @??= True,
      testCase "Partition3 branch comparison" $
        run
          ( do
              -- 0 <= i2 < i1 < n
              addRange (Var  n) $ mkRangeLB (int 1)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              -- Assume predicates of cases:
              let pc = POR $ S.singleton c0 -- conds[i1] == 1
              let pd = POR $ S.singleton d0 -- conds[i2] == 2
              addEquiv (Idx pc (sVar i1)) (int 1)
              addEquiv (Idx pd (sVar i2)) (int 1)
              -- Predicates are disjoint.
              addProperty (Var c0) (Disjoint set_de)
              addProperty (Var d0) (Disjoint set_ce)
              let sum1 = Sum pc (int 0)
                  sum2 = Sum pd (int 0) (sVar i2 .-. int 1)
              sym2SoP (sum1 (sVar i1 .-. int 1)) FM.$<$ (sum1 (sVar n .-. int 1) ~+~ sum2)
          )
          @??= True,
      testCase "Partition3 branch comparison (new form)" $
        run
          ( do
              debugOn
              -- 0 <= i2 < i1 < n
              addRange (Var  n) $ mkRangeLB (int 1)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              -- Assume predicates of cases:
              let pc = POR $ S.singleton c0 -- conds[i1] == 1
              let pd = POR $ S.singleton d0 -- conds[i2] == 2
              addEquiv (Idx pc (sVar i1)) (int 1)
              addEquiv (Idx pd (sVar i2)) (int 1)
              -- Due to pairwise disjointedness, we also know:
              addEquiv (Idx pc (sVar i2)) (int 0)
              addEquiv (Idx pd (sVar i1)) (int 0)
              -- Predicates are disjoint.
              addProperty (Var c0) (Disjoint set_de)
              addProperty (Var d0) (Disjoint set_ce)
              let sum1_to = Sum pc (int 0)
                  sum1_from from = Sum pc from (sVar n .-. int 1)
                  sum2 = Sum pd (int 0) (sVar i2 .-. int 1)
              sym2SoP (sum1_to (sVar i1 .-. int 1)) FM.$<$ (sum1_to (sVar i2 .-. int 1) ~+~ sum1_from (sVar i2 .+. int 1) .+. sym2SoP sum2)
          )
          @??= True,
      testCase "Sum c[0:i-1] < Sum c[0:n-1]  where  c is POR (boolean)" $
        run
          ( do
              -- 0 <= i1 < n
              addRange (Var  n) $ mkRangeLB (int 1)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              let pc = POR $ S.singleton c0
              let sum1 = sym2SoP . Sum pc (int 0)
              sum1 (sVar i1 .-. int 1) FM.$<=$ sum1 (sVar n .-. int 1)
          )
          @??= True,
      testCase "Monotonicity" $
        run
          ( do
              -- 0 <= i2 < i1 < n
              addRange (Var  n) $ mkRangeLB (int 1)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              addRange (Var c0) $ mkRangeLB (int 0)
              let sum0to = sym2SoP . Sum (One c0) (int 0)
              debugPrettyM "" (sum0to (sVar i2 .-. int 1), sum0to (sVar i1 .-. int 1))
              sum0to (sVar i2 .-. int 1) FM.$<=$ sum0to (sVar i1 .-. int 1)
          )
          @??= True,
      testCase "Sum a[0:i2] - Sum a[0:i1] = -1 * Sum a[i2+1:i1] where 0 <= i2 < i1 < n and a >= 0" $
        run
          ( do
              -- 0 <= i2 < i1 < n
              addRange (Var  n) $ mkRangeLB (int 1)
              addRange (Var i1) $ mkRange (int 1) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              addRange (Var c0) $ mkRangeLB (int 0)
              let sum0to = sym2SoP . Sum (One c0) (int 0)
              simplify $ sum0to (sVar i2) .-. sum0to (sVar i1)
          )
          @??= negSoP (sym2SoP (Sum (One c0) (sVar i2 .+. int2SoP 1) (sVar i1))),
      testCase "Sum a[0:i2-1] - Sum a[0:i1-1] = -1 * Sum a[i2:i1-1] where 0 <= i2 < i1 < n and a >= 0" $
        run
          ( do
              -- 0 <= i2 < i1 < n
              addRange (Var  n) $ mkRangeLB (int 1)
              addRange (Var i1) $ mkRange (int 1) (sVar n .-. int 1)
              addRange (Var i2) $ mkRange (int 0) (sVar i1 .-. int 1)
              addRange (Var c0) $ mkRangeLB (int 0)
              let sum0to = sym2SoP . Sum (One c0) (int 0)
              simplify $ sum0to (sVar i2 .-. int 1) .-. sum0to (sVar i1 .-. int 1)
          )
          @??= negSoP (sym2SoP (Sum (One c0) (sVar i2) (sVar i1 .-. int 1))),
      testCase "Merge split-sum" $
        run
          ( do
              -- addRange (Var  n) $ mkRangeLB (int 0)
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              let c_sum1 = sym2SoP $ Sum (One c0) (int 0) (sVar i1)
              let c_sum2 = sym2SoP $ Sum (One c0) (sVar i1 .+. int 1) (sVar n .-. int 1)
              simplify $ c_sum1 .+. c_sum2
          )
          @??= sym2SoP (Sum (One c0) (int 0) (sVar n .-. int 1)),
      testCase "Sum no-op" $
        run
          ( do
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              let c_sum = sym2SoP $ Sum (One c0) (int 0) (sVar n .-. int 1)
              simplify c_sum
          )
          @??= sym2SoP (Sum (One c0) (int 0) (sVar n .-. int 1)),
      testCase "Peel off one" $
        run
          ( do
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              let pd = POR $ S.singleton d0
              addEquiv (Idx pd $ sVar i1) (int 1)
              let d_sum = sym2SoP $ Sum pd (int 0) (sVar i1)
              simplify $ d_sum .-. int 1
          )
          @??= sym2SoP (Sum (POR $ S.singleton d0) (int 0) (sVar i1 .-. int 1)),
      testCase "Unite sums spicy (part2indicesL)" $
        -- y₃₈₃₂₇[∑x₃₃₃₂₉[0 : -1 + k₃₈₂₇₉]] + ∑y₃₈₃₂₇[1 + ∑x₃₃₃₂₉[0 : -1 + k₃₈₂₇₉] : i₈₇₄₃]
        let a_sum = sym2SoP $ Sum a (int 0) (sVar i1 .-. int 1) -- a = x₃₃₃₂₉, i1 = k₃₈₂₇₉
        in run
          ( do
              clearAlgEnv
              let b_sum = sym2SoP $ Sum b (int 1 .+. a_sum) (sVar i2) -- i2 = i₈₇₄₃
              addRange (Var a0) $ mkRangeLB (int2SoP 0) -- k₃₈₂₇₉
              -- max{0} <= i₈₇₄₃
              addRange (Var i1) $ mkRangeLB (int2SoP 0)
              -- max{0, ∑x₃₃₃₂₉[0 : -1 + k₃₈₂₇₉]} <= i₈₇₄₃
              addRange (Var i2) $ mkRangeLB (int2SoP 0)
              addRange (Var i2) $ mkRangeLB a_sum
              simplify $ sym2SoP (Idx b a_sum) .+. b_sum
          )
          @??= sym2SoP (Sum b a_sum (sVar i2)),
      testCase "Monotonicity (from part2indicesL)" $
        -- j < i, shp >= 0, c[i] = 1, c[j] = 1
          -- => ∑⟦cª₄₉₈₉₅⟧[∑shpª₃₃₇₈₀[0 : -1 + k₄₉₈₄₈] : i₉₆₆₄]
        --        > ∑⟦cª₄₉₈₉₅⟧[∑shpª₃₃₇₈₀[0 : -1 + k₄₉₈₄₈] : j₅₀₁₃₁]
        run
          ( do
              clearAlgEnv
              i <- newNameFromString "i"
              k <- newNameFromString "k"
              j <- newNameFromString "j"
              m <- newNameFromString "m"
              vn_shp <- newNameFromString "shp"
              let shp = One vn_shp
              -- \s -> ∑shpª₃₃₇₈₀[0 : -1 + s]
              let shp_sum' s = sym2SoP $ Sum shp (int 0) (s .-. int 1)
              let shp_sum = shp_sum' . sVar
              -- max{1} <= m₄₆₇₈
              addRange (Var m) $ mkRangeLB (int 1)
              -- max{0} <= shapeª₃₃₇₈₀ <= min{}
              addRange (Var vn_shp) $ mkRangeLB (int 0)
              -- max{0} <= k₄₉₈₄₈ <= min{-1 + m₄₆₇₈}
              addRange (Var k) $ mkRange (int 0) (sVar m .-. int 1)
              -- max{0, 1, ∑shapeª₃₃₇₈₀[0 : -1 + k₄₉₈₄₈]}
              --   <= i₉₆₆₄
              --   <= min{-1 + ∑shapeª₃₃₇₈₀[0 : -1 + m₄₆₇₈], -1 + ∑shapeª₃₃₇₈₀[0 : k₄₉₈₄₈]}
              addRange (Var i) $ mkRange (int 1) (shp_sum m .-. int 1)
              addRange (Var i) $ mkRange (shp_sum k) (shp_sum' (sVar k .+. int 1) .-. int 1)
              -- max{0} <= j₅₀₁₃₁ <= min{-1 + i₉₆₆₄}
              addRange (Var j) $ mkRange (int 0) (sVar i .-. int 1)
              -- c is disjoint with some other predicate d.
              addProperty (Var c0) (Disjoint $ S.singleton d0)
              addProperty (Var c0) Boolean
              addRange (Var c0) $ mkRange (int 0) (int 1)
              -- Add equivalences.
              addEquiv (Idx c (sVar i)) (int 1)
              addEquiv (Idx c (sVar j)) (int 1)
              -- \s -> ∑⟦cª₄₉₈₉₅⟧[∑shpª₃₃₇₈₀[0 : -1 + k₄₉₈₄₈] : s]
              let c_sum s = sym2SoP $ Sum c (shp_sum k) (sVar s)
              c_sum i FM.$>$ c_sum j
          )
          @??= True,
      testCase "Monotonicity 2 (from part2indicesL)" $
        -- j < i, shp >= 0, c[i] = 0, c[j] = 0
        -- =>  i₉₆₆₄ + ∑⟦cª₄₉₈₉₅⟧[1 + i₉₆₆₄ : -1 + ∑shapeª₃₃₇₈₀[0 : k₄₉₈₄₈]]
        --       > j₅₀₂₈₂ + ∑⟦cª₄₉₈₉₅⟧[1 + j₅₀₂₈₂ : -1 + ∑shapeª₃₃₇₈₀[0 : k₄₉₈₄₈]]
        --
        --   Proof:
        --     i₉₆₆₄
        --       + ∑⟦cª₄₉₈₉₅⟧[1 + i₉₆₆₄ : -1 + ∑shapeª₃₃₇₈₀[0 : k₄₉₈₄₈]]
        --       - ∑⟦cª₄₉₈₉₅⟧[1 + j₅₀₂₈₂ : -1 + ∑shapeª₃₃₇₈₀[0 : k₄₉₈₄₈]]
        --       > j₅₀₂₈₂
        --
        --   Both sums satisfy "empty by atmost -1" given ranges on i₉₆₆₄ and j₅₀₂₈₂.
        --     i₉₆₆₄ - ∑⟦cª₄₉₈₉₅⟧[1 + j₅₀₂₈₂ : i₉₆₆₄]
        --       > j₅₀₂₈₂
        --
        --   Replace sum by its upper bound to minimize LHS:
        --     i₉₆₆₄ - (i₉₆₆₄ - 1 + j₅₀₂₈₂) > j₅₀₂₈₂
        --
        --     1 + j₅₀₂₈₂ > j₅₀₂₈₂
        run
          ( do
              clearAlgEnv
              i <- newNameFromString "i"
              k <- newNameFromString "k"
              j <- newNameFromString "j"
              m <- newNameFromString "m"
              vn_shp <- newNameFromString "shp"
              let shp = One vn_shp
              -- \s -> ∑shpª₃₃₇₈₀[0 : -1 + s]
              let shp_sum' s = sym2SoP $ Sum shp (int 0) (s .-. int 1)
              let shp_sum = shp_sum' . sVar
              -- max{1} <= m₄₆₇₈
              addRange (Var m) $ mkRangeLB (int 1)
              -- max{0} <= shapeª₃₃₇₈₀ <= min{}
              addRange (Var vn_shp) $ mkRangeLB (int 0)
              -- max{0} <= k₄₉₈₄₈ <= min{-1 + m₄₆₇₈}
              addRange (Var k) $ mkRange (int 0) (sVar m .-. int 1)
              -- max{0, 1, ∑shapeª₃₃₇₈₀[0 : -1 + k₄₉₈₄₈]}
              --   <= i₉₆₆₄
              --   <= min{-1 + ∑shapeª₃₃₇₈₀[0 : -1 + m₄₆₇₈], -1 + ∑shapeª₃₃₇₈₀[0 : k₄₉₈₄₈]}
              addRange (Var i) $ mkRange (int 1) (shp_sum m .-. int 1)
              addRange (Var i) $ mkRange (shp_sum k) (shp_sum' (sVar k .+. int 1) .-. int 1)
              -- max{0} <= j₅₀₁₃₁ <= min{-1 + i₉₆₆₄}
              addRange (Var j) $ mkRange (int 0) (sVar i .-. int 1)
              -- c is disjoint with some other predicate d.
              addProperty (Var c0) (Disjoint $ S.singleton d0)
              addProperty (Var c0) Boolean
              addRange (Var c0) $ mkRange (int 0) (int 1)
              -- Add equivalences.
              addEquiv (Idx c (sVar i)) (int 0)
              addEquiv (Idx c (sVar j)) (int 0)
              -- \idx -> ∑⟦cª₄₉₈₉₅⟧[1 + idx : ∑shpª₃₃₇₈₀[0 : k₄₉₈₄₈] - 1]
              let c_sum s = sym2SoP $ Sum c (sVar s .+. int 1) (shp_sum' (sVar k .+. int 1) .-. int 1)
              (sVar i .+. c_sum i) FM.$>$ (sVar j .+. c_sum j)
          )
          @??= True,
      testCase "In-bounds (from part2indicesL)" $
        -- ∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈]
        --     ≤ -1 + ∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈] + ∑⟦cª₄₉₈₉₅⟧[∑shape₄₆₇₉[0 : -1 + k₁] : i₄₉₉₁₂]
        --
        -- 1. Sum over cª₄₉₈₉₅ is non-empty due to bounds on i₄₉₉₁₂.
        -- 2. We have cª₄₉₈₉₅[i₄₉₉₁₂] = 1.
        -- 3. Hence -1 cancels out by peeling the sum.
        run
          ( do
              clearAlgEnv
              i <- newNameFromString "i"
              k <- newNameFromString "k"
              m <- newNameFromString "m"
              vn_shp <- newNameFromString "shp"
              let shp = One vn_shp
              -- \s -> ∑shpª₃₃₇₈₀[0 : -1 + s]
              let shp_sum' s = sym2SoP $ Sum shp (int 0) (s .-. int 1)
              let shp_sum = shp_sum' . sVar
              -- max{1} <= m₄₆₇₈
              addRange (Var m) $ mkRangeLB (int 1)
              -- max{0} <= shapeª₃₃₇₈₀ <= min{}
              addRange (Var vn_shp) $ mkRangeLB (int 0)
              -- max{0} <= k₄₉₈₄₈ <= min{-1 + m₄₆₇₈}
              addRange (Var k) $ mkRange (int 0) (sVar m .-. int 1)
              -- max{0} <= cª₄₉₈₉₅ <= min{1}
              addRange (Var c0) $ mkRange (int 0) (int 1)
              -- max{0, ∑shapeª₃₃₇₈₀[0 : -1 + k₄₉₈₄₈]}
              --   <= i₉₆₆₄
              --   <= min{-1 + ∑shapeª₃₃₇₈₀[0 : -1 + m₄₆₇₈], -1 + ∑shapeª₃₃₇₈₀[0 : k₄₉₈₄₈]}
              addRange (Var i) $ mkRange (int 0) (shp_sum m .-. int 1)
              addRange (Var i) $ mkRange (shp_sum k) (shp_sum' (sVar k .+. int 1) .-. int 1)
              -- c is disjoint with some other predicate d.
              addProperty (Var c0) (Disjoint $ S.singleton d0)
              addProperty (Var c0) Boolean
              -- Add equivalences.
              addEquiv (Idx c (sVar i)) (int 1)
              -- \idx -> ∑⟦cª₄₉₈₉₅⟧[∑shpª₃₃₇₈₀[0 : k₄₉₈₄₈ - 1] : idx]
              let c_sum s = sym2SoP $ Sum c (shp_sum k) (sVar s)
              shp_sum k FM.$<=$ (int (-1) .+. shp_sum k .+. c_sum i)
          )
          @??= True,
      --
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
      (,,,,,,,,,,,)
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
        <*> newVName "D"
        <*> newVName "E"
    (x, y, z, i1, i2, i3, n, a0, b0, c0, d0, e0) = runTest varsM env_empty
    (a, b, c, d, e) = (One a0, One b0, POR (S.singleton c0), POR (S.singleton d0), POR (S.singleton e0))

    set_de = S.insert d0 $ S.singleton e0
    set_ce = S.insert c0 $ S.singleton e0

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
