module Futhark.Analysis.Properties.AlgebraPC.TempTests (tests) where

import Control.Monad (unless)
import Data.Set qualified as S
import Futhark.MonadFreshNames
import Futhark.SoP.Monad (addEquiv, addRange, mkRange, addProperty, mkRangeLB)
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.+.), (.-.))
import Futhark.Analysis.Properties.AlgebraPC.Symbol
import Test.Tasty
import Test.Tasty.HUnit
import Futhark.Analysis.Properties.Monad (IndexFnM, runIndexFnM, clearAlgEnv)
import Futhark.SoP.FourierMotzkin qualified as FM

import Futhark.Util.Pretty
import Futhark.Analysis.Properties.Property
-------------------------------------
-- Run with:
--  $ cabal test --test-show-details=always --test-option="--pattern=Proofs.AlgebraPC.TempTests"
-------------------------------------

runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

num_tests_pow :: Integer
num_tests_pow = 1 -- 100000

num_tests_sum_sub :: Integer
num_tests_sum_sub = 1 -- 1000


tests :: TestTree
tests =
  testGroup
    "Properties.AlgebraPC.TempTests"
    [
{--
      --
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
--}
      --
      testCase "In-bounds 2 (from part2indicesL)" $
        -- -1 + ∑shape₄₆₇₉[0 : k₄₉₈₄₈]
        --     > -1 + ∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈] + ∑⟦cª₄₉₈₉₅⟧[∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈] : i₄₉₉₁₂]
        --
        -- Proof:
        -- ∑shape₄₆₇₉[0 : k₄₉₈₄₈] - ∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈]
        --     > ∑⟦cª₄₉₈₉₅⟧[∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈] : i₄₉₉₁₂]
        --
        -- shape₄₆₇₉[k₄₉₈₄₈]
        --     > ∑⟦cª₄₉₈₉₅⟧[∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈] : i₄₉₉₁₂]
        --
        -- Maximising sum (using minimum of upper bounds):
        -- shape₄₆₇₉[k₄₉₈₄₈]
        --     > ∑⟦cª₄₉₈₉₅⟧[∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈] : -1 + ∑shape₄₆₇₉[0 : k₄₉₈₄₈]]
        --
        -- Maximising RHS:
        -- shape₄₆₇₉[k₄₉₈₄₈] > (-1 + ∑shape₄₆₇₉[0 : k₄₉₈₄₈]) - ∑shape₄₆₇₉[0 : -1 + k₄₉₈₄₈]
        --
        -- shape₄₆₇₉[k₄₉₈₄₈] > -1 + ∑shape₄₆₇₉[k₄₉₈₄₈ : k₄₉₈₄₈]
        --
        -- shape₄₆₇₉[k₄₉₈₄₈] > -1 + ∑shape₄₆₇₉[k₄₉₈₄₈]
        run
          ( do
              clearAlgEnv
              i <- newNameFromString "i"
              k <- newNameFromString "k"
              m <- newNameFromString "m"
              vn_shp <- newNameFromString "shp"
              let shp = One vn_shp
              -- \s -> ∑shpª₃₃₇₈₀[0 : s]
              let shp_sum s = sym2SoP $ Sum shp (int 0) s
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
              addRange (Var i) $ mkRange (int 0) (shp_sum (sVar m .-. int 1) .-. int 1)
              addRange (Var i) $ mkRange (shp_sum (sVar k .-. int 1)) (shp_sum (sVar k) .-. int 1)
              -- c is disjoint with some other predicate d.
              addProperty (Var c0) (Disjoint $ S.singleton d0)
              addProperty (Var c0) Boolean
              -- Add equivalences.
              addEquiv (Idx c (sVar i)) (int 1)
              -- \idx -> ∑⟦cª₄₉₈₉₅⟧[∑shpª₃₃₇₈₀[0 : k₄₉₈₄₈ - 1] : idx]
              let c_sum s = sym2SoP $ Sum c (shp_sum (sVar k .-. int 1)) (sVar s)
              (int (-1) .+. shp_sum (sVar k)) FM.$>=$ (int (-1) .+. shp_sum (sVar k .-. int 1) .+. c_sum i)
          )
          @??= True
    ]
  where
    int = int2SoP
    sVar = sym2SoP . Var
    pow2 some_sop = sym2SoP $ Pow (2, some_sop)

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
    (x, y, z, i1, i2, i3, n, a0, b0, c0, d0, e0) = runTest varsM
    (a, b, c, d, e) = (One a0, One b0, POR (S.singleton c0), POR (S.singleton d0), POR (S.singleton e0))

    set_de = S.insert d0 $ S.singleton e0
    set_ce = S.insert c0 $ S.singleton e0

    varsFFT =
      (,,)
        <$> newVName "q"
        <*> newVName "k"
        <*> newVName "j"
    (q,k,j) = runTest varsFFT

    run = runTest

    -- Less fragile renaming.
    e @??= e' = do
      let (actual, expected) = (e, e') -- runTest $ renameSame e e'
      unless (actual == expected) (assertFailure $ msg actual expected)
      where
        msg actual expected =
          docString $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual
