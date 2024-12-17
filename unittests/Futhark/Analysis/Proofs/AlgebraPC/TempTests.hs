module Futhark.Analysis.Proofs.AlgebraPC.TempTests (tests) where

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
--  $ cabal test --test-show-details=always --test-option="--pattern=Proofs.AlgebraPC.TempTests"
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
    "Proofs.AlgebraPC.TempTests"
    [
      testCase "Partition3 within bounds: case3 < n" $
        run
          ( do
              -- n >  i₁₈₀₉₀ + ∑j₁₈₀₀₂∈(i₁₈₀₉₀ .. -1 + n₄₅₄₃) (conds₄₅₄₅[j₁₈₀₀₂] = 1) + ∑j₁₈₀₀₂∈(i₁₈₀₉₀ .. -1 + n₄₅₄₃) (conds₄₅₄₅[j₁₈₀₀₂] = 2)
              -- Untranslatable: []
              -- Equivalences: [(A₇[i2₄], 0), (⟦C₉⟧[i1₃], 0), (⟦D₁₀⟧[i1₃], 0)]
              -- Ranges: max{0} <= i1₃ <= min{-1 + n₆}
              -- Properties: [(C₉, {Disjoint (D₁₀, E₁₁)}), (D₁₀, {Disjoint (C₉, E₁₁)})]
              -- 
              addRange (Var i1) $ mkRange (int 0) (sVar n .-. int 1)
              let pc = POR $ S.singleton c0 -- conds[i] == 1
              let pd = POR $ S.singleton d0 -- conds[i] == 2
              let pe = POR $ S.singleton e0 -- conds[i] /= 1 ^ conds[i] /= 2
              -- We are in the case for conds[i] /= 1 ^ conds[i] /= 2.
              addEquiv (Idx pd (sVar i1)) (int 0)
              addEquiv (Idx pc (sVar i1)) (int 0)
              addEquiv (Idx pe (sVar i1)) (int 1)
              addProperty (Var c0) (Disjoint set_de)
              addProperty (Var d0) (Disjoint set_ce)
              debugOn
              debugPrintAlgEnv
              let sum1 = Sum pc (sVar i1) (sVar n .-. int 1)
                  sum2 = Sum pd (sVar i1) (sVar n .-. int 1)
              z <- simplify $ sVar i1 .+. (sum1 ~+~ sum2) .-. sVar n
              debugPrettyM "simplify" z
              sym2elim <- findSymbolLEq0 z
              debugPrettyM "sym2Elim: " sym2elim
              (sVar i1 .+. (sum1 ~+~ sum2)) FM.$<$ sVar n
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
