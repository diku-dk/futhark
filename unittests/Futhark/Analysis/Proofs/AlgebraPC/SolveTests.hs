module Futhark.Analysis.Proofs.AlgebraPC.SolveTests (tests) where

import Control.Monad (unless, forM)
import Data.Set qualified as S
import Data.Map qualified as M
import Language.Futhark (VName)
import Futhark.Analysis.PrimExp hiding (Pow)
--import Futhark.Analysis.Proofs.IndexFn
--import Futhark.Analysis.Proofs.Rewrite
--import Futhark.Analysis.Proofs.Symbol
--import Futhark.Analysis.Proofs.Unify
import Futhark.MonadFreshNames
import Futhark.SoP.Monad -- (addEquiv, addRange)
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.*.), (.+.), (.-.), Range(..))
import Futhark.Util.Pretty (docString, line, pretty, (<+>))
import Futhark.Analysis.Proofs.AlgebraPC.Algebra
import Futhark.Analysis.Proofs.AlgebraPC.Solve
import Test.Tasty
import Test.Tasty.HUnit

-------------------------------------
-- Run with:
--  $ cabal test --test-show-details=always  --test-option="--pattern=Proofs.AlgebraPC.SolveTests"
-------------------------------------

-- runTest :: IndexFnM a -> a
-- runTest test = fst $ runIndexFnM test blankNameSource

runTest :: AlgM (PrimExp VName) a -> AlgEnv Symbol (PrimExp VName) Property -> a
runTest test env = fst $ runAlgM test env blankNameSource

tests :: TestTree
tests =
  testGroup
    "Proofs.AlgebraPC.SolveTests"
    [ testCase "Pow Exact Norm" $
        run
          ( forM [1..100000] $ \ kk -> do
              let xy_p_3x= sVar x .*. (sVar y .+. int 3)
                  pow_xy = int 2 .*. sVar i1 .*. sym2SoP (Pow(2, xy_p_3x)) 
                  pow_z  = int 8 .*. sVar i2 .*. sym2SoP (Pow (2, sVar z .*. int 2)) 
              simplifyLevel $ pow_xy .*. pow_z .+. int kk
          )
          @??= map (\ kk ->
                       let expo = sVar x .*. sVar y .+. int 3 .*. sVar x .+. int 2 .*. sVar z .+. int 4
                       in  sVar i1 .*. sVar i2 .*. sym2SoP (Pow(2, expo)) .+. int kk
                   ) [1..100000],
      testCase "Sum subtraction and Peel off (from partition2)" $
        run
          ( do
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
              let sum1 = sym2SoP $ Sum b (sVar y .+. int 1) $ sVar z
                  idx1 = sym2SoP $ Idx b $ sVar y
                  sum2 = sym2SoP $ Sum a (sVar i1 .+. int 1) $ sVar i2
                  idx2 = sym2SoP $ Idx a $ sVar i2 .+. int 1
              simplifyLevel $ int 1 .+. idx1 .+. sum1 .+. sum2 .+. idx2
          )
          @??= ( let sum1 = sym2SoP $ Sum b (sVar y) $ sVar z
                     sum2 = sym2SoP $ Sum a (sVar i1 .+. int 1) $ sVar i2 .+. int 1
                 in  int 1 .+. sum1 .+. sum2
               ),
      testCase "Complex sum subtraction" $
        run
          ( forM [1..1000] $ \i -> do
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
                     ) [1..1000]
               )
    ]
  where
    int = int2SoP
    sVar = sym2SoP . Var
    -- a ~+~ b = sym2SoP a .+. sym2SoP b
    -- a ~-~ b = sym2SoP a .-. sym2SoP b

    env_empty = AlgEnv { untrans = Unknowns M.empty M.empty
                       , equivs  = M.empty
                       , ranges  = M.empty
                       , properties = M.empty
                       }
    
    varsM =
      (,,,,,,,,)
        <$> newVName "x"
        <*> newVName "y"
        <*> newVName "z"
        <*> newVName "i1"
        <*> newVName "i2"
        <*> newVName "i3"
        <*> newVName "n"
        <*> newVName "A"
        <*> newVName "B"
    (x, y, z, i1, i2, i3, n, a0, b0) = runTest varsM env_empty
    (a, b) = (One a0, One b0)
    
    (n_rg, i1_rg, i2_rg, i3_rg) =
      ( Range (S.singleton (int 1)) 1 S.empty
      , Range (S.singleton (int 0)) 1 $ S.singleton $ sVar n  .-. int 1
      , Range (S.singleton (int 0)) 1 $ S.singleton $ sVar i1 .-. int 1
      , Range (S.singleton (int 2 .*. sVar i1 .-. int 1)) 3 $ S.empty
      )
    env = env_empty { equivs = M.singleton (Idx a (sVar i2)) (int 0)
                    , ranges = M.fromList $ zip (map Var [n,i1,i2,i3]) $ [n_rg,i1_rg,i2_rg,i3_rg]
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
