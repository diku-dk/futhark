module Futhark.Analysis.PrimExp.Generalize
  (
    leastGeneralGeneralization
  ) where

import           Control.Monad
import qualified Data.Map as M

import           Futhark.Analysis.PrimExp
import           Futhark.Representation.AST.Syntax.Core (Ext(..))

type MapPExp v = M.Map Int (PrimExp v, PrimExp v)

-- | Generalization (anti-unification)
-- We assume that the two expressions have the same type.
leastGeneralGeneralization :: (Eq v) => Int -> PrimExp v -> PrimExp v ->
                              Maybe (Int, PrimExp (Ext v), MapPExp v)
leastGeneralGeneralization k exp1@(LeafExp v1 t1) exp2@(LeafExp v2 _) =
  if v1 == v2 then
    Just (k, LeafExp (Free v1) t1, M.empty)
  else
    Just (k+1, LeafExp (Ext k) t1, M.singleton k (exp1, exp2))
leastGeneralGeneralization k exp1@(ValueExp v1) exp2@(ValueExp v2) =
  if v1 == v2 then
    Just (k, ValueExp v1, M.empty)
  else
    Just (k+1, LeafExp (Ext k) $ primExpType exp1, M.singleton k (exp1, exp2))
leastGeneralGeneralization k exp1@(BinOpExp op1 e11 e12) exp2@(BinOpExp op2 e21 e22) =
  if op1 == op2 then do
    (k', e1, m1) <- leastGeneralGeneralization k e11 e21
    (k'', e2, m2) <- leastGeneralGeneralization k' e12 e22
    return (k'', BinOpExp op1 e1 e2, m1 `M.union` m2)
  else
    Just $ generalize k exp1 exp2
leastGeneralGeneralization k exp1@(CmpOpExp op1 e11 e12) exp2@(CmpOpExp op2 e21 e22) =
  if op1 == op2 then do
    (k', e1, m1) <- leastGeneralGeneralization k e11 e21
    (k'', e2, m2) <- leastGeneralGeneralization k' e12 e22
    return (k'', CmpOpExp op1 e1 e2, m1 `M.union` m2)
  else
    Just $ generalize k exp1 exp2
leastGeneralGeneralization k exp1@(UnOpExp op1 e1) exp2@(UnOpExp op2 e2) =
  if op1 == op2 then do
    (k', e, m) <- leastGeneralGeneralization k e1 e2
    return (k', UnOpExp op1 e, m)
  else
    Just $ generalize k exp1 exp2
leastGeneralGeneralization k exp1@(ConvOpExp op1 e1) exp2@(ConvOpExp op2 e2) =
  if op1 == op2 then do
    (k', e, m) <- leastGeneralGeneralization k e1 e2
    return (k', ConvOpExp op1 e, m)
  else
    Just $ generalize k exp1 exp2
leastGeneralGeneralization k exp1@(FunExp s1 args1 t1) exp2@(FunExp s2 args2 _) =
  if s1 == s2 && length args1 == length args2 then do
    (k'', args, m') <- foldM (\(k_acc, arg_acc, m_acc) (a1, a2) -> do
                                 (k', a, m) <- leastGeneralGeneralization k_acc a1 a2
                                 return (k', a : arg_acc, m `M.union` m_acc)
                              ) (k, [], M.empty) (zip args1 args2)
    return (k'', FunExp s1 (reverse args) t1, m')
  else
    Just $ generalize k exp1 exp2
leastGeneralGeneralization _ _ _ = Nothing


generalize :: Int -> PrimExp v -> PrimExp v -> (Int, PrimExp (Ext v), MapPExp v)
generalize k exp1 exp2 =
  (k+1, LeafExp (Ext k) (primExpType exp1), M.singleton k (exp1, exp2))
