module Futhark.Analysis.PrimExp.Generalize
  (
    leastGeneralGeneralization
  ) where

import           Control.Monad
import           Data.List (elemIndex)


import           Futhark.Analysis.PrimExp
import           Futhark.Representation.AST.Syntax.Core (Ext(..))

-- | Generalization (anti-unification)
-- We assume that the two expressions have the same type.
leastGeneralGeneralization :: (Eq v) => [(PrimExp v, PrimExp v)] -> PrimExp v -> PrimExp v ->
                              Maybe (PrimExp (Ext v), [(PrimExp v, PrimExp v)])
leastGeneralGeneralization m exp1@(LeafExp v1 t1) exp2@(LeafExp v2 _) =
  if v1 == v2 then
    Just (LeafExp (Free v1) t1, m)
  else
    Just $ generalize m exp1 exp2
leastGeneralGeneralization m exp1@(ValueExp v1) exp2@(ValueExp v2) =
  if v1 == v2 then
    Just (ValueExp v1, m)
  else
    Just $ generalize m exp1 exp2
leastGeneralGeneralization m exp1@(BinOpExp op1 e11 e12) exp2@(BinOpExp op2 e21 e22) =
  if op1 == op2 then do
    (e1, m1) <- leastGeneralGeneralization m e11 e21
    (e2, m2) <- leastGeneralGeneralization m1 e12 e22
    return (BinOpExp op1 e1 e2, m2)
  else
    Just $ generalize m exp1 exp2
leastGeneralGeneralization m exp1@(CmpOpExp op1 e11 e12) exp2@(CmpOpExp op2 e21 e22) =
  if op1 == op2 then do
    (e1, m1) <- leastGeneralGeneralization m e11 e21
    (e2, m2) <- leastGeneralGeneralization m1 e12 e22
    return (CmpOpExp op1 e1 e2, m2)
  else
    Just $ generalize m exp1 exp2
leastGeneralGeneralization m exp1@(UnOpExp op1 e1) exp2@(UnOpExp op2 e2) =
  if op1 == op2 then do
    (e, m1) <- leastGeneralGeneralization m e1 e2
    return (UnOpExp op1 e, m1)
  else
    Just $ generalize m exp1 exp2
leastGeneralGeneralization m exp1@(ConvOpExp op1 e1) exp2@(ConvOpExp op2 e2) =
  if op1 == op2 then do
    (e, m1) <- leastGeneralGeneralization m e1 e2
    return (ConvOpExp op1 e, m1)
  else
    Just $ generalize m exp1 exp2
leastGeneralGeneralization m exp1@(FunExp s1 args1 t1) exp2@(FunExp s2 args2 _) =
  if s1 == s2 && length args1 == length args2 then do
    (args, m') <- foldM (\(arg_acc, m_acc) (a1, a2) -> do
                            (a, m'') <- leastGeneralGeneralization m_acc a1 a2
                            return (a : arg_acc, m'')
                        ) ([], m) (zip args1 args2)
    return (FunExp s1 (reverse args) t1, m')
  else
    Just $ generalize m exp1 exp2
leastGeneralGeneralization _ _ _ = Nothing

generalize :: Eq v => [(PrimExp v, PrimExp v)] -> PrimExp v -> PrimExp v -> (PrimExp (Ext v), [(PrimExp v, PrimExp v)])
generalize m exp1 exp2 =
  let t = primExpType exp1
  in case elemIndex (exp1, exp2) m of
       Just i -> (LeafExp (Ext i) t, m)
       Nothing -> (LeafExp (Ext $ length m) t, m ++ [(exp1, exp2)])
