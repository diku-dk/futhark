module Futhark.Analysis.PrimExp.Generalize
  (
    leastGeneralGeneralization
  ) where

import           Control.Monad
import qualified Data.Map as M
import           Data.List (find)

import           Futhark.Analysis.PrimExp
import           Futhark.Representation.AST.Syntax.Core (Ext(..))

type MapPExp v = M.Map Int (PrimExp v, PrimExp v)

-- | Generalization (anti-unification)
-- We assume that the two expressions have the same type.
leastGeneralGeneralization :: (Eq v) => MapPExp v -> Int -> PrimExp v -> PrimExp v ->
                              Maybe (Int, PrimExp (Ext v), MapPExp v)
leastGeneralGeneralization m k exp1@(LeafExp v1 t1) exp2@(LeafExp v2 _) =
  if v1 == v2 then
    Just (k, LeafExp (Free v1) t1, m)
  else
    let (k', k'', m') = maybeAdd m k (exp1, exp2)
    in Just (k', LeafExp (Ext k'') t1, m')
leastGeneralGeneralization m k exp1@(ValueExp v1) exp2@(ValueExp v2) =
  if v1 == v2 then
    Just (k, ValueExp v1, m)
  else
    let (k', k'', m') = maybeAdd m k (exp1, exp2)
    in Just (k', LeafExp (Ext k'') $ primExpType exp1, m')
leastGeneralGeneralization m k exp1@(BinOpExp op1 e11 e12) exp2@(BinOpExp op2 e21 e22) =
  if op1 == op2 then do
    (k', e1, m1) <- leastGeneralGeneralization m k e11 e21
    (k'', e2, m2) <- leastGeneralGeneralization m1 k' e12 e22
    return (k'', BinOpExp op1 e1 e2, m2)
  else
    Just $ generalize m k exp1 exp2
leastGeneralGeneralization m k exp1@(CmpOpExp op1 e11 e12) exp2@(CmpOpExp op2 e21 e22) =
  if op1 == op2 then do
    (k', e1, m1) <- leastGeneralGeneralization m k e11 e21
    (k'', e2, m2) <- leastGeneralGeneralization m1 k' e12 e22
    return (k'', CmpOpExp op1 e1 e2, m2)
  else
    Just $ generalize m k exp1 exp2
leastGeneralGeneralization m k exp1@(UnOpExp op1 e1) exp2@(UnOpExp op2 e2) =
  if op1 == op2 then do
    (k', e, m') <- leastGeneralGeneralization m k e1 e2
    return (k', UnOpExp op1 e, m')
  else
    Just $ generalize m k exp1 exp2
leastGeneralGeneralization m k exp1@(ConvOpExp op1 e1) exp2@(ConvOpExp op2 e2) =
  if op1 == op2 then do
    (k', e, m') <- leastGeneralGeneralization m k e1 e2
    return (k', ConvOpExp op1 e, m')
  else
    Just $ generalize m k exp1 exp2
leastGeneralGeneralization m k exp1@(FunExp s1 args1 t1) exp2@(FunExp s2 args2 _) =
  if s1 == s2 && length args1 == length args2 then do
    (k'', args, m') <- foldM (\(k_acc, arg_acc, m_acc) (a1, a2) -> do
                                 (k', a, m') <- leastGeneralGeneralization m_acc k_acc a1 a2
                                 return (k', a : arg_acc, m')
                              ) (k, [], m) (zip args1 args2)
    return (k'', FunExp s1 (reverse args) t1, m')
  else
    Just $ generalize m k exp1 exp2
leastGeneralGeneralization _ _ _ _ = Nothing

maybeAdd :: Eq v => MapPExp v -> Int -> (PrimExp v, PrimExp v) -> (Int, Int, MapPExp v)
maybeAdd m k x =
  case find (\(_, x') -> x == x') $ M.toList m of
    Just (k', _) -> (k, k', m)
    Nothing -> (k + 1, k, m `M.union` (M.singleton k x))

generalize :: Eq v => MapPExp v -> Int -> PrimExp v -> PrimExp v -> (Int, PrimExp (Ext v), MapPExp v)
generalize m k exp1 exp2 =
  let (k', k'', m') = maybeAdd m k (exp1, exp2)
  in (k', LeafExp (Ext k'') $ primExpType exp1, m')
