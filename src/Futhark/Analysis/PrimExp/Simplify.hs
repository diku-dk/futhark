{-# LANGUAGE FlexibleContexts #-}
-- | Defines simplification functions for 'PrimExp's.
module Futhark.Analysis.PrimExp.Simplify
  (simplifyPrimExp)
where

import           Futhark.Analysis.PrimExp
import           Futhark.Optimise.Simplifier.Engine as Engine
import           Futhark.Representation.AST

-- | Simplify a 'PrimExp', including copy propagation.  If a 'LeafExp'
-- refers to a name that is a 'Constant', the node turns into a
-- 'ValueExp'.
simplifyPrimExp :: SimplifiableLore lore =>
                   PrimExp VName -> SimpleM lore (PrimExp VName)
simplifyPrimExp (LeafExp v pt) = do
  se <- simplify $ Var v
  case se of
    Var v' -> return $ LeafExp v' pt
    Constant pv -> return $ ValueExp pv
simplifyPrimExp (ValueExp pv) =
  return $ ValueExp pv
simplifyPrimExp (BinOpExp bop e1 e2) =
  BinOpExp bop <$> simplifyPrimExp e1 <*> simplifyPrimExp e2
simplifyPrimExp (CmpOpExp cmp e1 e2) =
  CmpOpExp cmp <$> simplifyPrimExp e1 <*> simplifyPrimExp e2
simplifyPrimExp (UnOpExp op e) =
  UnOpExp op <$> simplifyPrimExp e
simplifyPrimExp (ConvOpExp conv e) =
  ConvOpExp conv <$> simplifyPrimExp e
