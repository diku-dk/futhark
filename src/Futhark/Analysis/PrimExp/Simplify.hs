{-# LANGUAGE FlexibleContexts #-}
-- | Defines simplification functions for 'PrimExp's.
module Futhark.Analysis.PrimExp.Simplify
  (simplifyPrimExp, simplifyExtPrimExp)
where

import           Futhark.Analysis.PrimExp
import           Futhark.Optimise.Simplify.Engine as Engine
import           Futhark.Representation.AST

-- | Simplify a 'PrimExp', including copy propagation.  If a 'LeafExp'
-- refers to a name that is a 'Constant', the node turns into a
-- 'ValueExp'.
simplifyPrimExp :: SimplifiableLore lore =>
                   PrimExp VName -> SimpleM lore (PrimExp VName)
simplifyPrimExp = simplifyAnyPrimExp onLeaf
  where onLeaf v pt = do
          se <- simplify $ Var v
          case se of
            Var v' -> return $ LeafExp v' pt
            Constant pv -> return $ ValueExp pv

-- | Like 'simplifyPrimExp', but where leaves may be 'Ext's.
simplifyExtPrimExp :: SimplifiableLore lore =>
                      PrimExp (Ext VName) -> SimpleM lore (PrimExp (Ext VName))
simplifyExtPrimExp = simplifyAnyPrimExp onLeaf
  where onLeaf (Free v) pt = do
          se <- simplify $ Var v
          case se of
            Var v' -> return $ LeafExp (Free v') pt
            Constant pv -> return $ ValueExp pv
        onLeaf (Ext i) pt = return $ LeafExp (Ext i) pt

simplifyAnyPrimExp :: SimplifiableLore lore =>
                      (a -> PrimType -> SimpleM lore (PrimExp a))
                   -> PrimExp a -> SimpleM lore (PrimExp a)
simplifyAnyPrimExp f (LeafExp v pt) = f v pt
simplifyAnyPrimExp _ (ValueExp pv) =
  return $ ValueExp pv
simplifyAnyPrimExp f (BinOpExp bop e1 e2) =
  BinOpExp bop <$> simplifyAnyPrimExp f e1 <*> simplifyAnyPrimExp f e2
simplifyAnyPrimExp f (CmpOpExp cmp e1 e2) =
  CmpOpExp cmp <$> simplifyAnyPrimExp f e1 <*> simplifyAnyPrimExp f e2
simplifyAnyPrimExp f (UnOpExp op e) =
  UnOpExp op <$> simplifyAnyPrimExp f e
simplifyAnyPrimExp f (ConvOpExp conv e) =
  ConvOpExp conv <$> simplifyAnyPrimExp f e
simplifyAnyPrimExp f (FunExp h args t) =
  FunExp h <$> mapM (simplifyAnyPrimExp f) args <*> pure t
