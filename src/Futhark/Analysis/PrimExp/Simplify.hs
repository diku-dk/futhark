-- | Defines simplification functions for 'PrimExp's.
module Futhark.Analysis.PrimExp.Simplify (simplifyPrimExp, simplifyExtPrimExp) where

import Futhark.Analysis.PrimExp
import Futhark.IR
import Futhark.Optimise.Simplify.Engine as Engine

-- | Simplify a 'PrimExp', including copy propagation.  If a 'LeafExp'
-- refers to a name that is a 'Constant', the node turns into a
-- 'ValueExp'.
simplifyPrimExp ::
  (SimplifiableRep rep) =>
  PrimExp VName ->
  SimpleM rep (PrimExp VName)
simplifyPrimExp = simplifyAnyPrimExp onLeaf
  where
    onLeaf v pt = do
      se <- simplify $ Var v
      case se of
        Var v' -> pure $ LeafExp v' pt
        Constant pv -> pure $ ValueExp pv

-- | Like 'simplifyPrimExp', but where leaves may be 'Ext's.
simplifyExtPrimExp ::
  (SimplifiableRep rep) =>
  PrimExp (Ext VName) ->
  SimpleM rep (PrimExp (Ext VName))
simplifyExtPrimExp = simplifyAnyPrimExp onLeaf
  where
    onLeaf (Free v) pt = do
      se <- simplify $ Var v
      case se of
        Var v' -> pure $ LeafExp (Free v') pt
        Constant pv -> pure $ ValueExp pv
    onLeaf (Ext i) pt = pure $ LeafExp (Ext i) pt

simplifyAnyPrimExp ::
  (SimplifiableRep rep) =>
  (a -> PrimType -> SimpleM rep (PrimExp a)) ->
  PrimExp a ->
  SimpleM rep (PrimExp a)
simplifyAnyPrimExp f (LeafExp v pt) = f v pt
simplifyAnyPrimExp _ (ValueExp pv) =
  pure $ ValueExp pv
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
