{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Converting back and forth between 'PrimExp's.  Use the 'ToExp'
-- instance to convert to Futhark expressions.
module Futhark.Analysis.PrimExp.Convert
  ( primExpFromExp,
    primExpFromSubExp,
    primExpFromSubExpM,
    replaceInPrimExp,
    replaceInPrimExpM,
    substituteInPrimExp,
    primExpSlice,
    subExpSlice,

    -- * Module reexport
    module Futhark.Analysis.PrimExp,
  )
where

import qualified Control.Monad.Fail as Fail
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Analysis.PrimExp
import Futhark.Construct
import Futhark.IR

instance ToExp v => ToExp (PrimExp v) where
  toExp (BinOpExp op x y) =
    BasicOp <$> (BinOp op <$> toSubExp "binop_x" x <*> toSubExp "binop_y" y)
  toExp (CmpOpExp op x y) =
    BasicOp <$> (CmpOp op <$> toSubExp "cmpop_x" x <*> toSubExp "cmpop_y" y)
  toExp (UnOpExp op x) =
    BasicOp <$> (UnOp op <$> toSubExp "unop_x" x)
  toExp (ConvOpExp op x) =
    BasicOp <$> (ConvOp op <$> toSubExp "convop_x" x)
  toExp (ValueExp v) =
    return $ BasicOp $ SubExp $ Constant v
  toExp (FunExp h args t) =
    Apply (nameFromString h) <$> args' <*> pure [primRetType t]
      <*> pure (Safe, mempty, [])
    where
      args' = zip <$> mapM (toSubExp "apply_arg") args <*> pure (repeat Observe)
  toExp (LeafExp v _) =
    toExp v

-- | Convert an expression to a 'PrimExp'.  The provided function is
-- used to convert expressions that are not trivially 'PrimExp's.
-- This includes constants and variable names, which are passed as
-- t'SubExp's.
primExpFromExp ::
  (Fail.MonadFail m, Decorations lore) =>
  (VName -> m (PrimExp v)) ->
  Exp lore ->
  m (PrimExp v)
primExpFromExp f (BasicOp (BinOp op x y)) =
  BinOpExp op <$> primExpFromSubExpM f x <*> primExpFromSubExpM f y
primExpFromExp f (BasicOp (CmpOp op x y)) =
  CmpOpExp op <$> primExpFromSubExpM f x <*> primExpFromSubExpM f y
primExpFromExp f (BasicOp (UnOp op x)) =
  UnOpExp op <$> primExpFromSubExpM f x
primExpFromExp f (BasicOp (ConvOp op x)) =
  ConvOpExp op <$> primExpFromSubExpM f x
primExpFromExp f (BasicOp (SubExp se)) =
  primExpFromSubExpM f se
primExpFromExp f (Apply fname args ts _)
  | isBuiltInFunction fname,
    [Prim t] <- map declExtTypeOf ts =
    FunExp (nameToString fname) <$> mapM (primExpFromSubExpM f . fst) args <*> pure t
primExpFromExp _ _ = fail "Not a PrimExp"

-- | Like 'primExpFromExp', but for a t'SubExp'.
primExpFromSubExpM :: Applicative m => (VName -> m (PrimExp v)) -> SubExp -> m (PrimExp v)
primExpFromSubExpM f (Var v) = f v
primExpFromSubExpM _ (Constant v) = pure $ ValueExp v

-- | Convert t'SubExp's of a given type.
primExpFromSubExp :: PrimType -> SubExp -> PrimExp VName
primExpFromSubExp t (Var v) = LeafExp v t
primExpFromSubExp _ (Constant v) = ValueExp v

-- | Applying a monadic transformation to the leaves in a 'PrimExp'.
replaceInPrimExpM ::
  Monad m =>
  (a -> PrimType -> m (PrimExp b)) ->
  PrimExp a ->
  m (PrimExp b)
replaceInPrimExpM f (LeafExp v pt) =
  f v pt
replaceInPrimExpM _ (ValueExp v) =
  return $ ValueExp v
replaceInPrimExpM f (BinOpExp bop pe1 pe2) =
  constFoldPrimExp
    <$> (BinOpExp bop <$> replaceInPrimExpM f pe1 <*> replaceInPrimExpM f pe2)
replaceInPrimExpM f (CmpOpExp cop pe1 pe2) =
  CmpOpExp cop <$> replaceInPrimExpM f pe1 <*> replaceInPrimExpM f pe2
replaceInPrimExpM f (UnOpExp uop pe) =
  UnOpExp uop <$> replaceInPrimExpM f pe
replaceInPrimExpM f (ConvOpExp cop pe) =
  ConvOpExp cop <$> replaceInPrimExpM f pe
replaceInPrimExpM f (FunExp h args t) =
  FunExp h <$> mapM (replaceInPrimExpM f) args <*> pure t

-- | As 'replaceInPrimExpM', but in the identity monad.
replaceInPrimExp ::
  (a -> PrimType -> PrimExp b) ->
  PrimExp a ->
  PrimExp b
replaceInPrimExp f e = runIdentity $ replaceInPrimExpM f' e
  where
    f' x y = return $ f x y

-- | Substituting names in a PrimExp with other PrimExps
substituteInPrimExp ::
  Ord v =>
  M.Map v (PrimExp v) ->
  PrimExp v ->
  PrimExp v
substituteInPrimExp tab = replaceInPrimExp $ \v t ->
  fromMaybe (LeafExp v t) $ M.lookup v tab

-- | Convert a 'SubExp' slice to a 'PrimExp' slice.
primExpSlice :: Slice SubExp -> Slice (PrimExp VName)
primExpSlice = map $ fmap $ primExpFromSubExp int32

-- | Convert a 'PrimExp' slice to a 'SubExp' slice.
subExpSlice :: MonadBinder m => Slice (PrimExp VName) -> m (Slice SubExp)
subExpSlice = mapM $ traverse $ toSubExp "slice"
