{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Converting back and forth between 'PrimExp's.  Use the 'ToExp'
-- instance to convert to Futhark expressions.
module Futhark.Analysis.PrimExp.Convert
  ( primExpFromExp,
    primExpFromSubExp,
    pe32,
    le32,
    pe64,
    le64,
    f32pe,
    f32le,
    f64pe,
    f64le,
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

import Control.Monad.Fail qualified as Fail
import Control.Monad.Identity
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.PrimExp
import Futhark.Construct
import Futhark.IR

instance (ToExp v) => ToExp (PrimExp v) where
  toExp (BinOpExp op x y) =
    BasicOp <$> (BinOp op <$> toSubExp "binop_x" x <*> toSubExp "binop_y" y)
  toExp (CmpOpExp op x y) =
    BasicOp <$> (CmpOp op <$> toSubExp "cmpop_x" x <*> toSubExp "cmpop_y" y)
  toExp (UnOpExp op x) =
    BasicOp <$> (UnOp op <$> toSubExp "unop_x" x)
  toExp (ConvOpExp op x) =
    BasicOp <$> (ConvOp op <$> toSubExp "convop_x" x)
  toExp (ValueExp v) =
    pure $ BasicOp $ SubExp $ Constant v
  toExp (FunExp h args t) =
    Apply (nameFromString h)
      <$> args'
      <*> pure [(primRetType t, mempty)]
      <*> pure (Safe, mempty, [])
    where
      args' = zip <$> mapM (toSubExp "apply_arg") args <*> pure (repeat Observe)
  toExp (LeafExp v _) =
    toExp v

instance (ToExp v) => ToExp (TPrimExp t v) where
  toExp = toExp . untyped

-- | Convert an expression to a 'PrimExp'.  The provided function is
-- used to convert expressions that are not trivially 'PrimExp's.
-- This includes constants and variable names, which are passed as
-- t'SubExp's.
primExpFromExp ::
  (Fail.MonadFail m, RepTypes rep) =>
  (VName -> m (PrimExp v)) ->
  Exp rep ->
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
    [Prim t] <- map (declExtTypeOf . fst) ts =
      FunExp (nameToString fname) <$> mapM (primExpFromSubExpM f . fst) args <*> pure t
primExpFromExp _ _ = fail "Not a PrimExp"

-- | Like 'primExpFromExp', but for a t'SubExp'.
primExpFromSubExpM :: (Applicative m) => (VName -> m (PrimExp v)) -> SubExp -> m (PrimExp v)
primExpFromSubExpM f (Var v) = f v
primExpFromSubExpM _ (Constant v) = pure $ ValueExp v

-- | Convert t'SubExp's of a given type.
primExpFromSubExp :: PrimType -> SubExp -> PrimExp VName
primExpFromSubExp t (Var v) = LeafExp v t
primExpFromSubExp _ (Constant v) = ValueExp v

-- | Shorthand for constructing a 'TPrimExp' of type v'Int32'.
pe32 :: SubExp -> TPrimExp Int32 VName
pe32 = isInt32 . primExpFromSubExp int32

-- | Shorthand for constructing a 'TPrimExp' of type v'Int32', from a leaf.
le32 :: a -> TPrimExp Int32 a
le32 = isInt32 . flip LeafExp int32

-- | Shorthand for constructing a 'TPrimExp' of type v'Int64'.
pe64 :: SubExp -> TPrimExp Int64 VName
pe64 = isInt64 . primExpFromSubExp int64

-- | Shorthand for constructing a 'TPrimExp' of type v'Int64', from a leaf.
le64 :: a -> TPrimExp Int64 a
le64 = isInt64 . flip LeafExp int64

-- | Shorthand for constructing a 'TPrimExp' of type 'Float32'.
f32pe :: SubExp -> TPrimExp Float VName
f32pe = isF32 . primExpFromSubExp float32

-- | Shorthand for constructing a 'TPrimExp' of type v'Float32', from a leaf.
f32le :: a -> TPrimExp Float a
f32le = isF32 . flip LeafExp float32

-- | Shorthand for constructing a 'TPrimExp' of type v'Float64'.
f64pe :: SubExp -> TPrimExp Double VName
f64pe = isF64 . primExpFromSubExp float64

-- | Shorthand for constructing a 'TPrimExp' of type v'Float64', from a leaf.
f64le :: a -> TPrimExp Double a
f64le = isF64 . flip LeafExp float64

-- | Applying a monadic transformation to the leaves in a 'PrimExp'.
replaceInPrimExpM ::
  (Monad m) =>
  (a -> PrimType -> m (PrimExp b)) ->
  PrimExp a ->
  m (PrimExp b)
replaceInPrimExpM f (LeafExp v pt) =
  f v pt
replaceInPrimExpM _ (ValueExp v) =
  pure $ ValueExp v
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
    f' x y = pure $ f x y

-- | Substituting names in a PrimExp with other PrimExps
substituteInPrimExp ::
  (Ord v) =>
  M.Map v (PrimExp v) ->
  PrimExp v ->
  PrimExp v
substituteInPrimExp tab = replaceInPrimExp $ \v t ->
  fromMaybe (LeafExp v t) $ M.lookup v tab

-- | Convert a t'SubExp' slice to a 'PrimExp' slice.
primExpSlice :: Slice SubExp -> Slice (TPrimExp Int64 VName)
primExpSlice = fmap pe64

-- | Convert a 'PrimExp' slice to a t'SubExp' slice.
subExpSlice :: (MonadBuilder m) => Slice (TPrimExp Int64 VName) -> m (Slice SubExp)
subExpSlice = traverse $ toSubExp "slice"
