{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.Analysis.PrimExp
  ( PrimExp (..)
  , PrimExpAnyType(..)
  , primExpType
  , primExpSizeAtLeast
  , leafExpTypes
  , true
  , false
  , constFoldPrimExp

  , module Futhark.IR.Primitive
  , sExt, zExt
  , (.&&.), (.||.), (.<.), (.<=.), (.>.), (.>=.), (.==.), (.&.), (.|.), (.^.)

  -- * Typed functions
  , fIsnan32
  , fIsnan64
  ) where


import Control.Monad
import qualified Data.Map as M

import Futhark.IR.Prop.Names
import Futhark.IR.Primitive
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty

data PrimExp t a where
  LeafExp :: PrimType t -> a -> PrimExp t a
  ValueExp :: PrimValue t -> PrimExp t a
  BinOpExp :: BinOp t -> PrimExp t a -> PrimExp t a -> PrimExp t a
  CmpOpExp :: CmpOp t -> PrimExp t a -> PrimExp t a -> PrimExp Bool a
  UnOpExp :: UnOp t -> PrimExp t a -> PrimExp t a
  ConvOpExp :: ConvOp f t -> PrimExp f a -> PrimExp t a
  FunExp :: String -> [PrimExpAnyType a] -> PrimType t -> PrimExp t a

data PrimExpAnyType a where
  AnyE :: forall t a. PrimExp t a -> PrimExpAnyType a

instance Functor (PrimExp t) where
  fmap = fmapDefault

instance Foldable (PrimExp t) where
  foldMap = foldMapDefault

instance Traversable (PrimExp t) where
  traverse f (LeafExp v t) =
    LeafExp <$> f v <*> pure t
  traverse _ (ValueExp v) =
    pure $ ValueExp v
  traverse f (BinOpExp op x y) =
    constFoldPrimExp <$> (BinOpExp op <$> traverse f x <*> traverse f y)
  traverse f (CmpOpExp op x y) =
    CmpOpExp op <$> traverse f x <*> traverse f y
  traverse f (ConvOpExp op x) =
    ConvOpExp op <$> traverse f x
  traverse f (UnOpExp op x) =
    UnOpExp op <$> traverse f x
  traverse f (FunExp h args t) =
    FunExp h <$> traverse onArg args <*> pure t
    where onArg (AnyE e) = traverse f e

instance FreeIn v => FreeIn (PrimExp v) where
  freeIn' = foldMap freeIn'

primExpType :: PrimExp t a -> PrimType t
primExpType (LeafExp t _) = t
primExpType (ValueExp v) = primValueType v
primExpType (BinOpExp op _ _) = binOpType op
primExpType CmpOpExp{} = Bool
primExpType (ConvOpExp op _) = snd $ convOpType op
primExpType (UnOpExp op _) = unOpType op
primExpType (FunExp _ _ t) = t

-- | True if the 'PrimExp' has at least this many nodes.  This can be
-- much more efficient than comparing with 'length' for large
-- 'PrimExp's, as this function is lazy.
primExpSizeAtLeast :: forall t v.Int -> PrimExp t v -> Bool
primExpSizeAtLeast k = maybe True (>=k) . descend 0
  where descend :: Int -> PrimExp t' v -> Maybe Int
        descend i _
          | i >= k = Nothing
        descend i LeafExp{} = Just (i+1)
        descend i ValueExp{} = Just (i+1)
        descend i (BinOpExp _ x y) = do x' <- descend (i+1) x
                                        descend x' y
        descend i (CmpOpExp _ x y) = do x' <- descend (i+1) x
                                        descend x' y
        descend i (ConvOpExp _ x) = descend (i+1) x
        descend i (UnOpExp _ x) = descend (i+1) x
        descend i (FunExp _ args _) = foldM descend' (i+1) args

        descend' i (AnyE e) = descend i e

-- | Perform quick and dirty constant folding on the top level of a
-- PrimExp.  This is necessary because we want to consider
-- e.g. equality modulo constant folding.
constFoldPrimExp :: PrimExp t v -> PrimExp t v
constFoldPrimExp = id -- FIXME

-- | Produce a mapping from the leaves of the 'PrimExp' to their
-- designated types.
leafExpTypes :: Ord a => PrimExp t a -> M.Map a (UT PrimType)
leafExpTypes (LeafExp ptp x) = M.singleton x (UT ptp)
leafExpTypes (ValueExp _) = mempty
leafExpTypes (UnOpExp _ e) = leafExpTypes e
leafExpTypes (ConvOpExp _ e) = leafExpTypes e
leafExpTypes (BinOpExp _ e1 e2) =
  leafExpTypes e1 <> leafExpTypes e2
leafExpTypes (CmpOpExp _ e1 e2) =
  leafExpTypes e1 <> leafExpTypes e2
leafExpTypes (FunExp _ pes _) =
  foldMap onArg pes
  where onArg (AnyE e) = leafExpTypes e

-- Construction

class CmpExp t where
  (.<.) :: PrimExp t a -> PrimExp t a -> PrimExp Bool a
  (.==.) :: PrimExp t a -> PrimExp t a -> PrimExp Bool a
  (.<=.) :: PrimExp t a -> PrimExp t a -> PrimExp Bool a

  (.>.) :: PrimExp t a -> PrimExp t a -> PrimExp Bool a
  (.>.) = flip (.<.)

  (.>=.) :: PrimExp t a -> PrimExp t a -> PrimExp Bool a
  (.>=.) = flip (.<=.)

instance CmpExp Int8 where
  (.<.) = CmpOpExp $ CmpSlt Int8
  (.==.) = CmpOpExp $ CmpEq $ IntType Int8
  (.<=.) = CmpOpExp $ CmpSle Int8

instance CmpExp Float where
  (.<.) = CmpOpExp $ FCmpLt Float32
  (.==.) = CmpOpExp $ CmpEq $ FloatType Float32
  (.<=.) = CmpOpExp $ FCmpLe Float32

instance CmpExp Double where
  (.<.) = CmpOpExp $ FCmpLt Float64
  (.==.) = CmpOpExp $ CmpEq $ FloatType Float64
  (.<=.) = CmpOpExp $ FCmpLe Float64

instance Num (PrimExp Int8 a) where
  (+) = BinOpExp $ Add Int8 OverflowUndef
  (-) = BinOpExp $ Sub Int8 OverflowUndef
  (*) = BinOpExp $ Mul Int8 OverflowUndef

  signum = UnOpExp $ SSignum Int8
  abs = UnOpExp $ Abs Int8

  fromInteger = ValueExp . IntValue . Int8Value . fromInteger

instance Num (PrimExp Float a) where
  (+) = BinOpExp $ FAdd Float32
  (-) = BinOpExp $ FSub Float32
  (*) = BinOpExp $ FMul Float32

  signum = UnOpExp $ FSignum Float32
  abs = UnOpExp $ FAbs Float32

  fromInteger = ValueExp . FloatValue . Float32Value . fromInteger

instance Num (PrimExp Double a) where
  (+) = BinOpExp $ FAdd Float64
  (-) = BinOpExp $ FSub Float64
  (*) = BinOpExp $ FMul Float64

  signum = UnOpExp $ FSignum Float64
  abs = UnOpExp $ FAbs Float64

  fromInteger = ValueExp . FloatValue . Float64Value . fromInteger

-- | Lifted logical conjunction.
(.&&.) :: PrimExp Bool v -> PrimExp Bool v -> PrimExp Bool v
x .&&. y = BinOpExp LogAnd x y

-- | Lifted logical conjunction.
(.||.) :: PrimExp Bool v -> PrimExp Bool v -> PrimExp Bool v
x .||. y = BinOpExp LogOr x y

class IntExp t where
  -- ^ The reified type.
  intType :: IntType t

instance IntExp Int8 where
  intType = Int8

instance IntExp Int16 where
  intType = Int16

instance IntExp Int32 where
  intType = Int32

instance IntExp Int64 where
  intType = Int64

-- | Lifted bitwise operators.
(.&.), (.|.), (.^.) :: IntExp t => PrimExp t v -> PrimExp t v -> PrimExp t v
x .&. y = BinOpExp (And intType) x y
x .|. y = BinOpExp (Or intType) x y
x .^. y = BinOpExp (Xor intType) x y

-- | Smart constructor for sign extension that does a bit of constant
-- folding.
sExt :: (IntExp f, IntExp t) => PrimExp f a -> PrimExp t a
sExt = ConvOpExp (SExt intType intType)

-- | Smart constructor for zero extension that does a bit of constant
-- folding.
zExt :: (IntExp f, IntExp t) => PrimExp f a -> PrimExp t a
zExt = ConvOpExp (ZExt intType intType)

-- | @f32.isnan@
fIsnan32 :: PrimExp Float a -> PrimExp Bool a
fIsnan32 x = FunExp "isnan32" [AnyE x] Bool

-- | @f64.isnan@
fIsnan64 :: PrimExp Double a -> PrimExp Bool a
fIsnan64 x = FunExp "isnan64" [AnyE x] Bool

-- | Boolean-valued PrimExps.
true, false :: PrimExp Bool a
true = ValueExp $ BoolValue True
false = ValueExp $ BoolValue False

-- Prettyprinting instances

instance Pretty a => Pretty (PrimExpAnyType a) where
  ppr (AnyE e) = ppr e

instance Pretty a => Pretty (PrimExp t a) where
  ppr (LeafExp v _)     = ppr v
  ppr (ValueExp v)      = ppr v
  ppr (BinOpExp op x y) = ppr op <+> parens (ppr x) <+> parens (ppr y)
  ppr (CmpOpExp op x y) = ppr op <+> parens (ppr x) <+> parens (ppr y)
  ppr (ConvOpExp op x)  = ppr op <+> parens (ppr x)
  ppr (UnOpExp op x)    = ppr op <+> parens (ppr x)
  ppr (FunExp h args _) = text h <+> parens (commasep $ map ppr args)
