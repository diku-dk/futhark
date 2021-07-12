{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

-- | The most primitive ("core") aspects of the AST.  Split out of
-- "Futhark.IR.Syntax" in order for
-- "Futhark.IR.Decorations" to use these definitions.  This
-- module is re-exported from "Futhark.IR.Syntax" and
-- there should be no reason to include it explicitly.
module Futhark.IR.Syntax.Core
  ( module Language.Futhark.Core,
    module Futhark.IR.Primitive,

    -- * Types
    Uniqueness (..),
    NoUniqueness (..),
    ShapeBase (..),
    Shape,
    Ext (..),
    ExtSize,
    ExtShape,
    Rank (..),
    ArrayShape (..),
    Space (..),
    SpaceId,
    TypeBase (..),
    Type,
    ExtType,
    DeclType,
    DeclExtType,
    Diet (..),
    ErrorMsg (..),
    ErrorMsgPart (..),
    errorMsgArgTypes,

    -- * Values
    PrimValue (..),

    -- * Abstract syntax tree
    Ident (..),
    Certificates (..),
    SubExp (..),
    Param (..),
    DimIndex (..),
    Slice (..),
    dimFix,
    sliceIndices,
    sliceDims,
    unitSlice,
    fixSlice,
    sliceSlice,
    PatElemT (..),
  )
where

import Control.Category
import Control.Monad.State
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import Data.Traversable (fmapDefault, foldMapDefault)
import Futhark.IR.Primitive
import Language.Futhark.Core
import Prelude hiding (id, (.))

-- | The size of an array type as a list of its dimension sizes, with
-- the type of sizes being parametric.
newtype ShapeBase d = Shape {shapeDims :: [d]}
  deriving (Eq, Ord, Show)

instance Functor ShapeBase where
  fmap = fmapDefault

instance Foldable ShapeBase where
  foldMap = foldMapDefault

instance Traversable ShapeBase where
  traverse f = fmap Shape . traverse f . shapeDims

instance Semigroup (ShapeBase d) where
  Shape l1 <> Shape l2 = Shape $ l1 `mappend` l2

instance Monoid (ShapeBase d) where
  mempty = Shape mempty

-- | The size of an array as a list of subexpressions.  If a variable,
-- that variable must be in scope where this array is used.
type Shape = ShapeBase SubExp

-- | Something that may be existential.
data Ext a
  = Ext Int
  | Free a
  deriving (Eq, Ord, Show)

instance Functor Ext where
  fmap = fmapDefault

instance Foldable Ext where
  foldMap = foldMapDefault

instance Traversable Ext where
  traverse _ (Ext i) = pure $ Ext i
  traverse f (Free v) = Free <$> f v

-- | The size of this dimension.
type ExtSize = Ext SubExp

-- | Like t'Shape' but some of its elements may be bound in a local
-- environment instead.  These are denoted with integral indices.
type ExtShape = ShapeBase ExtSize

-- | The size of an array type as merely the number of dimensions,
-- with no further information.
newtype Rank = Rank Int
  deriving (Show, Eq, Ord)

-- | A class encompassing types containing array shape information.
class (Monoid a, Eq a, Ord a) => ArrayShape a where
  -- | Return the rank of an array with the given size.
  shapeRank :: a -> Int

  -- | @stripDims n shape@ strips the outer @n@ dimensions from
  -- @shape@.
  stripDims :: Int -> a -> a

  -- | Check whether one shape if a subset of another shape.
  subShapeOf :: a -> a -> Bool

instance ArrayShape (ShapeBase SubExp) where
  shapeRank (Shape l) = length l
  stripDims n (Shape dims) = Shape $ drop n dims
  subShapeOf = (==)

instance ArrayShape (ShapeBase ExtSize) where
  shapeRank (Shape l) = length l
  stripDims n (Shape dims) = Shape $ drop n dims
  subShapeOf (Shape ds1) (Shape ds2) =
    -- Must agree on Free dimensions, and ds1 may not be existential
    -- where ds2 is Free.  Existentials must also be congruent.
    length ds1 == length ds2
      && evalState (and <$> zipWithM subDimOf ds1 ds2) M.empty
    where
      subDimOf (Free se1) (Free se2) = return $ se1 == se2
      subDimOf (Ext _) (Free _) = return False
      subDimOf (Free _) (Ext _) = return True
      subDimOf (Ext x) (Ext y) = do
        extmap <- get
        case M.lookup y extmap of
          Just ywas
            | ywas == x -> return True
            | otherwise -> return False
          Nothing -> do
            put $ M.insert y x extmap
            return True

instance Semigroup Rank where
  Rank x <> Rank y = Rank $ x + y

instance Monoid Rank where
  mempty = Rank 0

instance ArrayShape Rank where
  shapeRank (Rank x) = x
  stripDims n (Rank x) = Rank $ x - n
  subShapeOf = (==)

-- | The memory space of a block.  If 'DefaultSpace', this is the "default"
-- space, whatever that is.  The exact meaning of the 'SpaceId'
-- depends on the backend used.  In GPU kernels, for example, this is
-- used to distinguish between constant, global and shared memory
-- spaces.  In GPU-enabled host code, it is used to distinguish
-- between host memory ('DefaultSpace') and GPU space.
data Space
  = DefaultSpace
  | Space SpaceId
  | -- | A special kind of memory that is a statically sized
    -- array of some primitive type.  Used for private memory
    -- on GPUs.
    ScalarSpace [SubExp] PrimType
  deriving (Show, Eq, Ord)

-- | A string representing a specific non-default memory space.
type SpaceId = String

-- | A fancier name for @()@ - encodes no uniqueness information.
data NoUniqueness = NoUniqueness
  deriving (Eq, Ord, Show)

instance Semigroup NoUniqueness where
  NoUniqueness <> NoUniqueness = NoUniqueness

instance Monoid NoUniqueness where
  mempty = NoUniqueness

-- | The type of a value.  When comparing types for equality with
-- '==', shapes must match.
data TypeBase shape u
  = Prim PrimType
  | -- | Token, index space, element type, and uniqueness.
    Acc VName Shape [Type] u
  | Array PrimType shape u
  | Mem Space
  deriving (Show, Eq, Ord)

instance Bitraversable TypeBase where
  bitraverse f g (Array t shape u) = Array t <$> f shape <*> g u
  bitraverse _ _ (Prim pt) = pure $ Prim pt
  bitraverse _ g (Acc arrs ispace ts u) = Acc arrs ispace ts <$> g u
  bitraverse _ _ (Mem s) = pure $ Mem s

instance Bifunctor TypeBase where
  bimap = bimapDefault

instance Bifoldable TypeBase where
  bifoldMap = bifoldMapDefault

-- | A type with shape information, used for describing the type of
-- variables.
type Type = TypeBase Shape NoUniqueness

-- | A type with existentially quantified shapes - used as part of
-- function (and function-like) return types.  Generally only makes
-- sense when used in a list.
type ExtType = TypeBase ExtShape NoUniqueness

-- | A type with shape and uniqueness information, used declaring
-- return- and parameters types.
type DeclType = TypeBase Shape Uniqueness

-- | An 'ExtType' with uniqueness information, used for function
-- return types.
type DeclExtType = TypeBase ExtShape Uniqueness

-- | Information about which parts of a value/type are consumed.  For
-- example, we might say that a function taking three arguments of
-- types @([int], *[int], [int])@ has diet @[Observe, Consume,
-- Observe]@.
data Diet
  = -- | Consumes this value.
    Consume
  | -- | Only observes value in this position, does
    -- not consume.  A result may alias this.
    Observe
  | -- | As 'Observe', but the result will not
    -- alias, because the parameter does not carry
    -- aliases.
    ObservePrim
  deriving (Eq, Ord, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data Ident = Ident
  { identName :: VName,
    identType :: Type
  }
  deriving (Show)

instance Eq Ident where
  x == y = identName x == identName y

instance Ord Ident where
  x `compare` y = identName x `compare` identName y

-- | A list of names used for certificates in some expressions.
newtype Certificates = Certificates {unCertificates :: [VName]}
  deriving (Eq, Ord, Show)

instance Semigroup Certificates where
  Certificates x <> Certificates y = Certificates (x <> y)

instance Monoid Certificates where
  mempty = Certificates mempty

-- | A subexpression is either a scalar constant or a variable.  One
-- important property is that evaluation of a subexpression is
-- guaranteed to complete in constant time.
data SubExp
  = Constant PrimValue
  | Var VName
  deriving (Show, Eq, Ord)

-- | A function or lambda parameter.
data Param dec = Param
  { -- | Name of the parameter.
    paramName :: VName,
    -- | Function parameter decoration.
    paramDec :: dec
  }
  deriving (Ord, Show, Eq)

instance Foldable Param where
  foldMap = foldMapDefault

instance Functor Param where
  fmap = fmapDefault

instance Traversable Param where
  traverse f (Param name dec) = Param name <$> f dec

-- | How to index a single dimension of an array.
data DimIndex d
  = -- | Fix index in this dimension.
    DimFix d
  | -- | @DimSlice start_offset num_elems stride@.
    DimSlice d d d
  deriving (Eq, Ord, Show)

instance Functor DimIndex where
  fmap f (DimFix i) = DimFix $ f i
  fmap f (DimSlice i j s) = DimSlice (f i) (f j) (f s)

instance Foldable DimIndex where
  foldMap f (DimFix d) = f d
  foldMap f (DimSlice i j s) = f i <> f j <> f s

instance Traversable DimIndex where
  traverse f (DimFix d) = DimFix <$> f d
  traverse f (DimSlice i j s) = DimSlice <$> f i <*> f j <*> f s

-- | A list of 'DimFix's, indicating how an array should be sliced.
-- Whenever a function accepts a 'Slice', that slice should be total,
-- i.e, cover all dimensions of the array.  Deviators should be
-- indicated by taking a list of 'DimIndex'es instead.
data Slice d
  = DimIndices [DimIndex d]
  | DimArrs [d]
  deriving (Eq, Ord, Show)

instance Foldable Slice where
  foldMap = foldMapDefault
  length (DimIndices idxs) = length idxs
  length (DimArrs arrs) = undefined -- TODO, given by type of arrs

instance Functor Slice where
  fmap = fmapDefault

instance Traversable Slice where
  traverse f (DimIndices idxs) = DimIndices <$> traverse (traverse f) idxs
  traverse f (DimArrs arrs) = DimArrs <$> traverse f arrs

-- | If the argument is a 'DimFix', return its component.
dimFix :: DimIndex d -> Maybe d
dimFix (DimFix d) = Just d
dimFix _ = Nothing

-- | If the slice is all 'DimFix's, return the components.
sliceIndices :: Slice d -> Maybe [d]
sliceIndices (DimIndices idxs) = mapM dimFix idxs
sliceIndices (DimArrs arrs) = Nothing

-- | The dimensions of the array produced by this slice.
sliceDims :: Slice d -> [d]
sliceDims (DimIndices idxs) = mapMaybe dimSlice idxs
  where
    dimSlice (DimSlice _ d _) = Just d
    dimSlice DimFix {} = Nothing
sliceDims (DimArrs arrs) = undefined

-- | A slice with a stride of one.
unitSlice :: Num d => d -> d -> DimIndex d
unitSlice offset n = DimSlice offset n 1

-- | Fix the 'DimSlice's of a slice.  The number of indexes must equal
-- the length of 'sliceDims' for the slice.
fixSlice :: Num d => Slice d -> [d] -> [d]
fixSlice (DimIndices idxs) = fixDimIndices idxs
fixSlice (DimArrs arrs) = undefined

-- | Fix the 'DimSlice's of a slice.  The number of indexes must equal
-- the length of 'sliceDims' for the slice.
fixDimIndices :: Num d => [DimIndex d] -> [d] -> [d]
fixDimIndices (DimFix j : mis') is' =
  j : fixDimIndices mis' is'
fixDimIndices (DimSlice orig_k _ orig_s : mis') (i : is') =
  (orig_k + i * orig_s) : fixDimIndices mis' is'
fixDimIndices _ _ = []

-- | Further slice the 'DimSlice's of a slice.  The number of slices
-- must equal the length of 'sliceDims' for the slice.
sliceSlice :: Num d => Slice d -> Slice d -> Slice d
sliceSlice (DimIndices js) (DimIndices is) = DimIndices $ sliceSlice' js is

sliceSlice' :: Num d => [DimIndex d] -> [DimIndex d] -> [DimIndex d]
sliceSlice' (DimFix j : js') is' =
  DimFix j : sliceSlice' js' is'
sliceSlice' (DimSlice j _ s : js') (DimFix i : is') =
  DimFix (j + (i * s)) : sliceSlice' js' is'
sliceSlice' (DimSlice j _ s0 : js') (DimSlice i n s1 : is') =
  DimSlice (j + (s0 * i)) n (s0 * s1) : sliceSlice' js' is'
sliceSlice' _ _ = []

-- | An element of a pattern - consisting of a name and an addditional
-- parametric decoration.  This decoration is what is expected to
-- contain the type of the resulting variable.
data PatElemT dec = PatElem
  { -- | The name being bound.
    patElemName :: VName,
    -- | Pattern element decoration.
    patElemDec :: dec
  }
  deriving (Ord, Show, Eq)

instance Functor PatElemT where
  fmap = fmapDefault

instance Foldable PatElemT where
  foldMap = foldMapDefault

instance Traversable PatElemT where
  traverse f (PatElem name dec) =
    PatElem name <$> f dec

-- | An error message is a list of error parts, which are concatenated
-- to form the final message.
newtype ErrorMsg a = ErrorMsg [ErrorMsgPart a]
  deriving (Eq, Ord, Show)

instance IsString (ErrorMsg a) where
  fromString = ErrorMsg . pure . fromString

-- | A part of an error message.
data ErrorMsgPart a
  = -- | A literal string.
    ErrorString String
  | -- | A run-time integer value.
    ErrorInt32 a
  | -- | A bigger run-time integer value.
    ErrorInt64 a
  deriving (Eq, Ord, Show)

instance IsString (ErrorMsgPart a) where
  fromString = ErrorString

instance Functor ErrorMsg where
  fmap f (ErrorMsg parts) = ErrorMsg $ map (fmap f) parts

instance Foldable ErrorMsg where
  foldMap f (ErrorMsg parts) = foldMap (foldMap f) parts

instance Traversable ErrorMsg where
  traverse f (ErrorMsg parts) = ErrorMsg <$> traverse (traverse f) parts

instance Functor ErrorMsgPart where
  fmap _ (ErrorString s) = ErrorString s
  fmap f (ErrorInt32 a) = ErrorInt32 $ f a
  fmap f (ErrorInt64 a) = ErrorInt64 $ f a

instance Foldable ErrorMsgPart where
  foldMap _ ErrorString {} = mempty
  foldMap f (ErrorInt32 a) = f a
  foldMap f (ErrorInt64 a) = f a

instance Traversable ErrorMsgPart where
  traverse _ (ErrorString s) = pure $ ErrorString s
  traverse f (ErrorInt32 a) = ErrorInt32 <$> f a
  traverse f (ErrorInt64 a) = ErrorInt64 <$> f a

-- | How many non-constant parts does the error message have, and what
-- is their type?
errorMsgArgTypes :: ErrorMsg a -> [PrimType]
errorMsgArgTypes (ErrorMsg parts) = mapMaybe onPart parts
  where
    onPart ErrorString {} = Nothing
    onPart ErrorInt32 {} = Just $ IntType Int32
    onPart ErrorInt64 {} = Just $ IntType Int64
