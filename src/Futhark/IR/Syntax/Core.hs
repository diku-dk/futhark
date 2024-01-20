{-# LANGUAGE Strict #-}

-- | The most primitive ("core") aspects of the AST.  Split out of
-- "Futhark.IR.Syntax" in order for
-- "Futhark.IR.Rep" to use these definitions.  This
-- module is re-exported from "Futhark.IR.Syntax" and
-- there should be no reason to include it explicitly.
module Futhark.IR.Syntax.Core
  ( module Language.Futhark.Core,
    module Language.Futhark.Primitive,

    -- * Types
    Commutativity (..),
    Uniqueness (..),
    ShapeBase (..),
    Shape,
    stripDims,
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

    -- * Entry point information
    ValueType (..),
    OpaqueType (..),
    OpaqueTypes (..),
    Signedness (..),
    EntryPointType (..),

    -- * Attributes
    Attr (..),
    Attrs (..),
    oneAttr,
    inAttrs,
    withoutAttrs,
    mapAttrs,

    -- * Values
    PrimValue (..),

    -- * Abstract syntax tree
    Ident (..),
    Certs (..),
    SubExp (..),
    Param (..),
    DimIndex (..),
    Slice (..),
    dimFix,
    sliceIndices,
    sliceDims,
    sliceShape,
    unitSlice,
    fixSlice,
    sliceSlice,
    PatElem (..),

    -- * Flat (LMAD) slices
    FlatSlice (..),
    FlatDimIndex (..),
    flatSliceDims,
    flatSliceStrides,
  )
where

import Control.Category
import Control.Monad
import Control.Monad.State
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.String
import Data.Text qualified as T
import Data.Traversable (fmapDefault, foldMapDefault)
import Language.Futhark.Core
import Language.Futhark.Primitive
import Prelude hiding (id, (.))

-- | Whether some operator is commutative or not.  The 'Monoid'
-- instance returns the least commutative of its arguments.
data Commutativity
  = Noncommutative
  | Commutative
  deriving (Eq, Ord, Show)

instance Semigroup Commutativity where
  (<>) = min

instance Monoid Commutativity where
  mempty = Commutative

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

-- | @stripDims n shape@ strips the outer @n@ dimensions from
-- @shape@.
stripDims :: Int -> ShapeBase d -> ShapeBase d
stripDims n (Shape dims) = Shape $ drop n dims

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

  -- | Check whether one shape if a subset of another shape.
  subShapeOf :: a -> a -> Bool

instance ArrayShape (ShapeBase SubExp) where
  shapeRank (Shape l) = length l
  subShapeOf = (==)

instance ArrayShape (ShapeBase ExtSize) where
  shapeRank (Shape l) = length l
  subShapeOf (Shape ds1) (Shape ds2) =
    -- Must agree on Free dimensions, and ds1 may not be existential
    -- where ds2 is Free.  Existentials must also be congruent.
    length ds1 == length ds2
      && evalState (and <$> zipWithM subDimOf ds1 ds2) M.empty
    where
      subDimOf (Free se1) (Free se2) = pure $ se1 == se2
      subDimOf (Ext _) (Free _) = pure False
      subDimOf (Free _) (Ext _) = pure True
      subDimOf (Ext x) (Ext y) = do
        extmap <- get
        case M.lookup y extmap of
          Just ywas
            | ywas == x -> pure True
            | otherwise -> pure False
          Nothing -> do
            put $ M.insert y x extmap
            pure True

instance Semigroup Rank where
  Rank x <> Rank y = Rank $ x + y

instance Monoid Rank where
  mempty = Rank 0

instance ArrayShape Rank where
  shapeRank (Rank x) = x
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

instance Functor (TypeBase shape) where
  fmap = fmapDefault

instance Foldable (TypeBase shape) where
  foldMap = foldMapDefault

instance Traversable (TypeBase shape) where
  traverse = bitraverse pure

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
newtype Certs = Certs {unCerts :: [VName]}
  deriving (Eq, Ord, Show)

instance Semigroup Certs where
  Certs x <> Certs y = Certs (x <> filter (`notElem` x) y)

instance Monoid Certs where
  mempty = Certs mempty

-- | A subexpression is either a scalar constant or a variable.  One
-- important property is that evaluation of a subexpression is
-- guaranteed to complete in constant time.
data SubExp
  = Constant PrimValue
  | Var VName
  deriving (Show, Eq, Ord)

-- | A function or lambda parameter.
data Param dec = Param
  { -- | Attributes of the parameter.  When constructing a parameter,
    -- feel free to just pass 'mempty'.
    paramAttrs :: Attrs,
    -- | Name of the parameter.
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
  traverse f (Param attr name dec) = Param attr name <$> f dec

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

-- | A list of 'DimIndex's, indicating how an array should be sliced.
-- Whenever a function accepts a 'Slice', that slice should be total,
-- i.e, cover all dimensions of the array.  Deviators should be
-- indicated by taking a list of 'DimIndex'es instead.
newtype Slice d = Slice {unSlice :: [DimIndex d]}
  deriving (Eq, Ord, Show)

instance Traversable Slice where
  traverse f = fmap Slice . traverse (traverse f) . unSlice

instance Functor Slice where
  fmap = fmapDefault

instance Foldable Slice where
  foldMap = foldMapDefault

-- | If the argument is a 'DimFix', return its component.
dimFix :: DimIndex d -> Maybe d
dimFix (DimFix d) = Just d
dimFix _ = Nothing

-- | If the slice is all 'DimFix's, return the components.
sliceIndices :: Slice d -> Maybe [d]
sliceIndices = mapM dimFix . unSlice

-- | The dimensions of the array produced by this slice.
sliceDims :: Slice d -> [d]
sliceDims = mapMaybe dimSlice . unSlice
  where
    dimSlice (DimSlice _ d _) = Just d
    dimSlice DimFix {} = Nothing

-- | The shape of the array produced by this slice.
sliceShape :: Slice d -> ShapeBase d
sliceShape = Shape . sliceDims

-- | A slice with a stride of one.
unitSlice :: (Num d) => d -> d -> DimIndex d
unitSlice offset n = DimSlice offset n 1

-- | Fix the 'DimSlice's of a slice.  The number of indexes must equal
-- the length of 'sliceDims' for the slice.
fixSlice :: (Num d) => Slice d -> [d] -> [d]
fixSlice = fixSlice' . unSlice
  where
    fixSlice' (DimFix j : mis') is' =
      j : fixSlice' mis' is'
    fixSlice' (DimSlice orig_k _ orig_s : mis') (i : is') =
      (orig_k + i * orig_s) : fixSlice' mis' is'
    fixSlice' _ _ = []

-- | Further slice the 'DimSlice's of a slice.  The number of slices
-- must equal the length of 'sliceDims' for the slice.
sliceSlice :: (Num d) => Slice d -> Slice d -> Slice d
sliceSlice (Slice jslice) (Slice islice) = Slice $ sliceSlice' jslice islice
  where
    sliceSlice' (DimFix j : js') is' =
      DimFix j : sliceSlice' js' is'
    sliceSlice' (DimSlice j _ s : js') (DimFix i : is') =
      DimFix (j + (i * s)) : sliceSlice' js' is'
    sliceSlice' (DimSlice j _ s0 : js') (DimSlice i n s1 : is') =
      DimSlice (j + (s0 * i)) n (s0 * s1) : sliceSlice' js' is'
    sliceSlice' _ _ = []

-- | A dimension in a 'FlatSlice'.
data FlatDimIndex d
  = FlatDimIndex
      -- | Number of elements in dimension
      d
      -- | Stride of dimension
      d
  deriving (Eq, Ord, Show)

instance Traversable FlatDimIndex where
  traverse f (FlatDimIndex n s) = FlatDimIndex <$> f n <*> f s

instance Functor FlatDimIndex where
  fmap = fmapDefault

instance Foldable FlatDimIndex where
  foldMap = foldMapDefault

-- | A flat slice is a way of viewing a one-dimensional array as a
-- multi-dimensional array, using a more compressed mechanism than
-- reshaping and using 'Slice'.  The initial @d@ is an offset, and the
-- list then specifies the shape of the resulting array.
data FlatSlice d = FlatSlice d [FlatDimIndex d]
  deriving (Eq, Ord, Show)

instance Traversable FlatSlice where
  traverse f (FlatSlice offset is) =
    FlatSlice <$> f offset <*> traverse (traverse f) is

instance Functor FlatSlice where
  fmap = fmapDefault

instance Foldable FlatSlice where
  foldMap = foldMapDefault

-- | The dimensions (shape) of the view produced by a flat slice.
flatSliceDims :: FlatSlice d -> [d]
flatSliceDims (FlatSlice _ ds) = map dimSlice ds
  where
    dimSlice (FlatDimIndex n _) = n

-- | The strides of each dimension produced by a flat slice.
flatSliceStrides :: FlatSlice d -> [d]
flatSliceStrides (FlatSlice _ ds) = map dimStride ds
  where
    dimStride (FlatDimIndex _ s) = s

-- | An element of a pattern - consisting of a name and an addditional
-- parametric decoration.  This decoration is what is expected to
-- contain the type of the resulting variable.
data PatElem dec = PatElem
  { -- | The name being bound.
    patElemName :: VName,
    -- | Pat element decoration.
    patElemDec :: dec
  }
  deriving (Ord, Show, Eq)

instance Functor PatElem where
  fmap = fmapDefault

instance Foldable PatElem where
  foldMap = foldMapDefault

instance Traversable PatElem where
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
    ErrorString T.Text
  | -- | A run-time value.
    ErrorVal PrimType a
  deriving (Eq, Ord, Show)

instance IsString (ErrorMsgPart a) where
  fromString = ErrorString . T.pack

instance Functor ErrorMsg where
  fmap f (ErrorMsg parts) = ErrorMsg $ map (fmap f) parts

instance Foldable ErrorMsg where
  foldMap f (ErrorMsg parts) = foldMap (foldMap f) parts

instance Traversable ErrorMsg where
  traverse f (ErrorMsg parts) = ErrorMsg <$> traverse (traverse f) parts

instance Functor ErrorMsgPart where
  fmap = fmapDefault

instance Foldable ErrorMsgPart where
  foldMap = foldMapDefault

instance Traversable ErrorMsgPart where
  traverse _ (ErrorString s) = pure $ ErrorString s
  traverse f (ErrorVal t a) = ErrorVal t <$> f a

-- | How many non-constant parts does the error message have, and what
-- is their type?
errorMsgArgTypes :: ErrorMsg a -> [PrimType]
errorMsgArgTypes (ErrorMsg parts) = mapMaybe onPart parts
  where
    onPart ErrorString {} = Nothing
    onPart (ErrorVal t _) = Just t

-- | A single attribute.
data Attr
  = AttrName Name
  | AttrInt Integer
  | AttrComp Name [Attr]
  deriving (Ord, Show, Eq)

instance IsString Attr where
  fromString = AttrName . fromString

-- | Every statement is associated with a set of attributes, which can
-- have various effects throughout the compiler.
newtype Attrs = Attrs {unAttrs :: S.Set Attr}
  deriving (Ord, Show, Eq, Monoid, Semigroup)

-- | Construct 'Attrs' from a single 'Attr'.
oneAttr :: Attr -> Attrs
oneAttr = Attrs . S.singleton

-- | Is the given attribute to be found in the attribute set?
inAttrs :: Attr -> Attrs -> Bool
inAttrs attr (Attrs attrs) = attr `S.member` attrs

-- | @x `withoutAttrs` y@ gives @x@ except for any attributes also in @y@.
withoutAttrs :: Attrs -> Attrs -> Attrs
withoutAttrs (Attrs x) (Attrs y) = Attrs $ x `S.difference` y

-- | Map a function over an attribute set.
mapAttrs :: (Attr -> a) -> Attrs -> [a]
mapAttrs f (Attrs attrs) = map f $ S.toList attrs

-- | Since the core language does not care for signedness, but the
-- source language does, entry point input/output information has
-- metadata for integer types (and arrays containing these) that
-- indicate whether they are really unsigned integers.  This doesn't
-- matter for non-integer types.
data Signedness
  = Unsigned
  | Signed
  deriving (Eq, Ord, Show)

-- | An actual non-opaque type that can be passed to and from Futhark
-- programs, or serve as the contents of opaque types.  Scalars are
-- represented with zero rank.
data ValueType
  = ValueType Signedness Rank PrimType
  deriving (Eq, Ord, Show)

-- | Every entry point argument and return value has an annotation
-- indicating how it maps to the original source program type.
data EntryPointType
  = -- | An opaque type of this name.
    TypeOpaque Name
  | -- | A transparent type, which is scalar if the rank is zero.
    TypeTransparent ValueType
  deriving (Eq, Show, Ord)

-- | The representation of an opaque type.
data OpaqueType
  = OpaqueType [ValueType]
  | -- | Note that the field ordering here denote the actual
    -- representation - make sure it is preserved.
    OpaqueRecord [(Name, EntryPointType)]
  | -- | Constructor ordering also denotes representation, in that the
    -- index of the constructor is the identifying number.
    --
    -- The total values used to represent a sum values is the
    -- 'ValueType' list. The 'Int's associated with each
    -- 'EntryPointType' are the indexes of the values used to
    -- represent that constructor payload. This is necessary because
    -- we deduplicate payloads across constructors.
    OpaqueSum [ValueType] [(Name, [(EntryPointType, [Int])])]
  deriving (Eq, Ord, Show)

-- | Names of opaque types and their representation.
newtype OpaqueTypes = OpaqueTypes [(Name, OpaqueType)]
  deriving (Eq, Ord, Show)

instance Monoid OpaqueTypes where
  mempty = OpaqueTypes mempty

instance Semigroup OpaqueTypes where
  OpaqueTypes x <> OpaqueTypes y =
    OpaqueTypes $ x <> filter ((`notElem` map fst x) . fst) y
