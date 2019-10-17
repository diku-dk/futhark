{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The most primitive ("core") aspects of the AST.  Split out of
-- "Futhark.Representation.AST.Syntax" in order for
-- "Futhark.Representation.AST.Annotations" to use these definitions.  This
-- module is re-exported from "Futhark.Representation.AST.Syntax" and
-- there should be no reason to include it explicitly.
module Futhark.Representation.AST.Syntax.Core
       (
           module Language.Futhark.Core
         , module Futhark.Representation.Primitive

         -- * Types
         , Uniqueness(..)
         , NoUniqueness(..)
         , ShapeBase(..)
         , Shape
         , Ext(..)
         , ExtSize
         , ExtShape
         , Rank(..)
         , ArrayShape(..)
         , Space (..)
         , SpaceId
         , TypeBase(..)
         , Type
         , ExtType
         , DeclType
         , DeclExtType
         , Diet(..)
         , ErrorMsg (..)
         , ErrorMsgPart (..)

         -- * Values
         , PrimValue(..)

         -- * Abstract syntax tree
         , Ident (..)
         , Certificates(..)
         , SubExp(..)
         , Param (..)
         , DimIndex (..)
         , Slice
         , dimFix
         , sliceIndices
         , sliceDims
         , unitSlice
         , fixSlice
         , PatElemT (..)
         ) where

import Control.Monad.State
import Data.Maybe
import Data.String
import qualified Data.Map.Strict as M
import Data.Traversable

import Language.Futhark.Core
import Futhark.Representation.Primitive

-- | The size of an array type as a list of its dimension sizes, with
-- the type of sizes being parametric.
newtype ShapeBase d = Shape { shapeDims :: [d] }
                    deriving (Eq, Ord, Show)

-- | The size of an array as a list of subexpressions.  If a variable,
-- that variable must be in scope where this array is used.
type Shape = ShapeBase SubExp

-- | Something that may be existential.
data Ext a = Ext Int
           | Free a
           deriving (Eq, Ord, Show)

-- | The size of this dimension.
type ExtSize = Ext SubExp

-- | Like 'Shape' but some of its elements may be bound in a local
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

instance Semigroup (ShapeBase d) where
  Shape l1 <> Shape l2 = Shape $ l1 `mappend` l2

instance Monoid (ShapeBase d) where
  mempty = Shape mempty

instance Functor ShapeBase where
  fmap f = Shape . map f . shapeDims

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
    length ds1 == length ds2 &&
    evalState (and <$> zipWithM subDimOf ds1 ds2) M.empty
    where subDimOf (Free se1) (Free se2) = return $ se1 == se2
          subDimOf (Ext _)    (Free _)   = return False
          subDimOf (Free _)   (Ext _)    = return True
          subDimOf (Ext x)    (Ext y)    = do
            extmap <- get
            case M.lookup y extmap of
              Just ywas | ywas == x -> return True
                        | otherwise -> return False
              Nothing -> do put $ M.insert y x extmap
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
-- space, whatever that is.  The exact meaning of the 'SpaceID'
-- depends on the backend used.  In GPU kernels, for example, this is
-- used to distinguish between constant, global and shared memory
-- spaces.  In GPU-enabled host code, it is used to distinguish
-- between host memory ('DefaultSpace') and GPU space.
data Space = DefaultSpace
           | Space SpaceId
             deriving (Show, Eq, Ord)

-- | A string representing a specific non-default memory space.
type SpaceId = String

-- | A fancier name for '()' - encodes no uniqueness information.
data NoUniqueness = NoUniqueness
                  deriving (Eq, Ord, Show)

-- | An Futhark type is either an array or an element type.  When
-- comparing types for equality with '==', shapes must match.
data TypeBase shape u = Prim PrimType
                      | Array PrimType shape u
                      | Mem Space
                    deriving (Show, Eq, Ord)

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
data Diet = Consume -- ^ Consumes this value.
          | Observe -- ^ Only observes value in this position, does
                    -- not consume.  A result may alias this.
          | ObservePrim -- ^ As 'Observe', but the result will not
                        -- alias, because the parameter does not carry
                        -- aliases.
            deriving (Eq, Ord, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data Ident = Ident { identName :: VName
                   , identType :: Type
                   }
               deriving (Show)

instance Eq Ident where
  x == y = identName x == identName y

instance Ord Ident where
  x `compare` y = identName x `compare` identName y

-- | A list of names used for certificates in some expressions.
newtype Certificates = Certificates { unCertificates :: [VName] }
                     deriving (Eq, Ord, Show)

instance Semigroup Certificates where
  Certificates x <> Certificates y = Certificates (x <> y)

instance Monoid Certificates where
  mempty = Certificates mempty

-- | A subexpression is either a scalar constant or a variable.  One
-- important property is that evaluation of a subexpression is
-- guaranteed to complete in constant time.
data SubExp = Constant PrimValue
            | Var      VName
            deriving (Show, Eq, Ord)

-- | A function or lambda parameter.
data Param attr = Param
                  { paramName :: VName
                    -- ^ Name of the parameter.
                  , paramAttr :: attr
                    -- ^ Function parameter attribute.
                  }
                  deriving (Ord, Show, Eq)

instance Foldable Param where
  foldMap = foldMapDefault

instance Functor Param where
  fmap = fmapDefault

instance Traversable Param where
  traverse f (Param name attr) = Param name <$> f attr

-- | How to index a single dimension of an array.
data DimIndex d = DimFix
                  d -- ^ Fix index in this dimension.
                | DimSlice d d d
                  -- ^ @DimSlice start_offset num_elems stride@.
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
type Slice d = [DimIndex d]

-- | If the argument is a 'DimFix', return its component.
dimFix :: DimIndex d -> Maybe d
dimFix (DimFix d) = Just d
dimFix _ = Nothing

-- | If the slice is all 'DimFix's, return the components.
sliceIndices :: Slice d -> Maybe [d]
sliceIndices = mapM dimFix

-- | The dimensions of the array produced by this slice.
sliceDims :: Slice d -> [d]
sliceDims = mapMaybe dimSlice
  where dimSlice (DimSlice _ d _) = Just d
        dimSlice DimFix{}         = Nothing

-- | A slice with a stride of one.
unitSlice :: Num d => d -> d -> DimIndex d
unitSlice offset n = DimSlice offset n 1

-- | Fix the 'DimSlice's of a slice.  The number of indexes must equal
-- the length of 'sliceDims' for the slice.
fixSlice :: Num d => Slice d -> [d] -> [d]
fixSlice (DimFix j:mis') is' =
  j : fixSlice mis' is'
fixSlice (DimSlice orig_k _ orig_s:mis') (i:is') =
  (orig_k+i*orig_s) : fixSlice mis' is'
fixSlice _ _ = []

-- | An element of a pattern - consisting of a name (essentially a
-- pair of the name and type) and an addditional parametric attribute.
-- This attribute is what is expected to contain the type of the
-- resulting variable.
data PatElemT attr = PatElem { patElemName :: VName
                               -- ^ The name being bound.
                             , patElemAttr :: attr
                               -- ^ Pattern element attribute.
                             }
                   deriving (Ord, Show, Eq)

instance Functor PatElemT where
  fmap f (PatElem name attr) = PatElem name (f attr)

-- | An error message is a list of error parts, which are concatenated
-- to form the final message.
newtype ErrorMsg a = ErrorMsg [ErrorMsgPart a]
  deriving (Eq, Ord, Show)

instance IsString (ErrorMsg a) where
  fromString = ErrorMsg . pure . fromString

-- | A part of an error message.
data ErrorMsgPart a = ErrorString String -- ^ A literal string.
                    | ErrorInt32 a -- ^ A run-time integer value.
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

instance Foldable ErrorMsgPart where
  foldMap _ ErrorString{} = mempty
  foldMap f (ErrorInt32 a) = f a

instance Traversable ErrorMsgPart where
  traverse _ (ErrorString s) = pure $ ErrorString s
  traverse f (ErrorInt32 a) = ErrorInt32 <$> f a
