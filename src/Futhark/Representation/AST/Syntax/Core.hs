{-# LANGUAGE FlexibleContexts #-}
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
         , Shape(..)
         , ExtDimSize(..)
         , ExtShape(..)
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

         -- * Values
         , PrimValue(..)
         , Value(..)

         -- * Abstract syntax tree
         , Ident (..)
         , Certificates
         , SubExp(..)
         , ParamT (..)
         , Param
         , Bindage (..)
         , DimIndex (..)
         , Slice
         , dimFix
         , sliceIndices
         , sliceDims
         , PatElemT (..)

         -- * Miscellaneous
         , Names
         ) where

import Control.Applicative
import Control.Monad.State
import Data.Array
import Data.Foldable hiding (and)
import Data.Traversable hiding (mapM)
import Data.Hashable
import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Prelude

import Language.Futhark.Core
import Futhark.Representation.Primitive

-- | The size of an array type as a list of its dimension sizes.  If a
-- variable, that variable must be in scope where this array is used.
newtype Shape = Shape { shapeDims :: [SubExp] }
              deriving (Eq, Ord, Show)

-- | The size of this dimension.
data ExtDimSize = Free SubExp -- ^ Some known dimension.
                | Ext Int -- ^ Existentially quantified.
                  deriving (Eq, Ord, Show)

-- | Like 'Shape' but some of its elements may be bound in a local
-- environment instead.  These are denoted with integral indices.
newtype ExtShape = ExtShape { extShapeDims :: [ExtDimSize] }
                 deriving (Eq, Ord, Show)

-- | The size of an array type as merely the number of dimensions,
-- with no further information.
data Rank = Rank Int
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

instance Monoid Shape where
  mempty = Shape mempty
  Shape l1 `mappend` Shape l2 = Shape $ l1 `mappend` l2

instance ArrayShape Shape where
  shapeRank (Shape l) = length l
  stripDims n (Shape dims) = Shape $ drop n dims
  subShapeOf = (==)

instance Monoid ExtShape where
  mempty = ExtShape mempty
  ExtShape l1 `mappend` ExtShape l2 = ExtShape $ l1 `mappend` l2

instance ArrayShape ExtShape where
  shapeRank (ExtShape l) = length l
  stripDims n (ExtShape dims) = ExtShape $ drop n dims
  subShapeOf (ExtShape ds1) (ExtShape ds2) =
    -- Must agree on Free dimensions, and ds1 may not be existential
    -- where ds2 is Free.  Existentials must also be congruent.
    length ds1 == length ds2 &&
    evalState (and <$> zipWithM subDimOf ds1 ds2) HM.empty
    where subDimOf (Free se1) (Free se2) = return $ se1 == se2
          subDimOf (Ext _)    (Free _)   = return False
          subDimOf (Free _)   (Ext _)    = return True
          subDimOf (Ext x)    (Ext y)    = do
            extmap <- get
            case HM.lookup y extmap of
              Just ywas | ywas == x -> return True
                        | otherwise -> return False
              Nothing -> do put $ HM.insert y x extmap
                            return True

instance Monoid Rank where
  mempty = Rank 0
  Rank x `mappend` Rank y = Rank $ x + y

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
                      | Mem SubExp Space
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
                    -- not consume.
            deriving (Eq, Ord, Show)

-- | Every possible value in Futhark.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = PrimVal !PrimValue
           | ArrayVal !(Array Int PrimValue) PrimType [Int]
             -- ^ It is assumed that the array is 0-indexed.
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

instance Hashable Ident where
  hashWithSalt salt = hashWithSalt salt . identName

-- | A list of names used for certificates in some expressions.
type Certificates = [VName]

-- | A subexpression is either a scalar constant or a variable.  One
-- important property is that evaluation of a subexpression is
-- guaranteed to complete in constant time.
data SubExp = Constant PrimValue
            | Var      VName
            deriving (Show, Eq, Ord)

-- | A function parameter.
data ParamT attr = Param
                   { paramName :: VName
                     -- ^ Name of the parameter.
                   , paramAttr :: attr
                     -- ^ Function parameter attribute.
                   }
                   deriving (Ord, Show, Eq)

-- | A type alias for namespace control.
type Param = ParamT

instance Functor ParamT where
  fmap f (Param name attr) = Param name (f attr)


-- | How to index a single dimension of an array.
data DimIndex d = DimFix
                  d -- ^ Fix index in this dimension.
                | DimSlice d d
                  -- ^ A slice starting from there and continuing for
                  -- this many elements.
                  deriving (Eq, Ord, Show)

instance Functor DimIndex where
  fmap f (DimFix i) = DimFix $ f i
  fmap f (DimSlice i j) = DimSlice (f i) (f j)

instance Foldable DimIndex where
  foldMap f (DimFix d) = f d
  foldMap f (DimSlice i j) = f i <> f j

instance Traversable DimIndex where
  traverse f (DimFix d) = DimFix <$> f d
  traverse f (DimSlice i j) = DimSlice <$> f i <*> f j

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
  where dimSlice (DimSlice _ d) = Just d
        dimSlice DimFix{}       = Nothing

-- | How a name in a let-binding is bound - either as a plain
-- variable, or in the form of an in-place update.
data Bindage = BindVar -- ^ Bind as normal.
             | BindInPlace Certificates VName (Slice SubExp)
               -- ^ Perform an in-place update, in which the value
               -- being bound is inserted at the given index in the
               -- array referenced by the 'VName'.  Note that the
               -- result of the binding is the entire array, not just
               -- the value that has been inserted..  The
               -- 'Certificates' contain bounds checking certificates
               -- (if necessary).
                  deriving (Ord, Show, Eq)

-- | An element of a pattern - consisting of an name (essentially a
-- pair of the name andtype), a 'Bindage', and an addditional
-- parametric attribute.  This attribute is what is expected to
-- contain the type of the resulting variable.
data PatElemT attr = PatElem { patElemName :: VName
                               -- ^ The name being bound.
                             , patElemBindage :: Bindage
                               -- ^ How the name is bound.
                             , patElemAttr :: attr
                               -- ^ Pattern element attribute.
                             }
                   deriving (Ord, Show, Eq)

instance Functor PatElemT where
  fmap f (PatElem name bindage attr) = PatElem name bindage (f attr)

-- | A set of names.
type Names = HS.HashSet VName
