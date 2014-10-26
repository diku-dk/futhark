module Futhark.Representation.AST.Syntax.Core
       (
         module Language.Futhark.Core

         -- * Types
         , Uniqueness(..)
         , Shape(..)
         , ExtDimSize(..)
         , ExtShape(..)
         , Rank(..)
         , ArrayShape(..)
         , TypeBase(..)
         , Type
         , DeclType
         , ExtType
         , Diet(..)

         -- * Values
         , BasicValue(..)
         , Value(..)

         -- * Abstract syntax tree
         , IdentBase(..)
         , Ident
         , Param
         , Certificates
         , SubExp(..)
         , BindeeT(..)
         , Bindee

         -- * Miscellaneous
         , Names
         ) where

import Data.Array
import Data.Hashable
import Data.Loc
import Data.Monoid
import Data.Ord
import qualified Data.HashSet as HS

import Language.Futhark.Core

-- | The size of an array type as a list of its dimension sizes.  If a
-- variable, that variable must be in scope where this array is used.
-- When compared for equality, only the number of dimensions is
-- considered.
newtype Shape = Shape { shapeDims :: [SubExp] }
              deriving (Show)

instance Eq Shape where
  Shape l1 == Shape l2 = length l1 == length l2

instance Ord Shape where
  compare = comparing shapeRank

-- | The size of this dimension.
data ExtDimSize = Free SubExp -- ^ Some known dimension.
                | Ext Int -- ^ Existentially quantified.
                  deriving (Show)

-- | Like 'Shape' but some of its elements may be bound in a local
-- environment instead.  These are denoted with integral indices.
newtype ExtShape = ExtShape { extShapeDims :: [ExtDimSize] }
                 deriving (Show)

instance Eq ExtShape where
  ExtShape l1 == ExtShape l2 = length l1 == length l2

instance Ord ExtShape where
  compare = comparing shapeRank

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

instance Monoid Shape where
  mempty = Shape mempty
  Shape l1 `mappend` Shape l2 = Shape $ l1 `mappend` l2

instance ArrayShape Shape where
  shapeRank (Shape l) = length l
  stripDims n (Shape dims) = Shape $ drop n dims

instance Monoid ExtShape where
  mempty = ExtShape mempty
  ExtShape l1 `mappend` ExtShape l2 = ExtShape $ l1 `mappend` l2

instance ArrayShape ExtShape where
  shapeRank (ExtShape l) = length l
  stripDims n (ExtShape dims) = ExtShape $ drop n dims

instance Monoid Rank where
  mempty = Rank 0
  Rank x `mappend` Rank y = Rank $ x + y

instance ArrayShape Rank where
  shapeRank (Rank x) = x
  stripDims n (Rank x) = Rank $ x - n

-- | An Futhark type is either an array or an element type.  When comparing
-- types for equality with '==', aliases are ignored, as are
-- dimension sizes (but not the number of dimensions themselves).
data TypeBase shape = Basic BasicType
                    | Array BasicType shape Uniqueness
                    | Mem SubExp
                      -- ^ 1st arg: array's element type, 2nd arg:
                      -- lengths of dimensions, 3rd arg: uniqueness
                      -- attribute
                    deriving (Show, Eq, Ord)

-- | A type with shape information, used for describing the type of
-- a computation.
type Type = TypeBase Shape

-- | A type without shape information, used for declarations.
type DeclType = TypeBase Rank

-- | A type with existentially quantified shapes - used as part of
-- function (and function-like) return types.  Generally only makes
-- sense when used in a list.
type ExtType = TypeBase ExtShape

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
data Value = BasicVal BasicValue
           | ArrayVal !(Array Int Value) DeclType
             -- ^ It is assumed that the array is 0-indexed.  The type
             -- is the row type.
             deriving (Eq, Ord, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data IdentBase shape = Ident { identName :: VName
                             , identType :: TypeBase shape
                             , identSrcLoc :: SrcLoc
                             }
                    deriving (Show)

-- | A name with aliasing information.  Used for normal variables.
type Ident = IdentBase Shape

-- | A name with no aliasing information.  These are used for function
-- parameters.
type Param = IdentBase Shape

instance Eq (IdentBase shape) where
  x == y = identName x == identName y

instance Ord (IdentBase shape) where
  x `compare` y = identName x `compare` identName y

instance Located (IdentBase shape) where
  locOf = locOf . identSrcLoc

instance Hashable (IdentBase shape) where
  hashWithSalt salt = hashWithSalt salt . identName

-- | A list of identifiers used for certificates in some expressions.
type Certificates = [Ident]

-- | A subexpression is either a constant or a variable.  One
-- important property is that evaluation of a subexpression is
-- guaranteed to complete in constant time.
data SubExp = Constant Value SrcLoc
            | Var      Ident
            deriving (Show, Eq, Ord)

instance Located SubExp where
  locOf (Constant _ loc) = locOf loc
  locOf (Var ident)      = locOf ident

data BindeeT annot = Bindee { bindeeIdent :: Ident
                            , bindeeLore  :: annot
                            }
                     deriving (Ord, Show, Eq)

instance Located (BindeeT annot) where
  locOf = locOf . bindeeIdent

type Bindee = BindeeT

-- | A set of names.
type Names = HS.HashSet VName
