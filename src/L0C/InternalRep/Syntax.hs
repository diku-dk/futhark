-- | This Is an ever-changing abstract syntax for L0.  Some types,
-- such as @Exp@, are parametrised by type and name representation.
-- See the @doc/@ subdirectory in the L0 repository for a language
-- reference, or this module may be a little hard to understand.
module L0C.InternalRep.Syntax
  (
    module Language.L0.Core

  -- * Types
  , Uniqueness(..)
  , DimSize
  , Shape(..)
  , Rank(..)
  , ArrayShape(..)
  , TypeBase(..)
  , Type
  , DeclType
  , ExpType
  , ConstType
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
  , Body(..)
  , Exp(..)
  , Lambda(..)

  -- * Definitions
  , FunDec
  , Prog(..)

  -- * Miscellaneous
  , Names
  )
  where

import Data.Array
import Data.Hashable
import Data.Loc
import Data.Monoid
import Data.Ord
import qualified Data.HashSet as HS

import Language.L0.Core

-- | The size of this dimension.
type DimSize = SubExp

-- | The size of an array type as a list of its dimension sizes.  If a
-- variable, that variable must be in scope where this array is used.
-- When compared for equality, only the number of dimensions is
-- considered.
newtype Shape = Shape { shapeDims :: [DimSize] }
  deriving (Show)

instance Eq Shape where
  Shape l1 == Shape l2 = length l1 == length l2

instance Ord Shape where
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

instance Monoid Rank where
  mempty = Rank 0
  Rank x `mappend` Rank y = Rank $ x + y

instance ArrayShape Rank where
  shapeRank (Rank x) = x
  stripDims n (Rank x) = Rank $ x - n

-- | An L0 type is either an array or an element type.  When comparing
-- types for equality with '==', aliases are ignored, as are
-- dimension sizes (but not the number of dimensions themselves).
data TypeBase as shape = Basic BasicType
                       | Array BasicType shape Uniqueness as
                         -- ^ 1st arg: array's element type, 2nd arg:
                         -- lengths of dimensions, 3rd arg: uniqueness
                         -- attribute, 4th arg: aliasing information.
                         deriving (Show)

instance ArrayShape shape => Eq (TypeBase als shape) where
  Basic et1 == Basic et2 = et1 == et2
  Array et1 dims1 u1 _ == Array et2 dims2 u2 _ =
    et1 == et2 && u1 == u2 && dims1 == dims2
  _ == _ = False

instance ArrayShape shape => Ord (TypeBase als shape) where
  Basic et1 <= Basic et2 =
    et1 <= et2
  Array et1 dims1 u1 _ <= Array et2 dims2 u2 _
    | et1 < et2                   = True
    | et1 > et2                   = False
    | dims1 < dims2 = True
    | dims1 > dims2 = False
    | u1 < u2                     = True
    | u1 > u2                     = False
    | otherwise                   = True
  Basic {} <= Array {} = True
  Array {} <= Basic {} = False

-- | A type with aliasing information, used for describing the type of
-- a computation.
type Type = TypeBase Names Shape

-- | A type without aliasing information, used for declarations.
type DeclType = TypeBase () Rank

-- | A type with aliasing information, but without shape information.
-- This is used when computing the type of expressions, as the shape
-- information generally cannot be deduced from the expression in
-- isolation.
type ExpType = TypeBase Names Rank

-- | A type with shape information, but no aliasing information.  Can
-- be used for parameters and constants.
type ConstType = TypeBase () Shape

-- | Information about which parts of a value/type are consumed.  For
-- example, we might say that a function taking three arguments of
-- types @([int], *[int], [int])@ has diet @[Observe, Consume,
-- Observe]@.
data Diet = Consume -- ^ Consumes this value.
          | Observe -- ^ Only observes value in this position, does
                    -- not consume.
            deriving (Eq, Ord, Show)

-- | Every possible value in L0.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = BasicVal BasicValue
           | ArrayVal !(Array Int Value) DeclType
             -- ^ It is assumed that the array is 0-indexed.  The type
             -- is the row type.
             deriving (Eq, Ord, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data IdentBase als shape = Ident { identName :: VName
                                 , identType :: TypeBase als shape
                                 , identSrcLoc :: SrcLoc
                                 }
                    deriving (Show)

-- | A name with aliasing information.  Used for normal variables.
type Ident = IdentBase Names Shape

-- | A name with no aliasing information.  These are used for function
-- parameters.
type Param = IdentBase () Shape

instance Eq (IdentBase als shape) where
  x == y = identName x == identName y

instance Ord (IdentBase als shape) where
  x `compare` y = identName x `compare` identName y

instance Located (IdentBase als shape) where
  locOf = locOf . identSrcLoc

instance Hashable (IdentBase als shape) where
  hashWithSalt salt = hashWithSalt salt . identName

-- | A list of identifiers used for certificates in some expressions.
type Certificates = [Ident]

-- | A subexpression is either a constant or a variable.  One
-- important property is that evaluation of a subexpression is
-- guaranteed to complete in constant time.
data SubExp = Constant Value SrcLoc
            | Var      Ident
              deriving (Eq, Ord, Show)

instance Located SubExp where
  locOf (Constant _ loc) = locOf loc
  locOf (Var ident)      = locOf ident

-- | A body consists of a number of bindings, terminating in a result
-- (essentially a tuple literal).
data Body = LetPat [Ident] Exp Body SrcLoc
          | DoLoop
            [(Ident,SubExp)] -- Merge variable pattern and initial
                             -- values.
            Ident -- Iterator.
            SubExp -- Upper bound.
            Body -- Loop body.
            Body -- Let-body.
            SrcLoc
          | LetWith Certificates Ident Ident
            [SubExp] SubExp
            Body SrcLoc
          | Result Certificates [SubExp] SrcLoc
            deriving (Eq, Ord, Show)

instance Located Body where
  locOf (LetPat _ _ _ loc)        = locOf loc
  locOf (DoLoop _ _ _ _ _ loc)    = locOf loc
  locOf (LetWith _ _ _ _ _ _ loc) = locOf loc
  locOf (Result _ _ loc)          = locOf loc

-- | L0 Expression Language: literals + vars + int binops + array
-- constructors + array combinators (SOAC) + if + function calls +
-- let + tuples (literals & identifiers) TODO: please add float,
-- double, long int, etc.
data Exp =
            -- Core language
              SubExp    SubExp
            | TupLit    [SubExp] SrcLoc
            -- ^ Tuple literals, e.g., (1+3, (x, y+z)).

            | ArrayLit  [SubExp] Type SrcLoc
            -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
            -- Second arg is the type of of the rows of the array (not
            -- the element type).

            | If     SubExp Body Body [Type] SrcLoc

            | Apply  Name [(SubExp, Diet)] [Type] SrcLoc

            -- Scalar operations
            | BinOp BinOp SubExp SubExp Type SrcLoc

            -- Unary Ops: Not for bools and Negate for ints
            | Not    SubExp SrcLoc -- ^ E.g., @not True == False@.
            | Negate SubExp SrcLoc -- ^ E.g., @~(~1) = 1@.

            -- Assertion management.
            | Assert SubExp SrcLoc
            -- ^ Turn a boolean into a certificate, halting the
            -- program if the boolean is false.

            | Conjoin [SubExp] SrcLoc
            -- ^ Convert several certificates into a single certificate.

            -- Primitive array operations

            | Index Certificates
                    Ident
                    [SubExp]
                    SrcLoc
            -- ^ 3rd arg are (optional) certificates for bounds
            -- checking.  If given (even as an empty list), no
            -- run-time bounds checking is done.

            | Split Certificates SubExp SubExp SrcLoc
            -- ^ @split(1, [ 1, 2, 3, 4 ]) = {[1],[2, 3, 4]}@.

            | Concat Certificates SubExp SubExp SrcLoc
            -- ^ @concat([1],[2, 3, 4]) = [1, 2, 3, 4]@.

            | Copy SubExp SrcLoc
            -- ^ Copy the value return by the expression.  This only
            -- makes a difference in do-loops with merge variables.

            -- Array construction.
            | Iota SubExp SrcLoc
            -- ^ @iota(n) = [0,1,..,n-1]@
            | Replicate SubExp SubExp SrcLoc
            -- ^ @replicate(3,1) = [1, 1, 1]@

            -- Array index space transformation.
            | Reshape Certificates [SubExp] SubExp SrcLoc
             -- ^ 1st arg is the new shape, 2nd arg is the input array *)

            | Rearrange Certificates [Int] SubExp SrcLoc
            -- ^ Permute the dimensions of the input array.  The list
            -- of integers is a list of dimensions (0-indexed), which
            -- must be a permutation of @[0,n-1]@, where @n@ is the
            -- number of dimensions in the input array.

            | Map Certificates Lambda [SubExp] SrcLoc
             -- ^ @map(op +(1), {1,2,..,n}) = [2,3,..,n+1]@.
             -- 3rd arg is either a tuple of multi-dim arrays
             --   of basic type, or a multi-dim array of basic type.
             -- 4th arg is the input-array row types

            | Reduce  Certificates Lambda [(SubExp, SubExp)] SrcLoc
            | Scan   Certificates Lambda [(SubExp, SubExp)] SrcLoc
            | Filter  Certificates Lambda [SubExp] Ident SrcLoc
            -- ^ The 'Ident' should contain the outer shape of the
            -- result.
            | Redomap Certificates Lambda Lambda [SubExp] [SubExp] SrcLoc

              deriving (Eq, Ord, Show)

instance Located Exp where
  locOf (SubExp e) = locOf e
  locOf (TupLit _ pos) = locOf pos
  locOf (ArrayLit _ _ pos) = locOf pos
  locOf (BinOp _ _ _ _ pos) = locOf pos
  locOf (Not _ pos) = locOf pos
  locOf (Negate _ pos) = locOf pos
  locOf (If _ _ _ _ pos) = locOf pos
  locOf (Apply _ _ _ pos) = locOf pos
  locOf (Index _ _ _ pos) = locOf pos
  locOf (Iota _ pos) = locOf pos
  locOf (Replicate _ _ pos) = locOf pos
  locOf (Reshape _ _ _ pos) = locOf pos
  locOf (Rearrange _ _ _ pos) = locOf pos
  locOf (Split _ _ _ pos) = locOf pos
  locOf (Concat _ _ _ pos) = locOf pos
  locOf (Copy _ pos) = locOf pos
  locOf (Assert _ loc) = locOf loc
  locOf (Conjoin _ loc) = locOf loc
  locOf (Map _ _ _ pos) = locOf pos
  locOf (Reduce _ _ _ pos) = locOf pos
  locOf (Scan _ _ _ pos) = locOf pos
  locOf (Filter _ _ _ _ pos) = locOf pos
  locOf (Redomap _ _ _ _ _ pos) = locOf pos

-- | Anonymous function for use in a tuple-SOAC.
data Lambda =
  Lambda { lambdaParams     :: [Param]
         , lambdaBody       :: Body
         , lambdaReturnType :: [ConstType]
         , lambdaSrcLoc     :: SrcLoc
         }
  deriving (Eq, Ord, Show)

instance Located Lambda where
  locOf (Lambda _ _ _ loc) = locOf loc

-- | Function Declarations
type FunDec = (Name,
               [DeclType],
               [Param],
               Body,
               SrcLoc)

-- | An entire L0 program.
newtype Prog = Prog { progFunctions :: [FunDec] }
  deriving (Show)

-- | A set of names.
type Names = HS.HashSet VName
