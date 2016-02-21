{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | This Is an ever-changing abstract syntax for Futhark.  Some types,
-- such as @Exp@, are parametrised by type and name representation.
-- See the @docs/@ subdirectory in the Futhark repository for a language
-- reference, or this module may be a little hard to understand.
module Language.Futhark.Syntax
  (
   module Language.Futhark.Core

  -- * Types
  , Uniqueness(..)
  , IntType(..)
  , FloatType(..)
  , PrimType(..)
  , ArrayShape (..)
  , DimDecl (..)
  , ShapeDecl (..)
  , Rank (..)
  , TypeBase(..)
  , TupleArrayElemTypeBase(..)
  , ArrayTypeBase(..)
  , CompTypeBase
  , DeclTypeBase
  , DeclArrayTypeBase
  , DeclTupleArrayElemTypeBase
  , Diet(..)

    -- * Values
  , IntValue(..)
  , FloatValue(..)
  , PrimValue(..)
  , Value(..)

  -- * Abstract syntax tree
  , UnOp (..)
  , BinOp (..)
  , IdentBase(..)
  , ParamBase
  , ExpBase(..)
  , LoopFormBase (..)
  , ForLoopDirection (..)
  , LambdaBase(..)
  , PatternBase(..)
  , StreamForm(..)

  -- * Definitions
  , FunDecBase
  , ProgBase(..)

  -- * Miscellaneous
  , NoInfo(..)
  , Names
  )
  where

import Data.Array
import Data.Hashable
import Data.Loc
import Data.Monoid
import qualified Data.HashSet as HS

import Prelude

import Futhark.Representation.Primitive
  (IntType(..), FloatType(..), IntValue(..), FloatValue(..))
import Language.Futhark.Core

-- | No information.  Usually used for placeholder type- or aliasing
-- information.
data NoInfo vn = NoInfo
                 deriving (Eq, Ord, Show)

instance Monoid (NoInfo vn) where
  mempty = NoInfo
  _ `mappend` _ = NoInfo

-- | Low-level primitive types.
data PrimType = Signed IntType
              | Unsigned IntType
              | FloatType FloatType
              | Bool
              | Char
              deriving (Eq, Ord, Show)

-- | Non-array values.
data PrimValue = SignedValue !IntValue
               | UnsignedValue !IntValue
               | FloatValue !FloatValue
               | BoolValue !Bool
               | CharValue !Char
               deriving (Eq, Ord, Show)

-- | The class of types that can represent an array size.  The
-- 'Monoid' instance must define 'mappend' such that @dims1 `mappend`
-- dims2@ adds @dims1@ as the outer dimensions of @dims2@.
class (Eq shape, Ord shape, Monoid shape) => ArrayShape shape where
  -- | Number of dimensions.
  shapeRank :: shape -> Int
  -- | @stripDims n shape@ strips the outer @n@ dimensions from
  -- @shape@, returning 'Nothing' if this would result in zero or
  -- fewer dimensions.
  stripDims :: Int -> shape -> Maybe shape

-- | Declaration of a dimension size.
data DimDecl vn = NamedDim vn
                  -- ^ The size of the dimension is this name.  In a
                  -- function parameter, this is in a binding
                  -- position.  In a return type, this will give rise
                  -- to an assertion.
                | ConstDim Int
                  -- ^ The size is a constant.
                | AnyDim
                  -- ^ No dimension declaration.
                deriving (Eq, Ord, Show)

-- | The size of an array type is a list of its dimension sizes.  If
-- 'Nothing', that dimension is of a (statically) unknown size.
newtype ShapeDecl vn = ShapeDecl { shapeDims :: [DimDecl vn] }
                     deriving (Eq, Ord, Show)

-- | The rank of an array as a positive natural number.
newtype Rank vn = Rank Int
                deriving (Eq, Ord, Show)

instance Monoid (Rank vn) where
  mempty = Rank 0
  Rank n `mappend` Rank m = Rank $ n + m

instance ArrayShape (Rank vn) where
  shapeRank (Rank n) = n
  stripDims i (Rank n) | i < n     = Just $ Rank $ n - i
                       | otherwise = Nothing

instance Monoid (ShapeDecl vn) where
  mempty = ShapeDecl []
  ShapeDecl l1 `mappend` ShapeDecl l2 = ShapeDecl $ l1 ++ l2

instance (Eq vn, Ord vn) => ArrayShape (ShapeDecl vn) where
  shapeRank (ShapeDecl l) = length l
  stripDims i (ShapeDecl l)
    | i < length l = Just $ ShapeDecl $ drop i l
    | otherwise    = Nothing

-- | Types that can be elements of tuple-arrays.
data TupleArrayElemTypeBase shape as vn =
    PrimArrayElem PrimType (as vn)
  | ArrayArrayElem (ArrayTypeBase shape as vn)
  | TupleArrayElem [TupleArrayElemTypeBase shape as vn]
  deriving (Show)

instance Eq (shape vn) =>
         Eq (TupleArrayElemTypeBase shape as vn) where
  PrimArrayElem bt1 _ == PrimArrayElem bt2 _ = bt1 == bt2
  ArrayArrayElem at1   == ArrayArrayElem at2   = at1 == at2
  TupleArrayElem ts1   == TupleArrayElem ts2   = ts1 == ts2
  _                    == _                    = False

instance Ord (shape vn) =>
         Ord (TupleArrayElemTypeBase shape as vn) where
  PrimArrayElem bt1 _ `compare` PrimArrayElem bt2 _ = bt1 `compare` bt2
  ArrayArrayElem at1   `compare` ArrayArrayElem at2   = at1 `compare` at2
  TupleArrayElem ts1   `compare` TupleArrayElem ts2   = ts1 `compare` ts2
  PrimArrayElem {}    `compare` ArrayArrayElem {}    = LT
  PrimArrayElem {}    `compare` TupleArrayElem {}    = LT
  ArrayArrayElem {}    `compare` TupleArrayElem {}    = LT
  ArrayArrayElem {}    `compare` PrimArrayElem {}    = GT
  TupleArrayElem {}    `compare` PrimArrayElem {}    = GT
  TupleArrayElem {}    `compare` ArrayArrayElem {}    = GT

-- | An array type.
data ArrayTypeBase shape as vn =
    PrimArray PrimType (shape vn) Uniqueness (as vn)
    -- ^ An array whose elements are primitive types.
  | TupleArray [TupleArrayElemTypeBase shape as vn] (shape vn) Uniqueness
    -- ^ An array whose elements are tuples.
    deriving (Show)

instance Eq (shape vn) =>
         Eq (ArrayTypeBase shape as vn) where
  PrimArray et1 dims1 u1 _ == PrimArray et2 dims2 u2 _ =
    et1 == et2 && dims1 == dims2 && u1 == u2
  TupleArray ts1 dims1 u1 == TupleArray ts2 dims2 u2 =
    ts1 == ts2 && dims1 == dims2 && u1 == u2
  _ == _ =
    False

instance Ord (shape vn) =>
         Ord (ArrayTypeBase shape as vn) where
  PrimArray et1 dims1 u1 _ <= PrimArray et2 dims2 u2 _
    | et1 < et2     = True
    | et1 > et2     = False
    | dims1 < dims2 = True
    | dims1 > dims2 = False
    | u1 < u2       = True
    | u1 > u2       = False
    | otherwise     = True
  TupleArray ts1 dims1 u1 <= TupleArray ts2 dims2 u2
    | ts1 < ts2     = True
    | ts1 > ts2     = False
    | dims1 < dims2 = True
    | dims1 > dims2 = False
    | u1 < u2       = True
    | u1 > u2       = False
    | otherwise     = True
  PrimArray {} <= TupleArray {} =
    True
  TupleArray {} <= PrimArray {} =
    False

-- | An Futhark type is either an array, a prim type, or a tuple.
-- When comparing types for equality with '==', aliases are ignored,
-- but dimensions much match.
data TypeBase shape as vn = Prim PrimType
                          | Array (ArrayTypeBase shape as vn)
                          | Tuple [TypeBase shape as vn]
                          deriving (Eq, Ord, Show)

-- | A type with aliasing information and no shape annotations, used
-- for describing the type of a computation.
type CompTypeBase = TypeBase Rank Names

-- | A type with shape annotations and no aliasing information, used
-- for declarations.
type DeclTypeBase = TypeBase ShapeDecl NoInfo

-- | An array type with shape annotations and no aliasing information,
-- used for declarations.
type DeclArrayTypeBase = ArrayTypeBase ShapeDecl NoInfo

-- | A tuple array element type with shape annotations and no aliasing
-- information, used for declarations.
type DeclTupleArrayElemTypeBase = TupleArrayElemTypeBase ShapeDecl NoInfo

-- | Information about which parts of a value/type are consumed.  For
-- example, we might say that a function taking an argument of type
-- @([int], *[int], [int])@ has diet @ConsumeTuple [Observe, Consume,
-- Observe]@.
data Diet = TupleDiet [Diet] -- ^ Consumes these parts of the tuple.
          | Consume -- ^ Consumes this value.
          | Observe -- ^ Only observes value in this position, does
                    -- not consume.
            deriving (Eq, Ord, Show)

-- | Every possible value in Futhark.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = PrimValue !PrimValue
           | TupValue ![Value]
           | ArrayValue !(Array Int Value) (TypeBase Rank NoInfo ())
             -- ^ It is assumed that the array is 0-indexed.  The type
             -- is the row type.
             deriving (Eq, Ord, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data IdentBase ty vn = Ident { identName :: vn
                             , identType :: ty vn
                             , identSrcLoc :: SrcLoc
                             }
                deriving (Show)

-- | A name with no aliasing information, but known type.  These are
-- used for function parameters.
type ParamBase = IdentBase DeclTypeBase

instance Eq vn => Eq (IdentBase ty vn) where
  x == y = identName x == identName y

instance Ord vn => Ord (IdentBase ty vn) where
  x `compare` y = identName x `compare` identName y

instance Located (IdentBase ty vn) where
  locOf = locOf . identSrcLoc

instance Hashable vn => Hashable (IdentBase ty vn) where
  hashWithSalt salt = hashWithSalt salt . identName

-- | Unary operators.
data UnOp = Not
          | Negate
          | Complement
          | Abs
          | Signum
          | ToFloat FloatType
          | ToSigned IntType
          | ToUnsigned IntType
          deriving (Eq, Ord, Show)

-- | Binary operators.
data BinOp = Plus -- Binary Ops for Numbers
           | Minus
           | Pow
           | Times
           | Divide
           | Mod
           | Quot
           | Rem
           | ShiftR
           | ZShiftR -- ^ Zero-extend right shift.
           | ShiftL
           | Band
           | Xor
           | Bor
           | LogAnd
           | LogOr
           -- Relational Ops for all primitive types at least
           | Equal
           | Less
           | Leq
           | Greater
           | Geq
             deriving (Eq, Ord, Show, Enum, Bounded)

-- | The Futhark expression language.
--
-- In a value of type @Exp tt vn@, all 'Type' values are kept as @tt@
-- values, and all (variable) names are of type @vn@.
--
-- This allows us to encode whether or not the expression has been
-- type-checked in the Haskell type of the expression.  Specifically,
-- the parser will produce expressions of type @Exp 'NoInfo'@, and the
-- type checker will convert these to @Exp 'Type'@, in which type
-- information is always present.
data ExpBase ty vn =
            -- Core language
              Literal Value SrcLoc

            | TupLit    [ExpBase ty vn] SrcLoc
            -- ^ Tuple literals, e.g., @{1+3, {x, y+z}}@.

            | ArrayLit  [ExpBase ty vn] (ty vn) SrcLoc

            | Var    (IdentBase ty vn)
            -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
            -- Second arg is the type of of the rows of the array (not
            -- the element type).
            | LetPat (PatternBase ty vn) (ExpBase ty vn) (ExpBase ty vn) SrcLoc

            | If     (ExpBase ty vn) (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc

            | Apply  Name [(ExpBase ty vn, Diet)] (ty vn) SrcLoc

            | DoLoop
              (PatternBase ty vn) -- Merge variable pattern
              (ExpBase ty vn) -- Initial values of merge variables.
              (LoopFormBase ty vn) -- Do or while loop.
              (ExpBase ty vn) -- Loop body.
              (ExpBase ty vn) -- Let-body.
              SrcLoc

            | BinOp BinOp (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
            | UnOp UnOp (ExpBase ty vn) SrcLoc

            -- Primitive array operations
            | LetWith (IdentBase ty vn) (IdentBase ty vn)
                      [ExpBase ty vn] (ExpBase ty vn)
                      (ExpBase ty vn) SrcLoc

            | Index (ExpBase ty vn)
                    [ExpBase ty vn]
                    SrcLoc

            | Size Int (ExpBase ty vn) SrcLoc
            -- ^ The size of the specified array dimension.

            | Split [ExpBase ty vn] (ExpBase ty vn) SrcLoc
            -- ^ @split( (1,1,3), [ 1, 2, 3, 4 ]) = {[1], [], [2, 3], [4]}@.
            -- Note that this is different from the internal representation

            | Concat (ExpBase ty vn) [ExpBase ty vn] SrcLoc
            -- ^ @concat([1],[2, 3, 4]) = [1, 2, 3, 4]@.

            | Copy (ExpBase ty vn) SrcLoc
            -- ^ Copy the value return by the expression.  This only
            -- makes a difference in do-loops with merge variables.

            -- Array construction.
            | Iota (ExpBase ty vn) SrcLoc
            -- ^ @iota(n) = [0,1,..,n-1]@
            | Replicate (ExpBase ty vn) (ExpBase ty vn) SrcLoc
            -- ^ @replicate(3,1) = [1, 1, 1]@

            -- Array index space transformation.
            | Reshape [ExpBase ty vn] (ExpBase ty vn) SrcLoc
             -- ^ 1st arg is the new shape, 2nd arg is the input array.

            | Transpose (ExpBase ty vn) SrcLoc
            -- ^ Transpose two-dimensional array.  @transpose(a) =
            -- rearrange((1,0), a)@.

            | Rearrange [Int] (ExpBase ty vn) SrcLoc
            -- ^ Permute the dimensions of the input array.  The list
            -- of integers is a list of dimensions (0-indexed), which
            -- must be a permutation of @[0,n-1]@, where @n@ is the
            -- number of dimensions in the input array.

            | Stripe (ExpBase ty vn) (ExpBase ty vn) SrcLoc

            | Unstripe (ExpBase ty vn) (ExpBase ty vn) SrcLoc

            -- Second-Order Array Combinators accept curried and
            -- anonymous functions as first params.
            | Map (LambdaBase ty vn) (ExpBase ty vn) SrcLoc
             -- ^ @map(op +(1), [1,2,..,n]) = [2,3,..,n+1]@.

            | Reduce Commutativity (LambdaBase ty vn) (ExpBase ty vn) (ExpBase ty vn) SrcLoc
             -- ^ @reduce(op +, 0, [1,2,...,n]) = (0+1+2+...+n)@.

            | Scan (LambdaBase ty vn) (ExpBase ty vn) (ExpBase ty vn) SrcLoc
             -- ^ @scan(plus, 0, [ 1, 2, 3 ]) = [ 1, 3, 6 ]@.

            | Filter (LambdaBase ty vn) (ExpBase ty vn) SrcLoc
            -- ^ Return those elements of the array that satisfy the
            -- predicate.

            | Partition [LambdaBase ty vn] (ExpBase ty vn) SrcLoc
            -- ^ @partition(f_1, ..., f_n, a)@ returns @n+1@ arrays, with
            -- the @i@th array consisting of those elements for which
            -- function @f_1@ returns 'True', and no previous function
            -- has returned 'True'.  The @n+1@th array contains those
            -- elements for which no function returns 'True'.

            | Stream (StreamForm ty vn) (LambdaBase ty vn) (ExpBase ty vn) SrcLoc
            -- ^ Streaming: intuitively, this gives a size-parameterized
            -- composition for SOACs that cannot be fused, e.g., due to scan.
            -- For example, assuming @A : [int], f : int->int, g : real->real@,
            -- the code: @let x = map(f,A) in let y = scan(op+,0,x) in map(g,y)@
            -- can be re-written (streamed) in the source-Futhark language as:
            -- @let {acc, z} =
            -- @stream( 0, A,@
            -- @      , fn {int,[real]} (real chunk, real acc, [int] a) =>@
            -- @            let x = map (f,         A ) in@
            -- @            let y0= scan(op +, 0,   x ) in@
            -- @            let y = map (op +(acc), y0) in@
            -- @            { acc+y0[chunk-1], map(g, y) }@
            -- @      )@
            -- where (i)  @chunk@ is a symbolic int denoting the chunk
            -- size, (ii) @0@ is the initial value of the accumulator,
            -- which allows the streaming of @scan@.
            -- Finally, the unnamed function (@fn...@) implements the a fold that:
            -- computes the accumulator of @scan@, as defined inside its body, AND
            -- implicitly concatenates each of the result arrays across
            -- the iteration space.
            -- In essence, sequential codegen can choose chunk = 1 and thus
            -- eliminate the SOACs on the outermost level, while parallel codegen
            -- may choose the maximal chunk size that still satisfies the memory
            -- requirements of the device.

            | Zip [(ExpBase ty vn, ty vn)] SrcLoc
            -- ^ Normal zip supporting variable number of arguments.
            -- The type paired to each expression is the full type of
            -- the array returned by that expression.

            | Unzip (ExpBase ty vn) [ty vn] SrcLoc
            -- ^ Unzip that can unzip to tuples of arbitrary size.
            -- The types are the elements of the tuple.

            | Unsafe (ExpBase ty vn) SrcLoc
            -- ^ Explore the Danger Zone and elide safety checks on
            -- array operations that are (lexically) within this
            -- expression.  Make really sure the code is correct.

              deriving (Eq, Ord, Show)

data StreamForm ty vn = MapLike    StreamOrd
                      | RedLike    StreamOrd Commutativity (LambdaBase ty vn) (ExpBase ty vn)
                      | Sequential (ExpBase ty vn)
                        deriving (Eq, Ord, Show)

instance Located (ExpBase ty vn) where
  locOf (Literal _ loc) = locOf loc
  locOf (TupLit _ pos) = locOf pos
  locOf (ArrayLit _ _ pos) = locOf pos
  locOf (BinOp _ _ _ _ pos) = locOf pos
  locOf (UnOp _ _ pos) = locOf pos
  locOf (If _ _ _ _ pos) = locOf pos
  locOf (Var ident) = locOf ident
  locOf (Apply _ _ _ pos) = locOf pos
  locOf (LetPat _ _ _ pos) = locOf pos
  locOf (LetWith _ _ _ _ _ pos) = locOf pos
  locOf (Index _ _ pos) = locOf pos
  locOf (Iota _ pos) = locOf pos
  locOf (Size _ _ pos) = locOf pos
  locOf (Replicate _ _ pos) = locOf pos
  locOf (Reshape _ _ pos) = locOf pos
  locOf (Transpose _ pos) = locOf pos
  locOf (Rearrange _ _ pos) = locOf pos
  locOf (Stripe _ _ pos) = locOf pos
  locOf (Unstripe _ _ pos) = locOf pos
  locOf (Map _ _ pos) = locOf pos
  locOf (Reduce _ _ _ _ pos) = locOf pos
  locOf (Zip _ pos) = locOf pos
  locOf (Unzip _ _ pos) = locOf pos
  locOf (Scan _ _ _ pos) = locOf pos
  locOf (Filter _ _ pos) = locOf pos
  locOf (Partition _ _ pos) = locOf pos
  locOf (Split _ _ pos) = locOf pos
  locOf (Concat _ _ pos) = locOf pos
  locOf (Copy _ pos) = locOf pos
  locOf (DoLoop _ _ _ _ _ pos) = locOf pos
  locOf (Stream _ _ _  pos) = locOf pos
  locOf (Unsafe _ loc) = locOf loc

-- | Whether the loop is a @for@-loop or a @while@-loop.
data LoopFormBase ty vn = For ForLoopDirection (ExpBase ty vn) (IdentBase ty vn) (ExpBase ty vn)
                        | While (ExpBase ty vn)
                          deriving (Eq, Ord, Show)

-- | The iteration order of a @for@-loop.
data ForLoopDirection = FromUpTo -- ^ Iterates from the lower bound to
                                 -- just below the upper bound.
                      | FromDownTo -- ^ Iterates from just below the
                                   -- upper bound to the lower bound.
                        deriving (Eq, Ord, Show)

-- | Anonymous Function
data LambdaBase ty vn = AnonymFun [ParamBase vn] (ExpBase ty vn) (DeclTypeBase vn) SrcLoc
                      -- ^ @fn int (bool x, char z) => if(x) then ord(z) else ord(z)+1 *)@
                      | CurryFun Name [ExpBase ty vn] (ty vn) SrcLoc
                        -- ^ @f(4)@
                      | UnOpFun UnOp (ty vn) (ty vn) SrcLoc
                        -- ^ @-@; first type is operand, second is result.
                      | BinOpFun BinOp (ty vn) (ty vn) (ty vn) SrcLoc
                        -- ^ @+@; first two types are operands, third is result.
                      | CurryBinOpLeft BinOp (ExpBase ty vn) (ty vn) (ty vn) SrcLoc
                        -- ^ @2+@; first type is operand, second is result.
                      | CurryBinOpRight BinOp (ExpBase ty vn) (ty vn) (ty vn) SrcLoc
                        -- ^ @+2@; first type is operand, second is result.
                        deriving (Eq, Ord, Show)

instance Located (LambdaBase ty vn) where
  locOf (AnonymFun _ _ _ loc)         = locOf loc
  locOf (CurryFun  _ _ _ loc)         = locOf loc
  locOf (UnOpFun _ _ _ loc)           = locOf loc
  locOf (BinOpFun _ _ _ _ loc)        = locOf loc
  locOf (CurryBinOpLeft _ _ _ _ loc)  = locOf loc
  locOf (CurryBinOpRight _ _ _ _ loc) = locOf loc

-- | Tuple IdentBaseifier, i.e., pattern matching
data PatternBase ty vn = TuplePattern [PatternBase ty vn] SrcLoc
                       | Id (IdentBase ty vn)
                       | Wildcard (ty vn) SrcLoc -- Nothing, i.e. underscore.
                       deriving (Eq, Ord, Show)

instance Located (PatternBase ty vn) where
  locOf (TuplePattern _ loc) = locOf loc
  locOf (Id ident) = locOf ident
  locOf (Wildcard _ loc) = locOf loc

-- | Function Declarations
type FunDecBase ty vn = (Name,
                         DeclTypeBase vn,
                         [ParamBase vn],
                         ExpBase ty vn,
                         SrcLoc)

-- | An entire Futhark program.
newtype ProgBase ty vn = Prog { progFunctions :: [FunDecBase ty vn] }
  deriving (Show)

-- | A set of names.
type Names = HS.HashSet
