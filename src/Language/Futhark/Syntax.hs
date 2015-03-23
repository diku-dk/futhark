-- | This Is an ever-changing abstract syntax for Futhark.  Some types,
-- such as @Exp@, are parametrised by type and name representation.
-- See the @doc/@ subdirectory in the Futhark repository for a language
-- reference, or this module may be a little hard to understand.
module Language.Futhark.Syntax
  (
   module Language.Futhark.Core

  -- * Types
  , Uniqueness(..)
  , DimSize
  , ArraySize
  , TypeBase(..)
  , TupleArrayElemTypeBase(..)
  , ArrayTypeBase(..)
  , CompTypeBase
  , DeclTypeBase
  , DeclArrayTypeBase
  , DeclTupleArrayElemTypeBase
  , Diet(..)

  -- * Values
  , Value(..)

  -- * Abstract syntax tree
  , UnOp (..)
  , BinOp (..)
  , IdentBase(..)
  , ParamBase
  , ExpBase(..)
  , LoopFormBase (..)
  , LambdaBase(..)
  , TupIdentBase(..)

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

import Language.Futhark.Core

-- | No information.  Usually used for placeholder type- or aliasing
-- information.
data NoInfo vn = NoInfo
                 deriving (Eq, Ord, Show)

instance Monoid (NoInfo vn) where
  mempty = NoInfo
  _ `mappend` _ = NoInfo

-- | Don't use this for anything.
type DimSize vn = ExpBase (TypeBase Names) vn

-- | The size of an array type is a list of its dimension sizes.  If
-- 'Nothing', that dimension is of a (statically) unknown size.
type ArraySize vn = [Maybe (DimSize vn)]

-- | Types that can be elements of tuple-arrays.
data TupleArrayElemTypeBase as vn =
    BasicArrayElem BasicType (as vn)
  | ArrayArrayElem (ArrayTypeBase as vn)
  | TupleArrayElem [TupleArrayElemTypeBase as vn]
  deriving (Show)

instance Eq (TupleArrayElemTypeBase as vn) where
  BasicArrayElem bt1 _ == BasicArrayElem bt2 _ = bt1 == bt2
  ArrayArrayElem at1   == ArrayArrayElem at2   = at1 == at2
  TupleArrayElem ts1   == TupleArrayElem ts2   = ts1 == ts2
  _                    == _                    = False

instance Ord (TupleArrayElemTypeBase as vn) where
  BasicArrayElem bt1 _ `compare` BasicArrayElem bt2 _ = bt1 `compare` bt2
  ArrayArrayElem at1   `compare` ArrayArrayElem at2   = at1 `compare` at2
  TupleArrayElem ts1   `compare` TupleArrayElem ts2   = ts1 `compare` ts2
  BasicArrayElem {}    `compare` ArrayArrayElem {}    = LT
  BasicArrayElem {}    `compare` TupleArrayElem {}    = LT
  ArrayArrayElem {}    `compare` TupleArrayElem {}    = LT
  ArrayArrayElem {}    `compare` BasicArrayElem {}    = GT
  TupleArrayElem {}    `compare` BasicArrayElem {}    = GT
  TupleArrayElem {}    `compare` ArrayArrayElem {}    = GT

-- | An array type.
data ArrayTypeBase as vn =
    BasicArray BasicType (ArraySize vn) Uniqueness (as vn)
    -- ^ An array whose elements are basic elements.
  | TupleArray [TupleArrayElemTypeBase as vn] (ArraySize vn) Uniqueness
    -- ^ An array whose elements are tuples.
    deriving (Show)

instance Eq (ArrayTypeBase as vn) where
  BasicArray et1 dims1 u1 _ == BasicArray et2 dims2 u2 _ =
    et1 == et2 && length dims1 == length dims2 && u1 == u2
  TupleArray ts1 dims1 u1 == TupleArray ts2 dims2 u2 =
    ts1 == ts2 && length dims1 == length dims2 && u1 == u2
  _ == _ =
    False

instance Ord (ArrayTypeBase as vn) where
  BasicArray et1 dims1 u1 _ <= BasicArray et2 dims2 u2 _
    | et1 < et2     = True
    | et1 > et2     = False
    | length dims1 < length dims2 = True
    | length dims1 > length dims2 = False
    | u1 < u2       = True
    | u1 > u2       = False
    | otherwise     = True
  TupleArray ts1 dims1 u1 <= TupleArray ts2 dims2 u2
    | ts1 < ts2     = True
    | ts1 > ts2     = False
    | length dims1 < length dims2 = True
    | length dims1 > length dims2 = False
    | u1 < u2       = True
    | u1 > u2       = False
    | otherwise     = True
  BasicArray {} <= TupleArray {} =
    True
  TupleArray {} <= BasicArray {} =
    False

-- | An Futhark type is either an array or an element type.  When comparing
-- types for equality with '==', aliases are ignored, as are
-- dimension sizes (but not the number of dimensions themselves).
data TypeBase as vn = Basic BasicType
                    | Array (ArrayTypeBase as vn)
                    | Tuple [TypeBase as vn]
                    deriving (Eq, Ord, Show)

-- | A type with aliasing information, used for describing the type of
-- a computation.
type CompTypeBase = TypeBase Names

-- | A type without aliasing information, used for declarations.
type DeclTypeBase = TypeBase NoInfo

-- | An array type without aliasing information, used for declarations.
type DeclArrayTypeBase = ArrayTypeBase NoInfo

-- | A tuple array element type without aliasing information, used for
-- declarations.
type DeclTupleArrayElemTypeBase = TupleArrayElemTypeBase NoInfo

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
data Value = BasicVal !BasicValue
           | TupVal ![Value]
           | ArrayVal !(Array Int Value) (DeclTypeBase ())
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
          deriving (Eq, Ord, Show)

-- | Binary operators.
data BinOp = Plus -- Binary Ops for Numbers
           | Minus
           | Pow
           | Times
           | Divide
           | Mod
           | ShiftR
           | ShiftL
           | Band
           | Xor
           | Bor
           | LogAnd
           | LogOr
           -- Relational Ops for all basic types at least
           | Equal
           | Less
           | Leq
           | Greater
           | Geq
             deriving (Eq, Ord, Show)

-- | Futhark Expression Language: literals + vars + int binops + array
-- constructors + array combinators (SOAC) + if + function calls +
-- let + tuples (literals & identifiers)
--
-- In a value of type @Exp tt@, all 'Type' values are kept as @tt@
-- values.
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
            | LetPat (TupIdentBase ty vn) (ExpBase ty vn) (ExpBase ty vn) SrcLoc

            | If     (ExpBase ty vn) (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc

            | Apply  Name [(ExpBase ty vn, Diet)] (ty vn) SrcLoc

            | DoLoop
              (TupIdentBase ty vn) -- Merge variable pattern
              (ExpBase ty vn) -- Initial values of merge variables.
              (LoopFormBase ty vn) -- Do or while loop.
              (ExpBase ty vn) -- Loop body.
              (ExpBase ty vn) -- Let-body.
              SrcLoc

            -- Scalar operations
            | BinOp BinOp (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc

            -- Unary Ops: Not for bools and Negate for ints
            | UnOp UnOp (ExpBase ty vn) SrcLoc

            -- Primitive array operations
            | LetWith (IdentBase ty vn) (IdentBase ty vn)
                      [ExpBase ty vn] (ExpBase ty vn)
                      (ExpBase ty vn) SrcLoc

            | Index (IdentBase ty vn)
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

            | Transpose Int Int (ExpBase ty vn) SrcLoc
            -- ^ If @b=transpose(k,n,a)@, then @a[i_1, ..., i_k
            -- ,i_{k+1}, ..., i_{k+n}, ..., i_q ] = b[i_1 ,.., i_{k+1}
            -- , ..., i_{k+n} ,i_k, ..., i_q ]@.  Thus,
            -- @transpose(0,1,a)@ is the common two-dimensional
            -- transpose.

            | Rearrange [Int] (ExpBase ty vn) SrcLoc
            -- ^ Permute the dimensions of the input array.  The list
            -- of integers is a list of dimensions (0-indexed), which
            -- must be a permutation of @[0,n-1]@, where @n@ is the
            -- number of dimensions in the input array.

            -- Second-Order Array Combinators accept curried and
            -- anonymous functions as first params.
            | Map (LambdaBase ty vn) (ExpBase ty vn) SrcLoc
             -- ^ @map(op +(1), [1,2,..,n]) = [2,3,..,n+1]@.

            | Reduce (LambdaBase ty vn) (ExpBase ty vn) (ExpBase ty vn) SrcLoc
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

            | Redomap (LambdaBase ty vn) (LambdaBase ty vn) (ExpBase ty vn) (ExpBase ty vn) SrcLoc
             -- ^ @redomap(g, f, n, a) = reduce(g, n, map(f, a))@.
             -- 5th arg is the row type of the input  array.

            | Stream (IdentBase  ty vn)  (IdentBase ty vn)
                     (ExpBase    ty vn)  (ExpBase   ty vn)
                     (LambdaBase ty vn)  SrcLoc
            -- ^ Streaming: intuitively, this gives a size-parameterized
            -- composition for SOACs that cannot be fused, e.g., due to scan.
            -- For example, assuming @A : [int], f : int->int, g : real->real@,
            -- the code: @let x = map(f,A) in let y = scan(op+,0,x) in map(g,y)@
            -- can be re-written (streamed) in the source-Futhark language as:
            -- @let {acc, z} =
            -- @stream( chunk, i, 0, A,@
            -- @      , fn {int,[real]} (real acc, [int] a) =>@
            -- @            let x = map (f,         A ) in@
            -- @            let y0= scan(op +, 0,   x ) in@
            -- @            let y = map (op +(acc), y0) in@
            -- @            { acc+y0[chunk-1], map(g, y) }@
            -- @      )@
            -- where (i) @chunk@ is a symbolic int denoting the chunk
            -- size, (ii) @i@ maps the global iteration space, hence together
            -- with @chunk@ identify accurately the current chunk of the input
            -- array that is being processed, (iii) @0@ is the initial value of
            -- the accumulator, which allows the streaming of @scan@.
            -- Finally, the unnamed function (@fn...@) implements the a fold that:
            -- computes the accumulator of @scan@, as defined inside its body, AND
            -- implicitly concatenates each of the result arrays across
            -- the iteration space.
            -- In essence, sequential codegen can choose chunk = 1 and thus
            -- eliminate the SOACs on the outermost level, while parallel codegen
            -- may choose the maximal chunk size that still satisfies the memory
            -- requirements of the device.

            | ConcatMap (LambdaBase ty vn) (ExpBase ty vn) [ExpBase ty vn] SrcLoc

            | Zip [(ExpBase ty vn, ty vn)] SrcLoc
            -- ^ Normal zip supporting variable number of arguments.
            -- The type paired to each expression is the element type
            -- of the array returned by that expression.

            | Unzip (ExpBase ty vn) [ty vn] SrcLoc
            -- ^ Unzip that can unzip tuples of arbitrary size.  The
            -- types are the elements of the tuple.

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
  locOf (Transpose _ _ _ pos) = locOf pos
  locOf (Rearrange _ _ pos) = locOf pos
  locOf (Map _ _ pos) = locOf pos
  locOf (ConcatMap _ _ _ pos) = locOf pos
  locOf (Reduce _ _ _ pos) = locOf pos
  locOf (Zip _ pos) = locOf pos
  locOf (Unzip _ _ pos) = locOf pos
  locOf (Scan _ _ _ pos) = locOf pos
  locOf (Filter _ _ pos) = locOf pos
  locOf (Partition _ _ pos) = locOf pos
  locOf (Redomap _ _ _ _ pos) = locOf pos
  locOf (Split _ _ pos) = locOf pos
  locOf (Concat _ _ pos) = locOf pos
  locOf (Copy _ pos) = locOf pos
  locOf (DoLoop _ _ _ _ _ pos) = locOf pos
  locOf (Stream _ _ _ _ _ pos) = locOf pos

data LoopFormBase ty vn = ForLoop (IdentBase ty vn) (ExpBase ty vn)
                        | WhileLoop (ExpBase ty vn)
                          deriving (Eq, Ord, Show)

-- | Anonymous Function
data LambdaBase ty vn = AnonymFun [ParamBase vn] (ExpBase ty vn) (DeclTypeBase vn) SrcLoc
                      -- ^ @fn int (bool x, char z) => if(x) then ord(z) else ord(z)+1 *)@
                      | CurryFun Name [ExpBase ty vn] (ty vn) SrcLoc
                        -- ^ @f(4)@
                      | UnOpFun UnOp (ty vn) SrcLoc
                        -- ^ @-@.
                      | BinOpFun BinOp (ty vn) SrcLoc
                      | CurryBinOpLeft BinOp (ExpBase ty vn) (ty vn) SrcLoc
                        -- ^ @2+@.
                      | CurryBinOpRight BinOp (ExpBase ty vn) (ty vn) SrcLoc
                        -- ^ @+2@.
                        deriving (Eq, Ord, Show)

instance Located (LambdaBase ty vn) where
  locOf (AnonymFun _ _ _ loc)       = locOf loc
  locOf (CurryFun  _ _ _ loc)       = locOf loc
  locOf (UnOpFun _ _ loc)           = locOf loc
  locOf (BinOpFun _ _ loc)          = locOf loc
  locOf (CurryBinOpLeft _ _ _ loc)  = locOf loc
  locOf (CurryBinOpRight _ _ _ loc) = locOf loc

-- | Tuple IdentBaseifier, i.e., pattern matching
data TupIdentBase ty vn = TupId [TupIdentBase ty vn] SrcLoc
                        | Id (IdentBase ty vn)
                        | Wildcard (ty vn) SrcLoc -- Nothing, i.e. underscore.
                          deriving (Eq, Ord, Show)

instance Located (TupIdentBase ty vn) where
  locOf (TupId _ loc) = locOf loc
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
