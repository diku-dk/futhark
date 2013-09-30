-- | This Is an ever-changing abstract syntax for L0.  Some types,
-- such as @Exp@, are parametrised by type and name representation.
-- See the @doc/@ subdirectory in the L0 repository for a language
-- reference, or this module may be a little hard to understand.
module Language.L0.Syntax
  (
   -- * Names
   Name(..)
  , nameToString
  , nameFromString

  -- * Types
  , Uniqueness(..)
  , DimSize
  , ArraySize
  , ElemTypeBase(..)
  , TypeBase(..)
  , CompTypeBase
  , DeclTypeBase
  , Diet(..)

  -- * Values
  , Value(..)

  -- * Abstract syntax tree
  , IdentBase(..)
  , ParamBase
  , ExpBase(..)
  , BinOp(..)
  , opStr
  , LambdaBase(..)
  , TupIdentBase(..)

  -- * Definitions
  , FunDecBase
  , ProgBase(..)

  -- * Special identifiers
  , defaultEntryPoint
  , isBuiltInFun
  , builtInFuns

  -- * Miscellaneous
  , NoInfo(..)
  , Names
  )
  where

import Data.Array
import Data.Loc
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T

-- | The abstract (not really) type representing names in the L0
-- compiler.  'String's, being lists of characters, are very slow,
-- while 'T.Text's are based on byte-arrays.
newtype Name = Name T.Text
  deriving (Show, Eq, Ord)

-- | Convert a name to the corresponding list of characters.
nameToString :: Name -> String
nameToString (Name t) = T.unpack t

-- | Convert a list of characters to the corresponding name.
nameFromString :: String -> Name
nameFromString = Name . T.pack

-- | No information.  Usually used for placeholder type- or aliasing
-- information.
data NoInfo vn = NoInfo
                 deriving (Eq, Ord, Show)

instance Monoid (NoInfo vn) where
  mempty = NoInfo
  _ `mappend` _ = NoInfo

-- | The uniqueness attribute of a type.  This essentially indicates
-- whether or not in-place modifications are acceptable.
data Uniqueness = Unique    -- ^ At most one outer reference.
                | Nonunique -- ^ Any number of references.
                  deriving (Eq, Ord, Show)

instance Monoid Uniqueness where
  mempty = Unique
  _ `mappend` Nonunique = Nonunique
  Nonunique `mappend` _ = Nonunique
  u `mappend` _         = u

-- | Don't use this for anything.
type DimSize vn = ExpBase (TypeBase Names) vn

-- | The size of an array type is a list of its dimension sizes.  If
-- 'Nothing', that dimension is of a (statically) unknown size.
type ArraySize vn = [Maybe (DimSize vn)]

-- | Types that can be elements of arrays.  TODO: please add float,
-- double, long int, etc.
data ElemTypeBase as vn = Int
                        | Bool
                        | Char
                        | Real
                        | Tuple [TypeBase as vn]
                deriving (Eq, Ord, Show)

-- | An L0 type is either an array or an element type.  When comparing
-- types for equality with '==', aliases are ignored, as are
-- dimension sizes (but not the number of dimensions themselves).
data TypeBase as vn = Elem (ElemTypeBase as vn)
                    | Array (ElemTypeBase NoInfo vn) (ArraySize vn) Uniqueness (as vn)
                    -- ^ 1st arg: array's element type, 2nd arg:
                    -- lengths of dimensions, 3rd arg: uniqueness
                    -- attribute, 4th arg: aliasing information.
                    deriving (Ord, Show)

instance Eq (TypeBase as vn) where
  Elem et1 == Elem et2 = et1 == et2
  Array et1 dims1 u1 _ == Array et2 dims2 u2 _ =
    et1 == et2 && u1 == u2 && length dims1 == length dims2
  _ == _ = False

-- | A type with aliasing information, used for describing the type of
-- a computation.
type CompTypeBase = TypeBase Names

-- | A type without aliasing information, used for declarations.
type DeclTypeBase = TypeBase NoInfo

-- | Information about which parts of a value/type are consumed.  For
-- example, we might say that a function taking an argument of type
-- @([int], *[int], [int])@ has diet @ConsumeTuple [Observe, Consume,
-- Observe]@.
data Diet = TupleDiet [Diet] -- ^ Consumes these parts of the tuple.
          | Consume -- ^ Consumes this value.
          | Observe -- ^ Only observes value in this position, does
                    -- not consume.
            deriving (Eq, Ord, Show)

-- | Every possible value in L0.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = IntVal !Int
           | RealVal !Double
           | LogVal !Bool
           | CharVal !Char
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

-- | L0 Expression Language: literals + vars + int binops + array
-- constructors + array combinators (SOAC) + if + function calls +
-- let + tuples (literals & identifiers) TODO: please add float,
-- double, long int, etc.
--
-- In a value of type @Exp tt@, all 'Type' values are kept as @tt@
-- values.  -- This allows us to encode whether or not the expression
-- has been type-checked in the Haskell type of the expression.
-- Specifically, the parser will produce expressions of type @Exp
-- 'Maybe Type'@, and the type checker will convert these to @Exp
-- 'Type'@, in which type information is always present.
data ExpBase ty vn =
              Literal Value SrcLoc
            | TupLit    [ExpBase ty vn] SrcLoc
            -- ^ Tuple literals, e.g., (1+3, (x, y+z)).  Second
            -- argument is the tuple's type.
            | ArrayLit  [ExpBase ty vn] (ty vn) SrcLoc
            -- ^ Array literals, e.g., { {1+x, 3}, {2, 1+4} }.  Second
            -- arg is the type of of the rows of the array (not the
            -- element type).
            | BinOp BinOp (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
            -- Binary Ops for Booleans
            | And    (ExpBase ty vn) (ExpBase ty vn) SrcLoc
            | Or     (ExpBase ty vn) (ExpBase ty vn) SrcLoc
            -- Unary Ops: Not for bools and Negate for ints
            | Not    (ExpBase ty vn) SrcLoc -- e.g., not True = False
            | Negate (ExpBase ty vn) (ty vn) SrcLoc -- e.g., ~(~1) = 1
            | If     (ExpBase ty vn) (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
            | Var    (IdentBase ty vn)
            -- Function Call and Let Construct
            | Apply  Name [(ExpBase ty vn, Diet)] (ty vn) SrcLoc
            | LetPat (TupIdentBase ty vn) (ExpBase ty vn) (ExpBase ty vn) SrcLoc

            | LetWith (IdentBase ty vn) (IdentBase ty vn) [ExpBase ty vn] (ExpBase ty vn) (ExpBase ty vn) SrcLoc
            -- ^ Array Indexing and Array Constructors

            | Index (IdentBase ty vn) [ExpBase ty vn] (ty vn) SrcLoc
             -- ^ 3rd arg is the result type

            | Iota (ExpBase ty vn) SrcLoc
            -- ^ @iota(n) = {0,1,..,n-1@

            | Size Int (ExpBase ty vn) SrcLoc
            -- ^ The size of the specified array dimension.

            | Replicate (ExpBase ty vn) (ExpBase ty vn) SrcLoc
            -- ^ @replicate(3,1) = {1, 1, 1}@

            | Reshape [ExpBase ty vn] (ExpBase ty vn) SrcLoc
             -- ^ 1st arg is the new shape, 2nd arg is the input array *)

            | Transpose Int Int (ExpBase ty vn) SrcLoc
              -- ^ If @b=transpose(k,n,a)@, then @a[i_1, ..., i_k
              -- ,i_{k+1}, ..., i_{k+n}, ..., i_q ] = b[i_1 ,..,
              -- i_{k+1} , ..., i_{k+n} ,i_k, ..., i_q ]@.  Thus,
              -- @transpose(0,1,a)@ is the common two-dimensional
              -- transpose.

            -- Second-Order Array Combinators
            -- accept curried and anonymous
            -- functions as (first) params
            | Map (LambdaBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
             -- @map(op +(1), {1,2,..,n}) = {2,3,..,n+1}@
             -- 3st arg is the input-array row type

            | Reduce (LambdaBase ty vn) (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
             -- @reduce(op +, 0, {1,2,..,n}) = (0+1+2+..+n)@
             -- 4th arg is the input-array element type

            | Zip [(ExpBase ty vn, ty vn)] SrcLoc
            -- ^ Normal zip supporting variable number of arguments.
            -- The type paired to each expression is the element type
            -- of the array returned by that expression.

            | Unzip (ExpBase ty vn) [ty vn] SrcLoc
            -- ^ Unzip that can unzip tuples of arbitrary size.  The
            -- types are the elements of the tuple.

            | Scan (LambdaBase ty vn) (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
             -- ^ @scan(plus, 0, { 1, 2, 3 }) = { 1, 3, 6 }@.
             -- 4th arg is the element type of the input array

            | Filter (LambdaBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
            -- ^ 3rd arg is the row type of the input (and
            -- result) array


            | Redomap (LambdaBase ty vn) (LambdaBase ty vn) (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
             -- ^ @redomap(g, f, n, a) = reduce(g, n, map(f, a))@.
             -- 5th arg is the row type of the input  array.

            | Split (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
             -- ^ @split(1, { 1, 2, 3, 4 }) = ({1},{2, 3, 4})@.
             -- 3rd arg is the element type of the input array

            | Concat (ExpBase ty vn) (ExpBase ty vn) SrcLoc
             -- ^ @concat ({1},{2, 3, 4}) = {1, 2, 3, 4}@.

            | Copy (ExpBase ty vn) SrcLoc
            -- ^ Copy the value return by the expression.  This only
            -- makes a difference in do-loops with merge variables.

            | DoLoop
              (TupIdentBase ty vn) -- Merge variable pattern
              (ExpBase ty vn) -- Initial values of merge variables.
              (IdentBase ty vn) -- Iterator.
              (ExpBase ty vn) -- Upper bound.
              (ExpBase ty vn) -- Loop body.
              (ExpBase ty vn) -- Let-body.
              SrcLoc

            -----------------------------------------------------
            -- Second-Order Array Combinators
            -- with support for n-ary multi-dim 
            -- arrays of BASIC type (i.e., no tuples inside)
            -- accept curried and anonymous
            -- functions as (first) params
            -----------------------------------------------------
            | Map2 (LambdaBase ty vn) [ExpBase ty vn] [ty vn] SrcLoc
             -- @map(op +(1), {1,2,..,n}) = {2,3,..,n+1}@
             -- 2nd arg is either a tuple of multi-dim arrays 
             --   of basic type, or a multi-dim array of basic type.
             -- 3rd arg is the input-array row types

            | Reduce2 (LambdaBase ty vn) [ExpBase ty vn] [ExpBase ty vn] [ty vn] SrcLoc
            | Scan2   (LambdaBase ty vn) [ExpBase ty vn] [ExpBase ty vn] [ty vn] SrcLoc
            | Filter2 (LambdaBase ty vn) [ExpBase ty vn]          SrcLoc
            | Redomap2(LambdaBase ty vn) (LambdaBase ty vn) [ExpBase ty vn] [ExpBase ty vn] [ty vn] SrcLoc

            | Min (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc
            | Max (ExpBase ty vn) (ExpBase ty vn) (ty vn) SrcLoc

              deriving (Eq, Ord, Show)

instance Located (ExpBase ty vn) where
  locOf (Literal _ loc) = locOf loc
  locOf (TupLit _ pos) = locOf pos
  locOf (ArrayLit _ _ pos) = locOf pos
  locOf (BinOp _ _ _ _ pos) = locOf pos
  locOf (And _ _ pos) = locOf pos
  locOf (Or _ _ pos) = locOf pos
  locOf (Not _ pos) = locOf pos
  locOf (Negate _ _ pos) = locOf pos
  locOf (If _ _ _ _ pos) = locOf pos
  locOf (Var ident) = locOf ident
  locOf (Apply _ _ _ pos) = locOf pos
  locOf (LetPat _ _ _ pos) = locOf pos
  locOf (LetWith _ _ _ _ _ pos) = locOf pos
  locOf (Index _ _ _ pos) = locOf pos
  locOf (Iota _ pos) = locOf pos
  locOf (Size _ _ pos) = locOf pos
  locOf (Replicate _ _ pos) = locOf pos
  locOf (Reshape _ _ pos) = locOf pos
  locOf (Transpose _ _ _ pos) = locOf pos
  locOf (Map _ _ _ pos) = locOf pos
  locOf (Reduce _ _ _ _ pos) = locOf pos
  locOf (Zip _ pos) = locOf pos
  locOf (Unzip _ _ pos) = locOf pos
  locOf (Scan _ _ _ _ pos) = locOf pos
  locOf (Filter _ _ _ pos) = locOf pos
  locOf (Redomap _ _ _ _ _ pos) = locOf pos
  locOf (Split _ _ _ pos) = locOf pos
  locOf (Concat _ _ pos) = locOf pos
  locOf (Copy _ pos) = locOf pos
  locOf (DoLoop _ _ _ _ _ _ pos) = locOf pos
  -- locOf for soac2 (Cosmin)
  locOf (Map2 _ _ _ pos) = locOf pos
  locOf (Reduce2 _ _ _ _ pos) = locOf pos
  locOf (Scan2 _ _ _ _ pos) = locOf pos
  locOf (Filter2 _ _ pos) = locOf pos
  locOf (Redomap2 _ _ _ _ _ pos) = locOf pos
  locOf (Min _ _ _ pos) = locOf pos
  locOf (Max _ _ _ pos) = locOf pos

-- | Eagerly evaluated binary operators.  In particular, the
-- short-circuited operators && and || are not here, although an
-- eagerly evaluated variant is.
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
             deriving (Eq, Ord, Enum, Bounded, Show)

-- | The Operator, without whitespace, that corresponds to this
-- @BinOp@.  For example, @opStr Plus@ gives @"+"@.
opStr :: BinOp -> String
opStr Plus = "+"
opStr Minus = "-"
opStr Pow = "pow"
opStr Times = "*"
opStr Divide = "/"
opStr Mod = "%"
opStr ShiftR = ">>"
opStr ShiftL = "<<"
opStr Band = "&"
opStr Xor = "^"
opStr Bor = "|"
opStr LogAnd = "&&"
opStr LogOr = "||"
opStr Equal = "="
opStr Less = "<"
opStr Leq = "<="

-- | Anonymous Function
data LambdaBase ty vn = AnonymFun [ParamBase vn] (ExpBase ty vn) (DeclTypeBase vn) SrcLoc
                      -- ^ @fn int (bool x, char z) => if(x) then ord(z) else ord(z)+1 *)@
                      | CurryFun Name [ExpBase ty vn] (ty vn) SrcLoc -- ^ @op +(4)@
                        deriving (Eq, Ord, Show)

instance Located (LambdaBase ty vn) where
  locOf (AnonymFun _ _ _ loc) = locOf loc
  locOf (CurryFun  _ _ _ loc) = locOf loc

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

-- | An entire L0 program.
newtype ProgBase ty vn = Prog { progFunctions :: [FunDecBase ty vn] }
  deriving (Show)

-- | A set of names.
type Names = S.Set

-- | The name of the default program entry point (main).
defaultEntryPoint :: Name
defaultEntryPoint = nameFromString "main"

-- | @isBuiltInFun k@ is 'True' if @k@ is an element of 'builtInFuns'.
isBuiltInFun :: Name -> Bool
isBuiltInFun fnm = fnm `elem` builtInFuns

-- | A list of names of all built-in functions.
builtInFuns :: [Name]
builtInFuns = map nameFromString ["toReal", "trunc", "sqrt", "log", "exp", "trace", "assertZip"]
