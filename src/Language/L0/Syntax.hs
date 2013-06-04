{-# LANGUAGE FlexibleInstances #-}
-- | This Is an ever-changing abstract syntax for L0.  Some types,
-- such as @Exp@, are parametrised by type representation.  See
-- "L0C.TypeChecker" and the 'Exp' type for more information.
module Language.L0.Syntax
  (

   -- * Names
   Name
  , nameToString
  , nameFromString

  -- * Types
  , Uniqueness(..)
  , DimSize
  , ArraySize
  , ElemType(..)
  , Type(..)

  -- * Values
  , Value(..)

  -- * Abstract syntax tree
  , Ident(..)
  , Exp(..)
  , BinOp(..)
  , opStr
  , Lambda(..)
  , TupIdent(..)

  -- * Definitions
  , FunDec
  , Prog

  -- * Special identifiers
  , defaultEntryPoint
  , isBuiltInFun
  , builtInFuns
  )
  where

import Data.Array
import Data.Loc
import Data.Monoid
import qualified Data.Text as T

-- | The abstract (not really) type representing names in the L0
-- compiler.  'String's, being lists of characters, are very slow,
-- while 'T.Text's are based on byte-arrays.
type Name = T.Text

-- | Convert a name to the corresponding list of characters.
nameToString :: Name -> String
nameToString = T.unpack

-- | Convert a list of characters to the corresponding name.
nameFromString :: String -> Name
nameFromString = T.pack

data Uniqueness = Unique | Nonunique
                  deriving (Eq, Ord, Show)

instance Monoid Uniqueness where
  mempty = Unique
  _ `mappend` Nonunique = Nonunique
  Nonunique `mappend` _ = Nonunique
  u `mappend` _         = u

-- | Don't use this for anything.
type DimSize = Exp (Maybe Type)

type ArraySize = [Maybe DimSize]

data ElemType = Int
              | Bool
              | Char
              | Real
              | Tuple [Type]
                deriving (Eq, Ord, Show)

-- | L0 Types: Int, Bool, Char, Tuple, multidim-regular Array
--  TODO: please add float, double, long int, etc.
data Type = Elem ElemType
          | Array ElemType ArraySize Uniqueness
            -- ^ 1st arg: array's element type, 2nd arg: length of
            -- first dimension and lengths of remaining dimensions, if
            -- any.
            deriving (Eq, Ord, Show)

-- | Every possible value in L0.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = IntVal !Int
           | RealVal !Double
           | LogVal !Bool
           | CharVal !Char
           | TupVal ![Value]
           | ArrayVal !(Array Int Value) Type
             -- ^ It is assumed that the array is 0-indexed.  The type
             -- is the row type.
             deriving (Eq, Ord, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data Ident ty = Ident { identName :: Name
                      , identType :: ty
                      , identSrcLoc :: SrcLoc
                      }
                deriving (Eq, Ord, Show)

instance Located (Ident ty) where
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
data Exp ty = Literal Value SrcLoc
            | TupLit    [Exp ty] SrcLoc
            -- ^ Tuple literals, e.g., (1+3, (x, y+z)).  Second
            -- argument is the tuple's type.
            | ArrayLit  [Exp ty] ty SrcLoc
            -- ^ Array literals, e.g., { {1+x, 3}, {2, 1+4} }.  Second
            -- arg is the type of of the rows of the array (not the
            -- element type).
            | BinOp BinOp (Exp ty) (Exp ty) ty SrcLoc
            -- Binary Ops for Booleans
            | And    (Exp ty) (Exp ty) SrcLoc
            | Or     (Exp ty) (Exp ty) SrcLoc
            -- Unary Ops: Not for bools and Negate for ints
            | Not    (Exp ty) SrcLoc -- e.g., not True = False
            | Negate (Exp ty) ty SrcLoc -- e.g., ~(~1) = 1
            | If     (Exp ty) (Exp ty) (Exp ty) ty SrcLoc
            | Var    (Ident ty)
            -- Function Call and Let Construct
            | Apply  Name [Exp ty] ty SrcLoc
            | LetPat (TupIdent ty) (Exp ty) (Exp ty) SrcLoc

            | LetWith (Ident ty) (Ident ty) [Exp ty] (Exp ty) (Exp ty) SrcLoc
            -- ^ Array Indexing and Array Constructors

            | Index (Ident ty) [Exp ty] ty SrcLoc
             -- ^ 3rd arg is the result type

            | Iota (Exp ty) SrcLoc
            -- ^ @iota(n) = {0,1,..,n-1@

            | Size (Exp ty) SrcLoc
            -- ^ The number of elements in an array.

            | Replicate (Exp ty) (Exp ty) SrcLoc
            -- ^ @replicate(3,1) = {1, 1, 1}@

            | Reshape [Exp ty] (Exp ty) SrcLoc
             -- ^ 1st arg is the new shape, 2nd arg is the input array *)

            | Transpose (Exp ty) SrcLoc
             -- ^ 1st arg is the (input) to-be-transSrcLoced array.

            -- Second-Order Array Combinators
            -- accept curried and anonymous
            -- functions as (first) params
            | Map (Lambda ty) (Exp ty) ty SrcLoc
             -- @map(op +(1), {1,2,..,n}) = {2,3,..,n+1}@
             -- 3st arg is the input-array row type

            | Reduce (Lambda ty) (Exp ty) (Exp ty) ty SrcLoc
             -- @reduce(op +, 0, {1,2,..,n}) = (0+1+2+..+n)@
             -- 4th arg is the input-array element type

            | Zip [(Exp ty, ty)] SrcLoc
            -- ^ Normal zip supporting variable number of arguments.
            -- The type paired to each expression is the element type
            -- of the array returned by that expression.

            | Unzip (Exp ty) [ty] SrcLoc
            -- ^ Unzip that can unzip tuples of arbitrary size.  The
            -- types are the elements of the tuple.

            | Scan (Lambda ty) (Exp ty) (Exp ty) ty SrcLoc
             -- ^ @scan(plus, 0, { 1, 2, 3 }) = { 1, 3, 6 }@.
             -- 4th arg is the element type of the input array

            | Filter (Lambda ty) (Exp ty) ty SrcLoc
            -- ^ 3rd arg is the row type of the input (and
            -- result) array


            | Mapall (Lambda ty) (Exp ty) SrcLoc
             -- ^ @mapall(op ~, {{1,~2}, {~3,4}}) = {{~1,2}, {3,~4}}@.

            | Redomap (Lambda ty) (Lambda ty) (Exp ty) (Exp ty) ty SrcLoc
             -- ^ @redomap(g, f, n, a) = reduce(g, n, map(f, a))@.
             -- 5th arg is the row type of the input  array.

            | Split (Exp ty) (Exp ty) ty SrcLoc
             -- ^ @split(1, { 1, 2, 3, 4 }) = ({1},{2, 3, 4})@.
             -- 3rd arg is the element type of the input array

            | Concat (Exp ty) (Exp ty) SrcLoc
             -- ^ @concat ({1},{2, 3, 4}) = {1, 2, 3, 4}@.

            | Copy (Exp ty) SrcLoc
            -- ^ Copy the value return by the expression.  This only
            -- makes a difference in do-loops with merge variables.

            | DoLoop
              (TupIdent ty) -- Merge variable pattern
              (Exp ty) -- Initial values of merge variables.
              (Ident ty) -- Iterator.
              (Exp ty) -- Upper bound.
              (Exp ty) -- Loop body.
              (Exp ty) -- Let-body.
              SrcLoc

            -----------------------------------------------------
            -- Second-Order Array Combinators
            -- with support for n-ary multi-dim 
            -- arrays of BASIC type (i.e., no tuples inside)
            -- accept curried and anonymous
            -- functions as (first) params
            -----------------------------------------------------
            | Map2 (Lambda ty) [Exp ty] ty SrcLoc
             -- @map(op +(1), {1,2,..,n}) = {2,3,..,n+1}@
             -- 2nd arg is either a tuple of multi-dim arrays 
             --   of basic type, or a multi-dim array of basic type.
             -- 3st arg is the  input-array row type
             --   (either a tuple or an array)

            | Reduce2 (Lambda ty) (Exp ty) [Exp ty] ty SrcLoc
            | Scan2   (Lambda ty) (Exp ty) [Exp ty] ty SrcLoc
            | Filter2 (Lambda ty) [Exp ty]          SrcLoc
            | Mapall2 (Lambda ty) [Exp ty]          SrcLoc
            | Redomap2(Lambda ty) (Lambda ty) (Exp ty) [Exp ty] ty SrcLoc

              deriving (Eq, Ord, Show)

instance Located (Exp ty) where
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
  locOf (Size _ pos) = locOf pos
  locOf (Replicate _ _ pos) = locOf pos
  locOf (Reshape _ _ pos) = locOf pos
  locOf (Transpose _ pos) = locOf pos
  locOf (Map _ _ _ pos) = locOf pos
  locOf (Reduce _ _ _ _ pos) = locOf pos
  locOf (Zip _ pos) = locOf pos
  locOf (Unzip _ _ pos) = locOf pos
  locOf (Scan _ _ _ _ pos) = locOf pos
  locOf (Filter _ _ _ pos) = locOf pos
  locOf (Mapall _ _ pos) = locOf pos
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
  locOf (Mapall2 _ _ pos) = locOf pos
  locOf (Redomap2 _ _ _ _ _ pos) = locOf pos

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

-- ^ Print the operator, without whitespace, that corresponds to this
-- @BinOp@.
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
data Lambda ty = AnonymFun [Ident Type] (Exp ty) Type SrcLoc
                    -- fn int (bool x, char z) => if(x) then ord(z) else ord(z)+1 *)
               | CurryFun Name [Exp ty] ty SrcLoc
                    -- op +(4) *)
                 deriving (Eq, Ord, Show)

instance Located (Lambda ty) where
  locOf (AnonymFun _ _ _ loc) = locOf loc
  locOf (CurryFun  _ _ _ loc) = locOf loc

-- | Tuple Identifier, i.e., pattern matching
data TupIdent ty = TupId [TupIdent ty] SrcLoc
                 | Id (Ident ty)
                   deriving (Eq, Ord, Show)

instance Located (TupIdent ty) where
  locOf (TupId _ loc) = locOf loc
  locOf (Id ident) = locOf ident

-- | Function Declarations
type FunDec ty = (Name,Type,[Ident Type],Exp ty,SrcLoc)

-- | An entire L0 program.
type Prog ty = [FunDec ty]

-- | The name of the default program entry point (main).
defaultEntryPoint :: Name
defaultEntryPoint = nameFromString "main"

-- | @isBuiltInFun k@ is 'True' if @k@ is an element of 'builtInFuns'.
isBuiltInFun :: Name -> Bool
isBuiltInFun fnm = fnm `elem` builtInFuns

-- | A list of names of all built-in functions.
builtInFuns :: [Name]
builtInFuns = map nameFromString ["toReal", "trunc", "sqrt", "log", "exp", "trace", "assertZip"]
