{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
-- | Internal abstract syntax for Futhark.
module Futhark.Representation.AST.Syntax
  (
    module Language.Futhark.Core
  , module Futhark.Representation.AST.Annotations

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
  , Bindage (..)
  , PatElemT (..)
  , PatElem
  , PatternT (..)
  , Pattern
  , Binding(..)
  , Result
  , BodyT(..)
  , Body
  , PrimOp (..)
  , LoopOp (..)
  , UnOp (..)
  , BinOp (..)
  , CmpOp (..)
  , ConvOp (..)
  , DimChange (..)
  , ShapeChange
  , ExpT(..)
  , Exp
  , LoopForm (..)
  , LambdaT(..)
  , Lambda
  , ExtLambdaT (..)
  , ExtLambda
  , StreamForm(..)

  -- * Definitions
  , ParamT (..)
  , Param
  , FParam
  , LParam
  , FunDecT (..)
  , FunDec
  , ProgT(..)
  , Prog

  -- * Miscellaneous
  , Names
  )
  where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Loc

import Prelude

import Language.Futhark.Core
import Futhark.Representation.AST.Annotations
import Futhark.Representation.AST.Syntax.Core

-- | A pattern is conceptually just a list of names and their types.
data PatternT attr =
  Pattern { patternContextElements :: [PatElem attr]
          , patternValueElements   :: [PatElem attr]
          }
  deriving (Ord, Show, Eq)

instance Monoid (PatternT lore) where
  mempty = Pattern [] []
  Pattern cs1 vs1 `mappend` Pattern cs2 vs2 = Pattern (cs1++cs2) (vs1++vs2)

-- | A type alias for namespace control.
type Pattern lore = PatternT (LetAttr lore)

-- | A local variable binding.
data Binding lore = Let { bindingPattern :: Pattern lore
                        , bindingLore :: ExpAttr lore
                        , bindingExp :: Exp lore
                        }

deriving instance Annotations lore => Ord (Binding lore)
deriving instance Annotations lore => Show (Binding lore)
deriving instance Annotations lore => Eq (Binding lore)

-- | The result of a body is a sequence of subexpressions.
type Result = [SubExp]

-- | A body consists of a number of bindings, terminating in a result
-- (essentially a tuple literal).
data BodyT lore = Body { bodyLore :: BodyAttr lore
                       , bodyBindings :: [Binding lore]
                       , bodyResult :: Result
                       }

deriving instance Annotations lore => Ord (BodyT lore)
deriving instance Annotations lore => Show (BodyT lore)
deriving instance Annotations lore => Eq (BodyT lore)

type Body = BodyT

-- | Various unary operators.  It is a bit ad-hoc what is a unary
-- operator and what is a built-in function.  Perhaps these should all
-- go away eventually.
data UnOp = Not -- ^ E.g., @! True == False@.
          | Complement IntType -- ^ E.g., @~(~1) = 1@.
          | Abs IntType -- ^ @abs(-2) = 2@.
          | FAbs FloatType -- ^ @abs(-2.0) = 2.0@.
          | Signum IntType -- ^ @signum(2)@ = 1.
             deriving (Eq, Ord, Show)

-- | Binary operators.  These correspond closely to the binary operators in
-- LLVM.  Most are parametrised by their expected input and output
-- types.
data BinOp = Add IntType -- ^ Integer addition.
           | FAdd FloatType -- ^ Floating-point addition.

           | Sub IntType -- ^ Integer subtraction.
           | FSub FloatType -- ^ Floating-point subtraction.

           | Mul IntType -- ^ Integer multiplication.
           | FMul FloatType -- ^ Floating-point multiplication.

           | UDiv IntType
             -- ^ Unsigned integer division.  Rounds towards
             -- negativity infinity.  Note: this is different
             -- from LLVM.
           | SDiv IntType
             -- ^ Signed integer division.  Rounds towards
             -- negativity infinity.  Note: this is different
             -- from LLVM.
           | FDiv FloatType -- ^ Floating-point division.

           | UMod IntType
             -- ^ Unsigned integer modulus; the countepart to 'UDiv'.
           | SMod IntType
             -- ^ Signed integer modulus; the countepart to 'SDiv'.

           | SQuot IntType
             -- ^ Signed integer division.  Rounds towards zero.
             -- This corresponds to the @sdiv@ instruction in LLVM.
           | SRem IntType
             -- ^ Signed integer division.  Rounds towards zero.
             -- This corresponds to the @srem@ instruction in LLVM.

           | Shl IntType -- ^ Left-shift.
           | LShr IntType -- ^ Logical right-shift, zero-extended.
           | AShr IntType -- ^ Arithmetic right-shift, sign-extended.

           | And IntType -- ^ Bitwise and.
           | Or IntType -- ^ Bitwise or.
           | Xor IntType -- ^ Bitwise exclusive-or.

           | SPow IntType -- ^ Signed integer exponentatation.
           | FPow FloatType -- ^ Floating-point exponentatation.

           | LogAnd -- ^ Boolean and - not short-circuiting.
           | LogOr -- ^ Boolean or - not short-circuiting.
             deriving (Eq, Ord, Show)

-- | Comparison operators are like 'BinOp's, but they return 'Bool's.
-- The somewhat ugly constructor names are straight out of LLVM.
data CmpOp = CmpEq -- ^ All types equality.
           | CmpUlt IntType -- ^ Unsigned less than.
           | CmpUle IntType -- ^ Unsigned less than or equal.
           | CmpSlt IntType -- ^ Signed less than.
           | CmpSle IntType -- ^ Signed less than or equal.

             -- Comparison operators for floating-point values.  TODO: extend
             -- this to handle NaNs and such, like the LLVM fcmp instruction.
           | FCmpLt FloatType -- ^ Floating-point less than.
           | FCmpLe FloatType -- ^ Floating-point less than or equal.

             deriving (Eq, Ord, Show)

-- | Conversion operators try to generalise the @from t0 x to t1@
-- instructions from LLVM.
data ConvOp = Trunc IntType IntType
              -- ^ Truncate the former integer type to the latter.
            | ZExt IntType IntType
              -- ^ Zero-extend the former integer type to the latter.
            | SExt IntType IntType
              -- ^ Sign-extend the former integer type to the latter.
            | FPTrunc FloatType FloatType
              -- ^ Truncate the former floating-point type to the latter.
            | FPExt FloatType FloatType
              -- ^ Extend the former floating-point type to the latter.
            | FPToUI FloatType IntType
              -- ^ Convert a floating-point value to the nearest
              -- unsigned integer (rounding towards zero).
            | FPToSI FloatType IntType
              -- ^ Convert a floating-point value to the nearest
              -- signed integer (rounding towards zero).
            | UIToFP IntType FloatType
              -- ^ Convert an unsigned integer to a floating-point value.
            | SIToFP IntType FloatType
              -- ^ Convert a signed integer to a floating-point value.
             deriving (Eq, Ord, Show)

-- | The new dimension in a 'Reshape'-like operation.  This allows us to
-- disambiguate "real" reshapes, that change the actual shape of the
-- array, from type coercions that are just present to make the types
-- work out.
data DimChange d = DimCoercion d
                   -- ^ The new dimension is guaranteed to be numerically
                   -- equal to the old one.
                 | DimNew d
                   -- ^ The new dimension is not necessarily numerically
                   -- equal to the old one.
                 deriving (Eq, Ord, Show)

instance Functor DimChange where
  fmap f (DimCoercion d) = DimCoercion $ f d
  fmap f (DimNew      d) = DimNew $ f d

instance Foldable DimChange where
  foldMap f (DimCoercion d) = f d
  foldMap f (DimNew      d) = f d

instance Traversable DimChange where
  traverse f (DimCoercion d) = DimCoercion <$> f d
  traverse f (DimNew      d) = DimNew <$> f d

-- | A list of 'DimChange's, indicating the new dimensions of an array.
type ShapeChange d = [DimChange d]

data PrimOp lore
  = SubExp SubExp
    -- ^ Subexpressions, doubling as tuple literals if the
    -- list has anything but a single element.

  | ArrayLit  [SubExp] Type
    -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
    -- Second arg is the element type of of the rows of the array.
    -- Scalar operations

  | UnOp UnOp SubExp
    -- ^ Unary operation - result type is the same as the input type.

  | BinOp BinOp SubExp SubExp
    -- ^ Binary operation - result type is the same as the input
    -- types.

  | CmpOp CmpOp SubExp SubExp
    -- ^ Comparison - result type is always boolean.

  | ConvOp ConvOp SubExp
    -- ^ Conversion "casting".

  -- Assertion management.
  | Assert SubExp SrcLoc
  -- ^ Turn a boolean into a certificate, halting the
  -- program if the boolean is false.

  -- Primitive array operations

  | Index Certificates
          VName
          [SubExp]

  -- ^ 3rd arg are (optional) certificates for bounds
  -- checking.  If given (even as an empty list), no
  -- run-time bounds checking is done.

  | Split Certificates [SubExp] VName
  -- ^ 2nd arg is sizes of arrays you back, which is
  -- different from what the external language does.
  -- In the internal langauge,
  -- @a = [1,2,3,4]@
  -- @split( (1,0,2) , a ) = {[1], [], [2,3]}@

  | Concat Certificates VName [VName] SubExp
  -- ^ @concat([1],[2, 3, 4]) = [1, 2, 3, 4]@.

  | Copy VName
  -- ^ Copy the given array.  The result will not alias anything.

  -- Array construction.
  | Iota SubExp
  -- ^ @iota(n) = [0,1,..,n-1]@
  | Replicate SubExp SubExp
  -- ^ @replicate(3,1) = [1, 1, 1]@
  | Scratch PrimType [SubExp]
  -- ^ Create array of given type and shape, with undefined elements.

  -- Array index space transformation.
  | Reshape Certificates (ShapeChange SubExp) VName
   -- ^ 1st arg is the new shape, 2nd arg is the input array *)

  | Rearrange Certificates [Int] VName
  -- ^ Permute the dimensions of the input array.  The list
  -- of integers is a list of dimensions (0-indexed), which
  -- must be a permutation of @[0,n-1]@, where @n@ is the
  -- number of dimensions in the input array.

  | Stripe Certificates SubExp VName

  | Unstripe Certificates SubExp VName

  | Partition Certificates Int VName [VName]
    -- ^ First variable is the flag array, second is the element
    -- arrays.  If no arrays are given, the returned offsets are zero,
    -- and no arrays are returned.
  deriving (Eq, Ord, Show)

data LoopOp lore
   = DoLoop [VName] [(FParam lore, SubExp)] LoopForm (BodyT lore)
    -- ^ @loop {b} <- {a} = {v} (for i < n|while b) do b@.

deriving instance Annotations lore => Eq (LoopOp lore)
deriving instance Annotations lore => Show (LoopOp lore)
deriving instance Annotations lore => Ord (LoopOp lore)

data StreamForm lore  = MapLike    StreamOrd
                      | RedLike    StreamOrd Commutativity (LambdaT lore) [SubExp]
                      | Sequential [SubExp]
                        deriving (Eq, Ord, Show)

data LoopForm = ForLoop VName SubExp
              | WhileLoop VName
              deriving (Eq, Show, Ord)

-- | Futhark Expression Language: literals + vars + int binops + array
-- constructors + array combinators (SOAC) + if + function calls +
-- let + tuples (literals & identifiers) TODO: please add float,
-- double, long int, etc.
data ExpT lore
  = PrimOp (PrimOp lore)
    -- ^ A simple (non-recursive) operation.

  | LoopOp (LoopOp lore)

  | Apply  Name [(SubExp, Diet)] (RetType lore)

  | If     SubExp (BodyT lore) (BodyT lore) [ExtType]

  | Op (Op lore)

deriving instance Annotations lore => Eq (ExpT lore)
deriving instance Annotations lore => Show (ExpT lore)
deriving instance Annotations lore => Ord (ExpT lore)

-- | A type alias for namespace control.
type Exp = ExpT

-- | Anonymous function for use in a SOAC.
data LambdaT lore =
  Lambda { lambdaIndex      :: VName
         , lambdaParams     :: [LParam lore]
         , lambdaBody       :: BodyT lore
         , lambdaReturnType :: [Type]
         }

deriving instance Annotations lore => Eq (LambdaT lore)
deriving instance Annotations lore => Show (LambdaT lore)
deriving instance Annotations lore => Ord (LambdaT lore)

type Lambda = LambdaT

-- | Anonymous function for use in a SOAC, with an existential return
-- type.
data ExtLambdaT lore =
  ExtLambda { extLambdaIndex      :: VName
            , extLambdaParams     :: [LParam lore]
            , extLambdaBody       :: BodyT lore
            , extLambdaReturnType :: [ExtType]
            }

deriving instance Annotations lore => Eq (ExtLambdaT lore)
deriving instance Annotations lore => Show (ExtLambdaT lore)
deriving instance Annotations lore => Ord (ExtLambdaT lore)

type ExtLambda = ExtLambdaT

type FParam lore = ParamT (FParamAttr lore)

type LParam lore = ParamT (LParamAttr lore)

-- | Function Declarations
data FunDecT lore = FunDec { funDecName :: Name
                           , funDecRetType :: RetType lore
                           , funDecParams :: [FParam lore]
                           , funDecBody :: BodyT lore
                           }

deriving instance Annotations lore => Eq (FunDecT lore)
deriving instance Annotations lore => Show (FunDecT lore)
deriving instance Annotations lore => Ord (FunDecT lore)

type FunDec = FunDecT

-- | An entire Futhark program.
newtype ProgT lore = Prog { progFunctions :: [FunDec lore] }
                     deriving (Eq, Ord, Show)

type Prog = ProgT
