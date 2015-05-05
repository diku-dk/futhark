{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
-- | This Is an ever-changing abstract syntax for Futhark.  Some types,
-- such as @Exp@, are parametrised by type and name representation.
-- See the @doc/@ subdirectory in the Futhark repository for a language
-- reference, or this module may be a little hard to understand.
module Futhark.Representation.AST.Syntax
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
  , ExtType
  , Diet(..)

  -- * Values
  , BasicValue(..)
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
  , SegOp (..)
  , ScanType(..)
  , BinOp (..)
  , ExpT(..)
  , Exp
  , LoopForm (..)
  , LambdaT(..)
  , Lambda
  , ExtLambdaT (..)
  , ExtLambda
  , Lore.RetType

  -- * Definitions
  , ParamT (..)
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

import Data.Monoid
import Data.Loc

import Prelude

import Language.Futhark.Core
import Futhark.Representation.AST.Lore (Lore)
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST.Syntax.Core

type PatElem lore = PatElemT (Lore.LetBound lore)

-- | A pattern is conceptually just a list of names and their types.
data PatternT lore =
  Pattern { patternContextElements :: [PatElem lore]
          , patternValueElements   :: [PatElem lore]
          }

deriving instance Lore lore => Ord (PatternT lore)
deriving instance Lore lore => Show (PatternT lore)
deriving instance Lore lore => Eq (PatternT lore)

instance Monoid (PatternT lore) where
  mempty = Pattern [] []
  Pattern cs1 vs1 `mappend` Pattern cs2 vs2 = Pattern (cs1++cs2) (vs1++vs2)

-- | A type alias for namespace control.
type Pattern = PatternT

-- | A local variable binding.
data Binding lore = Let { bindingPattern :: Pattern lore
                        , bindingLore :: Lore.Exp lore
                        , bindingExp :: Exp lore
                        }

deriving instance Lore lore => Ord (Binding lore)
deriving instance Lore lore => Show (Binding lore)
deriving instance Lore lore => Eq (Binding lore)

-- | The result of a body is a sequence of subexpressions.
type Result = [SubExp]

-- | A body consists of a number of bindings, terminating in a result
-- (essentially a tuple literal).
data BodyT lore = Body { bodyLore :: Lore.Body lore
                       , bodyBindings :: [Binding lore]
                       , bodyResult :: Result
                       }

deriving instance Lore lore => Ord (BodyT lore)
deriving instance Lore lore => Show (BodyT lore)
deriving instance Lore lore => Eq (BodyT lore)

type Body = BodyT

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
             deriving (Eq, Ord, Enum, Bounded, Show)

data PrimOp lore
  = SubExp SubExp
    -- ^ Subexpressions, doubling as tuple literals if the
    -- list has anything but a single element.

  | ArrayLit  [SubExp] Type
    -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
    -- Second arg is the element type of of the rows of the array.
    -- Scalar operations

  | BinOp BinOp SubExp SubExp BasicType
    -- ^ The type is the result type.

  -- Unary Ops: Not for bools and Negate and Complement for ints
  | Not SubExp -- ^ E.g., @! True == False@.
  | Negate SubExp -- ^ E.g., @-(-1) = 1@.
  | Complement SubExp -- ^ E.g., @~(~1) = 1@.

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
  | Scratch BasicType [SubExp]
  -- ^ Create array of given type and shape, with undefined elements.

  -- Array index space transformation.
  | Reshape Certificates [SubExp] VName
   -- ^ 1st arg is the new shape, 2nd arg is the input array *)

  | Rearrange Certificates [Int] VName
  -- ^ Permute the dimensions of the input array.  The list
  -- of integers is a list of dimensions (0-indexed), which
  -- must be a permutation of @[0,n-1]@, where @n@ is the
  -- number of dimensions in the input array.

  | Partition Certificates Int VName VName
    -- ^ First variable is the flag array, second is the element
    -- array.

  | Alloc SubExp
    -- ^ Allocate a memory block.  This really should not be an
    -- expression, but what are you gonna do...
  deriving (Eq, Ord, Show)

data LoopOp lore
  = DoLoop [VName] [(FParam lore, SubExp)] LoopForm (BodyT lore)
    -- ^ @loop {b} <- {a} = {v} (for i < n|while b) do b@.

  | Map Certificates (LambdaT lore) [VName]
    -- ^ @map(op +(1), {1,2,..,n}) = [2,3,..,n+1]@.
    -- 3rd arg is either a tuple of multi-dim arrays
    --   of basic type, or a multi-dim array of basic type.
    -- 4th arg is the input-array row types

  | ConcatMap Certificates (LambdaT lore) [[VName]]

  | Reduce  Certificates (LambdaT lore) [(SubExp, VName)]
  | Scan   Certificates (LambdaT lore) [(SubExp, VName)]
  | Redomap Certificates (LambdaT lore) (LambdaT lore) [SubExp] [VName]
  | Stream  Certificates [SubExp] [VName] (ExtLambdaT lore)

-- | a @scan op ne xs@ can either be /'ScanInclusive'/ or /'ScanExclusive'/.
-- Inclusive = @[ ne `op` x_1 , ne `op` x_1 `op` x_2 , ... , ne `op` x_1 ... `op` x_n ]@
-- Exclusive = @[ ne, ne `op` x_1, ... , ne `op` x_1 ... `op` x_{n-1} ]@
--
-- Both versions generate arrays of the same size as @xs@ (this is not
-- always the semantics).
--
-- An easy way to remember which is which, is that inclusive /includes/
-- the last element in the calculation, whereas the exclusive does not
data ScanType = ScanInclusive
              | ScanExclusive
              deriving(Eq, Ord, Show)

-- | Segmented version of SOACS that use flat array representation.
-- This means a /single/ flat array for data, and segment descriptors
-- (integer arrays) for each dimension of the array.
--
-- For example the array
-- @ [ [ [1,2] , [3,4,5] ]
--   , [ [6]             ]
--   , []
--   ]
-- @
--
-- Can be represented as
-- @ data  = [1,2, 3,4,5, 6    ]
--   seg_1 = [2,   3,     1,   ]
--   seg_0 = [2,          1,  0]
-- @
data SegOp lore = SegReduce Certificates (LambdaT lore) [(SubExp, VName)] VName
                  -- ^ @map (\xs -> reduce(op,ne,xs), xss@ can loosely
                  -- be transformed into
                  -- @segreduce(op, ne, xss_flat, xss_descpritor)@
                  --
                  -- Note that this requires the neutral element to be constant
                | SegScan Certificates ScanType (LambdaT lore) [(SubExp, VName)] VName
                  -- ^ Identical to 'Scan', except that the last arg
                  -- is a segment descriptor.
                deriving (Eq, Ord, Show)

deriving instance Lore lore => Eq (LoopOp lore)
deriving instance Lore lore => Show (LoopOp lore)
deriving instance Lore lore => Ord (LoopOp lore)

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

  | SegOp (SegOp lore)

  | Apply  Name [(SubExp, Diet)] (Lore.RetType lore)

  | If     SubExp (BodyT lore) (BodyT lore) [ExtType]

deriving instance Lore lore => Eq (ExpT lore)
deriving instance Lore lore => Show (ExpT lore)
deriving instance Lore lore => Ord (ExpT lore)

-- | A type alias for namespace control.
type Exp = ExpT

-- | Anonymous function for use in a tuple-SOAC.
data LambdaT lore =
  Lambda { lambdaParams     :: [LParam lore]
         , lambdaBody       :: BodyT lore
         , lambdaReturnType :: [Type]
         }

deriving instance Lore lore => Eq (LambdaT lore)
deriving instance Lore lore => Show (LambdaT lore)
deriving instance Lore lore => Ord (LambdaT lore)

type Lambda = LambdaT

-- | Anonymous function for use in a tuple-SOAC.
data ExtLambdaT lore =
  ExtLambda { extLambdaParams     :: [LParam lore]
            , extLambdaBody       :: BodyT lore
            , extLambdaReturnType :: [ExtType]
            }

deriving instance Lore lore => Eq (ExtLambdaT lore)
deriving instance Lore lore => Show (ExtLambdaT lore)
deriving instance Lore lore => Ord (ExtLambdaT lore)

type ExtLambda = ExtLambdaT

type FParam lore = ParamT (Lore.FParam lore)

type LParam lore = ParamT (Lore.LParam lore)

-- | Function Declarations
data FunDecT lore = FunDec { funDecName :: Name
                           , funDecRetType :: Lore.RetType lore
                           , funDecParams :: [FParam lore]
                           , funDecBody :: BodyT lore
                           }

deriving instance Lore lore => Eq (FunDecT lore)
deriving instance Lore lore => Show (FunDecT lore)
deriving instance Lore lore => Ord (FunDecT lore)

type FunDec = FunDecT

-- | An entire Futhark program.
newtype ProgT lore = Prog { progFunctions :: [FunDec lore] }
                     deriving (Eq, Ord, Show)

type Prog = ProgT
