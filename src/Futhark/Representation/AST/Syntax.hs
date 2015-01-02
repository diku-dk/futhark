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
  , PatBindee
  , PatternT (..)
  , Pattern
  , Binding(..)
  , Result(..)
  , BodyT(..)
  , Body
  , PrimOp (..)
  , LoopOp (..)
  , ExpT(..)
  , Exp
  , LambdaT(..)
  , Lambda
  , Lore.ResType

  -- * Definitions
  , FParam
  , FunDecT (..)
  , FunDec
  , ProgT(..)
  , Prog

  -- * Miscellaneous
  , Names
  )
  where

import Data.Loc
import Data.Monoid

import Language.Futhark.Core
import Futhark.Representation.AST.Lore (Lore)
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST.Syntax.Core

-- | The kind of 'Bindee' used in a 'Pattern'.
type PatBindee lore = Bindee (Lore.LetBound lore)

-- | A pattern is conceptually just a list of names and their types.
newtype PatternT lore =
  Pattern { patternBindees :: [PatBindee lore] }

deriving instance Lore lore => Ord (PatternT lore)
deriving instance Lore lore => Show (PatternT lore)
deriving instance Lore lore => Eq (PatternT lore)

instance Monoid (PatternT lore) where
  mempty = Pattern []
  Pattern l1 `mappend` Pattern l2 = Pattern $ l1 ++ l2

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

-- | The result of a body - a sequence of subexpressions, possibly
-- predicated on one or more certificates.
data Result = Result { resultCertificates :: Certificates
                     , resultSubExps :: [SubExp]
                     , resultSrcLoc :: SrcLoc
                     }
                   deriving (Eq, Ord, Show)

instance Located Result where
  locOf = locOf . resultSrcLoc

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

instance Located (BodyT lore) where
  locOf = locOf . bodyResult

data PrimOp lore
  = SubExp SubExp
    -- ^ Subexpressions, doubling as tuple literals if the
    -- list has anything but a single element.

  | ArrayLit  [SubExp] Type SrcLoc
    -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
    -- Second arg is the element type of of the rows of the array.
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

  | Update Certificates Ident [SubExp] SubExp SrcLoc
  -- ^ @a with [i1,i2,i3] <- v@.

  | Split Certificates SubExp SubExp SubExp SrcLoc
  -- ^ @split(1, [ 1, 2, 3, 4 ]) = {[1],[2, 3, 4]}@.

  | Concat Certificates SubExp SubExp SubExp SrcLoc
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

  | Rotate Certificates Int SubExp SrcLoc
  -- ^ @rotate(n,a)@ returns a new array, where the element
  -- @a[i]@ is at position @i+n@, cycling over to the
  -- beginning of the array.

  | Alloc SubExp SrcLoc
    -- ^ Allocate a memory block.  This really should not be an
    -- expression, but what are you gonna do...
  deriving (Eq, Ord, Show)

instance Located (PrimOp lore) where
  locOf (SubExp se) = locOf se
  locOf (ArrayLit _ _ pos) = locOf pos
  locOf (BinOp _ _ _ _ pos) = locOf pos
  locOf (Not _ pos) = locOf pos
  locOf (Negate _ pos) = locOf pos
  locOf (Index _ _ _ pos) = locOf pos
  locOf (Update _ _ _ _ loc) = locOf loc
  locOf (Iota _ pos) = locOf pos
  locOf (Replicate _ _ pos) = locOf pos
  locOf (Reshape _ _ _ pos) = locOf pos
  locOf (Rearrange _ _ _ pos) = locOf pos
  locOf (Rotate _ _ _ pos) = locOf pos
  locOf (Split _ _ _ _ pos) = locOf pos
  locOf (Concat _ _ _ _ pos) = locOf pos
  locOf (Copy _ pos) = locOf pos
  locOf (Assert _ loc) = locOf loc
  locOf (Conjoin _ loc) = locOf loc
  locOf (Alloc _ loc) = locOf loc

data LoopOp lore
  = DoLoop [Ident] [(FParam lore, SubExp)] Ident SubExp (BodyT lore) SrcLoc
    -- ^ @loop {b} <- {a} = {v} for i < n do b@.

  | Map Certificates (LambdaT lore) [SubExp] SrcLoc
    -- ^ @map(op +(1), {1,2,..,n}) = [2,3,..,n+1]@.
    -- 3rd arg is either a tuple of multi-dim arrays
    --   of basic type, or a multi-dim array of basic type.
    -- 4th arg is the input-array row types

  | Reduce  Certificates (LambdaT lore) [(SubExp, SubExp)] SrcLoc
  | Scan   Certificates (LambdaT lore) [(SubExp, SubExp)] SrcLoc
  | Filter  Certificates (LambdaT lore) [SubExp] SrcLoc
  | Redomap Certificates (LambdaT lore) (LambdaT lore) [SubExp] [SubExp] SrcLoc

deriving instance Lore lore => Eq (LoopOp lore)
deriving instance Lore lore => Show (LoopOp lore)
deriving instance Lore lore => Ord (LoopOp lore)

instance Located (LoopOp lore) where
  locOf (DoLoop _ _ _ _ _ loc) = locOf loc
  locOf (Map _ _ _ pos) = locOf pos
  locOf (Reduce _ _ _ pos) = locOf pos
  locOf (Scan _ _ _ pos) = locOf pos
  locOf (Filter _ _ _ pos) = locOf pos
  locOf (Redomap _ _ _ _ _ pos) = locOf pos

-- | Futhark Expression Language: literals + vars + int binops + array
-- constructors + array combinators (SOAC) + if + function calls +
-- let + tuples (literals & identifiers) TODO: please add float,
-- double, long int, etc.
data ExpT lore
  = PrimOp (PrimOp lore)
    -- ^ A simple (non-recursive) operation.

  | LoopOp (LoopOp lore)

  | Apply  Name [(SubExp, Diet)] (Lore.ResType lore) SrcLoc

  | If     SubExp (BodyT lore) (BodyT lore) (Lore.ResType lore) SrcLoc

deriving instance Lore lore => Eq (ExpT lore)
deriving instance Lore lore => Show (ExpT lore)
deriving instance Lore lore => Ord (ExpT lore)

-- | A type alias for namespace control.
type Exp = ExpT

instance Located (ExpT lore) where
  locOf (PrimOp op) = locOf op
  locOf (LoopOp op) = locOf op
  locOf (Apply _ _ _ pos) = locOf pos
  locOf (If _ _ _ _ pos) = locOf pos

-- | Anonymous function for use in a tuple-SOAC.
data LambdaT lore =
  Lambda { lambdaParams     :: [Param]
         , lambdaBody       :: BodyT lore
         , lambdaReturnType :: [Type]
         , lambdaSrcLoc     :: SrcLoc
         }
  deriving (Eq, Ord, Show)

type Lambda = LambdaT

instance Located (LambdaT lore) where
  locOf (Lambda _ _ _ loc) = locOf loc

-- | A (non-lambda) function parameter.
type FParam lore = Bindee (Lore.FParam lore)

-- | Function Declarations
data FunDecT lore = FunDec { funDecName :: Name
                           , funDecRetType :: Lore.ResType lore
                           , funDecParams :: [FParam lore]
                           , funDecBody :: BodyT lore
                           , funDecSrcLoc :: SrcLoc
                           }

deriving instance Lore lore => Eq (FunDecT lore)
deriving instance Lore lore => Show (FunDecT lore)
deriving instance Lore lore => Ord (FunDecT lore)

type FunDec = FunDecT

instance Located (FunDecT lore) where
  locOf = locOf . funDecSrcLoc

-- | An entire Futhark program.
newtype ProgT lore = Prog { progFunctions :: [FunDec lore] }
                     deriving (Eq, Ord, Show)

type Prog = ProgT
