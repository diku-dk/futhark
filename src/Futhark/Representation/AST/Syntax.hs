{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving #-}
-- | This Is an ever-changing abstract syntax for Futhark.  Some types,
-- such as @Exp@, are parametrised by type and name representation.
-- See the @doc/@ subdirectory in the Futhark repository for a language
-- reference, or this module may be a little hard to understand.
module Futhark.Representation.AST.Syntax
  (
    module Language.Futhark.Core

  -- * Types
  , Uniqueness(..)
  , ShapeT(..)
  , Shape
  , ExtDimSize(..)
  , ExtShapeT(..)
  , ExtShape
  , Rank(..)
  , ArrayShape(..)
  , TypeBase(..)
  , Type
  , DeclType
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
  , SubExpT(..)
  , SubExp
  , Binding(..)
  , ResultT(..)
  , Result
  , BodyT(..)
  , Body
  , ExpT(..)
  , Exp
  , ResType
  , RetType
  , LambdaT(..)
  , Lambda

  -- * Definitions
  , FunDec
  , funDecName
  , funDecBody
  , funDecRetType
  , ProgT(..)
  , Prog

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

import Language.Futhark.Core
import qualified Futhark.Representation.AST.Lore as Lore

-- | The size of an array type as a list of its dimension sizes.  If a
-- variable, that variable must be in scope where this array is used.
-- When compared for equality, only the number of dimensions is
-- considered.
newtype ShapeT lore = Shape { shapeDims :: [SubExp lore] }
                      deriving (Show)

-- | Type alias for namespace control.
type Shape = ShapeT

instance Eq (ShapeT lore) where
  Shape l1 == Shape l2 = length l1 == length l2

instance Ord (ShapeT lore) where
  compare = comparing shapeRank

-- | The size of this dimension.
data ExtDimSize lore = Free (SubExp lore) -- ^ Some known dimension.
                     | Ext Int -- ^ Existentially quantified.

deriving instance Lore.Proper lore => Show (ExtDimSize lore)

-- | Like 'Shape' but some of its elements may be bound in a local
-- environment instead.  These are denoted with integral indices.
newtype ExtShapeT lore = ExtShape { extShapeDims :: [ExtDimSize lore] }
                       deriving (Show)

-- | Type alias for namespace control.
type ExtShape = ExtShapeT

instance Eq (ExtShapeT lore) where
  ExtShape l1 == ExtShape l2 = length l1 == length l2

instance Ord (ExtShapeT lore) where
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

instance Monoid (ShapeT lore) where
  mempty = Shape mempty
  Shape l1 `mappend` Shape l2 = Shape $ l1 `mappend` l2

instance ArrayShape (ShapeT lore) where
  shapeRank (Shape l) = length l
  stripDims n (Shape dims) = Shape $ drop n dims

instance Monoid (ExtShapeT lore) where
  mempty = ExtShape mempty
  ExtShape l1 `mappend` ExtShape l2 = ExtShape $ l1 `mappend` l2

instance ArrayShape (ExtShapeT lore) where
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
type Type lore = TypeBase Names (Shape lore)

-- | A type without aliasing information, used for declarations.
type DeclType = TypeBase () Rank

-- | A type with shape information, but no aliasing information.  Can
-- be used for parameters and constants.
type ConstType lore = TypeBase () (Shape lore)

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
data IdentBase als shape = Ident { identName :: VName
                                 , identType :: TypeBase als shape
                                 , identSrcLoc :: SrcLoc
                                 }
                    deriving (Show)

-- | A name with aliasing information.  Used for normal variables.
type Ident lore = IdentBase Names (Shape lore)

-- | A name with no aliasing information.  These are used for function
-- parameters.
type Param lore = IdentBase () (Shape lore)

instance Eq (IdentBase als shape) where
  x == y = identName x == identName y

instance Ord (IdentBase als shape) where
  x `compare` y = identName x `compare` identName y

instance Located (IdentBase als shape) where
  locOf = locOf . identSrcLoc

instance Hashable (IdentBase als shape) where
  hashWithSalt salt = hashWithSalt salt . identName

-- | A list of identifiers used for certificates in some expressions.
type Certificates lore = [Ident lore]

-- | A subexpression is either a constant or a variable.  One
-- important property is that evaluation of a subexpression is
-- guaranteed to complete in constant time.
data SubExpT lore = Constant Value SrcLoc
                  | Var      (Ident lore)
                  deriving (Show, Eq, Ord)

-- | Alias for 'SubExpT'.
type SubExp = SubExpT

instance Located (SubExpT lore) where
  locOf (Constant _ loc) = locOf loc
  locOf (Var ident)      = locOf ident

-- | A local variable binding.
data Binding lore = Let { bindingVars :: [Ident lore]
                        , bindingLore :: Lore.Binding lore
                        , bindingExp :: Exp lore
                        }

deriving instance Lore.Proper lore => Ord (Binding lore)
deriving instance Lore.Proper lore => Show (Binding lore)
deriving instance Lore.Proper lore => Eq (Binding lore)

-- | The result of a body - a sequence of subexpressions, possibly
-- predicated on one or more certificates.
data ResultT lore = Result { resultCertificates :: Certificates lore
                           , resultSubExps :: [SubExp lore]
                           , resultSrcLoc :: SrcLoc
                           }
                   deriving (Eq, Ord, Show)

-- | An alias of 'ResultT' for namespace management.
type Result = ResultT

instance Located (ResultT lore) where
  locOf = locOf . resultSrcLoc

-- | A body consists of a number of bindings, terminating in a result
-- (essentially a tuple literal).
data BodyT lore = Body { bodyBindings :: [Binding lore]
                      , bodyResult :: Result lore
                      }
                 deriving (Ord, Show, Eq)

type Body = BodyT

instance Located (BodyT lore) where
  locOf = locOf . bodyResult

-- | Futhark Expression Language: literals + vars + int binops + array
-- constructors + array combinators (SOAC) + if + function calls +
-- let + tuples (literals & identifiers) TODO: please add float,
-- double, long int, etc.
data ExpT lore =
            -- Core language
              SubExp (SubExp lore)
            -- ^ Subexpressions, doubling as tuple literals if the
            -- list has anything but a single element.

            | ArrayLit  [SubExp lore] (Type lore) SrcLoc
            -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
            -- Second arg is the element type of of the rows of the array.

            | If     (SubExp lore) (BodyT lore) (BodyT lore) (ResType lore) SrcLoc

            | Apply  Name [(SubExp lore, Diet)] (ResType lore) SrcLoc

            -- Scalar operations
            | BinOp BinOp (SubExp lore) (SubExp lore) (Type lore) SrcLoc

            -- Unary Ops: Not for bools and Negate for ints
            | Not    (SubExp lore) SrcLoc -- ^ E.g., @not True == False@.
            | Negate (SubExp lore) SrcLoc -- ^ E.g., @~(~1) = 1@.

            -- Assertion management.
            | Assert (SubExp lore) SrcLoc
            -- ^ Turn a boolean into a certificate, halting the
            -- program if the boolean is false.

            | Conjoin [SubExp lore] SrcLoc
            -- ^ Convert several certificates into a single certificate.

            -- Primitive array operations

            | Index (Certificates lore)
                    (Ident lore)
                    [SubExp lore]
                    SrcLoc
            -- ^ 3rd arg are (optional) certificates for bounds
            -- checking.  If given (even as an empty list), no
            -- run-time bounds checking is done.

            | Update (Certificates lore) (Ident lore) [SubExp lore] (SubExp lore) SrcLoc
            -- ^ @a with [i1,i2,i3] <- v@.

            | Split (Certificates lore) (SubExp lore) (SubExp lore) (SubExp lore) SrcLoc
            -- ^ @split(1, [ 1, 2, 3, 4 ]) = {[1],[2, 3, 4]}@.

            | Concat (Certificates lore) (SubExp lore) (SubExp lore) (SubExp lore) SrcLoc
            -- ^ @concat([1],[2, 3, 4]) = [1, 2, 3, 4]@.

            | Copy (SubExp lore) SrcLoc
            -- ^ Copy the value return by the expression.  This only
            -- makes a difference in do-loops with merge variables.

            -- Array construction.
            | Iota (SubExp lore) SrcLoc
            -- ^ @iota(n) = [0,1,..,n-1]@
            | Replicate (SubExp lore) (SubExp lore) SrcLoc
            -- ^ @replicate(3,1) = [1, 1, 1]@

            -- Array index space transformation.
            | Reshape (Certificates lore) [SubExp lore] (SubExp lore) SrcLoc
             -- ^ 1st arg is the new shape, 2nd arg is the input array *)

            | Rearrange (Certificates lore) [Int] (SubExp lore) SrcLoc
            -- ^ Permute the dimensions of the input array.  The list
            -- of integers is a list of dimensions (0-indexed), which
            -- must be a permutation of @[0,n-1]@, where @n@ is the
            -- number of dimensions in the input array.

            | Rotate (Certificates lore) Int (SubExp lore) SrcLoc
            -- ^ @rotate(n,a)@ returns a new array, where the element
            -- @a[i]@ is at position @i+n@, cycling over to the
            -- beginning of the array.

            | DoLoop [Ident lore] [(Ident lore, SubExp lore)] (Ident lore) (SubExp lore) (BodyT lore) SrcLoc
            -- ^ @loop {b} <- {a} = {v} for i < n do b@.

            | Map (Certificates lore) (LambdaT lore) [SubExp lore] SrcLoc
             -- ^ @map(op +(1), {1,2,..,n}) = [2,3,..,n+1]@.
             -- 3rd arg is either a tuple of multi-dim arrays
             --   of basic type, or a multi-dim array of basic type.
             -- 4th arg is the input-array row types

            | Reduce  (Certificates lore) (LambdaT lore) [(SubExp lore, SubExp lore)] SrcLoc
            | Scan   (Certificates lore) (LambdaT lore) [(SubExp lore, SubExp lore)] SrcLoc
            | Filter  (Certificates lore) (LambdaT lore) [SubExp lore] SrcLoc
            | Redomap (Certificates lore) (LambdaT lore) (LambdaT lore) [SubExp lore] [SubExp lore] SrcLoc
              deriving (Eq, Ord, Show)

-- | A type alias for namespace control.
type Exp = ExpT

instance Located (ExpT lore) where
  locOf (SubExp se) = locOf se
  locOf (ArrayLit _ _ pos) = locOf pos
  locOf (BinOp _ _ _ _ pos) = locOf pos
  locOf (Not _ pos) = locOf pos
  locOf (Negate _ pos) = locOf pos
  locOf (If _ _ _ _ pos) = locOf pos
  locOf (Apply _ _ _ pos) = locOf pos
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
  locOf (DoLoop _ _ _ _ _ loc) = locOf loc
  locOf (Map _ _ _ pos) = locOf pos
  locOf (Reduce _ _ _ pos) = locOf pos
  locOf (Scan _ _ _ pos) = locOf pos
  locOf (Filter _ _ _ pos) = locOf pos
  locOf (Redomap _ _ _ _ _ pos) = locOf pos

-- | A type denoting the return type of a function call.  If a
-- function returns an array, we can either know the size in advance
-- ('Known'), or receive it as part of the return value ('Existential'
-- - refers to the type being dependent).
type ResType lore = [TypeBase Names (ExtShape lore)]

-- | A type for declaring the return of a function.  Only the function
-- parameters are in scope within the type.
type RetType lore = [TypeBase () (ExtShape lore)]

-- | Anonymous function for use in a tuple-SOAC.
data LambdaT lore =
  Lambda { lambdaParams     :: [Param lore]
         , lambdaBody       :: BodyT lore
         , lambdaReturnType :: [ConstType lore]
         , lambdaSrcLoc     :: SrcLoc
         }
  deriving (Eq, Ord, Show)

type Lambda = LambdaT

instance Located (LambdaT lore) where
  locOf (Lambda _ _ _ loc) = locOf loc

-- | Function Declarations
type FunDec lore = (Name,
                    RetType lore,
                    [Param lore],
                    BodyT lore,
                    SrcLoc)

funDecName :: FunDec lore -> Name
funDecName (fname,_,_,_,_) = fname

funDecBody :: FunDec lore -> BodyT lore
funDecBody (_,_,_,body,_) = body

funDecRetType :: FunDec lore -> RetType lore
funDecRetType (_,rettype,_,_,_) = rettype

-- | An entire Futhark program.
newtype ProgT lore = Prog { progFunctions :: [FunDec lore] }
                  deriving (Show)

type Prog = ProgT

-- | A set of names.
type Names = HS.HashSet VName
