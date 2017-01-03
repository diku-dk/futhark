{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- | This is an ever-changing abstract syntax for Futhark.  Some
-- types, such as @Exp@, are parametrised by type and name
-- representation.  See the @https://futhark.readthedocs.org@ for a
-- language reference, or this module may be a little hard to
-- understand.
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
  , UserType(..)
  , TupleArrayElemTypeBase(..)
  , ArrayTypeBase(..)
  , CompTypeBase
  , StructTypeBase
  , DeclArrayTypeBase
  , DeclTupleArrayElemTypeBase
  , Diet(..)
  , TypeDeclBase (..)

    -- * Values
  , IntValue(..)
  , FloatValue(..)
  , PrimValue(..)
  , Value(..)

  -- * Abstract syntax tree
  , UnOp (..)
  , BinOp (..)
  , IdentBase (..)
  , DimIndexBase(..)
  , ExpBase(..)
  , LoopFormBase (..)
  , ForLoopDirection (..)
  , LowerBoundBase(..)
  , LambdaBase(..)
  , PatternBase(..)
  , StreamForm(..)

  -- * Definitions
  , FunBindBase(..)
  , ConstBindBase(..)
  , TypeBindBase(..)
  , SigBindBase(..)
  , SpecBase(..)
  , StructBindBase(..)
  , ProgBase(..)
  , ProgBaseWithHeaders(..)
  , ProgHeader(..)
  , DecBase(..)
  , ValDecBase(..)

  -- * Miscellaneous
  , NoInfo(..)
  , Info(..)
  , Names
  , QualName(..)
  )
  where

import           Control.Applicative
import           Data.Array
import           Data.Foldable
import           Data.Functor
import           Data.Hashable
import qualified Data.HashSet                     as HS
import           Data.Loc
import           Data.Monoid
import           Data.Traversable
import           Prelude

import           Futhark.Representation.Primitive (FloatType (..),
                                                   FloatValue (..),
                                                   IntType (..), IntValue (..))
import           Language.Futhark.Core

-- | Convenience class for deriving 'Show' instances for the AST.
class (Show vn,
       Show (f vn),
       Show (f (CompTypeBase vn)),
       Show (f (StructTypeBase vn))) => Showable f vn where

-- | No information functor.  Usually used for placeholder type- or
-- aliasing information.
data NoInfo a = NoInfo
              deriving (Eq, Ord, Show)

instance Show vn => Showable NoInfo vn where
instance Functor NoInfo where
  fmap _ NoInfo = NoInfo
instance Foldable NoInfo where
  foldr _ b NoInfo = b
instance Traversable NoInfo where
  traverse _ NoInfo = pure NoInfo
instance Monoid (NoInfo a) where
  mempty = NoInfo
  _ `mappend` _ = NoInfo

-- | Some information.  The dual to 'NoInfo'
newtype Info a = Info { unInfo :: a }
            deriving (Eq, Ord, Show)

instance Show vn => Showable Info vn where
instance Functor Info where
  fmap f (Info x) = Info $ f x
instance Foldable Info where
  foldr f b (Info x) = f x b
instance Traversable Info where
  traverse f (Info x) = Info <$> f x

-- | Low-level primitive types.
data PrimType = Signed IntType
              | Unsigned IntType
              | FloatType FloatType
              | Bool
              deriving (Eq, Ord, Show)

-- | Non-array values.
data PrimValue = SignedValue !IntValue
               | UnsignedValue !IntValue
               | FloatValue !FloatValue
               | BoolValue !Bool
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
newtype Rank = Rank Int
             deriving (Eq, Ord, Show)

instance Monoid Rank where
  mempty = Rank 0
  Rank n `mappend` Rank m = Rank $ n + m

instance ArrayShape Rank where
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
data TupleArrayElemTypeBase shape as =
    PrimArrayElem PrimType as Uniqueness
  | ArrayArrayElem (ArrayTypeBase shape as)
  | PolyArrayElem VName as Uniqueness
  | TupleArrayElem [TupleArrayElemTypeBase shape as]
  deriving (Show)

instance Eq shape =>
         Eq (TupleArrayElemTypeBase shape as) where
  PrimArrayElem bt1 _ u1 == PrimArrayElem bt2 _ u2 = bt1 == bt2 && u1 == u2
  ArrayArrayElem at1     == ArrayArrayElem at2     = at1 == at2
  TupleArrayElem ts1     == TupleArrayElem ts2     = ts1 == ts2
  _                      == _                      = False

-- | An array type.
data ArrayTypeBase shape as =
    PrimArray PrimType shape Uniqueness as
    -- ^ An array whose elements are primitive types.
  | PolyArray VName shape Uniqueness as
    -- ^ An array whose elements are some polymorphic type.
  | TupleArray [TupleArrayElemTypeBase shape as] shape Uniqueness
    -- ^ An array whose elements are tuples.
    deriving (Show)

instance Eq shape =>
         Eq (ArrayTypeBase shape as) where
  PrimArray et1 dims1 u1 _ == PrimArray et2 dims2 u2 _ =
    et1 == et2 && dims1 == dims2 && u1 == u2
  TupleArray ts1 dims1 u1 == TupleArray ts2 dims2 u2 =
    ts1 == ts2 && dims1 == dims2 && u1 == u2
  _ == _ =
    False

-- | An expanded Futhark type is either an array, a prim type, a
-- tuple, or a type variable.  When comparing types for equality with
-- '==', aliases are ignored, but dimensions much match.
data TypeBase shape as = Prim PrimType
                       | Array (ArrayTypeBase shape as)
                       | Tuple [TypeBase shape as]
                       | TypeVar VName
                          deriving (Eq, Show)


-- | A type with aliasing information and no shape annotations, used
-- for describing the type of a computation.
type CompTypeBase vn = TypeBase Rank (Names vn)

-- | An unstructured type with type variables and possibly shape
-- declarations - this is what the user types in the source program.
data UserType vn = UserPrim PrimType SrcLoc
                 | UserArray (UserType vn) (DimDecl vn) SrcLoc
                 | UserTuple [UserType vn] SrcLoc
                 | UserTypeAlias (QualName vn) SrcLoc
                 | UserUnique (UserType vn) SrcLoc
                 deriving (Eq, Show)

instance Located (UserType vn) where
  locOf (UserPrim _ loc)      = locOf loc
  locOf (UserArray _ _ loc)   = locOf loc
  locOf (UserTuple _ loc)     = locOf loc
  locOf (UserTypeAlias _ loc) = locOf loc
  locOf (UserUnique _ loc)    = locOf loc

--
-- | A "structural" type with shape annotations and no aliasing
-- information, used for declarations.

type StructTypeBase vn = TypeBase (ShapeDecl vn) ()


-- | An array type with shape annotations and no aliasing information,
-- used for declarations.
type DeclArrayTypeBase vn = ArrayTypeBase (ShapeDecl vn) ()

-- | A tuple array element type with shape annotations and no aliasing
-- information, used for declarations.
type DeclTupleArrayElemTypeBase vn = TupleArrayElemTypeBase (ShapeDecl vn) ()

-- | A declaration of the type of something.
data TypeDeclBase f vn =
  TypeDecl { declaredType :: UserType vn
                             -- ^ The type declared by the user.
           , expandedType :: f (StructTypeBase vn)
                             -- ^ The type deduced by the type checker.
           }
deriving instance Showable f vn => Show (TypeDeclBase f vn)

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
           | ArrayValue !(Array Int Value) (TypeBase Rank ())
             -- ^ It is assumed that the array is 0-indexed.  The type
             -- is the row type.
             deriving (Eq, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data IdentBase f vn = Ident { identName   :: vn
                            , identType   :: f (CompTypeBase vn)
                            , identSrcLoc :: SrcLoc
                            }
deriving instance Showable f vn => Show (IdentBase f vn)

instance Eq vn => Eq (IdentBase ty vn) where
  x == y = identName x == identName y

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
          | TupleProject Int
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
           | NotEqual
           | Less
           | Leq
           | Greater
           | Geq
             deriving (Eq, Ord, Show, Enum, Bounded)

-- | An indexing of a single dimension.
data DimIndexBase f vn = DimFix (ExpBase f vn)
                       | DimSlice (Maybe (ExpBase f vn)) (Maybe (ExpBase f vn))
deriving instance Showable f vn => Show (DimIndexBase f vn)

-- | A name qualified with a breadcrumb of module accesses.
newtype QualName vn = QualName ([Name], vn)
  deriving (Eq, Ord, Show, Hashable)

-- | The Futhark expression language.
--
-- In a value of type @Exp f vn@, annotations are wrapped in the
-- functor @f@, and all names are of type @vn@.
--
-- This allows us to encode whether or not the expression has been
-- type-checked in the Haskell type of the expression.  Specifically,
-- the parser will produce expressions of type @Exp 'NoInfo' 'Name'@,
-- and the type checker will convert these to @Exp 'Info' 'VName'@, in
-- which type information is always present and all names are unique.
data ExpBase f vn =
              Literal PrimValue SrcLoc

            | TupLit    [ExpBase f vn] SrcLoc
            -- ^ Tuple literals, e.g., @{1+3, {x, y+z}}@.

            | ArrayLit  [ExpBase f vn] (f (CompTypeBase vn)) SrcLoc
            -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
            -- Second arg is the row type of the rows of the array.

            | Empty (TypeDeclBase f vn) SrcLoc

            | Var    (QualName vn) (f (CompTypeBase vn)) SrcLoc

            | LetPat (PatternBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc

            | If     (ExpBase f vn) (ExpBase f vn) (ExpBase f vn) (f (CompTypeBase vn)) SrcLoc

            | Apply  (QualName vn) [(ExpBase f vn, Diet)] (f (CompTypeBase vn)) SrcLoc

            | DoLoop
              (PatternBase f vn) -- Merge variable pattern
              (ExpBase f vn) -- Initial values of merge variables.
              (LoopFormBase f vn) -- Do or while loop.
              (ExpBase f vn) -- Loop body.
              (ExpBase f vn) -- Let-body.
              SrcLoc

            | BinOp BinOp (ExpBase f vn) (ExpBase f vn) (f (CompTypeBase vn)) SrcLoc
            | UnOp UnOp (ExpBase f vn) (f (CompTypeBase vn)) SrcLoc

            -- Primitive array operations
            | LetWith (IdentBase f vn) (IdentBase f vn)
                      [DimIndexBase f vn] (ExpBase f vn)
                      (ExpBase f vn) SrcLoc

            | Index (ExpBase f vn)
                    [DimIndexBase f vn]
                    SrcLoc

            | Shape (ExpBase f vn) SrcLoc
            -- ^ The shape of the argument.

            | Split Int (ExpBase f vn) (ExpBase f vn) SrcLoc
            -- ^ @split@0( (1,1,3), [ 1, 2, 3, 4 ]) = {[1], [], [2,
            -- 3], [4]}@.  Note that this is different from in the
            -- core language.  The static integer indicates which
            -- dimension to concatenate across.

            | Concat Int (ExpBase f vn) [ExpBase f vn] SrcLoc
            -- ^ @concat@0([1],[2, 3, 4]) = [1, 2, 3, 4]@.  The
            -- static integer indicates which dimension to concatenate
            -- across.

            | Copy (ExpBase f vn) SrcLoc
            -- ^ Copy the value return by the expression.  This only
            -- makes a difference in do-loops with merge variables.

            -- Array construction.
            | Iota (ExpBase f vn) SrcLoc
            -- ^ @iota(n) = [0,1,..,n-1]@
            | Replicate (ExpBase f vn) (ExpBase f vn) SrcLoc
            -- ^ @replicate(3,1) = [1, 1, 1]@

            -- Array index space transformation.
            | Reshape (ExpBase f vn) (ExpBase f vn) SrcLoc
             -- ^ 1st arg is the new shape, 2nd arg is the input array.

            | Transpose (ExpBase f vn) SrcLoc
            -- ^ Transpose two-dimensional array.  @transpose(a) =
            -- rearrange((1,0), a)@.

            | Rearrange [Int] (ExpBase f vn) SrcLoc
            -- ^ Permute the dimensions of the input array.  The list
            -- of integers is a list of dimensions (0-indexed), which
            -- must be a permutation of @[0,n-1]@, where @n@ is the
            -- number of dimensions in the input array.

            | Rotate Int (ExpBase f vn) (ExpBase f vn) SrcLoc
            -- ^ Rotate the given dimension of the given array by the
            -- given amount.  The last expression is the array.

            -- Second-Order Array Combinators accept curried and
            -- anonymous functions as first params.
            | Map (LambdaBase f vn) [ExpBase f vn] SrcLoc
             -- ^ @map (+1) ([1, 2, ..., n]) = [2, 3, ..., n+1]@.
             -- OR
             -- ^ @zipWith (+) ([1, 2, ..., n]) ([1, 2, ..., n]) = [2, 4, ... , 2*n]@.
             --
             -- @map@ when exactly one array argument, otherwise @zipWith@.

            | Reduce Commutativity (LambdaBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc
             -- ^ @reduce (+) 0 ([1,2,...,n]) = (0+1+2+...+n)@.

            | Scan (LambdaBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc
             -- ^ @scan (+) 0 ([ 1, 2, 3 ]) = [ 1, 3, 6 ]@.

            | Filter (LambdaBase f vn) (ExpBase f vn) SrcLoc
            -- ^ Return those elements of the array that satisfy the
            -- predicate.

            | Partition [LambdaBase f vn] (ExpBase f vn) SrcLoc
            -- ^ @partition (f_1, ..., f_n) a@ returns @n+1@ arrays, with
            -- the @i@th array consisting of those elements for which
            -- function @f_1@ returns 'True', and no previous function
            -- has returned 'True'.  The @n+1@th array contains those
            -- elements for which no function returns 'True'.

            | Stream (StreamForm f vn) (LambdaBase f vn) (ExpBase f vn) SrcLoc
            -- ^ Streaming: intuitively, this gives a size-parameterized
            -- composition for SOACs that cannot be fused, e.g., due to scan.
            -- For example, assuming @A : [int], f : int->int, g : real->real@,
            -- the code: @let x = map(f,A) in let y = scan(op+,0,x) in map(g,y)@
            -- can be re-written (streamed) in the source-Futhark language as:
            -- @let (acc, z) =@
            -- @  stream (fn (int,[real]) (real chunk, real acc, [int] a) =>@
            -- @            let x = map (f,         A )@
            -- @            let y0= scan(op +, 0,   x )@
            -- @            let y = map (op +(acc), y0)@
            -- @            ( acc+y0[chunk-1], map(g, y) )@
            -- @         ) 0 A@
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

            | Write (ExpBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc
            -- ^ @write [0, 2, -1] [9, 7, 0] [3, 4, 5] = [9, 4, 7]@.

            | Zip Int (ExpBase f vn) [ExpBase f vn] SrcLoc
            -- ^ Conventional zip taking nonzero arrays as arguments.
            -- All arrays must have the exact same length.

            | Unzip (ExpBase f vn) [f (CompTypeBase vn)] SrcLoc
            -- ^ Unzip that can unzip to tuples of arbitrary size.
            -- The types are the elements of the tuple.

            | Unsafe (ExpBase f vn) SrcLoc
            -- ^ Explore the Danger Zone and elide safety checks on
            -- array operations that are (lexically) within this
            -- expression.  Make really sure the code is correct.

deriving instance Showable f vn => Show (ExpBase f vn)

data StreamForm f vn = MapLike    StreamOrd
                     | RedLike    StreamOrd Commutativity (LambdaBase f vn)
                     | Sequential (ExpBase f vn)
deriving instance Showable f vn => Show (StreamForm f vn)

instance Located (ExpBase f vn) where
  locOf (Literal _ loc)         = locOf loc
  locOf (TupLit _ pos)          = locOf pos
  locOf (ArrayLit _ _ pos)      = locOf pos
  locOf (Empty _ pos)           = locOf pos
  locOf (BinOp _ _ _ _ pos)     = locOf pos
  locOf (UnOp _ _ _ pos)        = locOf pos
  locOf (If _ _ _ _ pos)        = locOf pos
  locOf (Var _ _ loc)           = locOf loc
  locOf (Apply _ _ _ pos)       = locOf pos
  locOf (LetPat _ _ _ pos)      = locOf pos
  locOf (LetWith _ _ _ _ _ pos) = locOf pos
  locOf (Index _ _ pos)         = locOf pos
  locOf (Iota _ pos)            = locOf pos
  locOf (Shape _ pos)           = locOf pos
  locOf (Replicate _ _ pos)     = locOf pos
  locOf (Reshape _ _ pos)       = locOf pos
  locOf (Transpose _ pos)       = locOf pos
  locOf (Rearrange _ _ pos)     = locOf pos
  locOf (Rotate _ _ _ pos)      = locOf pos
  locOf (Map _ _ pos)           = locOf pos
  locOf (Reduce _ _ _ _ pos)    = locOf pos
  locOf (Zip _ _ _ pos)         = locOf pos
  locOf (Unzip _ _ pos)         = locOf pos
  locOf (Scan _ _ _ pos)        = locOf pos
  locOf (Filter _ _ pos)        = locOf pos
  locOf (Partition _ _ pos)     = locOf pos
  locOf (Split _ _ _ pos)       = locOf pos
  locOf (Concat _ _ _ pos)      = locOf pos
  locOf (Copy _ pos)            = locOf pos
  locOf (DoLoop _ _ _ _ _ pos)  = locOf pos
  locOf (Stream _ _ _  pos)     = locOf pos
  locOf (Unsafe _ loc)          = locOf loc
  locOf (Write _ _ _ loc)       = locOf loc

-- | Whether the loop is a @for@-loop or a @while@-loop.
data LoopFormBase f vn = For ForLoopDirection (LowerBoundBase f vn) (IdentBase f vn) (ExpBase f vn)
                       | While (ExpBase f vn)
deriving instance Showable f vn => Show (LoopFormBase f vn)

-- | The iteration order of a @for@-loop.
data ForLoopDirection = FromUpTo -- ^ Iterates from the lower bound to
                                 -- just below the upper bound.
                      | FromDownTo -- ^ Iterates from just below the
                                   -- upper bound to the lower bound.
                        deriving (Eq, Ord, Show)

-- | The lower bound of a for loop.
data LowerBoundBase f vn = ZeroBound
                         | ExpBound (ExpBase f vn)
deriving instance Showable f vn => Show (LowerBoundBase f vn)

-- | Anonymous function passed to a SOAC.
data LambdaBase f vn = AnonymFun [PatternBase f vn] (ExpBase f vn) (Maybe (TypeDeclBase f vn)) (f (StructTypeBase vn)) SrcLoc
                      -- ^ @fn (x: bool, z: char):int => if x then ord z else ord z + 1@
                      | CurryFun (QualName vn) [ExpBase f vn] (f (CompTypeBase vn)) SrcLoc
                        -- ^ @f(4)@
                      | UnOpFun UnOp (f (CompTypeBase vn)) (f (CompTypeBase vn)) SrcLoc
                        -- ^ @-@; first type is operand, second is result.
                      | BinOpFun BinOp (f (CompTypeBase vn)) (f (CompTypeBase vn)) (f (CompTypeBase vn)) SrcLoc
                        -- ^ @+@; first two types are operands, third is result.
                      | CurryBinOpLeft BinOp (ExpBase f vn) (f (CompTypeBase vn)) (f (CompTypeBase vn)) SrcLoc
                        -- ^ @2+@; first type is operand, second is result.
                      | CurryBinOpRight BinOp (ExpBase f vn) (f (CompTypeBase vn)) (f (CompTypeBase vn)) SrcLoc
                        -- ^ @+2@; first type is operand, second is result.
deriving instance Showable f vn => Show (LambdaBase f vn)

instance Located (LambdaBase f vn) where
  locOf (AnonymFun _ _ _ _ loc)       = locOf loc
  locOf (CurryFun  _ _ _ loc)         = locOf loc
  locOf (UnOpFun _ _ _ loc)           = locOf loc
  locOf (BinOpFun _ _ _ _ loc)        = locOf loc
  locOf (CurryBinOpLeft _ _ _ _ loc)  = locOf loc
  locOf (CurryBinOpRight _ _ _ _ loc) = locOf loc

-- | A pattern as used most places where variables are bound (function
-- parameters, @let@ expressions, etc).
data PatternBase f vn = TuplePattern [PatternBase f vn] SrcLoc
                      | Id (IdentBase f vn)
                      | Wildcard (f (CompTypeBase vn)) SrcLoc -- Nothing, i.e. underscore.
                      | PatternAscription (PatternBase f vn) (TypeDeclBase f vn)
deriving instance Showable f vn => Show (PatternBase f vn)

instance Located (PatternBase f vn) where
  locOf (TuplePattern _ loc)    = locOf loc
  locOf (Id ident)              = locOf ident
  locOf (Wildcard _ loc)        = locOf loc
  locOf (PatternAscription p _) = locOf p

-- | Function Declarations
data FunBindBase f vn = FunBind { funBindEntryPoint :: Bool
                                -- ^ True if this function is an entry point.
                                , funBindName       :: vn
                                , funBindRetDecl    :: Maybe (UserType vn)
                                , funBindRetType    :: f (StructTypeBase vn)
                                , funBindParams     :: [PatternBase f vn]
                                , funBindBody       :: ExpBase f vn
                                , funBindLocation   :: SrcLoc
                                }
deriving instance Showable f vn => Show (FunBindBase f vn)

instance Located (FunBindBase f vn) where
  locOf = locOf . funBindLocation

-- | Constant declaration
data ConstBindBase f vn = ConstBind { constBindName     :: vn
                                    , constBindType     :: TypeDeclBase f vn
                                    , constBindDef      :: ExpBase f vn
                                    , constBindLocation :: SrcLoc
                                    }
deriving instance Showable f vn => Show (ConstBindBase f vn)

instance Located (ConstBindBase f vn) where
  locOf = locOf . constBindLocation

-- | Type Declarations
data TypeBindBase f vn = TypeBind { typeAlias        :: vn
                                  , userType         :: TypeDeclBase f vn
                                  , typeBindLocation :: SrcLoc
                                  }
deriving instance Showable f vn => Show (TypeBindBase f vn)

instance Located (TypeBindBase f vn) where
  locOf = locOf . typeBindLocation

data SigBindBase f vn = SigBind { sigName     :: vn
                                , sigDecls    :: [SpecBase f vn]
                                , sigLocation :: SrcLoc
                                }
deriving instance Showable f vn => Show (SigBindBase f vn)

instance Located (SigBindBase f vn) where
  locOf = locOf . sigLocation

data SpecBase f vn = ValSpec  { specName     :: vn
                              , specParams   :: [TypeDeclBase f vn]
                              , specRettype  :: TypeDeclBase f vn
                              , specLocation :: SrcLoc
                              }
                   | TypeAbbrSpec (TypeBindBase f vn)
                   | TypeSpec vn SrcLoc -- ^ Abstract type.
deriving instance Showable f vn => Show (SpecBase f vn)

instance Located (SpecBase f vn) where
  locOf (ValSpec _ _ _ loc) = locOf loc
  locOf (TypeAbbrSpec tbind) = locOf tbind
  locOf (TypeSpec _ loc) = locOf loc

data StructBindBase f vn = StructBind { structName      :: vn
                                      , structSignature :: Maybe (QualName vn)
                                      , structDecls     :: [DecBase f vn]
                                      , structLocation  :: SrcLoc
                                      }
deriving instance Showable f vn => Show (StructBindBase f vn)

instance Located (StructBindBase f vn) where
  locOf = locOf . structLocation

-- | A top-level binding of a term variable to either a function or a
-- constant.
data ValDecBase f vn = FunDec (FunBindBase f vn)
                     | ConstDec (ConstBindBase f vn)
deriving instance Showable f vn => Show (ValDecBase f vn)

instance Located (ValDecBase f vn) where
  locOf (FunDec d) = locOf d
  locOf (ConstDec d) = locOf d

-- | A top-level binding.
data DecBase f vn = ValDec (ValDecBase f vn)
                  | TypeDec (TypeBindBase f vn)
                  | SigDec (SigBindBase f vn)
                  | StructDec (StructBindBase f vn)
deriving instance Showable f vn => Show (DecBase f vn)

instance Located (DecBase f vn) where
  locOf (ValDec d) = locOf d
  locOf (TypeDec d) = locOf d
  locOf (SigDec d) = locOf d
  locOf (StructDec d) = locOf d

data ProgBase f vn = Prog { progDecs :: [DecBase f vn] }
deriving instance Showable f vn => Show (ProgBase f vn)

data ProgBaseWithHeaders f vn =
  ProgWithHeaders { progWHHeaders :: [ProgHeader]
                  , progWHDecs    :: [DecBase f vn]
                  }
deriving instance Showable f vn => Show (ProgBaseWithHeaders f vn)

data ProgHeader = Include [String]
                deriving (Show)

-- | A set of names.
type Names = HS.HashSet
