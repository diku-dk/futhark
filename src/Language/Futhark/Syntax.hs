{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- | This is an ever-changing syntax representation for Futhark.  Some
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
  , ArrayDim (..)
  , DimDecl (..)
  , ShapeDecl (..)
  , shapeRank
  , stripDims
  , unifyShapes
  , TypeName(..)
  , typeNameFromQualName
  , qualNameFromTypeName
  , TypeBase(..)
  , TypeArg(..)
  , TypeExp(..)
  , TypeArgExp(..)
  , RecordArrayElemTypeBase(..)
  , ArrayTypeBase(..)
  , CompTypeBase
  , StructTypeBase
  , Diet(..)
  , TypeDeclBase (..)

    -- * Values
  , IntValue(..)
  , FloatValue(..)
  , PrimValue(..)
  , IsPrimValue(..)
  , Value(..)

  -- * Abstract syntax tree
  , BinOp (..)
  , IdentBase (..)
  , Inclusiveness(..)
  , DimIndexBase(..)
  , ExpBase(..)
  , FieldBase(..)
  , LoopFormBase (..)
  , LambdaBase(..)
  , PatternBase(..)
  , StreamForm(..)

  -- * Module language
  , ParamBase(..)
  , SpecBase(..)
  , SigExpBase(..)
  , TypeRefBase(..)
  , SigBindBase(..)
  , ModExpBase(..)
  , ModBindBase(..)
  , ModParamBase(..)

  -- * Definitions
  , FunBindBase(..)
  , ValBindBase(..)
  , TypeBindBase(..)
  , TypeParamBase(..)
  , typeParamName
  , ProgBase(..)
  , DecBase(..)

  -- * Miscellaneous
  , NoInfo(..)
  , Info(..)
  , Names
  , QualName(..)
  )
  where

import           Control.Applicative
import           Control.Monad
import           Data.Array
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Foldable
import           Data.Hashable
import           Data.Loc
import qualified Data.Map.Strict                  as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                         as S
import           Data.Traversable
import           Prelude

import           Futhark.Representation.Primitive (FloatType (..),
                                                   FloatValue (..),
                                                   IntType (..), IntValue (..))
import           Futhark.Util.Pretty
import           Language.Futhark.Core

-- | Convenience class for deriving 'Show' instances for the AST.
class (Show vn,
       Show (f vn),
       Show (f [VName]),
       Show (f (TypeBase (DimDecl vn) (Names vn))),
       Show (f (CompTypeBase vn)),
       Show (f [TypeBase () ()]),
       Show (f (StructTypeBase vn)),
       Show (f (M.Map VName VName)),
       Show (f ([CompTypeBase vn], CompTypeBase vn))) => Showable f vn where

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

class IsPrimValue v where
  primValue :: v -> PrimValue

instance IsPrimValue Int where
  primValue = SignedValue . Int32Value . fromIntegral

instance IsPrimValue Int8 where
  primValue = SignedValue . Int8Value
instance IsPrimValue Int16 where
  primValue = SignedValue . Int16Value
instance IsPrimValue Int32 where
  primValue = SignedValue . Int32Value
instance IsPrimValue Int64 where
  primValue = SignedValue . Int64Value

instance IsPrimValue Word8 where
  primValue = UnsignedValue . Int8Value . fromIntegral
instance IsPrimValue Word16 where
  primValue = UnsignedValue . Int16Value . fromIntegral
instance IsPrimValue Word32 where
  primValue = UnsignedValue . Int32Value . fromIntegral
instance IsPrimValue Word64 where
  primValue = UnsignedValue . Int64Value . fromIntegral

instance IsPrimValue Float where
  primValue = FloatValue . Float32Value

instance IsPrimValue Double where
  primValue = FloatValue . Float64Value

instance IsPrimValue Bool where
  primValue = BoolValue

class (Eq dim, Ord dim) => ArrayDim dim where
  -- | @unifyDims x y@ combines @x@ and @y@ to contain their maximum
  -- common information, and fails if they conflict.
  unifyDims :: dim -> dim -> Maybe dim

instance ArrayDim () where
  unifyDims () () = Just ()

-- | Declaration of a dimension size.
data DimDecl vn = NamedDim (QualName vn)
                  -- ^ The size of the dimension is this name, which
                  -- must be in scope.  In a return type, this will
                  -- give rise to an assertion.
                | ConstDim Int
                  -- ^ The size is a constant.
                | AnyDim
                  -- ^ No dimension declaration.
                deriving (Eq, Ord, Show)

instance Functor DimDecl where
  fmap f (NamedDim (QualName qs x)) = NamedDim $ QualName qs $ f x
  fmap _ (ConstDim x)               = ConstDim x
  fmap _ AnyDim                     = AnyDim

instance (Eq vn, Ord vn) => ArrayDim (DimDecl vn) where
  unifyDims AnyDim y = Just y
  unifyDims x AnyDim = Just x
  unifyDims (NamedDim x) (NamedDim y) | x == y = Just $ NamedDim x
  unifyDims (ConstDim x) (ConstDim y) | x == y = Just $ ConstDim x
  unifyDims _ _ = Nothing


-- | The size of an array type is a list of its dimension sizes.  If
-- 'Nothing', that dimension is of a (statically) unknown size.
newtype ShapeDecl dim = ShapeDecl { shapeDims :: [dim] }
                      deriving (Eq, Ord, Show)

instance Foldable ShapeDecl where
  foldr f x (ShapeDecl ds) = foldr f x ds

instance Traversable ShapeDecl where
  traverse f (ShapeDecl ds) = ShapeDecl <$> traverse f ds

instance Functor ShapeDecl where
  fmap f (ShapeDecl ds) = ShapeDecl $ map f ds

instance Monoid (ShapeDecl dim) where
  mempty = ShapeDecl []
  ShapeDecl l1 `mappend` ShapeDecl l2 = ShapeDecl $ l1 ++ l2

-- | The number of dimensions contained in a shape.
shapeRank :: ShapeDecl dim -> Int
shapeRank = length . shapeDims

-- | @stripDims n shape@ strips the outer @n@ dimensions from
-- @shape@, returning 'Nothing' if this would result in zero or
-- fewer dimensions.
stripDims :: Int -> ShapeDecl dim -> Maybe (ShapeDecl dim)
stripDims i (ShapeDecl l)
  | i < length l = Just $ ShapeDecl $ drop i l
  | otherwise    = Nothing


-- | @unifyShapes x y@ combines @x@ and @y@ to contain their maximum
-- common information, and fails if they conflict.
unifyShapes :: ArrayDim dim => ShapeDecl dim -> ShapeDecl dim -> Maybe (ShapeDecl dim)
unifyShapes (ShapeDecl xs) (ShapeDecl ys) = do
  guard $ length xs == length ys
  ShapeDecl <$> zipWithM unifyDims xs ys

-- | A type name consists of qualifiers (for error messages) and a
-- 'VName' (for equality checking).
data TypeName = TypeName [Name] VName
              deriving (Show)

instance Eq TypeName where
  TypeName _ x == TypeName _ y = x == y

instance Ord TypeName where
  TypeName _ x `compare` TypeName _ y = x `compare` y

typeNameFromQualName :: QualName VName -> TypeName
typeNameFromQualName (QualName qs x) = TypeName qs x

qualNameFromTypeName :: TypeName -> QualName VName
qualNameFromTypeName (TypeName qs x) = QualName qs x

-- | Types that can be elements of tuple-arrays.
data RecordArrayElemTypeBase dim as =
    PrimArrayElem PrimType as
  | ArrayArrayElem (ArrayTypeBase dim as)
  | PolyArrayElem TypeName [TypeArg dim as] as Uniqueness
  | RecordArrayElem (M.Map Name (RecordArrayElemTypeBase dim as))
  deriving (Show)

instance Eq dim => Eq (RecordArrayElemTypeBase dim as) where
  PrimArrayElem bt1 _ == PrimArrayElem bt2 _ =
    bt1 == bt2
  PolyArrayElem bt1 targs1 _ u1 == PolyArrayElem bt2 targs2 _ u2 =
    bt1 == bt2 && targs1 == targs2 && u1 == u2
  ArrayArrayElem at1 == ArrayArrayElem at2 =
    at1 == at2
  RecordArrayElem ts1 == RecordArrayElem ts2 =
    ts1 == ts2
  _ == _ =
    False

instance Bitraversable RecordArrayElemTypeBase where
  bitraverse _ g (PrimArrayElem t as) = PrimArrayElem t <$> g as
  bitraverse f g (ArrayArrayElem a) = ArrayArrayElem <$> bitraverse f g a
  bitraverse f g (PolyArrayElem t args as u) =
    PolyArrayElem t <$> traverse (bitraverse f g) args <*> g as <*> pure u
  bitraverse f g (RecordArrayElem fs) =
    RecordArrayElem <$> traverse (bitraverse f g) fs

instance Bifunctor RecordArrayElemTypeBase where
  bimap = bimapDefault

instance Bifoldable RecordArrayElemTypeBase where
  bifoldMap = bifoldMapDefault

-- | An array type.
data ArrayTypeBase dim as =
    PrimArray PrimType (ShapeDecl dim) Uniqueness as
    -- ^ An array whose elements are primitive types.
  | PolyArray TypeName [TypeArg dim as] (ShapeDecl dim) Uniqueness as
    -- ^ An array whose elements are some polymorphic type, possibly with arguments.
  | RecordArray (M.Map Name (RecordArrayElemTypeBase dim as)) (ShapeDecl dim) Uniqueness
    -- ^ An array whose elements are records.  Note that tuples are
    -- also just records.
    deriving (Show)

instance Eq dim =>
         Eq (ArrayTypeBase dim as) where
  PrimArray et1 dims1 u1 _ == PrimArray et2 dims2 u2 _ =
    et1 == et2 && dims1 == dims2 && u1 == u2
  PolyArray et1 args1 dims1 u1 _ == PolyArray et2 args2 dims2 u2 _ =
    et1 == et2 && args1 == args2 && dims1 == dims2 && u1 == u2
  RecordArray ts1 dims1 u1 == RecordArray ts2 dims2 u2 =
    ts1 == ts2 && dims1 == dims2 && u1 == u2
  _ == _ =
    False

instance Bitraversable ArrayTypeBase where
  bitraverse f g (PrimArray t dims u as) =
    PrimArray t <$> traverse f dims <*> pure u <*> g as
  bitraverse f g (PolyArray t args dims u as) =
    PolyArray t <$> traverse (bitraverse f g) args <*> traverse f dims <*> pure u <*> g as
  bitraverse f g (RecordArray fs dims u) =
    RecordArray <$> traverse (bitraverse f g) fs <*> traverse f dims <*> pure u

instance Bifunctor ArrayTypeBase where
  bimap = bimapDefault

instance Bifoldable ArrayTypeBase where
  bifoldMap = bifoldMapDefault

-- | An expanded Futhark type is either an array, a prim type, a
-- tuple, or a type variable.  When comparing types for equality with
-- '==', aliases are ignored, but dimensions much match.
data TypeBase dim as = Prim PrimType
                     | Array (ArrayTypeBase dim as)
                     | Record (M.Map Name (TypeBase dim as))
                     | TypeVar TypeName [TypeArg dim as]
                     deriving (Eq, Show)

instance Bitraversable TypeBase where
  bitraverse _ _ (Prim t) = pure $ Prim t
  bitraverse f g (Array a) = Array <$> bitraverse f g a
  bitraverse f g (Record fs) = Record <$> traverse (bitraverse f g) fs
  bitraverse f g (TypeVar t args) = TypeVar t <$> traverse (bitraverse f g) args

instance Bifunctor TypeBase where
  bimap = bimapDefault

instance Bifoldable TypeBase where
  bifoldMap = bifoldMapDefault

data TypeArg dim as = TypeArgDim dim SrcLoc
                    | TypeArgType (TypeBase dim as) SrcLoc
             deriving (Eq, Show)

instance Bitraversable TypeArg where
  bitraverse f _ (TypeArgDim v loc) = TypeArgDim <$> f v <*> pure loc
  bitraverse f g (TypeArgType t loc) = TypeArgType <$> bitraverse f g t <*> pure loc

instance Bifunctor TypeArg where
  bimap = bimapDefault

instance Bifoldable TypeArg where
  bifoldMap = bifoldMapDefault

-- | A type with aliasing information and no shape annotations, used
-- for describing the type of a computation.
type CompTypeBase vn = TypeBase () (Names vn)

-- | An unstructured type with type variables and possibly shape
-- declarations - this is what the user types in the source program.
data TypeExp vn = TEVar (QualName vn) SrcLoc
                | TETuple [TypeExp vn] SrcLoc
                | TERecord [(Name, TypeExp vn)] SrcLoc
                | TEArray (TypeExp vn) (DimDecl vn) SrcLoc
                | TEUnique (TypeExp vn) SrcLoc
                | TEApply (QualName vn) [TypeArgExp vn] SrcLoc
                 deriving (Eq, Show)

instance Located (TypeExp vn) where
  locOf (TEArray _ _ loc) = locOf loc
  locOf (TETuple _ loc)   = locOf loc
  locOf (TERecord _ loc)  = locOf loc
  locOf (TEVar _ loc)     = locOf loc
  locOf (TEUnique _ loc)  = locOf loc
  locOf (TEApply _ _ loc) = locOf loc

data TypeArgExp vn = TypeArgExpDim (DimDecl vn) SrcLoc
                   | TypeArgExpType (TypeExp vn)
                deriving (Eq, Show)

instance Located (TypeArgExp vn) where
  locOf (TypeArgExpDim _ loc) = locOf loc
  locOf (TypeArgExpType t)    = locOf t

-- | A "structural" type with shape annotations and no aliasing
-- information, used for declarations.
type StructTypeBase vn = TypeBase (DimDecl vn) ()

-- | A declaration of the type of something.
data TypeDeclBase f vn =
  TypeDecl { declaredType :: TypeExp vn
                             -- ^ The type declared by the user.
           , expandedType :: f (StructTypeBase vn)
                             -- ^ The type deduced by the type checker.
           }
deriving instance Showable f vn => Show (TypeDeclBase f vn)

instance Located (TypeDeclBase f vn) where
  locOf = locOf . declaredType

-- | Information about which parts of a value/type are consumed.  For
-- example, we might say that a function taking an argument of type
-- @([int], *[int], [int])@ has diet @ConsumeTuple [Observe, Consume,
-- Observe]@.
data Diet = RecordDiet (M.Map Name Diet) -- ^ Consumes these fields in the record.
          | Consume -- ^ Consumes this value.
          | Observe -- ^ Only observes value in this position, does
                    -- not consume.
            deriving (Eq, Show)

-- | Every possible value in Futhark.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = PrimValue !PrimValue
           | ArrayValue !(Array Int Value) (TypeBase () ())
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

instance Ord vn => Ord (IdentBase ty vn) where
  compare = comparing identName

instance Located (IdentBase ty vn) where
  locOf = locOf . identSrcLoc

instance Hashable vn => Hashable (IdentBase ty vn) where
  hashWithSalt salt = hashWithSalt salt . identName

-- | Default binary operators.
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

-- | Whether a bound for an end-point of a 'DimSlice' or a range
-- literal is inclusive or exclusive.
data Inclusiveness a = DownToExclusive a
                     | ToInclusive a -- ^ May be "down to" if step is negative.
                     | UpToExclusive a
                     deriving (Eq, Ord, Show)

instance Located a => Located (Inclusiveness a) where
  locOf (DownToExclusive x) = locOf x
  locOf (ToInclusive x) = locOf x
  locOf (UpToExclusive x) = locOf x

-- | An indexing of a single dimension.
data DimIndexBase f vn = DimFix (ExpBase f vn)
                       | DimSlice (Maybe (ExpBase f vn))
                                  (Maybe (ExpBase f vn))
                                  (Maybe (ExpBase f vn))
deriving instance Showable f vn => Show (DimIndexBase f vn)

-- | A name qualified with a breadcrumb of module accesses.
data QualName vn = QualName { qualQuals :: ![Name]
                            , qualLeaf  :: !vn
                            }
  deriving (Eq, Ord, Show)

instance Functor QualName where
  fmap f (QualName qs leaf) = QualName qs $ f leaf

instance Hashable vn => Hashable (QualName vn) where
  hashWithSalt salt (QualName quals leaf) =
    hashWithSalt salt (quals, leaf)

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

            | Parens (ExpBase f vn) SrcLoc
            -- ^ A parenthesized expression.

            | QualParens (QualName vn) (ExpBase f vn) SrcLoc

            | TupLit    [ExpBase f vn] SrcLoc
            -- ^ Tuple literals, e.g., @{1+3, {x, y+z}}@.

            | RecordLit [FieldBase f vn] SrcLoc
            -- ^ Record literals, e.g. @{x=2,y=3,z}@.

            | ArrayLit  [ExpBase f vn] (f (CompTypeBase vn)) SrcLoc
            -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
            -- Second arg is the row type of the rows of the array.

            | Range (ExpBase f vn) (Maybe (ExpBase f vn)) (Inclusiveness (ExpBase f vn)) SrcLoc

            | Empty (TypeDeclBase f vn) SrcLoc

            | Var    (QualName vn) (f (CompTypeBase vn)) SrcLoc

            | Ascript (ExpBase f vn) (TypeDeclBase f vn) SrcLoc
            -- ^ Type ascription: @e : t@.

            | LetPat [TypeParamBase vn] (PatternBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc

            | LetFun vn ([TypeParamBase vn], [PatternBase f vn], Maybe (TypeExp vn), f (StructTypeBase vn), ExpBase f vn)
              (ExpBase f vn) SrcLoc

            | If     (ExpBase f vn) (ExpBase f vn) (ExpBase f vn) (f (CompTypeBase vn)) SrcLoc

            | Apply (QualName vn) [(ExpBase f vn, Diet)] (f (CompTypeBase vn)) SrcLoc

            | Negate (ExpBase f vn) SrcLoc
              -- ^ Numeric negation (ugly special case; Haskell did it first).

            | DoLoop
              [TypeParamBase vn]
              (PatternBase f vn) -- Merge variable pattern
              (ExpBase f vn) -- Initial values of merge variables.
              (LoopFormBase f vn) -- Do or while loop.
              (ExpBase f vn) -- Loop body.
              SrcLoc

            | BinOp (QualName vn)
              (ExpBase f vn, Diet) (ExpBase f vn, Diet) (f (CompTypeBase vn)) SrcLoc

            | Project Name (ExpBase f vn) (f (CompTypeBase vn)) SrcLoc

            -- Primitive array operations
            | LetWith (IdentBase f vn) (IdentBase f vn)
                      [DimIndexBase f vn] (ExpBase f vn)
                      (ExpBase f vn) SrcLoc

            | Index (ExpBase f vn) [DimIndexBase f vn] SrcLoc

            | Update (ExpBase f vn) [DimIndexBase f vn] (ExpBase f vn) SrcLoc

            | Split Int (ExpBase f vn) (ExpBase f vn) SrcLoc
            -- ^ @split@0( (1,1,3), [ 1, 2, 3, 4 ]) = {[1], [], [2,
            -- 3], [4]}@.  Note that this is different from in the
            -- core language.  The static integer indicates which
            -- dimension to concatenate across.

            | Concat Int (ExpBase f vn) [ExpBase f vn] SrcLoc
            -- ^ @concat@0([1],[2, 3, 4]) = [1, 2, 3, 4]@.  The
            -- static integer indicates which dimension to concatenate
            -- across.

            -- Array index space transformation.
            | Reshape (ExpBase f vn) (ExpBase f vn) SrcLoc
             -- ^ 1st arg is the new shape, 2nd arg is the input array.

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
deriving instance Showable f vn => Show (StreamForm f vn)

instance Located (ExpBase f vn) where
  locOf (Literal _ loc)          = locOf loc
  locOf (Parens _ loc)           = locOf loc
  locOf (QualParens _ _ loc)     = locOf loc
  locOf (TupLit _ pos)           = locOf pos
  locOf (RecordLit _ pos)        = locOf pos
  locOf (Project _ _ _ pos)      = locOf pos
  locOf (ArrayLit _ _ pos)       = locOf pos
  locOf (Range _ _ _ pos)        = locOf pos
  locOf (Empty _ pos)            = locOf pos
  locOf (BinOp _ _ _ _ pos)      = locOf pos
  locOf (If _ _ _ _ pos)         = locOf pos
  locOf (Var _ _ loc)            = locOf loc
  locOf (Ascript _ _ loc)        = locOf loc
  locOf (Negate _ pos)           = locOf pos
  locOf (Apply _ _ _ pos)        = locOf pos
  locOf (LetPat _ _ _ _ pos)     = locOf pos
  locOf (LetFun _ _ _ loc)       = locOf loc
  locOf (LetWith _ _ _ _ _ pos)  = locOf pos
  locOf (Index _ _ pos)          = locOf pos
  locOf (Update _ _ _ pos)       = locOf pos
  locOf (Reshape _ _ pos)        = locOf pos
  locOf (Rearrange _ _ pos)      = locOf pos
  locOf (Rotate _ _ _ pos)       = locOf pos
  locOf (Map _ _ pos)            = locOf pos
  locOf (Reduce _ _ _ _ pos)     = locOf pos
  locOf (Zip _ _ _ pos)          = locOf pos
  locOf (Unzip _ _ pos)          = locOf pos
  locOf (Scan _ _ _ pos)         = locOf pos
  locOf (Filter _ _ pos)         = locOf pos
  locOf (Partition _ _ pos)      = locOf pos
  locOf (Split _ _ _ pos)        = locOf pos
  locOf (Concat _ _ _ pos)       = locOf pos
  locOf (DoLoop _ _ _ _ _ pos)   = locOf pos
  locOf (Stream _ _ _  pos)      = locOf pos
  locOf (Unsafe _ loc)           = locOf loc

-- | An entry in a record literal.
data FieldBase f vn = RecordField Name (ExpBase f vn) SrcLoc
                    | RecordRecord (ExpBase f vn)

deriving instance Showable f vn => Show (FieldBase f vn)

instance Located (FieldBase f vn) where
  locOf (RecordField _ _ loc) = locOf loc
  locOf (RecordRecord e)      = locOf e

-- | Whether the loop is a @for@-loop or a @while@-loop.
data LoopFormBase f vn = For (IdentBase f vn) (ExpBase f vn)
                       | ForIn (PatternBase f vn) (ExpBase f vn)
                       | While (ExpBase f vn)
deriving instance Showable f vn => Show (LoopFormBase f vn)

-- | Anonymous function passed to a SOAC.
data LambdaBase f vn = AnonymFun [TypeParamBase vn] [PatternBase f vn] (ExpBase f vn) (Maybe (TypeDeclBase f vn)) (f (StructTypeBase vn)) SrcLoc
                      -- ^ @fn (x: bool, z: char):int => if x then ord z else ord z + 1@
                      | CurryFun (QualName vn)
                        [ExpBase f vn] (f ([CompTypeBase vn], CompTypeBase vn)) SrcLoc
                        -- ^ @f(4)@
                      | BinOpFun (QualName vn)
                        (f (CompTypeBase vn)) (f (CompTypeBase vn)) (f (CompTypeBase vn)) SrcLoc
                        -- ^ @+@; first two types are operands, third is result.
                      | CurryBinOpLeft (QualName vn)
                        (ExpBase f vn) (f (CompTypeBase vn), f (CompTypeBase vn)) (f (CompTypeBase vn)) SrcLoc
                        -- ^ @2+@; first type is operand, second is result.
                      | CurryBinOpRight (QualName vn)
                        (ExpBase f vn) (f (CompTypeBase vn), f (CompTypeBase vn)) (f (CompTypeBase vn)) SrcLoc
                        -- ^ @+2@; first type is operand, second is result.
                      | CurryProject Name (f (CompTypeBase vn), f (CompTypeBase vn)) SrcLoc
                      -- ^ @#field@.  First type is operand, second is result.
deriving instance Showable f vn => Show (LambdaBase f vn)

instance Located (LambdaBase f vn) where
  locOf (AnonymFun _ _ _ _ _ loc)     = locOf loc
  locOf (CurryFun  _ _ _ loc)         = locOf loc
  locOf (BinOpFun _ _ _ _ loc)        = locOf loc
  locOf (CurryBinOpLeft _ _ _ _ loc)  = locOf loc
  locOf (CurryBinOpRight _ _ _ _ loc) = locOf loc
  locOf (CurryProject _ _ loc)        = locOf loc

-- | A pattern as used most places where variables are bound (function
-- parameters, @let@ expressions, etc).
data PatternBase f vn = TuplePattern [PatternBase f vn] SrcLoc
                      | RecordPattern [(Name, PatternBase f vn)] SrcLoc
                      | PatternParens (PatternBase f vn) SrcLoc
                      | Id vn (f (TypeBase (DimDecl vn) (Names vn))) SrcLoc
                      | Wildcard (f (TypeBase (DimDecl vn) (Names vn))) SrcLoc -- Nothing, i.e. underscore.
                      | PatternAscription (PatternBase f vn) (TypeDeclBase f vn)
deriving instance Showable f vn => Show (PatternBase f vn)

instance Located (PatternBase f vn) where
  locOf (TuplePattern _ loc)    = locOf loc
  locOf (RecordPattern _ loc)   = locOf loc
  locOf (PatternParens _ loc)   = locOf loc
  locOf (Id _ _ loc)            = locOf loc
  locOf (Wildcard _ loc)        = locOf loc
  locOf (PatternAscription p _) = locOf p

-- | Function Declarations
data FunBindBase f vn = FunBind { funBindEntryPoint :: Bool
                                -- ^ True if this function is an entry point.
                                , funBindName       :: vn
                                , funBindRetDecl    :: Maybe (TypeExp vn)
                                , funBindRetType    :: f (StructTypeBase vn)
                                , funBindTypeParams :: [TypeParamBase vn]
                                , funBindParams     :: [PatternBase f vn]
                                , funBindBody       :: ExpBase f vn
                                , funBindDoc        :: Maybe String
                                , funBindLocation   :: SrcLoc
                                }
deriving instance Showable f vn => Show (FunBindBase f vn)

instance Located (FunBindBase f vn) where
  locOf = locOf . funBindLocation

-- | Value declaration.
data ValBindBase f vn = ValBind { constBindEntryPoint :: Bool
                                -- ^ True if this value is an entry point.
                                , constBindName     :: vn
                                , constBindTypeDecl :: Maybe (TypeExp vn)
                                , constBindType     :: f (StructTypeBase vn)
                                , constBindDef      :: ExpBase f vn
                                , constDoc          :: Maybe String
                                , constBindLocation :: SrcLoc
                                }
deriving instance Showable f vn => Show (ValBindBase f vn)

instance Located (ValBindBase f vn) where
  locOf = locOf . constBindLocation

-- | Type Declarations
data TypeBindBase f vn = TypeBind { typeAlias        :: vn
                                  , typeParams       :: [TypeParamBase vn]
                                  , typeExp          :: TypeDeclBase f vn
                                  , typeDoc          :: Maybe String
                                  , typeBindLocation :: SrcLoc
                                  }
deriving instance Showable f vn => Show (TypeBindBase f vn)

instance Located (TypeBindBase f vn) where
  locOf = locOf . typeBindLocation

data TypeParamBase vn = TypeParamDim vn SrcLoc
                        -- ^ A type parameter that must be a size.
                      | TypeParamType vn SrcLoc
                        -- ^ A type parameter that must be a type.
  deriving (Show)

instance Located (TypeParamBase vn) where
  locOf (TypeParamDim _ loc)  = locOf loc
  locOf (TypeParamType _ loc) = locOf loc

typeParamName :: TypeParamBase vn -> vn
typeParamName (TypeParamDim v _)  = v
typeParamName (TypeParamType v _) = v

data ParamBase f vn = NamedParam vn (TypeDeclBase f vn) SrcLoc
                    | UnnamedParam (TypeDeclBase f vn)
deriving instance Showable f vn => Show (ParamBase f vn)

instance Located (ParamBase f vn) where
  locOf (NamedParam _ _ loc) = locOf loc
  locOf (UnnamedParam t)     = locOf t

data SpecBase f vn = ValSpec  { specName       :: vn
                              , specTypeParams :: [TypeParamBase vn]
                              , specParams     :: [ParamBase f vn]
                              , specRettype    :: TypeDeclBase f vn
                              , specDoc        :: Maybe String
                              , specLocation   :: SrcLoc
                              }
                   | TypeAbbrSpec (TypeBindBase f vn)
                   | TypeSpec vn [TypeParamBase vn] (Maybe String) SrcLoc -- ^ Abstract type.
                   | ModSpec vn (SigExpBase f vn) SrcLoc
                   | IncludeSpec (SigExpBase f vn) SrcLoc
deriving instance Showable f vn => Show (SpecBase f vn)

instance Located (SpecBase f vn) where
  locOf (ValSpec _ _ _ _ _ loc) = locOf loc
  locOf (TypeAbbrSpec tbind)    = locOf tbind
  locOf (TypeSpec _ _ _ loc)    = locOf loc
  locOf (ModSpec _ _ loc)       = locOf loc
  locOf (IncludeSpec _ loc)     = locOf loc

data SigExpBase f vn = SigVar (QualName vn) SrcLoc
                     | SigParens (SigExpBase f vn) SrcLoc
                     | SigSpecs [SpecBase f vn] SrcLoc
                     | SigWith (SigExpBase f vn) (TypeRefBase f vn) SrcLoc
                     | SigArrow (Maybe vn) (SigExpBase f vn) (SigExpBase f vn) SrcLoc
deriving instance Showable f vn => Show (SigExpBase f vn)

-- | A type refinement.
data TypeRefBase f vn = TypeRef (QualName vn) (TypeDeclBase f vn)
deriving instance Showable f vn => Show (TypeRefBase f vn)

instance Located (SigExpBase f vn) where
  locOf (SigVar _ loc)       = locOf loc
  locOf (SigParens _ loc)    = locOf loc
  locOf (SigSpecs _ loc)     = locOf loc
  locOf (SigWith _ _ loc)    = locOf loc
  locOf (SigArrow _ _ _ loc) = locOf loc

data SigBindBase f vn = SigBind { sigName :: vn
                                , sigExp  :: SigExpBase f vn
                                , sigDoc  :: Maybe String
                                , sigLoc  :: SrcLoc
                                }
deriving instance Showable f vn => Show (SigBindBase f vn)

instance Located (SigBindBase f vn) where
  locOf = locOf . sigLoc

data ModExpBase f vn = ModVar (QualName vn) SrcLoc
                     | ModParens (ModExpBase f vn) SrcLoc
                     | ModImport FilePath SrcLoc
                       -- ^ The contents of another file as a module.
                     | ModDecs [DecBase f vn] SrcLoc
                     | ModApply (ModExpBase f vn) (ModExpBase f vn) (f (M.Map VName VName)) (f (M.Map VName VName)) SrcLoc
                       -- ^ Functor application.
                     | ModAscript (ModExpBase f vn) (SigExpBase f vn) (f (M.Map VName VName)) SrcLoc
                     | ModLambda (ModParamBase f vn)
                                 (Maybe (SigExpBase f vn, f (M.Map VName VName)))
                                 (ModExpBase f vn)
                                 SrcLoc
deriving instance Showable f vn => Show (ModExpBase f vn)

instance Located (ModExpBase f vn) where
  locOf (ModVar _ loc)         = locOf loc
  locOf (ModParens _ loc)      = locOf loc
  locOf (ModImport _ loc)      = locOf loc
  locOf (ModDecs _ loc)        = locOf loc
  locOf (ModApply _ _ _ _ loc) = locOf loc
  locOf (ModAscript _ _ _ loc) = locOf loc
  locOf (ModLambda _ _ _ loc)  = locOf loc

data ModBindBase f vn =
  ModBind { modName      :: vn
          , modParams    :: [ModParamBase f vn]
          , modSignature :: Maybe (SigExpBase f vn, f (M.Map VName VName))
          , modExp       :: ModExpBase f vn
          , modDoc       :: Maybe String
          , modLocation  :: SrcLoc
          }
deriving instance Showable f vn => Show (ModBindBase f vn)

instance Located (ModBindBase f vn) where
  locOf = locOf . modLocation

data ModParamBase f vn = ModParam { modParamName     :: vn
                                  , modParamType     :: SigExpBase f vn
                                  , modParamLocation :: SrcLoc
                                  }
deriving instance Showable f vn => Show (ModParamBase f vn)

instance Located (ModParamBase f vn) where
  locOf = locOf . modParamLocation

-- | A top-level binding.
data DecBase f vn = ValDec (ValBindBase f vn)
                  | FunDec (FunBindBase f vn)
                  | TypeDec (TypeBindBase f vn)
                  | SigDec (SigBindBase f vn)
                  | ModDec (ModBindBase f vn)
                  | OpenDec (ModExpBase f vn) [ModExpBase f vn] (f [VName]) SrcLoc
                  | LocalDec (DecBase f vn) SrcLoc
deriving instance Showable f vn => Show (DecBase f vn)

instance Located (DecBase f vn) where
  locOf (ValDec d)          = locOf d
  locOf (FunDec d)          = locOf d
  locOf (TypeDec d)         = locOf d
  locOf (SigDec d)          = locOf d
  locOf (ModDec d)          = locOf d
  locOf (OpenDec _ _ _ loc) = locOf loc
  locOf (LocalDec _ loc)    = locOf loc

data ProgBase f vn = Prog { progDoc :: Maybe String
                          , progDecs :: [DecBase f vn]
                          }
deriving instance Showable f vn => Show (ProgBase f vn)

-- | A set of names.
type Names = S.Set

--- Some prettyprinting definitions are here because we need them in
--- the Attributes module.

instance Pretty PrimType where
  ppr (Unsigned Int8)  = text "u8"
  ppr (Unsigned Int16) = text "u16"
  ppr (Unsigned Int32) = text "u32"
  ppr (Unsigned Int64) = text "u64"
  ppr (Signed t)       = ppr t
  ppr (FloatType t)    = ppr t
  ppr Bool             = text "bool"

instance Pretty BinOp where
  ppr Plus     = text "+"
  ppr Minus    = text "-"
  ppr Pow      = text "**"
  ppr Times    = text "*"
  ppr Divide   = text "/"
  ppr Mod      = text "%"
  ppr Quot     = text "//"
  ppr Rem      = text "%%"
  ppr ShiftR   = text ">>"
  ppr ZShiftR  = text ">>>"
  ppr ShiftL   = text "<<"
  ppr Band     = text "&"
  ppr Xor      = text "^"
  ppr Bor      = text "|"
  ppr LogAnd   = text "&&"
  ppr LogOr    = text "||"
  ppr Equal    = text "=="
  ppr NotEqual = text "!="
  ppr Less     = text "<"
  ppr Leq      = text "<="
  ppr Greater  = text ">"
  ppr Geq      = text ">="
