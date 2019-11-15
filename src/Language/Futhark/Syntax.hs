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
  , DimExp(..)
  , TypeExp(..)
  , TypeArgExp(..)
  , PName(..)
  , ScalarTypeBase(..)
  , PatternType
  , StructType
  , ValueType
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
  , CaseBase(..)
  , LoopFormBase (..)
  , PatternBase(..)

  -- * Module language
  , SpecBase(..)
  , SigExpBase(..)
  , TypeRefBase(..)
  , SigBindBase(..)
  , ModExpBase(..)
  , ModBindBase(..)
  , ModParamBase(..)

  -- * Definitions
  , DocComment(..)
  , ValBindBase(..)
  , Liftedness(..)
  , TypeBindBase(..)
  , TypeParamBase(..)
  , typeParamName
  , ProgBase(..)
  , DecBase(..)

  -- * Miscellaneous
  , Showable
  , NoInfo(..)
  , Info(..)
  , Alias(..)
  , Aliasing
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
import           Data.Loc
import qualified Data.Map.Strict                  as M
import           Data.Monoid                      hiding (Sum)
import           Data.Ord
import qualified Data.Set                         as S
import           Data.Traversable
import qualified Data.List.NonEmpty               as NE
import           Data.List
import           Prelude

import           Futhark.Representation.Primitive (FloatType (..),
                                                   FloatValue (..),
                                                   IntType (..), IntValue (..))
import           Futhark.Util.Pretty
import           Language.Futhark.Core

-- | Convenience class for deriving 'Show' instances for the AST.
class (Show vn,
       Show (f VName),
       Show (f (Diet, Maybe VName)),
       Show (f String),
       Show (f [VName]),
       Show (f ([VName], [VName])),
       Show (f PatternType),
       Show (f (PatternType, [VName])),
       Show (f (StructType, [VName])),
       Show (f Int),
       Show (f StructType),
       Show (f (StructType, Maybe VName)),
       Show (f (Aliasing, StructType)),
       Show (f (M.Map VName VName)),
       Show (f Uniqueness)) => Showable f vn where

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

class Eq dim => ArrayDim dim where
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
                deriving Show
deriving instance Eq (DimDecl Name)
deriving instance Eq (DimDecl VName)
deriving instance Ord (DimDecl Name)
deriving instance Ord (DimDecl VName)

instance Functor DimDecl where
  fmap = fmapDefault

instance Foldable DimDecl where
  foldMap = foldMapDefault

instance Traversable DimDecl where
  traverse f (NamedDim qn) = NamedDim <$> traverse f qn
  traverse _ (ConstDim x) = pure $ ConstDim x
  traverse _ AnyDim = pure AnyDim

-- Note that the notion of unifyDims here is intentionally not what we
-- use when we do real type unification in the type checker.
instance ArrayDim (DimDecl VName) where
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

instance Semigroup (ShapeDecl dim) where
  ShapeDecl l1 <> ShapeDecl l2 = ShapeDecl $ l1 ++ l2

instance Monoid (ShapeDecl dim) where
  mempty = ShapeDecl []

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
data TypeName = TypeName { typeQuals :: [VName], typeLeaf :: VName }
              deriving (Show)

instance Eq TypeName where
  TypeName _ x == TypeName _ y = x == y

instance Ord TypeName where
  TypeName _ x `compare` TypeName _ y = x `compare` y

typeNameFromQualName :: QualName VName -> TypeName
typeNameFromQualName (QualName qs x) = TypeName qs x

qualNameFromTypeName :: TypeName -> QualName VName
qualNameFromTypeName (TypeName qs x) = QualName qs x

-- | The name (if any) of a function parameter.  The 'Eq' and 'Ord'
-- instances always compare values of this type equal.
data PName = Named VName | Unnamed
           deriving (Show)

instance Eq PName where
  _ == _ = True

instance Ord PName where
  _ <= _ = True

-- | Types that can be elements of arrays.  This representation does
-- allow arrays of records of functions, which is nonsensical, but it
-- convolutes the code too much if we try to statically rule it out.
data ScalarTypeBase dim as
  = Prim PrimType
  | TypeVar as Uniqueness TypeName [TypeArg dim]
  | Record (M.Map Name (TypeBase dim as))
  | Sum (M.Map Name [TypeBase dim as])
  | Arrow as PName (TypeBase dim as) (TypeBase dim as)
    -- ^ The aliasing corresponds to the lexical
    -- closure of the function.
  deriving (Eq, Ord, Show)

instance Bitraversable ScalarTypeBase where
  bitraverse _ _ (Prim t) = pure $ Prim t
  bitraverse f g (Record fs) = Record <$> traverse (bitraverse f g) fs
  bitraverse f g (TypeVar als u t args) =
    TypeVar <$> g als <*> pure u <*> pure t <*> traverse (traverse f) args
  bitraverse f g (Arrow als v t1 t2) =
    Arrow <$> g als <*> pure v <*> bitraverse f g t1 <*> bitraverse f g t2
  bitraverse f g (Sum cs) = Sum <$> (traverse . traverse) (bitraverse f g) cs

instance Bifunctor ScalarTypeBase where
  bimap = bimapDefault

instance Bifoldable ScalarTypeBase where
  bifoldMap = bifoldMapDefault

-- | An expanded Futhark type is either an array, or something that
-- can be an element of an array.  When comparing types for equality,
-- function parameter names are ignored.  This representation permits
-- some malformed types (arrays of functions), but importantly rules
-- out arrays-of-arrays.
data TypeBase dim as
  = Scalar (ScalarTypeBase dim as)
  | Array as Uniqueness (ScalarTypeBase dim ()) (ShapeDecl dim)
  deriving (Eq, Ord, Show)

instance Bitraversable TypeBase where
  bitraverse f g (Scalar t) = Scalar <$> bitraverse f g t
  bitraverse f g (Array a u t shape) =
    Array <$> g a <*> pure u <*> bitraverse f pure t <*> traverse f shape

instance Bifunctor TypeBase where
  bimap = bimapDefault

instance Bifoldable TypeBase where
  bifoldMap = bifoldMapDefault

data TypeArg dim = TypeArgDim dim SrcLoc
                 | TypeArgType (TypeBase dim ()) SrcLoc
             deriving (Eq, Ord, Show)

instance Traversable TypeArg where
  traverse f (TypeArgDim v loc) = TypeArgDim <$> f v <*> pure loc
  traverse f (TypeArgType t loc) = TypeArgType <$> bitraverse f pure t <*> pure loc

instance Functor TypeArg where
  fmap = fmapDefault

instance Foldable TypeArg where
  foldMap = foldMapDefault

-- | A variable that is aliased.  Can be still in-scope, or have gone
-- out of scope and be free.  In the latter case, it behaves more like
-- an equivalence class.  See uniqueness-error18.fut for an example of
-- why this is necessary.
data Alias = AliasBound { aliasVar :: VName }
           | AliasFree { aliasVar :: VName }
           deriving (Eq, Ord, Show)

-- | Aliasing for a type, which is a set of the variables that are
-- aliased.
type Aliasing = S.Set Alias

-- | A type with aliasing information and shape annotations, used for
-- describing the type patterns and expressions.
type PatternType = TypeBase (DimDecl VName) Aliasing

-- | A "structural" type with shape annotations and no aliasing
-- information, used for declarations.
type StructType = TypeBase (DimDecl VName) ()

-- | A value type contains full, manifest size information.
type ValueType = TypeBase Int32 ()

-- | A dimension declaration expression for use in a 'TypeExp'.
data DimExp vn = DimExpNamed (QualName vn) SrcLoc
                 -- ^ The size of the dimension is this name, which
                 -- must be in scope.
               | DimExpConst Int SrcLoc
                  -- ^ The size is a constant.
               | DimExpAny
                  -- ^ No dimension declaration.
                deriving Show
deriving instance Eq (DimExp Name)
deriving instance Eq (DimExp VName)
deriving instance Ord (DimExp Name)
deriving instance Ord (DimExp VName)

-- | An unstructured type with type variables and possibly shape
-- declarations - this is what the user types in the source program.
data TypeExp vn = TEVar (QualName vn) SrcLoc
                | TETuple [TypeExp vn] SrcLoc
                | TERecord [(Name, TypeExp vn)] SrcLoc
                | TEArray (TypeExp vn) (DimExp vn) SrcLoc
                | TEUnique (TypeExp vn) SrcLoc
                | TEApply (TypeExp vn) (TypeArgExp vn) SrcLoc
                | TEArrow (Maybe vn) (TypeExp vn) (TypeExp vn) SrcLoc
                | TESum [(Name, [TypeExp vn])] SrcLoc
                 deriving (Show)
deriving instance Eq (TypeExp Name)
deriving instance Eq (TypeExp VName)
deriving instance Ord (TypeExp Name)
deriving instance Ord (TypeExp VName)

instance Located (TypeExp vn) where
  locOf (TEArray _ _ loc)   = locOf loc
  locOf (TETuple _ loc)     = locOf loc
  locOf (TERecord _ loc)    = locOf loc
  locOf (TEVar _ loc)       = locOf loc
  locOf (TEUnique _ loc)    = locOf loc
  locOf (TEApply _ _ loc)   = locOf loc
  locOf (TEArrow _ _ _ loc) = locOf loc
  locOf (TESum _ loc)      = locOf loc

data TypeArgExp vn = TypeArgExpDim (DimExp vn) SrcLoc
                   | TypeArgExpType (TypeExp vn)
                deriving (Show)
deriving instance Eq (TypeArgExp Name)
deriving instance Eq (TypeArgExp VName)
deriving instance Ord (TypeArgExp Name)
deriving instance Ord (TypeArgExp VName)

instance Located (TypeArgExp vn) where
  locOf (TypeArgExpDim _ loc) = locOf loc
  locOf (TypeArgExpType t)    = locOf t

-- | A declaration of the type of something.
data TypeDeclBase f vn =
  TypeDecl { declaredType :: TypeExp vn
                             -- ^ The type declared by the user.
           , expandedType :: f StructType
                             -- ^ The type deduced by the type checker.
           }
deriving instance Showable f vn => Show (TypeDeclBase f vn)
deriving instance Eq (TypeDeclBase NoInfo VName)
deriving instance Ord (TypeDeclBase NoInfo VName)

instance Located (TypeDeclBase f vn) where
  locOf = locOf . declaredType

-- | Information about which parts of a value/type are consumed.
data Diet = RecordDiet (M.Map Name Diet) -- ^ Consumes these fields in the record.
          | FuncDiet Diet Diet
            -- ^ A function that consumes its argument(s) like this.
            -- The final 'Diet' should always be 'Observe', as there
            -- is no way for a function to consume its return value.
          | Consume -- ^ Consumes this value.
          | Observe -- ^ Only observes value in this position, does
                    -- not consume.
            deriving (Eq, Show)

-- | Simple Futhark values.  Values are fully evaluated and their type
-- is always unambiguous.
data Value = PrimValue !PrimValue
           | ArrayValue !(Array Int Value) ValueType
             -- ^ It is assumed that the array is 0-indexed.  The type
             -- is the full type.
             deriving (Eq, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data IdentBase f vn = Ident { identName   :: vn
                            , identType   :: f PatternType
                            , identSrcLoc :: SrcLoc
                            }
deriving instance Showable f vn => Show (IdentBase f vn)

instance Eq vn => Eq (IdentBase ty vn) where
  x == y = identName x == identName y

instance Ord vn => Ord (IdentBase ty vn) where
  compare = comparing identName

instance Located (IdentBase ty vn) where
  locOf = locOf . identSrcLoc

-- | Default binary operators.
data BinOp =  Backtick
              -- ^ A pseudo-operator standing in for any normal
              -- identifier used as an operator (they all have the
              -- same fixity).
           -- Binary Ops for Numbers
           | Plus
           | Minus
           | Pow
           | Times
           | Divide
           | Mod
           | Quot
           | Rem
           | ShiftR
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
           -- Some functional ops.
           | PipeRight -- ^ @|>@
           | PipeLeft -- ^ @<|@
           -- Misc
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

instance Functor Inclusiveness where
  fmap = fmapDefault

instance Foldable Inclusiveness where
  foldMap = foldMapDefault

instance Traversable Inclusiveness where
  traverse f (DownToExclusive x) = DownToExclusive <$> f x
  traverse f (ToInclusive x) = ToInclusive <$> f x
  traverse f (UpToExclusive x) = UpToExclusive <$> f x

-- | An indexing of a single dimension.
data DimIndexBase f vn = DimFix (ExpBase f vn)
                       | DimSlice (Maybe (ExpBase f vn))
                                  (Maybe (ExpBase f vn))
                                  (Maybe (ExpBase f vn))
deriving instance Showable f vn => Show (DimIndexBase f vn)
deriving instance Eq (DimIndexBase NoInfo VName)
deriving instance Ord (DimIndexBase NoInfo VName)

-- | A name qualified with a breadcrumb of module accesses.
data QualName vn = QualName { qualQuals :: ![vn]
                            , qualLeaf  :: !vn
                            }
  deriving (Show)

instance Eq (QualName Name) where
  QualName qs1 v1 == QualName qs2 v2 = qs1 == qs2 && v1 == v2

instance Eq (QualName VName) where
  QualName _ v1 == QualName _ v2 = v1 == v2

instance Ord (QualName Name) where
  QualName qs1 v1 `compare` QualName qs2 v2 = compare (qs1, v1) (qs2, v2)

instance Ord (QualName VName) where
  QualName _ v1 `compare` QualName _ v2 = compare v1 v2

instance Functor QualName where
  fmap = fmapDefault

instance Foldable QualName where
  foldMap = foldMapDefault

instance Traversable QualName where
  traverse f (QualName qs v) = QualName <$> traverse f qs <*> f v

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

            | IntLit Integer (f PatternType) SrcLoc
            -- ^ A polymorphic integral literal.

            | FloatLit Double (f PatternType) SrcLoc
            -- ^ A polymorphic decimal literal.

            | StringLit [Word8] SrcLoc
            -- ^ A string literal is just a fancy syntax for an array
            -- of bytes.

            | Parens (ExpBase f vn) SrcLoc
            -- ^ A parenthesized expression.

            | QualParens (QualName vn, SrcLoc) (ExpBase f vn) SrcLoc

            | TupLit    [ExpBase f vn] SrcLoc
            -- ^ Tuple literals, e.g., @{1+3, {x, y+z}}@.

            | RecordLit [FieldBase f vn] SrcLoc
            -- ^ Record literals, e.g. @{x=2,y=3,z}@.

            | ArrayLit  [ExpBase f vn] (f PatternType) SrcLoc
            -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
            -- Second arg is the row type of the rows of the array.

            | Range (ExpBase f vn) (Maybe (ExpBase f vn)) (Inclusiveness (ExpBase f vn))
              (f PatternType, f [VName]) SrcLoc

            | Var (QualName vn) (f PatternType) SrcLoc

            | Ascript (ExpBase f vn) (TypeDeclBase f vn) SrcLoc
            -- ^ Type ascription: @e : t@.

            | Coerce (ExpBase f vn) (TypeDeclBase f vn) (f PatternType, f [VName]) SrcLoc
            -- ^ Size coercion: @e :> t@.

            | LetPat (PatternBase f vn) (ExpBase f vn) (ExpBase f vn)
              (f PatternType, f [VName]) SrcLoc

            | LetFun vn ([TypeParamBase vn],
                         [PatternBase f vn],
                         Maybe (TypeExp vn),
                         f StructType,
                         ExpBase f vn)
              (ExpBase f vn) SrcLoc

            | If (ExpBase f vn) (ExpBase f vn) (ExpBase f vn) (f PatternType, f [VName]) SrcLoc

            | Apply (ExpBase f vn) (ExpBase f vn)
              (f (Diet, Maybe VName)) (f PatternType, f [VName]) SrcLoc
              -- ^ The @Maybe VName@ is a possible existential size
              -- that is instantiated by this argument..
              --
              -- The @[VName]@ are the existential sizes that come
              -- into being at this call site.

            | Negate (ExpBase f vn) SrcLoc
              -- ^ Numeric negation (ugly special case; Haskell did it first).

            | Lambda [PatternBase f vn] (ExpBase f vn)
              (Maybe (TypeExp vn)) (f (Aliasing, StructType)) SrcLoc

            | OpSection (QualName vn) (f PatternType) SrcLoc
              -- ^ @+@; first two types are operands, third is result.
            | OpSectionLeft (QualName vn) (f PatternType) (ExpBase f vn)
              (f (StructType, Maybe VName), f StructType) (f PatternType, f [VName]) SrcLoc
              -- ^ @2+@; first type is operand, second is result.
            | OpSectionRight (QualName vn) (f PatternType) (ExpBase f vn)
              (f StructType, f (StructType, Maybe VName)) (f PatternType) SrcLoc
              -- ^ @+2@; first type is operand, second is result.
            | ProjectSection [Name] (f PatternType) SrcLoc
              -- ^ Field projection as a section: @(.x.y.z)@.
            | IndexSection [DimIndexBase f vn] (f PatternType) SrcLoc
              -- ^ Array indexing as a section: @(.[i,j])@.

            | DoLoop
              [VName] -- Size parameters.
              (PatternBase f vn) -- Merge variable pattern.
              (ExpBase f vn) -- Initial values of merge variables.
              (LoopFormBase f vn) -- Do or while loop.
              (ExpBase f vn) -- Loop body.
              (f (PatternType, [VName])) -- Return type.
              SrcLoc

            | BinOp (QualName vn, SrcLoc) (f PatternType)
              (ExpBase f vn, f (StructType, Maybe VName))
              (ExpBase f vn, f (StructType, Maybe VName))
              (f PatternType) (f [VName]) SrcLoc

            | Project Name (ExpBase f vn) (f PatternType) SrcLoc

            -- Primitive array operations
            | LetWith (IdentBase f vn) (IdentBase f vn)
                      [DimIndexBase f vn] (ExpBase f vn)
                      (ExpBase f vn) (f PatternType) SrcLoc

            | Index (ExpBase f vn) [DimIndexBase f vn] (f PatternType, f [VName]) SrcLoc

            | Update (ExpBase f vn) [DimIndexBase f vn] (ExpBase f vn) SrcLoc

            | RecordUpdate (ExpBase f vn) [Name] (ExpBase f vn) (f PatternType) SrcLoc

            | Unsafe (ExpBase f vn) SrcLoc
            -- ^ Explore the Danger Zone and elide safety checks on
            -- array operations and other assertions during execution
            -- of this expression.  Make really sure the code is
            -- correct.

            | Assert (ExpBase f vn) (ExpBase f vn) (f String) SrcLoc
            -- ^ Fail if the first expression does not return true,
            -- and return the value of the second expression if it
            -- does.

            | Constr Name [ExpBase f vn] (f PatternType) SrcLoc
            -- ^ An n-ary value constructor.

            | Match (ExpBase f vn) (NE.NonEmpty (CaseBase f vn))
              (f PatternType, f [VName]) SrcLoc
            -- ^ A match expression.
deriving instance Showable f vn => Show (ExpBase f vn)
deriving instance Eq (ExpBase NoInfo VName)
deriving instance Ord (ExpBase NoInfo VName)

instance Located (ExpBase f vn) where
  locOf (Literal _ loc)                = locOf loc
  locOf (IntLit _ _ loc)               = locOf loc
  locOf (FloatLit _ _ loc)             = locOf loc
  locOf (Parens _ loc)                 = locOf loc
  locOf (QualParens _ _ loc)           = locOf loc
  locOf (TupLit _ pos)                 = locOf pos
  locOf (RecordLit _ pos)              = locOf pos
  locOf (Project _ _ _ pos)            = locOf pos
  locOf (ArrayLit _ _ pos)             = locOf pos
  locOf (StringLit _ loc)              = locOf loc
  locOf (Range _ _ _ _ pos)            = locOf pos
  locOf (BinOp _ _ _ _ _ _ loc)        = locOf loc
  locOf (If _ _ _ _ pos)               = locOf pos
  locOf (Var _ _ loc)                  = locOf loc
  locOf (Ascript _ _ loc)              = locOf loc
  locOf (Coerce _ _ _ loc)             = locOf loc
  locOf (Negate _ pos)                 = locOf pos
  locOf (Apply _ _ _ _ loc)            = locOf loc
  locOf (LetPat _ _ _ _ loc)           = locOf loc
  locOf (LetFun _ _ _ loc)             = locOf loc
  locOf (LetWith _ _ _ _ _ _ loc)      = locOf loc
  locOf (Index _ _ _ loc)              = locOf loc
  locOf (Update _ _ _ pos)             = locOf pos
  locOf (RecordUpdate _ _ _ _ pos)     = locOf pos
  locOf (Lambda _ _ _ _ loc)           = locOf loc
  locOf (OpSection _ _ loc)            = locOf loc
  locOf (OpSectionLeft _ _ _ _ _ loc)  = locOf loc
  locOf (OpSectionRight _ _ _ _ _ loc) = locOf loc
  locOf (ProjectSection _ _ loc)       = locOf loc
  locOf (IndexSection _ _ loc)         = locOf loc
  locOf (DoLoop _ _ _ _ _ _ loc)       = locOf loc
  locOf (Unsafe _ loc)                 = locOf loc
  locOf (Assert _ _ _ loc)             = locOf loc
  locOf (Constr _ _ _ loc)             = locOf loc
  locOf (Match _ _ _ loc)              = locOf loc

-- | An entry in a record literal.
data FieldBase f vn = RecordFieldExplicit Name (ExpBase f vn) SrcLoc
                    | RecordFieldImplicit vn (f PatternType) SrcLoc
deriving instance Showable f vn => Show (FieldBase f vn)
deriving instance Eq (FieldBase NoInfo VName)
deriving instance Ord (FieldBase NoInfo VName)

instance Located (FieldBase f vn) where
  locOf (RecordFieldExplicit _ _ loc) = locOf loc
  locOf (RecordFieldImplicit _ _ loc) = locOf loc

-- | A case in a match expression.
data CaseBase f vn = CasePat (PatternBase f vn) (ExpBase f vn) SrcLoc
deriving instance Showable f vn => Show (CaseBase f vn)
deriving instance Eq (CaseBase NoInfo VName)
deriving instance Ord (CaseBase NoInfo VName)

instance Located (CaseBase f vn) where
  locOf (CasePat _ _ loc) = locOf loc

-- | Whether the loop is a @for@-loop or a @while@-loop.
data LoopFormBase f vn = For (IdentBase f vn) (ExpBase f vn)
                       | ForIn (PatternBase f vn) (ExpBase f vn)
                       | While (ExpBase f vn)
deriving instance Showable f vn => Show (LoopFormBase f vn)
deriving instance Eq (LoopFormBase NoInfo VName)
deriving instance Ord (LoopFormBase NoInfo VName)

-- | A pattern as used most places where variables are bound (function
-- parameters, @let@ expressions, etc).
data PatternBase f vn = TuplePattern [PatternBase f vn] SrcLoc
                      | RecordPattern [(Name, PatternBase f vn)] SrcLoc
                      | PatternParens (PatternBase f vn) SrcLoc
                      | Id vn (f PatternType) SrcLoc
                      | Wildcard (f PatternType) SrcLoc -- Nothing, i.e. underscore.
                      | PatternAscription (PatternBase f vn) (TypeDeclBase f vn) SrcLoc
                      | PatternLit (ExpBase f vn) (f PatternType) SrcLoc
                      | PatternConstr Name (f PatternType) [PatternBase f vn] SrcLoc
deriving instance Showable f vn => Show (PatternBase f vn)
deriving instance Eq (PatternBase NoInfo VName)
deriving instance Ord (PatternBase NoInfo VName)

instance Located (PatternBase f vn) where
  locOf (TuplePattern _ loc)        = locOf loc
  locOf (RecordPattern _ loc)       = locOf loc
  locOf (PatternParens _ loc)       = locOf loc
  locOf (Id _ _ loc)                = locOf loc
  locOf (Wildcard _ loc)            = locOf loc
  locOf (PatternAscription _ _ loc) = locOf loc
  locOf (PatternLit _ _ loc)        = locOf loc
  locOf (PatternConstr _ _ _ loc)   = locOf loc

-- | Documentation strings, including source location.
data DocComment = DocComment String SrcLoc
  deriving (Show)

instance Located DocComment where
  locOf (DocComment _ loc) = locOf loc

-- | Function Declarations
data ValBindBase f vn = ValBind { valBindEntryPoint :: Maybe (f StructType)
                                -- ^ True if this function is an entry
                                -- point.  If so, it also contains the
                                -- externally visible type.  Note that
                                -- this may not strictly be well-typed
                                -- after some desugaring operations,
                                -- as it may refer to abstract types
                                -- that are no longer in scope.
                                , valBindName       :: vn
                                , valBindRetDecl    :: Maybe (TypeExp vn)
                                , valBindRetType    :: f (StructType, [VName])
                                , valBindTypeParams :: [TypeParamBase vn]
                                , valBindParams     :: [PatternBase f vn]
                                , valBindBody       :: ExpBase f vn
                                , valBindDoc        :: Maybe DocComment
                                , valBindLocation   :: SrcLoc
                                }
deriving instance Showable f vn => Show (ValBindBase f vn)

instance Located (ValBindBase f vn) where
  locOf = locOf . valBindLocation

-- | Type Declarations
data TypeBindBase f vn = TypeBind { typeAlias        :: vn
                                  , typeLiftedness   :: Liftedness
                                  , typeParams       :: [TypeParamBase vn]
                                  , typeExp          :: TypeDeclBase f vn
                                  , typeDoc          :: Maybe DocComment
                                  , typeBindLocation :: SrcLoc
                                  }
deriving instance Showable f vn => Show (TypeBindBase f vn)

instance Located (TypeBindBase f vn) where
  locOf = locOf . typeBindLocation

-- | The liftedness of a type parameter.  By the @Ord@ instance,
-- @Unlifted < SizeLifted < Lifted@.
data Liftedness
  = Unlifted
    -- ^ May only be instantiated with a zero-order type of (possibly
    -- symbolically) known size.
  | SizeLifted
    -- ^ May only be instantiated with a zero-order type, but the size
    -- can be varying.
  | Lifted
    -- ^ May be instantiated with a functional type.
  deriving (Eq, Ord, Show)

data TypeParamBase vn = TypeParamDim vn SrcLoc
                        -- ^ A type parameter that must be a size.
                      | TypeParamType Liftedness vn SrcLoc
                        -- ^ A type parameter that must be a type.
  deriving (Eq, Ord, Show)

instance Functor TypeParamBase where
  fmap = fmapDefault

instance Foldable TypeParamBase where
  foldMap = foldMapDefault

instance Traversable TypeParamBase where
  traverse f (TypeParamDim v loc) = TypeParamDim <$> f v <*> pure loc
  traverse f (TypeParamType l v loc) = TypeParamType l <$> f v <*> pure loc

instance Located (TypeParamBase vn) where
  locOf (TypeParamDim _ loc)    = locOf loc
  locOf (TypeParamType _ _ loc) = locOf loc

typeParamName :: TypeParamBase vn -> vn
typeParamName (TypeParamDim v _)    = v
typeParamName (TypeParamType _ v _) = v

data SpecBase f vn = ValSpec  { specName       :: vn
                              , specTypeParams :: [TypeParamBase vn]
                              , specType       :: TypeDeclBase f vn
                              , specDoc        :: Maybe DocComment
                              , specLocation   :: SrcLoc
                              }
                   | TypeAbbrSpec (TypeBindBase f vn)
                   | TypeSpec Liftedness vn [TypeParamBase vn] (Maybe DocComment) SrcLoc -- ^ Abstract type.
                   | ModSpec vn (SigExpBase f vn) (Maybe DocComment) SrcLoc
                   | IncludeSpec (SigExpBase f vn) SrcLoc
deriving instance Showable f vn => Show (SpecBase f vn)

instance Located (SpecBase f vn) where
  locOf (ValSpec _ _ _ _ loc)  = locOf loc
  locOf (TypeAbbrSpec tbind)   = locOf tbind
  locOf (TypeSpec _ _ _ _ loc) = locOf loc
  locOf (ModSpec _ _ _ loc)    = locOf loc
  locOf (IncludeSpec _ loc)    = locOf loc

data SigExpBase f vn = SigVar (QualName vn) (f (M.Map VName VName)) SrcLoc
                     | SigParens (SigExpBase f vn) SrcLoc
                     | SigSpecs [SpecBase f vn] SrcLoc
                     | SigWith (SigExpBase f vn) (TypeRefBase f vn) SrcLoc
                     | SigArrow (Maybe vn) (SigExpBase f vn) (SigExpBase f vn) SrcLoc
deriving instance Showable f vn => Show (SigExpBase f vn)

-- | A type refinement.
data TypeRefBase f vn = TypeRef (QualName vn) [TypeParamBase vn] (TypeDeclBase f vn) SrcLoc
deriving instance Showable f vn => Show (TypeRefBase f vn)

instance Located (TypeRefBase f vn) where
  locOf (TypeRef _ _ _ loc) = locOf loc

instance Located (SigExpBase f vn) where
  locOf (SigVar _ _ loc)     = locOf loc
  locOf (SigParens _ loc)    = locOf loc
  locOf (SigSpecs _ loc)     = locOf loc
  locOf (SigWith _ _ loc)    = locOf loc
  locOf (SigArrow _ _ _ loc) = locOf loc

data SigBindBase f vn = SigBind { sigName :: vn
                                , sigExp  :: SigExpBase f vn
                                , sigDoc  :: Maybe DocComment
                                , sigLoc  :: SrcLoc
                                }
deriving instance Showable f vn => Show (SigBindBase f vn)

instance Located (SigBindBase f vn) where
  locOf = locOf . sigLoc

data ModExpBase f vn = ModVar (QualName vn) SrcLoc
                     | ModParens (ModExpBase f vn) SrcLoc
                     | ModImport FilePath (f FilePath) SrcLoc
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
  locOf (ModImport _ _ loc)    = locOf loc
  locOf (ModDecs _ loc)        = locOf loc
  locOf (ModApply _ _ _ _ loc) = locOf loc
  locOf (ModAscript _ _ _ loc) = locOf loc
  locOf (ModLambda _ _ _ loc)  = locOf loc

data ModBindBase f vn =
  ModBind { modName      :: vn
          , modParams    :: [ModParamBase f vn]
          , modSignature :: Maybe (SigExpBase f vn, f (M.Map VName VName))
          , modExp       :: ModExpBase f vn
          , modDoc       :: Maybe DocComment
          , modLocation  :: SrcLoc
          }
deriving instance Showable f vn => Show (ModBindBase f vn)

instance Located (ModBindBase f vn) where
  locOf = locOf . modLocation

data ModParamBase f vn = ModParam { modParamName     :: vn
                                  , modParamType     :: SigExpBase f vn
                                  , modParamAbs      :: f [VName]
                                  , modParamLocation :: SrcLoc
                                  }
deriving instance Showable f vn => Show (ModParamBase f vn)

instance Located (ModParamBase f vn) where
  locOf = locOf . modParamLocation

-- | A top-level binding.
data DecBase f vn = ValDec (ValBindBase f vn)
                  | TypeDec (TypeBindBase f vn)
                  | SigDec (SigBindBase f vn)
                  | ModDec (ModBindBase f vn)
                  | OpenDec (ModExpBase f vn) SrcLoc
                  | LocalDec (DecBase f vn) SrcLoc
                  | ImportDec FilePath (f FilePath) SrcLoc
deriving instance Showable f vn => Show (DecBase f vn)

instance Located (DecBase f vn) where
  locOf (ValDec d)          = locOf d
  locOf (TypeDec d)         = locOf d
  locOf (SigDec d)          = locOf d
  locOf (ModDec d)          = locOf d
  locOf (OpenDec _ loc)     = locOf loc
  locOf (LocalDec _ loc)    = locOf loc
  locOf (ImportDec _ _ loc) = locOf loc

-- | The program described by a single Futhark file.  May depend on
-- other files.
data ProgBase f vn = Prog { progDoc :: Maybe DocComment
                          , progDecs :: [DecBase f vn]
                          }
deriving instance Showable f vn => Show (ProgBase f vn)

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
  ppr Backtick  = text "``"
  ppr Plus      = text "+"
  ppr Minus     = text "-"
  ppr Pow       = text "**"
  ppr Times     = text "*"
  ppr Divide    = text "/"
  ppr Mod       = text "%"
  ppr Quot      = text "//"
  ppr Rem       = text "%%"
  ppr ShiftR    = text ">>"
  ppr ShiftL    = text "<<"
  ppr Band      = text "&"
  ppr Xor       = text "^"
  ppr Bor       = text "|"
  ppr LogAnd    = text "&&"
  ppr LogOr     = text "||"
  ppr Equal     = text "=="
  ppr NotEqual  = text "!="
  ppr Less      = text "<"
  ppr Leq       = text "<="
  ppr Greater   = text ">"
  ppr Geq       = text ">="
  ppr PipeLeft  = text "<|"
  ppr PipeRight = text "|>"
