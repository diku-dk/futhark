{-# LANGUAGE Strict #-}

-- | The Futhark source language AST definition.  Many types, such as
-- 'ExpBase', are parametrised by type and name representation.
-- E.g. in a value of type @ExpBase f vn@, annotations are wrapped in
-- the functor @f@, and all names are of type @vn@.  See
-- https://futhark.readthedocs.org for a language reference, or this
-- module may be a little hard to understand.
--
-- The system of primitive types is interesting in itself.  See
-- "Language.Futhark.Primitive".
module Language.Futhark.Syntax
  ( module Language.Futhark.Core,
    prettyString,
    prettyText,

    -- * Types
    Uniqueness (..),
    IntType (..),
    FloatType (..),
    PrimType (..),
    Size (..),
    Shape (..),
    shapeRank,
    stripDims,
    TypeBase (..),
    TypeArg (..),
    SizeExp (..),
    TypeExp (..),
    TypeArgExp (..),
    PName (..),
    ScalarTypeBase (..),
    RetTypeBase (..),
    PatType,
    StructType,
    StructRetType,
    PatRetType,
    ValueType,
    Diet (..),

    -- * Values
    IntValue (..),
    FloatValue (..),
    PrimValue (..),
    IsPrimValue (..),

    -- * Abstract syntax tree
    AttrInfo (..),
    AttrAtom (..),
    BinOp (..),
    IdentBase (..),
    Inclusiveness (..),
    DimIndexBase (..),
    SliceBase,
    SizeBinder (..),
    AppExpBase (..),
    AppRes (..),
    ExpBase (..),
    FieldBase (..),
    CaseBase (..),
    LoopFormBase (..),
    PatLit (..),
    PatBase (..),

    -- * Module language
    ImportName (..),
    SpecBase (..),
    SigExpBase (..),
    TypeRefBase (..),
    SigBindBase (..),
    ModExpBase (..),
    ModBindBase (..),
    ModParamBase (..),

    -- * Definitions
    DocComment (..),
    ValBindBase (..),
    EntryPoint (..),
    EntryType (..),
    EntryParam (..),
    Liftedness (..),
    TypeBindBase (..),
    TypeParamBase (..),
    typeParamName,
    ProgBase (..),
    DecBase (..),

    -- * Miscellaneous
    NoInfo (..),
    Info (..),
    Alias (..),
    Aliasing,
    QualName (..),
    mkApply,
    mkApplyUT,
  )
where

import Control.Applicative
import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Monoid hiding (Sum)
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable
import Futhark.Util.Loc
import Futhark.Util.Pretty
import Language.Futhark.Core
import Language.Futhark.Primitive
  ( FloatType (..),
    FloatValue (..),
    IntType (..),
    IntValue (..),
  )
import System.FilePath.Posix qualified as Posix
import Prelude

-- | No information functor.  Usually used for placeholder type- or
-- aliasing information.
data NoInfo a = NoInfo
  deriving (Eq, Ord, Show)

instance Functor NoInfo where
  fmap _ NoInfo = NoInfo

instance Foldable NoInfo where
  foldr _ b NoInfo = b

instance Traversable NoInfo where
  traverse _ NoInfo = pure NoInfo

-- | Some information.  The dual to 'NoInfo'
newtype Info a = Info {unInfo :: a}
  deriving (Eq, Ord, Show)

instance Functor Info where
  fmap f (Info x) = Info $ f x

instance Foldable Info where
  foldr f b (Info x) = f x b

instance Traversable Info where
  traverse f (Info x) = Info <$> f x

-- | Low-level primitive types.
data PrimType
  = Signed IntType
  | Unsigned IntType
  | FloatType FloatType
  | Bool
  deriving (Eq, Ord, Show)

-- | Non-array values.
data PrimValue
  = SignedValue !IntValue
  | UnsignedValue !IntValue
  | FloatValue !FloatValue
  | BoolValue !Bool
  deriving (Eq, Ord, Show)

-- | A class for converting ordinary Haskell values to primitive
-- Futhark values.
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

-- | The value of an v'AttrAtom'.
data AttrAtom vn
  = AtomName Name
  | AtomInt Integer
  deriving (Eq, Ord, Show)

-- | The payload of an attribute.
data AttrInfo vn
  = AttrAtom (AttrAtom vn) SrcLoc
  | AttrComp Name [AttrInfo vn] SrcLoc
  deriving (Eq, Ord, Show)

-- | The elaborated size of a dimension.
data Size
  = -- | The size of the dimension is this name, which
    -- must be in scope.  In a return type, this will
    -- give rise to an assertion.
    NamedSize (QualName VName)
  | -- | The size is a constant.
    ConstSize Int64
  | -- | No known size.  If @Nothing@, then this is a name distinct
    -- from any other.  The type checker should _never_ produce these
    -- - they are a (hopefully temporary) thing introduced by
    -- defunctorisation and monomorphisation.
    AnySize (Maybe VName)
  deriving (Eq, Ord, Show)

-- | The size of an array type is a list of its dimension sizes.  If
-- 'Nothing', that dimension is of a (statically) unknown size.
newtype Shape dim = Shape {shapeDims :: [dim]}
  deriving (Eq, Ord, Show)

instance Foldable Shape where
  foldr f x (Shape ds) = foldr f x ds

instance Traversable Shape where
  traverse f (Shape ds) = Shape <$> traverse f ds

instance Functor Shape where
  fmap f (Shape ds) = Shape $ map f ds

instance Semigroup (Shape dim) where
  Shape l1 <> Shape l2 = Shape $ l1 ++ l2

instance Monoid (Shape dim) where
  mempty = Shape []

-- | The number of dimensions contained in a shape.
shapeRank :: Shape dim -> Int
shapeRank = length . shapeDims

-- | @stripDims n shape@ strips the outer @n@ dimensions from
-- @shape@, returning 'Nothing' if this would result in zero or
-- fewer dimensions.
stripDims :: Int -> Shape dim -> Maybe (Shape dim)
stripDims i (Shape l)
  | i < length l = Just $ Shape $ drop i l
  | otherwise = Nothing

-- | The name (if any) of a function parameter.  The 'Eq' and 'Ord'
-- instances always compare values of this type equal.
data PName = Named VName | Unnamed
  deriving (Show)

instance Eq PName where
  _ == _ = True

instance Ord PName where
  _ <= _ = True

-- | Types that can appear to the right of a function arrow.  This
-- just means they can be existentially quantified.
data RetTypeBase dim as = RetType
  { retDims :: [VName],
    retType :: TypeBase dim as
  }
  deriving (Eq, Ord, Show)

instance Bitraversable RetTypeBase where
  bitraverse f g (RetType dims t) = RetType dims <$> bitraverse f g t

instance Functor (RetTypeBase dim) where
  fmap = second

instance Bifunctor RetTypeBase where
  bimap = bimapDefault

instance Bifoldable RetTypeBase where
  bifoldMap = bifoldMapDefault

-- | Types that can be elements of arrays.  This representation does
-- allow arrays of records of functions, which is nonsensical, but it
-- convolutes the code too much if we try to statically rule it out.
data ScalarTypeBase dim as
  = Prim PrimType
  | TypeVar as Uniqueness (QualName VName) [TypeArg dim]
  | Record (M.Map Name (TypeBase dim as))
  | Sum (M.Map Name [TypeBase dim as])
  | -- | The aliasing corresponds to the lexical
    -- closure of the function.
    Arrow as PName Diet (TypeBase dim ()) (RetTypeBase dim as)
  deriving (Eq, Ord, Show)

instance Bitraversable ScalarTypeBase where
  bitraverse _ _ (Prim t) = pure $ Prim t
  bitraverse f g (Record fs) = Record <$> traverse (bitraverse f g) fs
  bitraverse f g (TypeVar als u t args) =
    TypeVar <$> g als <*> pure u <*> pure t <*> traverse (traverse f) args
  bitraverse f g (Arrow als v d t1 t2) =
    Arrow <$> g als <*> pure v <*> pure d <*> bitraverse f pure t1 <*> bitraverse f g t2
  bitraverse f g (Sum cs) = Sum <$> (traverse . traverse) (bitraverse f g) cs

instance Functor (ScalarTypeBase dim) where
  fmap = second

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
  | Array as Uniqueness (Shape dim) (ScalarTypeBase dim ())
  deriving (Eq, Ord, Show)

instance Bitraversable TypeBase where
  bitraverse f g (Scalar t) = Scalar <$> bitraverse f g t
  bitraverse f g (Array a u shape t) =
    Array <$> g a <*> pure u <*> traverse f shape <*> bitraverse f pure t

instance Functor (TypeBase dim) where
  fmap = second

instance Bifunctor TypeBase where
  bimap = bimapDefault

instance Bifoldable TypeBase where
  bifoldMap = bifoldMapDefault

-- | An argument passed to a type constructor.
data TypeArg dim
  = TypeArgDim dim SrcLoc
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
data Alias
  = AliasBound {aliasVar :: VName}
  | AliasFree {aliasVar :: VName}
  deriving (Eq, Ord, Show)

-- | Aliasing for a type, which is a set of the variables that are
-- aliased.
type Aliasing = S.Set Alias

-- | A type with aliasing information and shape annotations, used for
-- describing the type patterns and expressions.
type PatType = TypeBase Size Aliasing

-- | A "structural" type with shape annotations and no aliasing
-- information, used for declarations.
type StructType = TypeBase Size ()

-- | A value type contains full, manifest size information.
type ValueType = TypeBase Int64 ()

-- | The return type version of 'StructType'.
type StructRetType = RetTypeBase Size ()

-- | The return type version of 'PatType'.
type PatRetType = RetTypeBase Size Aliasing

-- | A dimension declaration expression for use in a 'TypeExp'.
-- Syntactically includes the brackets.
data SizeExp f vn
  = -- | The size of the dimension is this expression, all of which
    -- free variables must be in scope.
    SizeExp (ExpBase f vn) SrcLoc
  | -- | No dimension declaration.
    SizeExpAny SrcLoc

instance Located (SizeExp f vn) where
  locOf (SizeExp _ loc) = locOf loc
  locOf (SizeExpAny loc) = locOf loc

deriving instance Show (SizeExp Info VName)

deriving instance Show vn => Show (SizeExp NoInfo vn)

deriving instance Eq (SizeExp NoInfo VName)

deriving instance Eq (SizeExp Info VName)

deriving instance Ord (SizeExp NoInfo VName)

deriving instance Ord (SizeExp Info VName)

-- | An unstructured syntactic type with type variables and possibly
-- shape declarations - this is what the user types in the source
-- program.  These are used to construct 'TypeBase's in the type
-- checker.
data TypeExp f vn
  = TEVar (QualName vn) SrcLoc
  | TEParens (TypeExp f vn) SrcLoc
  | TETuple [TypeExp f vn] SrcLoc
  | TERecord [(Name, TypeExp f vn)] SrcLoc
  | TEArray (SizeExp f vn) (TypeExp f vn) SrcLoc
  | TEUnique (TypeExp f vn) SrcLoc
  | TEApply (TypeExp f vn) (TypeArgExp f vn) SrcLoc
  | TEArrow (Maybe vn) (TypeExp f vn) (TypeExp f vn) SrcLoc
  | TESum [(Name, [TypeExp f vn])] SrcLoc
  | TEDim [vn] (TypeExp f vn) SrcLoc

deriving instance Show (TypeExp Info VName)

deriving instance Show vn => Show (TypeExp NoInfo vn)

deriving instance Eq (TypeExp NoInfo VName)

deriving instance Eq (TypeExp Info VName)

deriving instance Ord (TypeExp NoInfo VName)

deriving instance Ord (TypeExp Info VName)

instance Located (TypeExp f vn) where
  locOf (TEArray _ _ loc) = locOf loc
  locOf (TETuple _ loc) = locOf loc
  locOf (TERecord _ loc) = locOf loc
  locOf (TEVar _ loc) = locOf loc
  locOf (TEParens _ loc) = locOf loc
  locOf (TEUnique _ loc) = locOf loc
  locOf (TEApply _ _ loc) = locOf loc
  locOf (TEArrow _ _ _ loc) = locOf loc
  locOf (TESum _ loc) = locOf loc
  locOf (TEDim _ _ loc) = locOf loc

-- | A type argument expression passed to a type constructor.
data TypeArgExp f vn
  = TypeArgExpSize (SizeExp f vn)
  | TypeArgExpType (TypeExp f vn)

deriving instance Show (TypeArgExp Info VName)

deriving instance Show vn => Show (TypeArgExp NoInfo vn)

deriving instance Eq (TypeArgExp NoInfo VName)

deriving instance Eq (TypeArgExp Info VName)

deriving instance Ord (TypeArgExp NoInfo VName)

deriving instance Ord (TypeArgExp Info VName)

instance Located (TypeArgExp f vn) where
  locOf (TypeArgExpSize e) = locOf e
  locOf (TypeArgExpType t) = locOf t

-- | Information about which parts of a parameter are consumed.  This
-- can be considered kind of an effect on the function.
data Diet
  = -- | Does not consume the parameter.
    Observe
  | -- | Consumes the parameter.
    Consume
  deriving (Eq, Ord, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data IdentBase f vn = Ident
  { identName :: vn,
    identType :: f PatType,
    identSrcLoc :: SrcLoc
  }

deriving instance Show (IdentBase Info VName)

deriving instance Show vn => Show (IdentBase NoInfo vn)

instance Eq vn => Eq (IdentBase ty vn) where
  x == y = identName x == identName y

instance Ord vn => Ord (IdentBase ty vn) where
  compare = comparing identName

instance Located (IdentBase ty vn) where
  locOf = locOf . identSrcLoc

-- | Default binary operators.
data BinOp
  = -- | A pseudo-operator standing in for any normal
    -- identifier used as an operator (they all have the
    -- same fixity).
    Backtick
  | -- | Not a real operator, but operator with this as a prefix may
    -- be defined by the user.
    Bang
  | -- | Not a real operator, but operator with this as a prefix
    -- may be defined by the user.
    Equ
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
  | -- Relational Ops for all primitive types at least
    Equal
  | NotEqual
  | Less
  | Leq
  | Greater
  | Geq
  | -- Some functional ops.

    -- | @|>@
    PipeRight
  | -- | @<|@
    -- Misc
    PipeLeft
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Whether a bound for an end-point of a 'DimSlice' or a range
-- literal is inclusive or exclusive.
data Inclusiveness a
  = DownToExclusive a
  | -- | May be "down to" if step is negative.
    ToInclusive a
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
data DimIndexBase f vn
  = DimFix (ExpBase f vn)
  | DimSlice
      (Maybe (ExpBase f vn))
      (Maybe (ExpBase f vn))
      (Maybe (ExpBase f vn))

deriving instance Show (DimIndexBase Info VName)

deriving instance Show vn => Show (DimIndexBase NoInfo vn)

deriving instance Eq (DimIndexBase NoInfo VName)

deriving instance Eq (DimIndexBase Info VName)

deriving instance Ord (DimIndexBase NoInfo VName)

deriving instance Ord (DimIndexBase Info VName)

-- | A slicing of an array (potentially multiple dimensions).
type SliceBase f vn = [DimIndexBase f vn]

-- | A name qualified with a breadcrumb of module accesses.
data QualName vn = QualName
  { qualQuals :: ![vn],
    qualLeaf :: !vn
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

-- | A binding of a size in a pattern (essentially a size parameter in
-- a @let@ expression).
data SizeBinder vn = SizeBinder {sizeName :: !vn, sizeLoc :: !SrcLoc}
  deriving (Eq, Ord, Show)

instance Located (SizeBinder vn) where
  locOf = locOf . sizeLoc

-- | An "application expression" is a semantic (not syntactic)
-- grouping of expressions that have "funcall-like" semantics, mostly
-- meaning that they can return existential sizes.  In our type
-- theory, these are all thought to be bound to names (*Administrative
-- Normal Form*), but as this is not practical in a real language, we
-- instead use an annotation ('AppRes') that stores the information we
-- need, so we can pretend that an application expression was really
-- bound to a name.
data AppExpBase f vn
  = -- | Function application.  Parts of the compiler expects that the
    -- function expression is never itself an 'Apply'.  Use the
    -- 'mkApply' function to maintain this invariant, rather than
    -- constructing 'Apply' directly.
    --
    -- The @Maybe VNames@ are existential sizes generated by this
    -- argumnet.  May have duplicates across the program, but they
    -- will all produce the same value (the expressions will be
    -- identical).
    Apply
      (ExpBase f vn)
      (NE.NonEmpty (f (Diet, Maybe VName), ExpBase f vn))
      SrcLoc
  | -- | Size coercion: @e :> t@.
    Coerce (ExpBase f vn) (TypeExp f vn) SrcLoc
  | Range
      (ExpBase f vn)
      (Maybe (ExpBase f vn))
      (Inclusiveness (ExpBase f vn))
      SrcLoc
  | LetPat
      [SizeBinder vn]
      (PatBase f vn)
      (ExpBase f vn)
      (ExpBase f vn)
      SrcLoc
  | LetFun
      vn
      ( [TypeParamBase vn],
        [PatBase f vn],
        Maybe (TypeExp f vn),
        f StructRetType,
        ExpBase f vn
      )
      (ExpBase f vn)
      SrcLoc
  | If (ExpBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc
  | DoLoop
      [VName] -- Size parameters.
      (PatBase f vn) -- Merge variable pattern.
      (ExpBase f vn) -- Initial values of merge variables.
      (LoopFormBase f vn) -- Do or while loop.
      (ExpBase f vn) -- Loop body.
      SrcLoc
  | BinOp
      (QualName vn, SrcLoc)
      (f PatType)
      (ExpBase f vn, f (StructType, Maybe VName))
      (ExpBase f vn, f (StructType, Maybe VName))
      SrcLoc
  | LetWith
      (IdentBase f vn)
      (IdentBase f vn)
      (SliceBase f vn)
      (ExpBase f vn)
      (ExpBase f vn)
      SrcLoc
  | Index (ExpBase f vn) (SliceBase f vn) SrcLoc
  | -- | A match expression.
    Match (ExpBase f vn) (NE.NonEmpty (CaseBase f vn)) SrcLoc

deriving instance Show (AppExpBase Info VName)

deriving instance Show vn => Show (AppExpBase NoInfo vn)

deriving instance Eq (AppExpBase NoInfo VName)

deriving instance Eq (AppExpBase Info VName)

deriving instance Ord (AppExpBase NoInfo VName)

deriving instance Ord (AppExpBase Info VName)

instance Located (AppExpBase f vn) where
  locOf (Range _ _ _ pos) = locOf pos
  locOf (BinOp _ _ _ _ loc) = locOf loc
  locOf (If _ _ _ loc) = locOf loc
  locOf (Coerce _ _ loc) = locOf loc
  locOf (Apply _ _ loc) = locOf loc
  locOf (LetPat _ _ _ _ loc) = locOf loc
  locOf (LetFun _ _ _ loc) = locOf loc
  locOf (LetWith _ _ _ _ _ loc) = locOf loc
  locOf (Index _ _ loc) = locOf loc
  locOf (DoLoop _ _ _ _ _ loc) = locOf loc
  locOf (Match _ _ loc) = locOf loc

-- | An annotation inserted by the type checker on constructs that are
-- "function calls" (either literally or conceptually).  This
-- annotation encodes the result type, as well as any existential
-- sizes that are generated here.
data AppRes = AppRes
  { appResType :: PatType,
    appResExt :: [VName]
  }
  deriving (Eq, Ord, Show)

-- | The Futhark expression language.
--
-- This allows us to encode whether or not the expression has been
-- type-checked in the Haskell type of the expression.  Specifically,
-- the parser will produce expressions of type @Exp 'NoInfo' 'Name'@,
-- and the type checker will convert these to @Exp 'Info' 'VName'@, in
-- which type information is always present and all names are unique.
data ExpBase f vn
  = Literal PrimValue SrcLoc
  | -- | A polymorphic integral literal.
    IntLit Integer (f PatType) SrcLoc
  | -- | A polymorphic decimal literal.
    FloatLit Double (f PatType) SrcLoc
  | -- | A string literal is just a fancy syntax for an array
    -- of bytes.
    StringLit [Word8] SrcLoc
  | Hole (f PatType) SrcLoc
  | Var (QualName vn) (f PatType) SrcLoc
  | -- | A parenthesized expression.
    Parens (ExpBase f vn) SrcLoc
  | QualParens (QualName vn, SrcLoc) (ExpBase f vn) SrcLoc
  | -- | Tuple literals, e.g., @{1+3, {x, y+z}}@.
    TupLit [ExpBase f vn] SrcLoc
  | -- | Record literals, e.g. @{x=2,y=3,z}@.
    RecordLit [FieldBase f vn] SrcLoc
  | -- | Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
    -- Second arg is the row type of the rows of the array.
    ArrayLit [ExpBase f vn] (f PatType) SrcLoc
  | -- | An attribute applied to the following expression.
    Attr (AttrInfo vn) (ExpBase f vn) SrcLoc
  | Project Name (ExpBase f vn) (f PatType) SrcLoc
  | -- | Numeric negation (ugly special case; Haskell did it first).
    Negate (ExpBase f vn) SrcLoc
  | -- | Logical and bitwise negation.
    Not (ExpBase f vn) SrcLoc
  | -- | Fail if the first expression does not return true,
    -- and return the value of the second expression if it
    -- does.
    Assert (ExpBase f vn) (ExpBase f vn) (f T.Text) SrcLoc
  | -- | An n-ary value constructor.
    Constr Name [ExpBase f vn] (f PatType) SrcLoc
  | Update (ExpBase f vn) (SliceBase f vn) (ExpBase f vn) SrcLoc
  | RecordUpdate (ExpBase f vn) [Name] (ExpBase f vn) (f PatType) SrcLoc
  | Lambda
      [PatBase f vn]
      (ExpBase f vn)
      (Maybe (TypeExp f vn))
      (f (Aliasing, StructRetType))
      SrcLoc
  | -- | @+@; first two types are operands, third is result.
    OpSection (QualName vn) (f PatType) SrcLoc
  | -- | @2+@; first type is operand, second is result.
    OpSectionLeft
      (QualName vn)
      (f PatType)
      (ExpBase f vn)
      (f (PName, StructType, Maybe VName), f (PName, StructType))
      (f PatRetType, f [VName])
      SrcLoc
  | -- | @+2@; first type is operand, second is result.
    OpSectionRight
      (QualName vn)
      (f PatType)
      (ExpBase f vn)
      (f (PName, StructType), f (PName, StructType, Maybe VName))
      (f PatRetType)
      SrcLoc
  | -- | Field projection as a section: @(.x.y.z)@.
    ProjectSection [Name] (f PatType) SrcLoc
  | -- | Array indexing as a section: @(.[i,j])@.
    IndexSection (SliceBase f vn) (f PatType) SrcLoc
  | -- | Type ascription: @e : t@.
    Ascript (ExpBase f vn) (TypeExp f vn) SrcLoc
  | AppExp (AppExpBase f vn) (f AppRes)

deriving instance Show (ExpBase Info VName)

deriving instance Show vn => Show (ExpBase NoInfo vn)

deriving instance Eq (ExpBase NoInfo VName)

deriving instance Ord (ExpBase NoInfo VName)

deriving instance Eq (ExpBase Info VName)

deriving instance Ord (ExpBase Info VName)

instance Located (ExpBase f vn) where
  locOf (Literal _ loc) = locOf loc
  locOf (IntLit _ _ loc) = locOf loc
  locOf (FloatLit _ _ loc) = locOf loc
  locOf (Parens _ loc) = locOf loc
  locOf (QualParens _ _ loc) = locOf loc
  locOf (TupLit _ pos) = locOf pos
  locOf (RecordLit _ pos) = locOf pos
  locOf (Project _ _ _ pos) = locOf pos
  locOf (ArrayLit _ _ pos) = locOf pos
  locOf (StringLit _ loc) = locOf loc
  locOf (Var _ _ loc) = locOf loc
  locOf (Ascript _ _ loc) = locOf loc
  locOf (Negate _ pos) = locOf pos
  locOf (Not _ pos) = locOf pos
  locOf (Update _ _ _ pos) = locOf pos
  locOf (RecordUpdate _ _ _ _ pos) = locOf pos
  locOf (Lambda _ _ _ _ loc) = locOf loc
  locOf (Hole _ loc) = locOf loc
  locOf (OpSection _ _ loc) = locOf loc
  locOf (OpSectionLeft _ _ _ _ _ loc) = locOf loc
  locOf (OpSectionRight _ _ _ _ _ loc) = locOf loc
  locOf (ProjectSection _ _ loc) = locOf loc
  locOf (IndexSection _ _ loc) = locOf loc
  locOf (Assert _ _ _ loc) = locOf loc
  locOf (Constr _ _ _ loc) = locOf loc
  locOf (Attr _ _ loc) = locOf loc
  locOf (AppExp e _) = locOf e

-- | An entry in a record literal.
data FieldBase f vn
  = RecordFieldExplicit Name (ExpBase f vn) SrcLoc
  | RecordFieldImplicit vn (f PatType) SrcLoc

deriving instance Show (FieldBase Info VName)

deriving instance Show vn => Show (FieldBase NoInfo vn)

deriving instance Eq (FieldBase NoInfo VName)

deriving instance Eq (FieldBase Info VName)

deriving instance Ord (FieldBase NoInfo VName)

deriving instance Ord (FieldBase Info VName)

instance Located (FieldBase f vn) where
  locOf (RecordFieldExplicit _ _ loc) = locOf loc
  locOf (RecordFieldImplicit _ _ loc) = locOf loc

-- | A case in a match expression.
data CaseBase f vn = CasePat (PatBase f vn) (ExpBase f vn) SrcLoc

deriving instance Show (CaseBase Info VName)

deriving instance Show vn => Show (CaseBase NoInfo vn)

deriving instance Eq (CaseBase NoInfo VName)

deriving instance Eq (CaseBase Info VName)

deriving instance Ord (CaseBase NoInfo VName)

deriving instance Ord (CaseBase Info VName)

instance Located (CaseBase f vn) where
  locOf (CasePat _ _ loc) = locOf loc

-- | Whether the loop is a @for@-loop or a @while@-loop.
data LoopFormBase f vn
  = For (IdentBase f vn) (ExpBase f vn)
  | ForIn (PatBase f vn) (ExpBase f vn)
  | While (ExpBase f vn)

deriving instance Show (LoopFormBase Info VName)

deriving instance Show vn => Show (LoopFormBase NoInfo vn)

deriving instance Eq (LoopFormBase NoInfo VName)

deriving instance Eq (LoopFormBase Info VName)

deriving instance Ord (LoopFormBase NoInfo VName)

deriving instance Ord (LoopFormBase Info VName)

-- | A literal in a pattern.
data PatLit
  = PatLitInt Integer
  | PatLitFloat Double
  | PatLitPrim PrimValue
  deriving (Eq, Ord, Show)

-- | A pattern as used most places where variables are bound (function
-- parameters, @let@ expressions, etc).
data PatBase f vn
  = TuplePat [PatBase f vn] SrcLoc
  | RecordPat [(Name, PatBase f vn)] SrcLoc
  | PatParens (PatBase f vn) SrcLoc
  | Id vn (f PatType) SrcLoc
  | Wildcard (f PatType) SrcLoc -- Nothing, i.e. underscore.
  | PatAscription (PatBase f vn) (TypeExp f vn) SrcLoc
  | PatLit PatLit (f PatType) SrcLoc
  | PatConstr Name (f PatType) [PatBase f vn] SrcLoc
  | PatAttr (AttrInfo vn) (PatBase f vn) SrcLoc

deriving instance Show (PatBase Info VName)

deriving instance Show vn => Show (PatBase NoInfo vn)

deriving instance Eq (PatBase NoInfo VName)

deriving instance Eq (PatBase Info VName)

deriving instance Ord (PatBase NoInfo VName)

deriving instance Ord (PatBase Info VName)

instance Located (PatBase f vn) where
  locOf (TuplePat _ loc) = locOf loc
  locOf (RecordPat _ loc) = locOf loc
  locOf (PatParens _ loc) = locOf loc
  locOf (Id _ _ loc) = locOf loc
  locOf (Wildcard _ loc) = locOf loc
  locOf (PatAscription _ _ loc) = locOf loc
  locOf (PatLit _ _ loc) = locOf loc
  locOf (PatConstr _ _ _ loc) = locOf loc
  locOf (PatAttr _ _ loc) = locOf loc

-- | Documentation strings, including source location.
data DocComment = DocComment String SrcLoc
  deriving (Show)

instance Located DocComment where
  locOf (DocComment _ loc) = locOf loc

-- | Part of the type of an entry point.  Has an actual type, and
-- maybe also an ascribed type expression.
data EntryType = EntryType
  { entryType :: StructType,
    entryAscribed :: Maybe (TypeExp Info VName)
  }
  deriving (Show)

-- | A parameter of an entry point.
data EntryParam = EntryParam
  { entryParamName :: Name,
    entryParamType :: EntryType
  }
  deriving (Show)

-- | Information about the external interface exposed by an entry
-- point.  The important thing is that that we remember the original
-- source-language types, without desugaring them at all.  The
-- annoying thing is that we do not require type annotations on entry
-- points, so the types can be either ascribed or inferred.
data EntryPoint = EntryPoint
  { entryParams :: [EntryParam],
    entryReturn :: EntryType
  }
  deriving (Show)

-- | Function Declarations
data ValBindBase f vn = ValBind
  { -- | Just if this function is an entry point.  If so, it also
    -- contains the externally visible interface.  Note that this may not
    -- strictly be well-typed after some desugaring operations, as it
    -- may refer to abstract types that are no longer in scope.
    valBindEntryPoint :: Maybe (f EntryPoint),
    valBindName :: vn,
    valBindRetDecl :: Maybe (TypeExp f vn),
    -- | If 'valBindParams' is null, then the 'retDims' are brought
    -- into scope at this point.
    valBindRetType :: f StructRetType,
    valBindTypeParams :: [TypeParamBase vn],
    valBindParams :: [PatBase f vn],
    valBindBody :: ExpBase f vn,
    valBindDoc :: Maybe DocComment,
    valBindAttrs :: [AttrInfo vn],
    valBindLocation :: SrcLoc
  }

deriving instance Show (ValBindBase Info VName)

deriving instance Show (ValBindBase NoInfo Name)

instance Located (ValBindBase f vn) where
  locOf = locOf . valBindLocation

-- | Type Declarations
data TypeBindBase f vn = TypeBind
  { typeAlias :: vn,
    typeLiftedness :: Liftedness,
    typeParams :: [TypeParamBase vn],
    typeExp :: TypeExp f vn,
    typeElab :: f StructRetType,
    typeDoc :: Maybe DocComment,
    typeBindLocation :: SrcLoc
  }

deriving instance Show (TypeBindBase Info VName)

deriving instance Show (TypeBindBase NoInfo Name)

instance Located (TypeBindBase f vn) where
  locOf = locOf . typeBindLocation

-- | The liftedness of a type parameter.  By the @Ord@ instance,
-- @Unlifted < SizeLifted < Lifted@.
data Liftedness
  = -- | May only be instantiated with a zero-order type of (possibly
    -- symbolically) known size.
    Unlifted
  | -- | May only be instantiated with a zero-order type, but the size
    -- can be varying.
    SizeLifted
  | -- | May be instantiated with a functional type.
    Lifted
  deriving (Eq, Ord, Show)

-- | A type parameter.
data TypeParamBase vn
  = -- | A type parameter that must be a size.
    TypeParamDim vn SrcLoc
  | -- | A type parameter that must be a type.
    TypeParamType Liftedness vn SrcLoc
  deriving (Eq, Ord, Show)

instance Functor TypeParamBase where
  fmap = fmapDefault

instance Foldable TypeParamBase where
  foldMap = foldMapDefault

instance Traversable TypeParamBase where
  traverse f (TypeParamDim v loc) = TypeParamDim <$> f v <*> pure loc
  traverse f (TypeParamType l v loc) = TypeParamType l <$> f v <*> pure loc

instance Located (TypeParamBase vn) where
  locOf (TypeParamDim _ loc) = locOf loc
  locOf (TypeParamType _ _ loc) = locOf loc

-- | The name of a type parameter.
typeParamName :: TypeParamBase vn -> vn
typeParamName (TypeParamDim v _) = v
typeParamName (TypeParamType _ v _) = v

-- | A spec is a component of a module type.
data SpecBase f vn
  = ValSpec
      { specName :: vn,
        specTypeParams :: [TypeParamBase vn],
        specTypeExp :: TypeExp f vn,
        specType :: f StructType,
        specDoc :: Maybe DocComment,
        specLocation :: SrcLoc
      }
  | TypeAbbrSpec (TypeBindBase f vn)
  | -- | Abstract type.
    TypeSpec Liftedness vn [TypeParamBase vn] (Maybe DocComment) SrcLoc
  | ModSpec vn (SigExpBase f vn) (Maybe DocComment) SrcLoc
  | IncludeSpec (SigExpBase f vn) SrcLoc

deriving instance Show (SpecBase Info VName)

deriving instance Show (SpecBase NoInfo Name)

instance Located (SpecBase f vn) where
  locOf (ValSpec _ _ _ _ _ loc) = locOf loc
  locOf (TypeAbbrSpec tbind) = locOf tbind
  locOf (TypeSpec _ _ _ _ loc) = locOf loc
  locOf (ModSpec _ _ _ loc) = locOf loc
  locOf (IncludeSpec _ loc) = locOf loc

-- | A module type expression.
data SigExpBase f vn
  = SigVar (QualName vn) (f (M.Map VName VName)) SrcLoc
  | SigParens (SigExpBase f vn) SrcLoc
  | SigSpecs [SpecBase f vn] SrcLoc
  | SigWith (SigExpBase f vn) (TypeRefBase f vn) SrcLoc
  | SigArrow (Maybe vn) (SigExpBase f vn) (SigExpBase f vn) SrcLoc

deriving instance Show (SigExpBase Info VName)

deriving instance Show (SigExpBase NoInfo Name)

-- | A type refinement.
data TypeRefBase f vn = TypeRef (QualName vn) [TypeParamBase vn] (TypeExp f vn) SrcLoc

deriving instance Show (TypeRefBase Info VName)

deriving instance Show (TypeRefBase NoInfo Name)

instance Located (TypeRefBase f vn) where
  locOf (TypeRef _ _ _ loc) = locOf loc

instance Located (SigExpBase f vn) where
  locOf (SigVar _ _ loc) = locOf loc
  locOf (SigParens _ loc) = locOf loc
  locOf (SigSpecs _ loc) = locOf loc
  locOf (SigWith _ _ loc) = locOf loc
  locOf (SigArrow _ _ _ loc) = locOf loc

-- | Module type binding.
data SigBindBase f vn = SigBind
  { sigName :: vn,
    sigExp :: SigExpBase f vn,
    sigDoc :: Maybe DocComment,
    sigLoc :: SrcLoc
  }

deriving instance Show (SigBindBase Info VName)

deriving instance Show (SigBindBase NoInfo Name)

instance Located (SigBindBase f vn) where
  locOf = locOf . sigLoc

-- | Canonical reference to a Futhark code file.  Does not include the
-- @.fut@ extension.  This is most often a path relative to the
-- working directory of the compiler.  In a multi-file program, a file
-- is known by exactly one import name, even if it is referenced
-- relatively by different names by files in different subdirectories.
newtype ImportName = ImportName Posix.FilePath
  deriving (Eq, Ord, Show)

-- | Module expression.
data ModExpBase f vn
  = ModVar (QualName vn) SrcLoc
  | ModParens (ModExpBase f vn) SrcLoc
  | -- | The contents of another file as a module.
    ModImport FilePath (f ImportName) SrcLoc
  | ModDecs [DecBase f vn] SrcLoc
  | -- | Functor application.  The first mapping is from parameter
    -- names to argument names, while the second maps names in the
    -- constructed module to the names inside the functor.
    ModApply
      (ModExpBase f vn)
      (ModExpBase f vn)
      (f (M.Map VName VName))
      (f (M.Map VName VName))
      SrcLoc
  | ModAscript (ModExpBase f vn) (SigExpBase f vn) (f (M.Map VName VName)) SrcLoc
  | ModLambda
      (ModParamBase f vn)
      (Maybe (SigExpBase f vn, f (M.Map VName VName)))
      (ModExpBase f vn)
      SrcLoc

deriving instance Show (ModExpBase Info VName)

deriving instance Show (ModExpBase NoInfo Name)

instance Located (ModExpBase f vn) where
  locOf (ModVar _ loc) = locOf loc
  locOf (ModParens _ loc) = locOf loc
  locOf (ModImport _ _ loc) = locOf loc
  locOf (ModDecs _ loc) = locOf loc
  locOf (ModApply _ _ _ _ loc) = locOf loc
  locOf (ModAscript _ _ _ loc) = locOf loc
  locOf (ModLambda _ _ _ loc) = locOf loc

-- | A module binding.
data ModBindBase f vn = ModBind
  { modName :: vn,
    modParams :: [ModParamBase f vn],
    modSignature :: Maybe (SigExpBase f vn, f (M.Map VName VName)),
    modExp :: ModExpBase f vn,
    modDoc :: Maybe DocComment,
    modLocation :: SrcLoc
  }

deriving instance Show (ModBindBase Info VName)

deriving instance Show (ModBindBase NoInfo Name)

instance Located (ModBindBase f vn) where
  locOf = locOf . modLocation

-- | A module parameter.
data ModParamBase f vn = ModParam
  { modParamName :: vn,
    modParamType :: SigExpBase f vn,
    modParamAbs :: f [VName],
    modParamLocation :: SrcLoc
  }

deriving instance Show (ModParamBase Info VName)

deriving instance Show (ModParamBase NoInfo Name)

instance Located (ModParamBase f vn) where
  locOf = locOf . modParamLocation

-- | A top-level binding.
data DecBase f vn
  = ValDec (ValBindBase f vn)
  | TypeDec (TypeBindBase f vn)
  | SigDec (SigBindBase f vn)
  | ModDec (ModBindBase f vn)
  | OpenDec (ModExpBase f vn) SrcLoc
  | LocalDec (DecBase f vn) SrcLoc
  | ImportDec FilePath (f ImportName) SrcLoc

deriving instance Show (DecBase Info VName)

deriving instance Show (DecBase NoInfo Name)

instance Located (DecBase f vn) where
  locOf (ValDec d) = locOf d
  locOf (TypeDec d) = locOf d
  locOf (SigDec d) = locOf d
  locOf (ModDec d) = locOf d
  locOf (OpenDec _ loc) = locOf loc
  locOf (LocalDec _ loc) = locOf loc
  locOf (ImportDec _ _ loc) = locOf loc

-- | The program described by a single Futhark file.  May depend on
-- other files.
data ProgBase f vn = Prog
  { progDoc :: Maybe DocComment,
    progDecs :: [DecBase f vn]
  }

deriving instance Show (ProgBase Info VName)

deriving instance Show (ProgBase NoInfo Name)

-- | Construct an 'Apply' node, with type information.
mkApply :: ExpBase Info vn -> [(Diet, Maybe VName, ExpBase Info vn)] -> AppRes -> ExpBase Info vn
mkApply f args (AppRes t ext)
  | Just args' <- NE.nonEmpty $ map onArg args =
      case f of
        (AppExp (Apply f' f_args loc) (Info (AppRes _ f_ext))) ->
          AppExp
            (Apply f' (f_args <> args') (srcspan loc $ snd $ NE.last args'))
            (Info $ AppRes t $ f_ext <> ext)
        _ ->
          AppExp (Apply f args' (srcspan f $ snd $ NE.last args')) (Info (AppRes t ext))
  | otherwise = f
  where
    onArg (d, v, x) = (Info (d, v), x)

-- | Construct an 'Apply' node, without type information.
mkApplyUT :: ExpBase NoInfo vn -> ExpBase NoInfo vn -> ExpBase NoInfo vn
mkApplyUT (AppExp (Apply f args loc) _) x =
  AppExp (Apply f (args <> NE.singleton (NoInfo, x)) (srcspan loc x)) NoInfo
mkApplyUT f x =
  AppExp (Apply f (NE.singleton (NoInfo, x)) (srcspan f x)) NoInfo

--- Some prettyprinting definitions are here because we need them in
--- the Attributes module.

instance Pretty PrimType where
  pretty (Unsigned Int8) = "u8"
  pretty (Unsigned Int16) = "u16"
  pretty (Unsigned Int32) = "u32"
  pretty (Unsigned Int64) = "u64"
  pretty (Signed t) = pretty t
  pretty (FloatType t) = pretty t
  pretty Bool = "bool"

instance Pretty BinOp where
  pretty Backtick = "``"
  pretty Bang = "!"
  pretty Equ = "="
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Pow = "**"
  pretty Times = "*"
  pretty Divide = "/"
  pretty Mod = "%"
  pretty Quot = "//"
  pretty Rem = "%%"
  pretty ShiftR = ">>"
  pretty ShiftL = "<<"
  pretty Band = "&"
  pretty Xor = "^"
  pretty Bor = "|"
  pretty LogAnd = "&&"
  pretty LogOr = "||"
  pretty Equal = "=="
  pretty NotEqual = "!="
  pretty Less = "<"
  pretty Leq = "<="
  pretty Greater = ">"
  pretty Geq = ">="
  pretty PipeLeft = "<|"
  pretty PipeRight = "|>"
