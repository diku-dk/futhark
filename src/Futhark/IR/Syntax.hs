{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- | = Definition of the Futhark core language IR
--
-- For actually /constructing/ ASTs, see "Futhark.Construct".
--
-- == Types and values
--
-- The core language type system is much more restricted than the core
-- language.  This is a theme that repeats often.  The only types that
-- are supported in the core language are various primitive types
-- t'PrimType' which can be combined in arrays (ignore v'Mem' for
-- now).  Types are represented as t'TypeBase', which is parameterised
-- by the shape of the array and whether we keep uniqueness
-- information.  The t'Type' alias, which is the most commonly used,
-- uses t'Shape' and t'NoUniqueness'.
--
-- This means that the records, tuples, and sum types of the source
-- language are represented merely as collections of primitives and
-- arrays.  This is implemented in "Futhark.Internalise", but the
-- specifics are not important for writing passes on the core
-- language.  What /is/ important is that many constructs that
-- conceptually return tuples instead return /multiple values/.  This
-- is not merely syntactic sugar for a tuple: each of those values are
-- eventually bound to distinct variables.  The prettyprinter for the
-- IR will typically print such collections of values or types in
-- curly braces.
--
-- The system of primitive types is interesting in itself.  See
-- "Futhark.IR.Primitive".
--
-- == Overall AST design
--
-- Internally, the Futhark compiler core intermediate representation
-- resembles a traditional compiler for an imperative language more
-- than it resembles, say, a Haskell or ML compiler.  All functions
-- are monomorphic (except for sizes), first-order, and defined at the
-- top level.  Notably, the IR does /not/ use continuation-passing
-- style (CPS) at any time.  Instead it uses Administrative Normal
-- Form (ANF), where all subexpressions t'SubExp' are either
-- constants 'PrimValue' or variables 'VName'.  Variables are
-- represented as a human-readable t'Name' (which doesn't matter to
-- the compiler) as well as a numeric /tag/, which is what the
-- compiler actually looks at.  All variable names when prettyprinted
-- are of the form @foo_123@.  Function names are just t'Name's,
-- though.
--
-- The body of a function ('FunDef') is a t'Body', which consists of
-- a sequence of statements ('Stms') and a t'Result'.  Execution of a
-- t'Body' consists of executing all of the statements, then returning
-- the values of the variables indicated by the result.
--
-- A statement ('Stm') consists of a t'Pattern' alongside an
-- expression 'ExpT'.  A pattern contains a "context" part and a
-- "value" part.  The context is used for things like the size of
-- arrays in the value part whose size is existential.
--
-- For example, the source language expression @let z = x + y - 1 in
-- z@ would in the core language be represented (in prettyprinted
-- form) as something like:
--
-- @
-- let {a_12} = x_10 + y_11
-- let {b_13} = a_12 - 1
-- in {b_13}
-- @
--
-- == Lores
--
-- Most AST types ('Stm', 'ExpT', t'Prog', etc) are parameterised by a
-- type parameter with the somewhat silly name @lore@.  The lore
-- specifies how to fill out various polymorphic parts of the AST.
-- For example, 'ExpT' has a constructor v'Op' whose payload depends
-- on @lore@, via the use of a type family called t'Op' (a kind of
-- type-level function) which is applied to the @lore@.  The SOACS
-- representation ("Futhark.IR.SOACS") thus uses a lore
-- called @SOACS@, and defines that @Op SOACS@ is a SOAC, while the
-- Kernels representation ("Futhark.IR.Kernels") defines
-- @Op Kernels@ as some kind of kernel construct.  Similarly, various
-- other decorations (e.g. what information we store in a t'PatElemT')
-- are also type families.
--
-- The full list of possible decorations is defined as part of the
-- type class 'Decorations' (although other type families are also
-- used elsewhere in the compiler on an ad hoc basis).
--
-- Essentially, the @lore@ type parameter functions as a kind of
-- proxy, saving us from having to parameterise the AST type with all
-- the different forms of decorations that we desire (it would easily
-- become a type with a dozen type parameters).
--
-- Defining a new representation (or /lore/) thus requires you to
-- define an empty datatype and implement a handful of type class
-- instances for it.  See the source of "Futhark.IR.Seq"
-- for what is likely the simplest example.
module Futhark.IR.Syntax
  ( module Language.Futhark.Core,
    module Futhark.IR.Decorations,
    module Futhark.IR.Syntax.Core,

    -- * Types
    Uniqueness (..),
    NoUniqueness (..),
    Rank (..),
    ArrayShape (..),
    Space (..),
    TypeBase (..),
    Diet (..),

    -- * Attributes
    Attr (..),
    Attrs (..),
    oneAttr,
    inAttrs,
    withoutAttrs,

    -- * Abstract syntax tree
    Ident (..),
    SubExp (..),
    PatElem,
    PatElemT (..),
    PatternT (..),
    Pattern,
    StmAux (..),
    Stm (..),
    Stms,
    Result,
    BodyT (..),
    Body,
    BasicOp (..),
    UnOp (..),
    BinOp (..),
    CmpOp (..),
    ConvOp (..),
    DimChange (..),
    ShapeChange,
    ExpT (..),
    Exp,
    LoopForm (..),
    IfDec (..),
    IfSort (..),
    Safety (..),
    LambdaT (..),
    Lambda,

    -- * Definitions
    Param (..),
    FParam,
    LParam,
    FunDef (..),
    EntryPoint,
    EntryPointType (..),
    Prog (..),

    -- * Utils
    oneStm,
    stmsFromList,
    stmsToList,
    stmsHead,
    stmsLast,
  )
where

import Control.Category
import Data.Foldable
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.String
import Data.Traversable (fmapDefault, foldMapDefault)
import Futhark.IR.Decorations
import Futhark.IR.Syntax.Core
import Language.Futhark.Core
import Prelude hiding (id, (.))

-- | A single attribute.
data Attr
  = AttrAtom Name
  | AttrComp Name [Attr]
  deriving (Ord, Show, Eq)

instance IsString Attr where
  fromString = AttrAtom . fromString

-- | Every statement is associated with a set of attributes, which can
-- have various effects throughout the compiler.
newtype Attrs = Attrs {unAttrs :: S.Set Attr}
  deriving (Ord, Show, Eq, Monoid, Semigroup)

-- | Construct 'Attrs' from a single 'Attr'.
oneAttr :: Attr -> Attrs
oneAttr = Attrs . S.singleton

-- | Is the given attribute to be found in the attribute set?
inAttrs :: Attr -> Attrs -> Bool
inAttrs attr (Attrs attrs) = attr `S.member` attrs

-- | @x `withoutAttrs` y@ gives @x@ except for any attributes also in @y@.
withoutAttrs :: Attrs -> Attrs -> Attrs
withoutAttrs (Attrs x) (Attrs y) = Attrs $ x `S.difference` y

-- | A type alias for namespace control.
type PatElem lore = PatElemT (LetDec lore)

-- | A pattern is conceptually just a list of names and their types.
data PatternT dec = Pattern
  { -- | existential context (sizes and memory blocks)
    patternContextElements :: [PatElemT dec],
    -- | "real" values
    patternValueElements :: [PatElemT dec]
  }
  deriving (Ord, Show, Eq)

instance Semigroup (PatternT dec) where
  Pattern cs1 vs1 <> Pattern cs2 vs2 = Pattern (cs1 ++ cs2) (vs1 ++ vs2)

instance Monoid (PatternT dec) where
  mempty = Pattern [] []

instance Functor PatternT where
  fmap = fmapDefault

instance Foldable PatternT where
  foldMap = foldMapDefault

instance Traversable PatternT where
  traverse f (Pattern ctx vals) =
    Pattern <$> traverse (traverse f) ctx <*> traverse (traverse f) vals

-- | A type alias for namespace control.
type Pattern lore = PatternT (LetDec lore)

-- | Auxilliary Information associated with a statement.
data StmAux dec = StmAux
  { stmAuxCerts :: !Certificates,
    stmAuxAttrs :: Attrs,
    stmAuxDec :: dec
  }
  deriving (Ord, Show, Eq)

instance Semigroup dec => Semigroup (StmAux dec) where
  StmAux cs1 attrs1 dec1 <> StmAux cs2 attrs2 dec2 =
    StmAux (cs1 <> cs2) (attrs1 <> attrs2) (dec1 <> dec2)

-- | A local variable binding.
data Stm lore = Let
  { -- | Pattern.
    stmPattern :: Pattern lore,
    -- | Auxiliary information statement.
    stmAux :: StmAux (ExpDec lore),
    -- | Expression.
    stmExp :: Exp lore
  }

deriving instance Decorations lore => Ord (Stm lore)

deriving instance Decorations lore => Show (Stm lore)

deriving instance Decorations lore => Eq (Stm lore)

-- | A sequence of statements.
type Stms lore = Seq.Seq (Stm lore)

-- | A single statement.
oneStm :: Stm lore -> Stms lore
oneStm = Seq.singleton

-- | Convert a statement list to a statement sequence.
stmsFromList :: [Stm lore] -> Stms lore
stmsFromList = Seq.fromList

-- | Convert a statement sequence to a statement list.
stmsToList :: Stms lore -> [Stm lore]
stmsToList = toList

-- | The first statement in the sequence, if any.
stmsHead :: Stms lore -> Maybe (Stm lore, Stms lore)
stmsHead stms = case Seq.viewl stms of
  stm Seq.:< stms' -> Just (stm, stms')
  Seq.EmptyL -> Nothing

-- | The last statement in the sequence, if any.
stmsLast :: Stms lore -> Maybe (Stms lore, Stm lore)
stmsLast stms = case Seq.viewr stms of
  stms' Seq.:> stm -> Just (stms', stm)
  Seq.EmptyR -> Nothing

-- | The result of a body is a sequence of subexpressions.
type Result = [SubExp]

-- | A body consists of a number of bindings, terminating in a result
-- (essentially a tuple literal).
data BodyT lore = Body
  { bodyDec :: BodyDec lore,
    bodyStms :: Stms lore,
    bodyResult :: Result
  }

deriving instance Decorations lore => Ord (BodyT lore)

deriving instance Decorations lore => Show (BodyT lore)

deriving instance Decorations lore => Eq (BodyT lore)

-- | Type alias for namespace reasons.
type Body = BodyT

-- | The new dimension in a 'Reshape'-like operation.  This allows us to
-- disambiguate "real" reshapes, that change the actual shape of the
-- array, from type coercions that are just present to make the types
-- work out.  The two constructors are considered equal for purposes of 'Eq'.
data DimChange d
  = -- | The new dimension is guaranteed to be numerically
    -- equal to the old one.
    DimCoercion d
  | -- | The new dimension is not necessarily numerically
    -- equal to the old one.
    DimNew d
  deriving (Ord, Show)

instance Eq d => Eq (DimChange d) where
  DimCoercion x == DimNew y = x == y
  DimCoercion x == DimCoercion y = x == y
  DimNew x == DimCoercion y = x == y
  DimNew x == DimNew y = x == y

instance Functor DimChange where
  fmap f (DimCoercion d) = DimCoercion $ f d
  fmap f (DimNew d) = DimNew $ f d

instance Foldable DimChange where
  foldMap f (DimCoercion d) = f d
  foldMap f (DimNew d) = f d

instance Traversable DimChange where
  traverse f (DimCoercion d) = DimCoercion <$> f d
  traverse f (DimNew d) = DimNew <$> f d

-- | A list of 'DimChange's, indicating the new dimensions of an array.
type ShapeChange d = [DimChange d]

-- | A primitive operation that returns something of known size and
-- does not itself contain any bindings.
data BasicOp
  = -- | A variable or constant.
    SubExp SubExp
  | -- | Semantically and operationally just identity, but is
    -- invisible/impenetrable to optimisations (hopefully).  This is
    -- just a hack to avoid optimisation (so, to work around compiler
    -- limitations).
    Opaque SubExp
  | -- | Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
    -- Second arg is the element type of the rows of the array.
    ArrayLit [SubExp] Type
  | -- | Unary operation.
    UnOp UnOp SubExp
  | -- | Binary operation.
    BinOp BinOp SubExp SubExp
  | -- | Comparison - result type is always boolean.
    CmpOp CmpOp SubExp SubExp
  | -- | Conversion "casting".
    ConvOp ConvOp SubExp
  | -- | Turn a boolean into a certificate, halting the program with the
    -- given error message if the boolean is false.
    Assert SubExp (ErrorMsg SubExp) (SrcLoc, [SrcLoc])
  | -- Primitive array operations

    -- | The certificates for bounds-checking are part of the 'Stm'.
    Index VName (Slice SubExp)
  | -- | An in-place update of the given array at the given position.
    -- Consumes the array.
    Update VName (Slice SubExp) SubExp
  | -- | @concat@0([1],[2, 3, 4]) = [1, 2, 3, 4]@.
    Concat Int VName [VName] SubExp
  | -- | Copy the given array.  The result will not alias anything.
    Copy VName
  | -- | Manifest an array with dimensions represented in the given
    -- order.  The result will not alias anything.
    Manifest [Int] VName
  | -- Array construction.

    -- | @iota(n, x, s) = [x,x+s,..,x+(n-1)*s]@.
    --
    -- The t'IntType' indicates the type of the array returned and the
    -- offset/stride arguments, but not the length argument.
    Iota SubExp SubExp SubExp IntType
  | -- | @replicate([3][2],1) = [[1,1], [1,1], [1,1]]@
    Replicate Shape SubExp
  | -- | Create array of given type and shape, with undefined elements.
    Scratch ElemType [SubExp]
  | -- Array index space transformation.

    -- | 1st arg is the new shape, 2nd arg is the input array *)
    Reshape (ShapeChange SubExp) VName
  | -- | Permute the dimensions of the input array.  The list
    -- of integers is a list of dimensions (0-indexed), which
    -- must be a permutation of @[0,n-1]@, where @n@ is the
    -- number of dimensions in the input array.
    Rearrange [Int] VName
  | -- | Rotate the dimensions of the input array.  The list of
    -- subexpressions specify how much each dimension is rotated.  The
    -- length of this list must be equal to the rank of the array.
    Rotate [SubExp] VName
  | -- Accumulator operations

    -- | Collapse an array of accumulators to the final arrays produced
    -- by the accumulations.  Consumes the accumulator and returns an
    -- array for each 'Type' (which is also incidentally the types of
    -- the arrays).
    UnAcc VName [Type]
  | -- | Update an accumulator at the given index with the given value.
    -- Consumes the accumulator and produces a new one.
    UpdateAcc VName [SubExp] [SubExp]
  deriving (Eq, Ord, Show)

-- | The root Futhark expression type.  The v'Op' constructor contains
-- a lore-specific operation.  Do-loops, branches and function calls
-- are special.  Everything else is a simple t'BasicOp'.
data ExpT lore
  = -- | A simple (non-recursive) operation.
    BasicOp BasicOp
  | Apply Name [(SubExp, Diet)] [RetType lore] (Safety, SrcLoc, [SrcLoc])
  | If SubExp (BodyT lore) (BodyT lore) (IfDec (BranchType lore))
  | -- | @loop {a} = {v} (for i < n|while b) do b@.  The merge
    -- parameters are divided into context and value part.
    DoLoop [(FParam lore, SubExp)] [(FParam lore, SubExp)] (LoopForm lore) (BodyT lore)
  | -- | Create an array of accumulators of the given shape and
    -- combination function, which is ultimately backed by the given
    -- arrays.  The second 'Shape' is the write index space.  The
    -- arrays must all have this shape outermost.  This is not part of
    -- 'BasicOp' because we need the @lore@ parameter.
    MkAcc Shape [VName] Shape (Maybe (Lambda lore, [SubExp]))
  | Op (Op lore)

deriving instance Decorations lore => Eq (ExpT lore)

deriving instance Decorations lore => Show (ExpT lore)

deriving instance Decorations lore => Ord (ExpT lore)

-- | For-loop or while-loop?
data LoopForm lore
  = ForLoop VName IntType SubExp [(LParam lore, VName)]
  | WhileLoop VName

deriving instance Decorations lore => Eq (LoopForm lore)

deriving instance Decorations lore => Show (LoopForm lore)

deriving instance Decorations lore => Ord (LoopForm lore)

-- | Data associated with a branch.
data IfDec rt = IfDec
  { ifReturns :: [rt],
    ifSort :: IfSort
  }
  deriving (Eq, Show, Ord)

-- | What kind of branch is this?  This has no semantic meaning, but
-- provides hints to simplifications.
data IfSort
  = -- | An ordinary branch.
    IfNormal
  | -- | A branch where the "true" case is what we are
    -- actually interested in, and the "false" case is only
    -- present as a fallback for when the true case cannot
    -- be safely evaluated.  The compiler is permitted to
    -- optimise away the branch if the true case contains
    -- only safe statements.
    IfFallback
  | -- | Both of these branches are semantically equivalent,
    -- and it is fine to eliminate one if it turns out to
    -- have problems (e.g. contain things we cannot generate
    -- code for).
    IfEquiv
  deriving (Eq, Show, Ord)

-- | A type alias for namespace control.
type Exp = ExpT

-- | Anonymous function for use in a SOAC.
data LambdaT lore = Lambda
  { lambdaParams :: [LParam lore],
    lambdaBody :: BodyT lore,
    lambdaReturnType :: [Type]
  }

deriving instance Decorations lore => Eq (LambdaT lore)

deriving instance Decorations lore => Show (LambdaT lore)

deriving instance Decorations lore => Ord (LambdaT lore)

-- | Type alias for namespacing reasons.
type Lambda = LambdaT

-- | A function and loop parameter.
type FParam lore = Param (FParamInfo lore)

-- | A lambda parameter.
type LParam lore = Param (LParamInfo lore)

-- | Function Declarations
data FunDef lore = FunDef
  { -- | Contains a value if this function is
    -- an entry point.
    funDefEntryPoint :: Maybe EntryPoint,
    funDefAttrs :: Attrs,
    funDefName :: Name,
    funDefRetType :: [RetType lore],
    funDefParams :: [FParam lore],
    funDefBody :: BodyT lore
  }

deriving instance Decorations lore => Eq (FunDef lore)

deriving instance Decorations lore => Show (FunDef lore)

deriving instance Decorations lore => Ord (FunDef lore)

-- | Information about the parameters and return value of an entry
-- point.  The first element is for parameters, the second for return
-- value.
type EntryPoint = ([EntryPointType], [EntryPointType])

-- | Every entry point argument and return value has an annotation
-- indicating how it maps to the original source program type.
data EntryPointType
  = -- | Is an unsigned integer or array of unsigned
    -- integers.
    TypeUnsigned
  | -- | A black box type comprising this many core
    -- values.  The string is a human-readable
    -- description with no other semantics.
    TypeOpaque String Int
  | -- | Maps directly.
    TypeDirect
  deriving (Eq, Show, Ord)

-- | An entire Futhark program.
data Prog lore = Prog
  { -- | Top-level constants that are computed at program startup, and
    -- which are in scope inside all functions.
    progConsts :: Stms lore,
    -- | The functions comprising the program.  All funtions are also
    -- available in scope in the definitions of the constants, so be
    -- careful not to introduce circular dependencies (not currently
    -- checked).
    progFuns :: [FunDef lore]
  }
  deriving (Eq, Ord, Show)
