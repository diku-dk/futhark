{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- t'PrimType' which can be combined in arrays (ignore v'Mem' and
-- v'Acc' for now).  Types are represented as t'TypeBase', which is
-- parameterised by the shape of the array and whether we keep
-- uniqueness information.  The t'Type' alias, which is the most
-- commonly used, uses t'Shape' and t'NoUniqueness'.
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
-- A statement ('Stm') consists of a t'Pat' alongside an
-- expression 'Exp'.  A pattern is a sequence of name/type pairs.
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
-- == Representations
--
-- Most AST types ('Stm', 'Exp', t'Prog', etc) are parameterised by a
-- type parameter @rep@.  The representation specifies how to fill out
-- various polymorphic parts of the AST.  For example, 'Exp' has a
-- constructor v'Op' whose payload depends on @rep@, via the use of a
-- type family called t'Op' (a kind of type-level function) which is
-- applied to the @rep@.  The SOACS representation
-- ("Futhark.IR.SOACS") thus uses a rep called @SOACS@, and defines
-- that @Op SOACS@ is a SOAC, while the Kernels representation
-- ("Futhark.IR.Kernels") defines @Op Kernels@ as some kind of kernel
-- construct.  Similarly, various other decorations (e.g. what
-- information we store in a t'PatElem') are also type families.
--
-- The full list of possible decorations is defined as part of the
-- type class 'RepTypes' (although other type families are also
-- used elsewhere in the compiler on an ad hoc basis).
--
-- Essentially, the @rep@ type parameter functions as a kind of
-- proxy, saving us from having to parameterise the AST type with all
-- the different forms of decorations that we desire (it would easily
-- become a type with a dozen type parameters).
--
-- Some AST elements (such as 'Pat') do not take a @rep@ type
-- parameter, but instead immediately the single type of decoration
-- that they contain.  We only use the more complicated machinery when
-- needed.
--
-- Defining a new representation (or /rep/) thus requires you to
-- define an empty datatype and implement a handful of type class
-- instances for it.  See the source of "Futhark.IR.Seq"
-- for what is likely the simplest example.
module Futhark.IR.Syntax
  ( module Language.Futhark.Core,
    pretty,
    module Futhark.IR.Rep,
    module Futhark.IR.Syntax.Core,

    -- * Types
    Uniqueness (..),
    NoUniqueness (..),
    Rank (..),
    ArrayShape (..),
    Space (..),
    TypeBase (..),
    Diet (..),

    -- * Abstract syntax tree
    Ident (..),
    SubExp (..),
    PatElem (..),
    Pat (..),
    StmAux (..),
    Stm (..),
    Stms,
    SubExpRes (..),
    Result,
    Body (..),
    BasicOp (..),
    UnOp (..),
    BinOp (..),
    CmpOp (..),
    ConvOp (..),
    OpaqueOp (..),
    DimChange (..),
    ShapeChange,
    WithAccInput,
    Exp (..),
    LoopForm (..),
    IfDec (..),
    IfSort (..),
    Safety (..),
    Lambda (..),

    -- * Definitions
    Param (..),
    FParam,
    LParam,
    FunDef (..),
    EntryPoint,
    EntryParam (..),
    EntryPointType (..),
    Prog (..),

    -- * Utils
    oneStm,
    stmsFromList,
    stmsToList,
    stmsHead,
    stmsLast,
    subExpRes,
    subExpsRes,
    varRes,
    varsRes,
    subExpResVName,
  )
where

import Control.Category
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Data.String
import Data.Traversable (fmapDefault, foldMapDefault)
import Futhark.IR.Rep
import Futhark.IR.Syntax.Core
import Futhark.Util.Pretty (pretty)
import Language.Futhark.Core
import Prelude hiding (id, (.))

-- | A pattern is conceptually just a list of names and their types.
newtype Pat dec = Pat {patElems :: [PatElem dec]}
  deriving (Ord, Show, Eq)

instance Semigroup (Pat dec) where
  Pat xs <> Pat ys = Pat (xs <> ys)

instance Monoid (Pat dec) where
  mempty = Pat mempty

instance Functor Pat where
  fmap = fmapDefault

instance Foldable Pat where
  foldMap = foldMapDefault

instance Traversable Pat where
  traverse f (Pat xs) =
    Pat <$> traverse (traverse f) xs

-- | Auxilliary Information associated with a statement.
data StmAux dec = StmAux
  { stmAuxCerts :: !Certs,
    stmAuxAttrs :: Attrs,
    stmAuxDec :: dec
  }
  deriving (Ord, Show, Eq)

instance Semigroup dec => Semigroup (StmAux dec) where
  StmAux cs1 attrs1 dec1 <> StmAux cs2 attrs2 dec2 =
    StmAux (cs1 <> cs2) (attrs1 <> attrs2) (dec1 <> dec2)

-- | A local variable binding.
data Stm rep = Let
  { -- | Pat.
    stmPat :: Pat (LetDec rep),
    -- | Auxiliary information statement.
    stmAux :: StmAux (ExpDec rep),
    -- | Expression.
    stmExp :: Exp rep
  }

deriving instance RepTypes rep => Ord (Stm rep)

deriving instance RepTypes rep => Show (Stm rep)

deriving instance RepTypes rep => Eq (Stm rep)

-- | A sequence of statements.
type Stms rep = Seq.Seq (Stm rep)

-- | A single statement.
oneStm :: Stm rep -> Stms rep
oneStm = Seq.singleton

-- | Convert a statement list to a statement sequence.
stmsFromList :: [Stm rep] -> Stms rep
stmsFromList = Seq.fromList

-- | Convert a statement sequence to a statement list.
stmsToList :: Stms rep -> [Stm rep]
stmsToList = toList

-- | The first statement in the sequence, if any.
stmsHead :: Stms rep -> Maybe (Stm rep, Stms rep)
stmsHead stms = case Seq.viewl stms of
  stm Seq.:< stms' -> Just (stm, stms')
  Seq.EmptyL -> Nothing

-- | The last statement in the sequence, if any.
stmsLast :: Stms lore -> Maybe (Stms lore, Stm lore)
stmsLast stms = case Seq.viewr stms of
  stms' Seq.:> stm -> Just (stms', stm)
  Seq.EmptyR -> Nothing

-- | A pairing of a subexpression and some certificates.
data SubExpRes = SubExpRes
  { resCerts :: Certs,
    resSubExp :: SubExp
  }
  deriving (Eq, Ord, Show)

-- | Construct a 'SubExpRes' with no certificates.
subExpRes :: SubExp -> SubExpRes
subExpRes = SubExpRes mempty

-- | Construct a 'SubExpRes' from a variable name.
varRes :: VName -> SubExpRes
varRes = subExpRes . Var

-- | Construct a 'Result' from subexpressions.
subExpsRes :: [SubExp] -> Result
subExpsRes = map subExpRes

-- | Construct a 'Result' from variable names.
varsRes :: [VName] -> Result
varsRes = map varRes

-- | The 'VName' of a 'SubExpRes', if it exists.
subExpResVName :: SubExpRes -> Maybe VName
subExpResVName (SubExpRes _ (Var v)) = Just v
subExpResVName _ = Nothing

-- | The result of a body is a sequence of subexpressions.
type Result = [SubExpRes]

-- | A body consists of a number of bindings, terminating in a result
-- (essentially a tuple literal).
data Body rep = Body
  { bodyDec :: BodyDec rep,
    bodyStms :: Stms rep,
    bodyResult :: Result
  }

deriving instance RepTypes rep => Ord (Body rep)

deriving instance RepTypes rep => Show (Body rep)

deriving instance RepTypes rep => Eq (Body rep)

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

-- | Apart from being Opaque, what else is going on here?
data OpaqueOp
  = -- | No special operation.
    OpaqueNil
  | -- | Print the argument, prefixed by this string.
    OpaqueTrace String
  deriving (Eq, Ord, Show)

-- | A primitive operation that returns something of known size and
-- does not itself contain any bindings.
data BasicOp
  = -- | A variable or constant.
    SubExp SubExp
  | -- | Semantically and operationally just identity, but is
    -- invisible/impenetrable to optimisations (hopefully).  This
    -- partially a hack to avoid optimisation (so, to work around
    -- compiler limitations), but is also used to implement tracing
    -- and other operations that are semantically invisible, but have
    -- some sort of effect (brrr).
    Opaque OpaqueOp SubExp
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
    -- Consumes the array.  If 'Safe', perform a run-time bounds check
    -- and ignore the write if out of bounds (like @Scatter@).
    Update Safety VName (Slice SubExp) SubExp
  | FlatIndex VName (FlatSlice SubExp)
  | FlatUpdate VName (FlatSlice SubExp) VName
  | -- | @concat(0, [1] :| [[2, 3, 4], [5, 6]], 6) = [1, 2, 3, 4, 5, 6]@
    --
    -- Concatenates the non-empty list of 'VName' resulting in an
    -- array of length t'SubExp'. The 'Int' argument is used to
    -- specify the dimension along which the arrays are
    -- concatenated. For instance:
    --
    -- @concat(1, [[1,2], [3, 4]] :| [[[5,6]], [[7, 8]]], 4) = [[1, 2, 5, 6], [3, 4, 7, 8]]@
    Concat Int (NonEmpty VName) SubExp
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
    Scratch PrimType [SubExp]
  | -- Array index space transformation.

    -- | 1st arg is the new shape, 2nd arg is the input array.
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
  | -- | Update an accumulator at the given index with the given value.
    -- Consumes the accumulator and produces a new one.
    UpdateAcc VName [SubExp] [SubExp]
  deriving (Eq, Ord, Show)

-- | The input to a 'WithAcc' construct.  Comprises the index space of
-- the accumulator, the underlying arrays, and possibly a combining
-- function.
type WithAccInput rep =
  (Shape, [VName], Maybe (Lambda rep, [SubExp]))

-- | The root Futhark expression type.  The v'Op' constructor contains
-- a rep-specific operation.  Do-loops, branches and function calls
-- are special.  Everything else is a simple t'BasicOp'.
data Exp rep
  = -- | A simple (non-recursive) operation.
    BasicOp BasicOp
  | Apply Name [(SubExp, Diet)] [RetType rep] (Safety, SrcLoc, [SrcLoc])
  | If SubExp (Body rep) (Body rep) (IfDec (BranchType rep))
  | -- | @loop {a} = {v} (for i < n|while b) do b@.
    DoLoop [(FParam rep, SubExp)] (LoopForm rep) (Body rep)
  | -- | Create accumulators backed by the given arrays (which are
    -- consumed) and pass them to the lambda, which must return the
    -- updated accumulators and possibly some extra values.  The
    -- accumulators are turned back into arrays.  The t'Shape' is the
    -- write index space.  The corresponding arrays must all have this
    -- shape outermost.  This construct is not part of t'BasicOp'
    -- because we need the @rep@ parameter.
    WithAcc [WithAccInput rep] (Lambda rep)
  | Op (Op rep)

deriving instance RepTypes rep => Eq (Exp rep)

deriving instance RepTypes rep => Show (Exp rep)

deriving instance RepTypes rep => Ord (Exp rep)

-- | For-loop or while-loop?
data LoopForm rep
  = ForLoop VName IntType SubExp [(LParam rep, VName)]
  | WhileLoop VName

deriving instance RepTypes rep => Eq (LoopForm rep)

deriving instance RepTypes rep => Show (LoopForm rep)

deriving instance RepTypes rep => Ord (LoopForm rep)

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

-- | Anonymous function for use in a SOAC.
data Lambda rep = Lambda
  { lambdaParams :: [LParam rep],
    lambdaBody :: Body rep,
    lambdaReturnType :: [Type]
  }

deriving instance RepTypes rep => Eq (Lambda rep)

deriving instance RepTypes rep => Show (Lambda rep)

deriving instance RepTypes rep => Ord (Lambda rep)

-- | A function and loop parameter.
type FParam rep = Param (FParamInfo rep)

-- | A lambda parameter.
type LParam rep = Param (LParamInfo rep)

-- | Function Declarations
data FunDef rep = FunDef
  { -- | Contains a value if this function is
    -- an entry point.
    funDefEntryPoint :: Maybe EntryPoint,
    funDefAttrs :: Attrs,
    funDefName :: Name,
    funDefRetType :: [RetType rep],
    funDefParams :: [FParam rep],
    funDefBody :: Body rep
  }

deriving instance RepTypes rep => Eq (FunDef rep)

deriving instance RepTypes rep => Show (FunDef rep)

deriving instance RepTypes rep => Ord (FunDef rep)

-- | Every entry point argument and return value has an annotation
-- indicating how it maps to the original source program type.
data EntryPointType
  = -- | Is an unsigned integer or array of unsigned
    -- integers.
    TypeUnsigned Uniqueness
  | -- | A black box type comprising this many core
    -- values.  The string is a human-readable
    -- description with no other semantics.
    TypeOpaque Uniqueness String Int
  | -- | Maps directly.
    TypeDirect Uniqueness
  deriving (Eq, Show, Ord)

-- | An entry point parameter, comprising its name and original type.
data EntryParam = EntryParam
  { entryParamName :: Name,
    entryParamType :: EntryPointType
  }
  deriving (Eq, Show, Ord)

-- | Information about the inputs and outputs (return value) of an entry
-- point.
type EntryPoint = (Name, [EntryParam], [EntryPointType])

-- | An entire Futhark program.
data Prog rep = Prog
  { -- | Top-level constants that are computed at program startup, and
    -- which are in scope inside all functions.
    progConsts :: Stms rep,
    -- | The functions comprising the program.  All funtions are also
    -- available in scope in the definitions of the constants, so be
    -- careful not to introduce circular dependencies (not currently
    -- checked).
    progFuns :: [FunDef rep]
  }
  deriving (Eq, Ord, Show)
