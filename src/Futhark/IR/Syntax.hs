{-# LANGUAGE Strict #-}
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
-- "Language.Futhark.Primitive".
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
    prettyString,
    prettyStringOneLine,
    prettyText,
    prettyTextOneLine,
    Pretty,
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
    GBody (..),
    Body,
    BasicOp (..),
    UnOp (..),
    BinOp (..),
    CmpOp (..),
    ConvOp (..),
    OpaqueOp (..),
    DimSplice (..),
    NewShape (..),
    WithAccInput,
    Exp (..),
    Case (..),
    LoopForm (..),
    MatchDec (..),
    MatchSort (..),
    Safety (..),
    Lambda (..),
    RetAls (..),

    -- * Definitions
    Param (..),
    FParam,
    LParam,
    FunDef (..),
    EntryParam (..),
    EntryResult (..),
    EntryPoint,
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
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Traversable (fmapDefault, foldMapDefault)
import Futhark.IR.Rep
import Futhark.IR.Syntax.Core
import Futhark.Util.Pretty (Pretty, prettyString, prettyStringOneLine, prettyText, prettyTextOneLine)
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
    stmAuxLoc :: Provenance,
    stmAuxDec :: dec
  }
  deriving (Ord, Show, Eq)

instance Functor StmAux where
  fmap = fmapDefault

instance Foldable StmAux where
  foldMap = foldMapDefault

instance Traversable StmAux where
  traverse f (StmAux cs attrs loc dec) =
    StmAux cs attrs loc <$> f dec

instance (Monoid dec) => Monoid (StmAux dec) where
  mempty = StmAux mempty mempty mempty mempty

instance (Semigroup dec) => Semigroup (StmAux dec) where
  StmAux cs1 attrs1 loc1 dec1 <> StmAux cs2 attrs2 loc2 dec2 =
    StmAux (cs1 <> cs2) (attrs1 <> attrs2) (loc1 <> loc2) (dec1 <> dec2)

-- | A local variable binding.
data Stm rep = Let
  { -- | Pat.
    stmPat :: Pat (LetDec rep),
    -- | Auxiliary information statement.
    stmAux :: StmAux (ExpDec rep),
    -- | Expression.
    stmExp :: Exp rep
  }

deriving instance (RepTypes rep) => Ord (Stm rep)

deriving instance (RepTypes rep) => Show (Stm rep)

deriving instance (RepTypes rep) => Eq (Stm rep)

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

-- | A generalised body consists of a sequence of statements, terminated in some
-- kind of result.
data GBody rep res = Body
  { bodyDec :: BodyDec rep,
    bodyStms :: Stms rep,
    bodyResult :: [res]
  }

-- | A body consists of a sequence of statements, terminating in a
-- list of result values.
type Body rep = GBody rep SubExpRes

deriving instance (RepTypes rep, Ord res) => Ord (GBody rep res)

deriving instance (RepTypes rep, Show res) => Show (GBody rep res)

deriving instance (RepTypes rep, Eq res) => Eq (GBody rep res)

-- | Apart from being Opaque, what else is going on here?
data OpaqueOp
  = -- | No special operation.
    OpaqueNil
  | -- | Print the argument, prefixed by this string.
    OpaqueTrace T.Text
  deriving (Eq, Ord, Show)

-- | Split or join a range of dimensions. A reshaping operation consists of a
-- sequence of these. The purpose is to maintain information about the original
-- operations (flatten/unflatten), which can then be used for algebraic
-- optimisations.
data DimSplice d
  = -- | @DimSplice i k s@ modifies dimensions @i@ to @i+k-1@ to instead have
    -- shape @s@.
    --
    -- If @k@ is 1 and the rank of @s@ is greater than 1, then this is
    -- equivalent to unflattening a dimension.
    --
    -- If @k@ is greater than 1 and the rank of @s@ is 1, then this is
    -- equivalent to flattening adjacent dimensions.
    --
    -- If @k@ is 1 and the rank of @s@ is 1, then it is a coercion - a change
    -- that only affects the type, but does not have any semantic effect.
    --
    -- Other cases can do arbitrary changes, but are harder for the compiler to
    -- analyse.
    DimSplice Int Int (ShapeBase d)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A reshaping operation consists of a sequence of splices, as well as an
-- annotation indicating the final shape.
data NewShape d = NewShape
  { -- | The changes to perform.
    dimSplices :: [DimSplice d],
    -- | The resulting shape.
    newShape :: ShapeBase d
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup (NewShape d) where
  NewShape ss1 _ <> NewShape ss2 shape = NewShape (ss1 <> ss2) shape

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
  | -- | A one-dimensional array literal that contains only constants.
    -- This is a fast-path for representing very large array literals
    -- that show up in some programs. The key rule for processing this
    -- in compiler passes is that you should never need to look at the
    -- individual elements. Has exactly the same semantics as an
    -- 'ArrayLit'.
    ArrayVal [PrimValue] PrimType
  | -- | Unary operation.
    UnOp UnOp SubExp
  | -- | Binary operation.
    BinOp BinOp SubExp SubExp
  | -- | Comparison - result type is always boolean.
    CmpOp CmpOp SubExp SubExp
  | -- | Conversion "casting".
    ConvOp ConvOp SubExp
  | -- | Turn a boolean into a certificate, halting the program with the given
    -- error message if the boolean is false. The error location comes from the
    -- provenance of the statement.
    Assert SubExp (ErrorMsg SubExp)
  | -- | The certificates for bounds-checking are part of the 'Stm'.
    Index VName (Slice SubExp)
  | -- | An in-place update of the given array at the given position.
    -- Consumes the array.  If 'Safe', perform a run-time bounds check
    -- and ignore the write if out of bounds (scatter-like).
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
  | -- | Manifest an array with dimensions represented in the given
    -- order.  The result will not alias anything.
    Manifest VName [Int]
  | -- Array construction.

    -- | @iota(n, x, s) = [x,x+s,..,x+(n-1)*s]@.
    --
    -- The t'IntType' indicates the type of the array returned and the
    -- offset/stride arguments, but not the length argument.
    Iota SubExp SubExp SubExp IntType
  | -- | @replicate([3][2],1) = [[1,1], [1,1], [1,1]]@.  The result
    -- has no aliases.  Copy a value by passing an empty shape.
    Replicate Shape SubExp
  | -- | Create array of given type and shape, with undefined elements.
    Scratch PrimType [SubExp]
  | -- | 1st arg is the input array, 2nd arg is new shape.
    Reshape VName (NewShape SubExp)
  | -- | Permute the dimensions of the input array.  The list
    -- of integers is a list of dimensions (0-indexed), which
    -- must be a permutation of @[0,n-1]@, where @n@ is the
    -- number of dimensions in the input array.
    Rearrange VName [Int]
  | -- | Update an accumulator at the given index with the given
    -- value. Consumes the accumulator and produces a new one. If
    -- 'Safe', perform a run-time bounds check and ignore the write if
    -- out of bounds (scatter-like).
    UpdateAcc Safety VName [SubExp] [SubExp]
  | -- | Value of a user-defined parameter (an implicit value provided at
    -- program startup), including a default if no value is provided.
    UserParam Name SubExp
  deriving (Eq, Ord, Show)

-- | The input to a 'WithAcc' construct.  Comprises the index space of
-- the accumulator, the underlying arrays, and possibly a combining
-- function.
type WithAccInput rep =
  (Shape, [VName], Maybe (Lambda rep, [SubExp]))

-- | A non-default case in a 'Match' statement.  The number of
-- elements in the pattern must match the number of scrutinees.  A
-- 'Nothing' value indicates that we don't care about it (i.e. a
-- wildcard).
data Case body = Case {casePat :: [Maybe PrimValue], caseBody :: body}
  deriving (Eq, Ord, Show)

instance Functor Case where
  fmap = fmapDefault

instance Foldable Case where
  foldMap = foldMapDefault

instance Traversable Case where
  traverse f (Case vs b) = Case vs <$> f b

-- | Information about the possible aliases of a function result.
data RetAls = RetAls
  { -- | Which of the parameters may be aliased, numbered from zero.
    -- Must be sorted in increasing order.
    paramAls :: [Int],
    -- | Which of the other results may be aliased, numbered from
    -- zero. This must be a reflexive relation. Must be sorted in
    -- increasing order.
    otherAls :: [Int]
  }
  deriving (Eq, Ord, Show)

instance Monoid RetAls where
  mempty = RetAls mempty mempty

instance Semigroup RetAls where
  RetAls pals1 rals1 <> RetAls pals2 rals2 =
    RetAls (pals1 <> pals2) (rals1 <> rals2)

-- | The root Futhark expression type.  The v'Op' constructor contains
-- a rep-specific operation.  Do-loops, branches and function calls
-- are special.  Everything else is a simple t'BasicOp'.
data Exp rep
  = -- | A simple (non-recursive) operation.
    BasicOp BasicOp
  | Apply Name [(SubExp, Diet)] [(RetType rep, RetAls)] Safety
  | -- | A match statement picks a branch by comparing the given
    -- subexpressions (called the /scrutinee/) with the pattern in
    -- each of the cases.  If none of the cases match, the /default
    -- body/ is picked.
    Match [SubExp] [Case (Body rep)] (Body rep) (MatchDec (BranchType rep))
  | -- | @loop {a} = {v} (for i < n|while b) do b@.
    Loop [(FParam rep, SubExp)] LoopForm (Body rep)
  | -- | Create accumulators backed by the given arrays (which are
    -- consumed) and pass them to the lambda, which must return the
    -- updated accumulators and possibly some extra values.  The
    -- accumulators are turned back into arrays.  In the lambda, the result
    -- accumulators come first, and are ordered in a manner consistent with
    -- that of the input (accumulator) arguments. The t'Shape' is the
    -- write index space.  The corresponding arrays must all have this
    -- shape outermost.  This construct is not part of t'BasicOp'
    -- because we need the @rep@ parameter.
    WithAcc [WithAccInput rep] (Lambda rep)
  | Op (Op rep)

deriving instance (RepTypes rep) => Eq (Exp rep)

deriving instance (RepTypes rep) => Show (Exp rep)

deriving instance (RepTypes rep) => Ord (Exp rep)

-- | For-loop or while-loop?
data LoopForm
  = ForLoop
      -- | The loop iterator var
      VName
      -- | The type of the loop iterator var
      IntType
      -- | The number of iterations.
      SubExp
  | WhileLoop VName
  deriving (Eq, Ord, Show)

-- | Data associated with a branch.
data MatchDec rt = MatchDec
  { matchReturns :: [rt],
    matchSort :: MatchSort
  }
  deriving (Eq, Show, Ord)

-- | What kind of branch is this?  This has no semantic meaning, but
-- provides hints to simplifications.
data MatchSort
  = -- | An ordinary branch.
    MatchNormal
  | -- | A branch where the "true" case is what we are
    -- actually interested in, and the "false" case is only
    -- present as a fallback for when the true case cannot
    -- be safely evaluated.  The compiler is permitted to
    -- optimise away the branch if the true case contains
    -- only safe statements.
    MatchFallback
  | -- | Both of these branches are semantically equivalent,
    -- and it is fine to eliminate one if it turns out to
    -- have problems (e.g. contain things we cannot generate
    -- code for).
    MatchEquiv
  deriving (Eq, Show, Ord)

-- | Anonymous function for use in a SOAC.
data Lambda rep = Lambda
  { lambdaParams :: [LParam rep],
    lambdaReturnType :: [Type],
    lambdaBody :: Body rep
  }

deriving instance (RepTypes rep) => Eq (Lambda rep)

deriving instance (RepTypes rep) => Show (Lambda rep)

deriving instance (RepTypes rep) => Ord (Lambda rep)

-- | A function and loop parameter.
type FParam rep = Param (FParamInfo rep)

-- | A lambda parameter.
type LParam rep = Param (LParamInfo rep)

-- | Function definitions.
data FunDef rep = FunDef
  { -- | Contains a value if this function is
    -- an entry point.
    funDefEntryPoint :: Maybe EntryPoint,
    funDefAttrs :: Attrs,
    funDefName :: Name,
    funDefRetType :: [(RetType rep, RetAls)],
    funDefParams :: [FParam rep],
    funDefBody :: Body rep
  }

deriving instance (RepTypes rep) => Eq (FunDef rep)

deriving instance (RepTypes rep) => Show (FunDef rep)

deriving instance (RepTypes rep) => Ord (FunDef rep)

-- | An entry point parameter, comprising its name and original type.
data EntryParam = EntryParam
  { entryParamName :: Name,
    entryParamUniqueness :: Uniqueness,
    entryParamType :: EntryPointType
  }
  deriving (Eq, Show, Ord)

-- | An entry point result type.
data EntryResult = EntryResult
  { entryResultUniqueness :: Uniqueness,
    entryResultType :: EntryPointType
  }
  deriving (Eq, Show, Ord)

-- | Information about the inputs and outputs (return value) of an entry
-- point.
type EntryPoint = (Name, [EntryParam], [EntryResult])

-- | An entire Futhark program.
data Prog rep = Prog
  { -- | The opaque types used in entry points.  This information is
    -- used to generate extra API functions for
    -- construction and deconstruction of values of these types.
    progTypes :: OpaqueTypes,
    -- | Top-level constants that are computed at program startup, and
    -- which are in scope inside all functions.
    progConsts :: Stms rep,
    -- | The functions comprising the program.  All functions are also
    -- available in scope in the definitions of the constants, so be
    -- careful not to introduce circular dependencies (not currently
    -- checked).
    progFuns :: [FunDef rep]
  }
  deriving (Eq, Ord, Show)

-- Note [Tracking Source Locations]
--
-- It is useful for such things as profiling to be able to relate the generated
-- code to the original source code. The Futhark compiler is not great at this,
-- but we have begun to try a bit.
--
-- Each 'Stm' is associated with a 'Provenance', which keeps information about
-- the (single) source expression that gave rise to the statement. This is by
-- itself not challenging (although a single source expression can give rise to
-- multiple core expressions). The real challenge is how to propagate this
-- information when the compiler starts rewriting the program, without every
-- rewrite having to be laboriously aware of provenances. In practice, we stick
-- it in the StmAux and hope that consistent use of such constructs as 'auxing'
-- will mostly do the right thing.
--
-- Another downside of our representation is that it is not flow-sensitive: an
-- expression can be reached in multiple ways and generally the innermost one is
-- picked. This is particularly problematic for the prelude functions (e.g.
-- f32.exp), as it is not exceptionally useful when the provenance simply points
-- at a file in /prelude. We could address this by just special casing /prelude,
-- but that won't help when users write their own libraries.
