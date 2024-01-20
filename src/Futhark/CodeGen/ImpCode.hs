{-# LANGUAGE Strict #-}

-- | ImpCode is an imperative intermediate language used as a stepping
-- stone in code generation.  The functional core IR
-- ("Futhark.IR.Syntax") gets translated into ImpCode by
-- "Futhark.CodeGen.ImpGen".  Later we then translate ImpCode to, for
-- example, C.
--
-- == Basic design
--
-- ImpCode distinguishes between /statements/ ('Code'), which may have
-- side effects, and /expressions/ ('Exp') which do not.  Expressions
-- involve only scalars and have a type.  The actual expression
-- definition is in "Futhark.Analysis.PrimExp", specifically
-- 'Futhark.Analysis.PrimExp.PrimExp' and its phantom-typed variant
-- 'Futhark.Analysis.PrimExp.TPrimExp'.
--
-- 'Code' is a generic representation parametrised on an extensible
-- arbitrary operation, represented by the 'Op' constructor.  Specific
-- instantiations of ImpCode, such as
-- "Futhark.CodeGen.ImpCode.Multicore", will pass in a specific kind
-- of operation to express backend-specific functionality (in the case
-- of multicore, this is
-- 'Futhark.CodeGen.ImpCode.Multicore.Multicore').
--
-- == Arrays and memory
--
-- ImpCode does not have arrays. 'DeclareArray' is for declaring
-- constant array literals, not arrays in general.  Instead, ImpCode
-- deals only with memory.  Array operations present in core IR
-- programs are turned into 'Write', v'Read', and 'Copy'
-- operations that use flat indexes and offsets based on the index
-- function of the original array.
--
-- == Scoping
--
-- ImpCode is much simpler than the functional core IR; partly because
-- we hope to do less work on it.  We don't have real optimisation
-- passes on ImpCode.  One result of this simplicity is that ImpCode
-- has a fairly naive view of scoping.  The /only/ things that can
-- bring new names into scope are 'DeclareMem', 'DeclareScalar',
-- 'DeclareArray', 'For', and function parameters.  In particular,
-- 'Op's /cannot/ bind parameters.  The standard workaround is to
-- define 'Op's that retrieve the value of an implicit parameter and
-- assign it to a variable declared with the normal
-- mechanisms. 'Futhark.CodeGen.ImpCode.Multicore.GetLoopBounds' is an
-- example of this pattern.
--
-- == Inspiration
--
-- ImpCode was originally inspired by the paper "Defunctionalizing
-- Push Arrays" (FHPC '14).
module Futhark.CodeGen.ImpCode
  ( Definitions (..),
    Functions (..),
    Function,
    FunctionT (..),
    EntryPoint (..),
    Constants (..),
    ValueDesc (..),
    ExternalValue (..),
    Param (..),
    paramName,
    MemSize,
    DimSize,
    Code (..),
    PrimValue (..),
    Exp,
    TExp,
    Volatility (..),
    Arg (..),
    var,
    ArrayContents (..),
    declaredIn,
    lexicalMemoryUsage,
    declsFirst,
    calledFuncs,
    callGraph,
    ParamMap,

    -- * Typed enumerations
    Bytes,
    Elements,
    elements,
    bytes,
    withElemType,

    -- * Re-exports from other modules.
    prettyText,
    prettyString,
    module Futhark.IR.Syntax.Core,
    module Language.Futhark.Core,
    module Language.Futhark.Primitive,
    module Futhark.Analysis.PrimExp,
    module Futhark.Analysis.PrimExp.Convert,
    module Futhark.IR.GPU.Sizes,
    module Futhark.IR.Prop.Names,
  )
where

import Data.Bifunctor (second)
import Data.List (intersperse, partition)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.GPU.Sizes (Count (..), SizeClass (..))
import Futhark.IR.Pretty ()
import Futhark.IR.Prop.Names
import Futhark.IR.Syntax.Core
  ( EntryPointType (..),
    ErrorMsg (..),
    ErrorMsgPart (..),
    OpaqueType (..),
    OpaqueTypes (..),
    Rank (..),
    Signedness (..),
    Space (..),
    SpaceId,
    SubExp (..),
    ValueType (..),
    errorMsgArgTypes,
  )
import Futhark.Util (nubByOrd)
import Futhark.Util.Pretty hiding (space)
import Language.Futhark.Core
import Language.Futhark.Primitive

-- | The size of a memory block.
type MemSize = SubExp

-- | The size of an array.
type DimSize = SubExp

-- | An ImpCode function parameter.
data Param
  = MemParam VName Space
  | ScalarParam VName PrimType
  deriving (Eq, Show)

-- | The name of a parameter.
paramName :: Param -> VName
paramName (MemParam name _) = name
paramName (ScalarParam name _) = name

-- | A collection of imperative functions and constants.
data Definitions a = Definitions
  { defTypes :: OpaqueTypes,
    defConsts :: Constants a,
    defFuns :: Functions a
  }
  deriving (Show)

instance Functor Definitions where
  fmap f (Definitions types consts funs) =
    Definitions types (fmap f consts) (fmap f funs)

-- | A collection of imperative functions.
newtype Functions a = Functions {unFunctions :: [(Name, Function a)]}
  deriving (Show)

instance Semigroup (Functions a) where
  Functions x <> Functions y = Functions $ x ++ y

instance Monoid (Functions a) where
  mempty = Functions []

-- | A collection of imperative constants.
data Constants a = Constants
  { -- | The constants that are made available to the functions.
    constsDecl :: [Param],
    -- | Setting the value of the constants.  Note that this must not
    -- contain declarations of the names defined in 'constsDecl'.
    constsInit :: Code a
  }
  deriving (Show)

instance Functor Constants where
  fmap f (Constants params code) = Constants params (fmap f code)

instance Monoid (Constants a) where
  mempty = Constants mempty mempty

instance Semigroup (Constants a) where
  Constants ps1 c1 <> Constants ps2 c2 =
    Constants (nubByOrd (comparing (prettyString . paramName)) $ ps1 <> ps2) (c1 <> c2)

-- | A description of an externally meaningful value.
data ValueDesc
  = -- | An array with memory block memory space, element type,
    -- signedness of element type (if applicable), and shape.
    ArrayValue VName Space PrimType Signedness [DimSize]
  | -- | A scalar value with signedness if applicable.
    ScalarValue PrimType Signedness VName
  deriving (Eq, Show)

-- | ^ An externally visible value.  This can be an opaque value
-- (covering several physical internal values), or a single value that
-- can be used externally.  We record the uniqueness because it is
-- important to the external interface as well.
data ExternalValue
  = -- | The string is a human-readable description with no other
    -- semantics.
    OpaqueValue Name [ValueDesc]
  | TransparentValue ValueDesc
  deriving (Show)

-- | Information about how this function can be called from the outside world.
data EntryPoint = EntryPoint
  { entryPointName :: Name,
    entryPointResults :: [(Uniqueness, ExternalValue)],
    entryPointArgs :: [((Name, Uniqueness), ExternalValue)]
  }
  deriving (Show)

-- | A imperative function, containing the body as well as its
-- low-level inputs and outputs, as well as its high-level arguments
-- and results.  The latter are only present if the function is an entry
-- point.
data FunctionT a = Function
  { functionEntry :: Maybe EntryPoint,
    functionOutput :: [Param],
    functionInput :: [Param],
    functionBody :: Code a
  }
  deriving (Show)

-- | Type alias for namespace control.
type Function = FunctionT

-- | The contents of a statically declared constant array.  Such
-- arrays are always unidimensional, and reshaped if necessary in the
-- code that uses them.
data ArrayContents
  = -- | Precisely these values.
    ArrayValues [PrimValue]
  | -- | This many zeroes.
    ArrayZeros Int
  deriving (Show)

-- | A block of imperative code.  Parameterised by an 'Op', which
-- allows extensibility.  Concrete uses of this type will instantiate
-- the type parameter with e.g. a construct for launching GPU kernels.
data Code a
  = -- | No-op.  Crucial for the 'Monoid' instance.
    Skip
  | -- | Statement composition.  Crucial for the 'Semigroup' instance.
    Code a :>>: Code a
  | -- | A for-loop iterating the given number of times.
    -- The loop parameter starts counting from zero and will
    -- have the same (integer) type as the bound.  The bound
    -- is evaluated just once, before the loop is entered.
    For VName Exp (Code a)
  | -- | While loop.  The conditional is (of course)
    -- re-evaluated before every iteration of the loop.
    While (TExp Bool) (Code a)
  | -- | Declare a memory block variable that will point to
    -- memory in the given memory space.  Note that this is
    -- distinct from allocation.  The memory block must be the
    -- target of either an 'Allocate' or a 'SetMem' before it
    -- can be used for reading or writing.
    DeclareMem VName Space
  | -- | Declare a scalar variable with an initially undefined value.
    DeclareScalar VName Volatility PrimType
  | -- | Create a DefaultSpace array containing the given values.  The
    -- lifetime of the array will be the entire application.  This is
    -- mostly used for constant arrays.
    DeclareArray VName PrimType ArrayContents
  | -- | Memory space must match the corresponding
    -- 'DeclareMem'.
    Allocate VName (Count Bytes (TExp Int64)) Space
  | -- | Indicate that some memory block will never again be
    -- referenced via the indicated variable.  However, it
    -- may still be accessed through aliases.  It is only
    -- safe to actually deallocate the memory block if this
    -- is the last reference.  There is no guarantee that
    -- all memory blocks will be freed with this statement.
    -- Backends are free to ignore it entirely.
    Free VName Space
  | -- | @Copy pt shape dest dest_lmad src src_lmad@.
    Copy
      PrimType
      [Count Elements (TExp Int64)]
      (VName, Space)
      ( Count Elements (TExp Int64),
        [Count Elements (TExp Int64)]
      )
      (VName, Space)
      ( Count Elements (TExp Int64),
        [Count Elements (TExp Int64)]
      )
  | -- | @Write mem i t space vol v@ writes the value @v@ to
    -- @mem@ offset by @i@ elements of type @t@.  The
    -- 'Space' argument is the memory space of @mem@
    -- (technically redundant, but convenient).
    Write VName (Count Elements (TExp Int64)) PrimType Space Volatility Exp
  | -- | Set a scalar variable.
    SetScalar VName Exp
  | -- | Read a scalar from memory from memory.  The first 'VName' is
    -- the target scalar variable, and the remaining arguments have
    -- the same meaning as with 'Write'.
    Read VName VName (Count Elements (TExp Int64)) PrimType Space Volatility
  | -- | Must be in same space.
    SetMem VName VName Space
  | -- | Function call.  The results are written to the
    -- provided 'VName' variables.
    Call [VName] Name [Arg]
  | -- | Conditional execution.
    If (TExp Bool) (Code a) (Code a)
  | -- | Assert that something must be true.  Should it turn
    -- out not to be true, then report a failure along with
    -- the given error message.
    Assert Exp (ErrorMsg Exp) (SrcLoc, [SrcLoc])
  | -- | Has the same semantics as the contained code, but
    -- the comment should show up in generated code for ease
    -- of inspection.
    Comment T.Text (Code a)
  | -- | Print the given value to the screen, somehow
    -- annotated with the given string as a description.  If
    -- no type/value pair, just print the string.  This has
    -- no semantic meaning, but is used entirely for
    -- debugging.  Code generators are free to ignore this
    -- statement.
    DebugPrint String (Maybe Exp)
  | -- | Log the given message, *without* a trailing linebreak (unless
    -- part of the message).
    TracePrint (ErrorMsg Exp)
  | -- | Perform an extensible operation.
    Op a
  deriving (Show)

-- | The volatility of a memory access or variable.  Feel free to
-- ignore this for backends where it makes no sense (anything but C
-- and similar low-level things)
data Volatility = Volatile | Nonvolatile
  deriving (Eq, Ord, Show)

instance Semigroup (Code a) where
  Skip <> y = y
  x <> Skip = x
  x <> y = x :>>: y

instance Monoid (Code a) where
  mempty = Skip

-- | Find those memory blocks that are used only lexically.  That is,
-- are not used as the source or target of a 'SetMem', or are the
-- result of the function, nor passed as arguments to other functions.
-- This is interesting because such memory blocks do not need
-- reference counting, but can be managed in a purely stack-like
-- fashion.
--
-- We do not look inside any 'Op's.  We assume that no 'Op' is going
-- to 'SetMem' a memory block declared outside it.
lexicalMemoryUsage :: Function a -> M.Map VName Space
lexicalMemoryUsage func =
  M.filterWithKey (const . (`notNameIn` nonlexical)) $
    declared $
      functionBody func
  where
    nonlexical =
      set (functionBody func)
        <> namesFromList (map paramName (functionOutput func))

    go f (x :>>: y) = f x <> f y
    go f (If _ x y) = f x <> f y
    go f (For _ _ x) = f x
    go f (While _ x) = f x
    go f (Comment _ x) = f x
    go _ _ = mempty

    declared (DeclareMem mem space) =
      M.singleton mem space
    declared x = go declared x

    set (SetMem x y _) = namesFromList [x, y]
    set (Call dests _ args) =
      -- Some of the dests might not be memory, but it does not matter.
      namesFromList dests <> foldMap onArg args
      where
        onArg ExpArg {} = mempty
        onArg (MemArg x) = oneName x
    set x = go set x

-- | Reorder the code such that all declarations appear first.  This
-- is always possible, because 'DeclareScalar' and 'DeclareMem' do
-- not depend on any local bindings.
declsFirst :: Code a -> Code a
declsFirst = mconcat . uncurry (<>) . partition isDecl . listify
  where
    listify (c1 :>>: c2) = listify c1 <> listify c2
    listify (If cond c1 c2) = [If cond (declsFirst c1) (declsFirst c2)]
    listify (For i e c) = [For i e (declsFirst c)]
    listify (While cond c) = [While cond (declsFirst c)]
    listify c = [c]
    isDecl (DeclareScalar {}) = True
    isDecl (DeclareMem {}) = True
    isDecl _ = False

-- | The set of functions that are called by this code.  Accepts a
-- function for determing function calls in 'Op's.
calledFuncs :: (a -> S.Set Name) -> Code a -> S.Set Name
calledFuncs _ (Call _ v _) = S.singleton v
calledFuncs f (Op x) = f x
calledFuncs f (x :>>: y) = calledFuncs f x <> calledFuncs f y
calledFuncs f (If _ x y) = calledFuncs f x <> calledFuncs f y
calledFuncs f (For _ _ x) = calledFuncs f x
calledFuncs f (While _ x) = calledFuncs f x
calledFuncs f (Comment _ x) = calledFuncs f x
calledFuncs _ _ = mempty

-- | Compute call graph, as per 'calledFuncs', but also include
-- transitive calls.
callGraph :: (a -> S.Set Name) -> Functions a -> M.Map Name (S.Set Name)
callGraph f (Functions funs) =
  loop $ M.fromList $ map (second $ calledFuncs f . functionBody) funs
  where
    loop cur =
      let grow v = maybe (S.singleton v) (S.insert v) (M.lookup v cur)
          next = M.map (foldMap grow) cur
       in if next == cur then cur else loop next

-- | A mapping from names of tuning parameters to their class, as well
-- as which functions make use of them (including transitively).
type ParamMap = M.Map Name (SizeClass, S.Set Name)

-- | A side-effect free expression whose execution will produce a
-- single primitive value.
type Exp = PrimExp VName

-- | Like 'Exp', but with a required/known type.
type TExp t = TPrimExp t VName

-- | A function call argument.
data Arg
  = ExpArg Exp
  | MemArg VName
  deriving (Show)

-- | Phantom type for a count of elements.
data Elements

-- | Phantom type for a count of bytes.
data Bytes

-- | This expression counts elements.
elements :: a -> Count Elements a
elements = Count

-- | This expression counts bytes.
bytes :: a -> Count Bytes a
bytes = Count

-- | Convert a count of elements into a count of bytes, given the
-- per-element size.
withElemType :: Count Elements (TExp Int64) -> PrimType -> Count Bytes (TExp Int64)
withElemType (Count e) t = bytes $ sExt64 e * primByteSize t

-- | Turn a 'VName' into a 'Exp'.
var :: VName -> PrimType -> Exp
var = LeafExp

-- Prettyprinting definitions.

instance (Pretty op) => Pretty (Definitions op) where
  pretty (Definitions types consts funs) =
    pretty types </> pretty consts </> pretty funs

instance (Pretty op) => Pretty (Functions op) where
  pretty (Functions funs) = stack $ intersperse mempty $ map ppFun funs
    where
      ppFun (name, fun) =
        "Function " <> pretty name <> colon </> indent 2 (pretty fun)

instance (Pretty op) => Pretty (Constants op) where
  pretty (Constants decls code) =
    "Constants:"
      </> indent 2 (stack $ map pretty decls)
      </> mempty
      </> "Initialisation:"
      </> indent 2 (pretty code)

instance Pretty EntryPoint where
  pretty (EntryPoint name results args) =
    "Name:"
      </> indent 2 (dquotes (pretty name))
      </> "Arguments:"
      </> indent 2 (stack $ map ppArg args)
      </> "Results:"
      </> indent 2 (stack $ map ppRes results)
    where
      ppArg ((p, u), t) = pretty p <+> ":" <+> ppRes (u, t)
      ppRes (u, t) = pretty u <> pretty t

instance (Pretty op) => Pretty (FunctionT op) where
  pretty (Function entry outs ins body) =
    "Inputs:"
      </> indent 2 (stack $ map pretty ins)
      </> "Outputs:"
      </> indent 2 (stack $ map pretty outs)
      </> "Entry:"
      </> indent 2 (pretty entry)
      </> "Body:"
      </> indent 2 (pretty body)

instance Pretty Param where
  pretty (ScalarParam name ptype) = pretty ptype <+> pretty name
  pretty (MemParam name space) = "mem" <> pretty space <> " " <> pretty name

instance Pretty ValueDesc where
  pretty (ScalarValue t ept name) =
    pretty t <+> pretty name <> ept'
    where
      ept' = case ept of
        Unsigned -> " (unsigned)"
        Signed -> mempty
  pretty (ArrayValue mem space et ept shape) =
    foldMap (brackets . pretty) shape
      <> (pretty et <+> "at" <+> pretty mem <> pretty space <+> ept')
    where
      ept' = case ept of
        Unsigned -> " (unsigned)"
        Signed -> mempty

instance Pretty ExternalValue where
  pretty (TransparentValue v) = pretty v
  pretty (OpaqueValue desc vs) =
    "opaque"
      <+> dquotes (pretty desc)
      <+> nestedBlock "{" "}" (stack $ map pretty vs)

instance Pretty ArrayContents where
  pretty (ArrayValues vs) = braces (commasep $ map pretty vs)
  pretty (ArrayZeros n) = braces "0" <+> "*" <+> pretty n

instance (Pretty op) => Pretty (Code op) where
  pretty (Op op) = pretty op
  pretty Skip = "skip"
  pretty (c1 :>>: c2) = pretty c1 </> pretty c2
  pretty (For i limit body) =
    "for"
      <+> pretty i
      <+> langle
      <+> pretty limit
      <+> "{"
      </> indent 2 (pretty body)
      </> "}"
  pretty (While cond body) =
    "while"
      <+> pretty cond
      <+> "{"
      </> indent 2 (pretty body)
      </> "}"
  pretty (DeclareMem name space) =
    "var" <+> pretty name <> ": mem" <> pretty space
  pretty (DeclareScalar name vol t) =
    "var" <+> pretty name <> ":" <+> vol' <> pretty t
    where
      vol' = case vol of
        Volatile -> "volatile "
        Nonvolatile -> mempty
  pretty (DeclareArray name t vs) =
    "array"
      <+> pretty name
      <+> ":"
      <+> pretty t
      <+> equals
      <+> pretty vs
  pretty (Allocate name e space) =
    pretty name <+> "<-" <+> "malloc" <> parens (pretty e) <> pretty space
  pretty (Free name space) =
    "free" <> parens (pretty name) <> pretty space
  pretty (Write name i bt space vol val) =
    pretty name
      <> langle
      <> vol'
      <> pretty bt
      <> pretty space
      <> rangle
      <> brackets (pretty i)
        <+> "<-"
        <+> pretty val
    where
      vol' = case vol of
        Volatile -> "volatile "
        Nonvolatile -> mempty
  pretty (Read name v is bt space vol) =
    pretty name
      <+> "<-"
      <+> pretty v
      <> langle
      <> vol'
      <> pretty bt
      <> pretty space
      <> rangle
      <> brackets (pretty is)
    where
      vol' = case vol of
        Volatile -> "volatile "
        Nonvolatile -> mempty
  pretty (SetScalar name val) =
    pretty name <+> "<-" <+> pretty val
  pretty (SetMem dest from DefaultSpace) =
    pretty dest <+> "<-" <+> pretty from
  pretty (SetMem dest from space) =
    pretty dest <+> "<-" <+> pretty from <+> "@" <> pretty space
  pretty (Assert e msg _) =
    "assert" <> parens (commasep [pretty msg, pretty e])
  pretty (Copy t shape (dst, dstspace) (dstoffset, dststrides) (src, srcspace) (srcoffset, srcstrides)) =
    ("lmadcopy_" <> pretty (length shape) <> "d_" <> pretty t)
      <> (parens . align)
        ( foldMap (brackets . pretty) shape
            <> ","
              </> p dst dstspace dstoffset dststrides
            <> ","
              </> p src srcspace srcoffset srcstrides
        )
    where
      p mem space offset strides =
        pretty mem
          <> pretty space
          <> "+"
          <> pretty offset
            <+> foldMap (brackets . pretty) strides
  pretty (If cond tbranch fbranch) =
    "if"
      <+> pretty cond
      <+> "then {"
      </> indent 2 (pretty tbranch)
      </> "} else"
      <+> case fbranch of
        If {} -> pretty fbranch
        _ ->
          "{" </> indent 2 (pretty fbranch) </> "}"
  pretty (Call [] fname args) =
    "call" <+> pretty fname <> parens (commasep $ map pretty args)
  pretty (Call dests fname args) =
    "call"
      <+> commasep (map pretty dests)
      <+> "<-"
      <+> pretty fname
      <> parens (commasep $ map pretty args)
  pretty (Comment s code) =
    "--" <+> pretty s </> pretty code
  pretty (DebugPrint desc (Just e)) =
    "debug" <+> parens (commasep [pretty (show desc), pretty e])
  pretty (DebugPrint desc Nothing) =
    "debug" <+> parens (pretty (show desc))
  pretty (TracePrint msg) =
    "trace" <+> parens (pretty msg)

instance Pretty Arg where
  pretty (MemArg m) = pretty m
  pretty (ExpArg e) = pretty e

instance Functor Functions where
  fmap = fmapDefault

instance Foldable Functions where
  foldMap = foldMapDefault

instance Traversable Functions where
  traverse f (Functions funs) =
    Functions <$> traverse f' funs
    where
      f' (name, fun) = (name,) <$> traverse f fun

instance Functor FunctionT where
  fmap = fmapDefault

instance Foldable FunctionT where
  foldMap = foldMapDefault

instance Traversable FunctionT where
  traverse f (Function entry outs ins body) =
    Function entry outs ins <$> traverse f body

instance Functor Code where
  fmap = fmapDefault

instance Foldable Code where
  foldMap = foldMapDefault

instance Traversable Code where
  traverse f (x :>>: y) =
    (:>>:) <$> traverse f x <*> traverse f y
  traverse f (For i bound code) =
    For i bound <$> traverse f code
  traverse f (While cond code) =
    While cond <$> traverse f code
  traverse f (If cond x y) =
    If cond <$> traverse f x <*> traverse f y
  traverse f (Op kernel) =
    Op <$> f kernel
  traverse _ Skip =
    pure Skip
  traverse _ (DeclareMem name space) =
    pure $ DeclareMem name space
  traverse _ (DeclareScalar name vol bt) =
    pure $ DeclareScalar name vol bt
  traverse _ (DeclareArray name t vs) =
    pure $ DeclareArray name t vs
  traverse _ (Allocate name size s) =
    pure $ Allocate name size s
  traverse _ (Free name space) =
    pure $ Free name space
  traverse _ (Copy t shape (dst, dstspace) (dstoffset, dststrides) (src, srcspace) (srcoffset, srcstrides)) =
    pure $ Copy t shape (dst, dstspace) (dstoffset, dststrides) (src, srcspace) (srcoffset, srcstrides)
  traverse _ (Write name i bt val space vol) =
    pure $ Write name i bt val space vol
  traverse _ (Read x name i bt space vol) =
    pure $ Read x name i bt space vol
  traverse _ (SetScalar name val) =
    pure $ SetScalar name val
  traverse _ (SetMem dest from space) =
    pure $ SetMem dest from space
  traverse _ (Assert e msg loc) =
    pure $ Assert e msg loc
  traverse _ (Call dests fname args) =
    pure $ Call dests fname args
  traverse f (Comment s code) =
    Comment s <$> traverse f code
  traverse _ (DebugPrint s v) =
    pure $ DebugPrint s v
  traverse _ (TracePrint msg) =
    pure $ TracePrint msg

-- | The names declared with 'DeclareMem', 'DeclareScalar', and
-- 'DeclareArray' in the given code.
declaredIn :: Code a -> Names
declaredIn (DeclareMem name _) = oneName name
declaredIn (DeclareScalar name _ _) = oneName name
declaredIn (DeclareArray name _ _) = oneName name
declaredIn (If _ t f) = declaredIn t <> declaredIn f
declaredIn (x :>>: y) = declaredIn x <> declaredIn y
declaredIn (For i _ body) = oneName i <> declaredIn body
declaredIn (While _ body) = declaredIn body
declaredIn (Comment _ body) = declaredIn body
declaredIn _ = mempty

instance FreeIn EntryPoint where
  freeIn' (EntryPoint _ res args) =
    freeIn' (map snd res) <> freeIn' (map snd args)

instance (FreeIn a) => FreeIn (Functions a) where
  freeIn' (Functions fs) = foldMap (onFun . snd) fs
    where
      onFun f =
        fvBind pnames $ freeIn' (functionBody f) <> freeIn' (functionEntry f)
        where
          pnames =
            namesFromList $ map paramName $ functionInput f <> functionOutput f

instance FreeIn ValueDesc where
  freeIn' (ArrayValue mem _ _ _ dims) = freeIn' mem <> freeIn' dims
  freeIn' ScalarValue {} = mempty

instance FreeIn ExternalValue where
  freeIn' (TransparentValue vd) = freeIn' vd
  freeIn' (OpaqueValue _ vds) = foldMap freeIn' vds

instance (FreeIn a) => FreeIn (Code a) where
  freeIn' (x :>>: y) =
    fvBind (declaredIn x) $ freeIn' x <> freeIn' y
  freeIn' Skip =
    mempty
  freeIn' (For i bound body) =
    fvBind (oneName i) $ freeIn' bound <> freeIn' body
  freeIn' (While cond body) =
    freeIn' cond <> freeIn' body
  freeIn' (DeclareMem _ space) =
    freeIn' space
  freeIn' DeclareScalar {} =
    mempty
  freeIn' DeclareArray {} =
    mempty
  freeIn' (Allocate name size space) =
    freeIn' name <> freeIn' size <> freeIn' space
  freeIn' (Free name _) =
    freeIn' name
  freeIn' (Copy _ shape (dst, _) (dstoffset, dststrides) (src, _) (srcoffset, srcstrides)) =
    freeIn' shape <> freeIn' dst <> freeIn' dstoffset <> freeIn' dststrides <> freeIn' src <> freeIn' srcoffset <> freeIn' srcstrides
  freeIn' (SetMem x y _) =
    freeIn' x <> freeIn' y
  freeIn' (Write v i _ _ _ e) =
    freeIn' v <> freeIn' i <> freeIn' e
  freeIn' (Read x v i _ _ _) =
    freeIn' x <> freeIn' v <> freeIn' i
  freeIn' (SetScalar x y) =
    freeIn' x <> freeIn' y
  freeIn' (Call dests _ args) =
    freeIn' dests <> freeIn' args
  freeIn' (If cond t f) =
    freeIn' cond <> freeIn' t <> freeIn' f
  freeIn' (Assert e msg _) =
    freeIn' e <> foldMap freeIn' msg
  freeIn' (Op op) =
    freeIn' op
  freeIn' (Comment _ code) =
    freeIn' code
  freeIn' (DebugPrint _ v) =
    maybe mempty freeIn' v
  freeIn' (TracePrint msg) =
    foldMap freeIn' msg

instance FreeIn Arg where
  freeIn' (MemArg m) = freeIn' m
  freeIn' (ExpArg e) = freeIn' e
