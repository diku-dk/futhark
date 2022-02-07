{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}

-- | Imperative intermediate language used as a stepping stone in code generation.
--
-- This is a generic representation parametrised on an extensible
-- arbitrary operation.
--
-- Originally inspired by the paper "Defunctionalizing Push Arrays"
-- (FHPC '14).
module Futhark.CodeGen.ImpCode
  ( Definitions (..),
    Functions (..),
    Function,
    FunctionT (..),
    Constants (..),
    ValueDesc (..),
    Signedness (..),
    ExternalValue (..),
    Param (..),
    paramName,
    SubExp (..),
    MemSize,
    DimSize,
    Space (..),
    SpaceId,
    Code (..),
    PrimValue (..),
    Exp,
    TExp,
    Volatility (..),
    Arg (..),
    var,
    ErrorMsg (..),
    ErrorMsgPart (..),
    errorMsgArgTypes,
    ArrayContents (..),
    declaredIn,
    lexicalMemoryUsage,
    calledFuncs,

    -- * Typed enumerations
    Bytes,
    Elements,
    elements,
    bytes,
    withElemType,

    -- * Re-exports from other modules.
    pretty,
    module Language.Futhark.Core,
    module Futhark.IR.Primitive,
    module Futhark.Analysis.PrimExp,
    module Futhark.Analysis.PrimExp.Convert,
    module Futhark.IR.GPU.Sizes,
    module Futhark.IR.Prop.Names,
  )
where

import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.GPU.Sizes (Count (..))
import Futhark.IR.Pretty ()
import Futhark.IR.Primitive
import Futhark.IR.Prop.Names
import Futhark.IR.Syntax.Core
  ( ErrorMsg (..),
    ErrorMsgPart (..),
    Space (..),
    SpaceId,
    SubExp (..),
    errorMsgArgTypes,
  )
import Futhark.Util.Pretty hiding (space)
import Language.Futhark.Core

-- | The size of a memory block.
type MemSize = SubExp

-- | The size of an array.
type DimSize = SubExp

-- | An ImpCode function parameter.
data Param
  = MemParam VName Space
  | ScalarParam VName PrimType
  deriving (Show)

-- | The name of a parameter.
paramName :: Param -> VName
paramName (MemParam name _) = name
paramName (ScalarParam name _) = name

-- | A collection of imperative functions and constants.
data Definitions a = Definitions
  { defConsts :: Constants a,
    defFuns :: Functions a
  }
  deriving (Show)

instance Functor Definitions where
  fmap f (Definitions consts funs) = Definitions (fmap f consts) (fmap f funs)

-- | A collection of imperative functions.
newtype Functions a = Functions [(Name, Function a)]
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

-- | Since the core language does not care for signedness, but the
-- source language does, entry point input/output information has
-- metadata for integer types (and arrays containing these) that
-- indicate whether they are really unsigned integers.
data Signedness
  = TypeUnsigned
  | TypeDirect
  deriving (Eq, Ord, Show)

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
    -- not matter.
    OpaqueValue Uniqueness String [ValueDesc]
  | TransparentValue Uniqueness ValueDesc
  deriving (Show)

-- | A imperative function, containing the body as well as its
-- low-level inputs and outputs, as well as its high-level arguments
-- and results.  The latter are only used if the function is an entry
-- point.
data FunctionT a = Function
  { functionEntry :: Maybe Name,
    functionOutput :: [Param],
    functionInput :: [Param],
    functionBody :: Code a,
    functionResult :: [ExternalValue],
    functionArgs :: [(Name, ExternalValue)]
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
  | -- | Create an array containing the given values.  The
    -- lifetime of the array will be the entire application.
    -- This is mostly used for constant arrays, but also for
    -- some bookkeeping data, like the synchronisation
    -- counts used to implement reduction.
    DeclareArray VName Space PrimType ArrayContents
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
  | -- | Destination, offset in destination, destination
    -- space, source, offset in source, offset space, number
    -- of bytes.
    Copy
      VName
      (Count Bytes (TExp Int64))
      Space
      VName
      (Count Bytes (TExp Int64))
      Space
      (Count Bytes (TExp Int64))
  | -- | @Write mem i t space vol v@ writes the value @v@ to
    -- @mem@ offset by @i@ elements of type @t@.  The
    -- 'Space' argument is the memory space of @mem@
    -- (technically redundant, but convenient).  Note that
    -- /reading/ is done with an 'Exp' ('Read').
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
    Comment String (Code a)
  | -- | Print the given value to the screen, somehow
    -- annotated with the given string as a description.  If
    -- no type/value pair, just print the string.  This has
    -- no semantic meaning, but is used entirely for
    -- debugging.  Code generators are free to ignore this
    -- statement.
    DebugPrint String (Maybe Exp)
  | -- | Log the given message, *without* a trailing linebreak (unless
    -- part of the mssage).
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
  M.filterWithKey (const . not . (`nameIn` nonlexical)) $
    declared $ functionBody func
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
    set (Call _ _ args) = foldMap onArg args
      where
        onArg ExpArg {} = mempty
        onArg (MemArg x) = oneName x
    set x = go set x

-- | The set of functions that are called by this code.  Assumes there
-- are no function calls in 'Op's.
calledFuncs :: Code a -> S.Set Name
calledFuncs (x :>>: y) = calledFuncs x <> calledFuncs y
calledFuncs (If _ x y) = calledFuncs x <> calledFuncs y
calledFuncs (For _ _ x) = calledFuncs x
calledFuncs (While _ x) = calledFuncs x
calledFuncs (Comment _ x) = calledFuncs x
calledFuncs (Call _ f _) = S.singleton f
calledFuncs _ = mempty

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

instance Pretty op => Pretty (Definitions op) where
  ppr (Definitions consts funs) =
    ppr consts </> ppr funs

instance Pretty op => Pretty (Functions op) where
  ppr (Functions funs) = stack $ intersperse mempty $ map ppFun funs
    where
      ppFun (name, fun) =
        text "Function " <> ppr name <> colon </> indent 2 (ppr fun)

instance Pretty op => Pretty (Constants op) where
  ppr (Constants decls code) =
    text "Constants:" </> indent 2 (stack $ map ppr decls)
      </> mempty
      </> text "Initialisation:"
      </> indent 2 (ppr code)

instance Pretty op => Pretty (FunctionT op) where
  ppr (Function _ outs ins body results args) =
    text "Inputs:" </> block ins
      </> text "Outputs:"
      </> block outs
      </> text "Arguments:"
      </> block args
      </> text "Result:"
      </> block results
      </> text "Body:"
      </> indent 2 (ppr body)
    where
      block :: Pretty a => [a] -> Doc
      block = indent 2 . stack . map ppr

instance Pretty Param where
  ppr (ScalarParam name ptype) = ppr ptype <+> ppr name
  ppr (MemParam name space) = text "mem" <> ppr space <> text " " <> ppr name

instance Pretty ValueDesc where
  ppr (ScalarValue t ept name) =
    ppr t <+> ppr name <> ept'
    where
      ept' = case ept of
        TypeUnsigned -> text " (unsigned)"
        TypeDirect -> mempty
  ppr (ArrayValue mem space et ept shape) =
    foldr f (ppr et) shape <+> text "at" <+> ppr mem <> ppr space <+> ept'
    where
      f e s = brackets $ s <> comma <> ppr e
      ept' = case ept of
        TypeUnsigned -> text " (unsigned)"
        TypeDirect -> mempty

instance Pretty ExternalValue where
  ppr (TransparentValue u v) = ppr u <> ppr v
  ppr (OpaqueValue u desc vs) =
    ppr u <> text "opaque" <+> text desc
      <+> nestedBlock "{" "}" (stack $ map ppr vs)

instance Pretty ArrayContents where
  ppr (ArrayValues vs) = braces (commasep $ map ppr vs)
  ppr (ArrayZeros n) = braces (text "0") <+> text "*" <+> ppr n

instance Pretty op => Pretty (Code op) where
  ppr (Op op) = ppr op
  ppr Skip = text "skip"
  ppr (c1 :>>: c2) = ppr c1 </> ppr c2
  ppr (For i limit body) =
    text "for" <+> ppr i <+> langle <+> ppr limit <+> text "{"
      </> indent 2 (ppr body)
      </> text "}"
  ppr (While cond body) =
    text "while" <+> ppr cond <+> text "{"
      </> indent 2 (ppr body)
      </> text "}"
  ppr (DeclareMem name space) =
    text "var" <+> ppr name <> text ": mem" <> ppr space
  ppr (DeclareScalar name vol t) =
    text "var" <+> ppr name <> text ":" <+> vol' <> ppr t
    where
      vol' = case vol of
        Volatile -> text "volatile "
        Nonvolatile -> mempty
  ppr (DeclareArray name space t vs) =
    text "array" <+> ppr name <> text "@" <> ppr space <+> text ":" <+> ppr t
      <+> equals
      <+> ppr vs
  ppr (Allocate name e space) =
    ppr name <+> text "<-" <+> text "malloc" <> parens (ppr e) <> ppr space
  ppr (Free name space) =
    text "free" <> parens (ppr name) <> ppr space
  ppr (Write name i bt space vol val) =
    ppr name <> langle <> vol' <> ppr bt <> ppr space <> rangle <> brackets (ppr i)
      <+> text "<-"
      <+> ppr val
    where
      vol' = case vol of
        Volatile -> text "volatile "
        Nonvolatile -> mempty
  ppr (Read name v is bt space vol) =
    ppr name <+> text "<-"
      <+> ppr v <> langle <> vol' <> ppr bt <> ppr space <> rangle <> brackets (ppr is)
    where
      vol' = case vol of
        Volatile -> text "volatile "
        Nonvolatile -> mempty
  ppr (SetScalar name val) =
    ppr name <+> text "<-" <+> ppr val
  ppr (SetMem dest from DefaultSpace) =
    ppr dest <+> text "<-" <+> ppr from
  ppr (SetMem dest from space) =
    ppr dest <+> text "<-" <+> ppr from <+> text "@" <> ppr space
  ppr (Assert e msg _) =
    text "assert" <> parens (commasep [ppr msg, ppr e])
  ppr (Copy dest destoffset destspace src srcoffset srcspace size) =
    text "memcpy"
      <> parens
        ( ppMemLoc dest destoffset <> ppr destspace <> comma
            </> ppMemLoc src srcoffset <> ppr srcspace <> comma
            </> ppr size
        )
    where
      ppMemLoc base offset =
        ppr base <+> text "+" <+> ppr offset
  ppr (If cond tbranch fbranch) =
    text "if" <+> ppr cond <+> text "then {"
      </> indent 2 (ppr tbranch)
      </> text "} else {"
      </> indent 2 (ppr fbranch)
      </> text "}"
  ppr (Call dests fname args) =
    commasep (map ppr dests) <+> text "<-"
      <+> ppr fname <> parens (commasep $ map ppr args)
  ppr (Comment s code) =
    text "--" <+> text s </> ppr code
  ppr (DebugPrint desc (Just e)) =
    text "debug" <+> parens (commasep [text (show desc), ppr e])
  ppr (DebugPrint desc Nothing) =
    text "debug" <+> parens (text (show desc))
  ppr (TracePrint msg) =
    text "trace" <+> parens (ppr msg)

instance Pretty Arg where
  ppr (MemArg m) = ppr m
  ppr (ExpArg e) = ppr e

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
  traverse f (Function entry outs ins body results args) =
    Function entry outs ins <$> traverse f body <*> pure results <*> pure args

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
  traverse _ (DeclareArray name space t vs) =
    pure $ DeclareArray name space t vs
  traverse _ (Allocate name size s) =
    pure $ Allocate name size s
  traverse _ (Free name space) =
    pure $ Free name space
  traverse _ (Copy dest destoffset destspace src srcoffset srcspace size) =
    pure $ Copy dest destoffset destspace src srcoffset srcspace size
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
declaredIn (DeclareArray name _ _ _) = oneName name
declaredIn (If _ t f) = declaredIn t <> declaredIn f
declaredIn (x :>>: y) = declaredIn x <> declaredIn y
declaredIn (For i _ body) = oneName i <> declaredIn body
declaredIn (While _ body) = declaredIn body
declaredIn (Comment _ body) = declaredIn body
declaredIn _ = mempty

instance FreeIn a => FreeIn (Functions a) where
  freeIn' (Functions fs) = foldMap (onFun . snd) fs
    where
      onFun f =
        fvBind pnames $
          freeIn' (functionBody f) <> freeIn' (functionResult f <> map snd (functionArgs f))
        where
          pnames =
            namesFromList $ map paramName $ functionInput f <> functionOutput f

instance FreeIn ValueDesc where
  freeIn' (ArrayValue mem _ _ _ dims) = freeIn' mem <> freeIn' dims
  freeIn' ScalarValue {} = mempty

instance FreeIn ExternalValue where
  freeIn' (TransparentValue _ vd) = freeIn' vd
  freeIn' (OpaqueValue _ _ vds) = foldMap freeIn' vds

instance FreeIn a => FreeIn (Code a) where
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
  freeIn' (Copy dest x _ src y _ n) =
    freeIn' dest <> freeIn' x <> freeIn' src <> freeIn' y <> freeIn' n
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
