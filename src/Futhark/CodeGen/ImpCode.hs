{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
-- | Imperative intermediate language used as a stepping stone in code generation.
--
-- This is a generic representation parametrised on an extensible
-- arbitrary operation.
--
-- Originally inspired by the paper "Defunctionalizing Push Arrays"
-- (FHPC '14).
module Futhark.CodeGen.ImpCode
  ( Functions (..)
  , Function
  , FunctionT (..)
  , ValueDesc (..)
  , Signedness (..)
  , ExternalValue (..)
  , Param (..)
  , paramName
  , Size (..)
  , MemSize
  , DimSize
  , Type (..)
  , Space (..)
  , SpaceId
  , Code (..)
  , PrimValue (..)
  , ExpLeaf (..)
  , Exp
  , Volatility (..)
  , Arg (..)
  , var
  , vi32
  , index
  , ErrorMsg(..)
  , ErrorMsgPart(..)
  , ArrayContents(..)

    -- * Typed enumerations
  , Bytes
  , Elements
  , elements
  , bytes
  , withElemType

    -- * Converting from sizes
  , sizeToExp
  , dimSizeToExp
  , memSizeToExp

    -- * Analysis

    -- * Re-exports from other modules.
  , module Language.Futhark.Core
  , module Futhark.Representation.Primitive
  , module Futhark.Analysis.PrimExp
  , module Futhark.Representation.Kernels.Sizes
  )
  where

import Data.List
import Data.Loc
import Data.Traversable

import Language.Futhark.Core
import Futhark.Representation.Primitive
import Futhark.Representation.AST.Syntax
  (Space(..), SpaceId, ErrorMsg(..), ErrorMsgPart(..))
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Pretty ()
import Futhark.Analysis.PrimExp
import Futhark.Util.Pretty hiding (space)
import Futhark.Representation.Kernels.Sizes (Count(..))

data Size = ConstSize Int64
          | VarSize VName
          deriving (Eq, Show)

type MemSize = Size
type DimSize = Size

data Type = Scalar PrimType | Mem MemSize Space

data Param = MemParam VName Space
           | ScalarParam VName PrimType
             deriving (Show)

paramName :: Param -> VName
paramName (MemParam name _) = name
paramName (ScalarParam name _) = name

-- | A collection of imperative functions.
newtype Functions a = Functions [(Name, Function a)]

instance Semigroup (Functions a) where
  Functions x <> Functions y = Functions $ x ++ y

instance Monoid (Functions a) where
  mempty = Functions []

data Signedness = TypeUnsigned
                | TypeDirect
                deriving (Eq, Show)

-- | A description of an externally meaningful value.
data ValueDesc = ArrayValue VName Space PrimType Signedness [DimSize]
               -- ^ An array with memory block, memory block size,
               -- memory space, element type, signedness of element
               -- type (if applicable), and shape.
               | ScalarValue PrimType Signedness VName
               -- ^ A scalar value with signedness if applicable.
               deriving (Eq, Show)

-- | ^ An externally visible value.  This can be an opaque value
-- (covering several physical internal values), or a single value that
-- can be used externally.
data ExternalValue = OpaqueValue String [ValueDesc]
                     -- ^ The string is a human-readable description
                     -- with no other semantics.
                   | TransparentValue ValueDesc
                 deriving (Show)

-- | A imperative function, containing the body as well as its
-- low-level inputs and outputs, as well as its high-level arguments
-- and results.  The latter are only used if the function is an entry
-- point.
data FunctionT a = Function { functionEntry :: Bool
                            , functionOutput :: [Param]
                            , functionInput :: [Param]
                            , functionbBody :: Code a
                            , functionResult :: [ExternalValue]
                            , functionArgs :: [ExternalValue]
                            }
                 deriving (Show)

-- | Type alias for namespace control.
type Function = FunctionT

-- | The contents of a statically declared constant array.  Such
-- arrays are always unidimensional, and reshaped if necessary in the
-- code that uses them.
data ArrayContents = ArrayValues [PrimValue]
                     -- ^ Precisely these values.
                   | ArrayZeros Int
                     -- ^ This many zeroes.
                     deriving (Show)

data Code a = Skip
            | Code a :>>: Code a
            | For VName IntType Exp (Code a)
            | While Exp (Code a)
            | DeclareMem VName Space
            | DeclareScalar VName PrimType
            | DeclareArray VName Space PrimType ArrayContents
              -- ^ Create an array containing the given values.  The
              -- lifetime of the array will be the entire application.
              -- This is mostly used for constant arrays, but also for
              -- some bookkeeping data, like the synchronisation
              -- counts used to implement reduction.
            | Allocate VName (Count Bytes Exp) Space
              -- ^ Memory space must match the corresponding
              -- 'DeclareMem'.
            | Free VName Space
              -- ^ Indicate that some memory block will never again be
              -- referenced via the indicated variable.  However, it
              -- may still be accessed through aliases.  It is only
              -- safe to actually deallocate the memory block if this
              -- is the last reference.  There is no guarantee that
              -- all memory blocks will be freed with this statement.
              -- Backends are free to ignore it entirely.
            | Copy VName (Count Bytes Exp) Space VName (Count Bytes Exp) Space (Count Bytes Exp)
              -- ^ Destination, offset in destination, destination
              -- space, source, offset in source, offset space, number
              -- of bytes.
            | Write VName (Count Elements Exp) PrimType Space Volatility Exp
            | SetScalar VName Exp
            | SetMem VName VName Space
              -- ^ Must be in same space.
            | Call [VName] Name [Arg]
            | If Exp (Code a) (Code a)
            | Assert Exp (ErrorMsg Exp) (SrcLoc, [SrcLoc])
            | Comment String (Code a)
              -- ^ Has the same semantics as the contained code, but
              -- the comment should show up in generated code for ease
              -- of inspection.
            | DebugPrint String (Maybe Exp)
              -- ^ Print the given value to the screen, somehow
              -- annotated with the given string as a description.  If
              -- no type/value pair, just print the string.  This has
              -- no semantic meaning, but is used entirely for
              -- debugging.  Code generators are free to ignore this
              -- statement.
            | Op a
            deriving (Show)

-- | The volatility of a memory access.
data Volatility = Volatile | Nonvolatile
                deriving (Eq, Ord, Show)

instance Semigroup (Code a) where
  Skip <> y    = y
  x    <> Skip = x
  x    <> y    = x :>>: y

instance Monoid (Code a) where
  mempty = Skip

data ExpLeaf = ScalarVar VName
             | SizeOf PrimType
             | Index VName (Count Elements Exp) PrimType Space Volatility
           deriving (Eq, Show)

type Exp = PrimExp ExpLeaf

-- | A function call argument.
data Arg = ExpArg Exp
         | MemArg VName
         deriving (Show)

-- | Phantom type for a count of elements.
data Elements

-- | Phantom type for a count of bytes.
data Bytes

elements :: Exp -> Count Elements Exp
elements = Count

bytes :: Exp -> Count Bytes Exp
bytes = Count

-- | Convert a count of elements into a count of bytes, given the
-- per-element size.
withElemType :: Count Elements Exp -> PrimType -> Count Bytes Exp
withElemType (Count e) t = bytes $ e * LeafExp (SizeOf t) (IntType Int32)

dimSizeToExp :: DimSize -> Count Elements Exp
dimSizeToExp = elements . sizeToExp

memSizeToExp :: MemSize -> Count Bytes Exp
memSizeToExp = bytes . sizeToExp

sizeToExp :: Size -> Exp
sizeToExp (VarSize v)   = LeafExp (ScalarVar v) (IntType Int32)
sizeToExp (ConstSize x) = ValueExp $ IntValue $ Int32Value $ fromIntegral x

var :: VName -> PrimType -> Exp
var = LeafExp . ScalarVar

-- | Turn a 'VName' into a 'int32' 'Imp.ScalarVar'.
vi32 :: VName -> Exp
vi32 = flip var $ IntType Int32

index :: VName -> Count Elements Exp -> PrimType -> Space -> Volatility -> Exp
index arr i t s vol = LeafExp (Index arr i t s vol) t

-- Prettyprinting definitions.

instance Pretty op => Pretty (Functions op) where
  ppr (Functions funs) = stack $ intersperse mempty $ map ppFun funs
    where ppFun (name, fun) =
            text "Function " <> ppr name <> colon </> indent 2 (ppr fun)

instance Pretty op => Pretty (FunctionT op) where
  ppr (Function _ outs ins body results args) =
    text "Inputs:" </> block ins </>
    text "Outputs:" </> block outs </>
    text "Arguments:" </> block args </>
    text "Result:" </> block results </>
    text "Body:" </> indent 2 (ppr body)
    where block :: Pretty a => [a] -> Doc
          block = indent 2 . stack . map ppr

instance Pretty Param where
  ppr (ScalarParam name ptype) =
    ppr ptype <+> ppr name
  ppr (MemParam name space) =
    text "mem" <> space' <+> ppr name
    where space' = case space of Space s      -> text "@" <> text s
                                 DefaultSpace -> mempty

instance Pretty ValueDesc where
  ppr (ScalarValue t ept name) =
    ppr t <+> ppr name <> ept'
    where ept' = case ept of TypeUnsigned -> text " (unsigned)"
                             TypeDirect   -> mempty
  ppr (ArrayValue mem space et ept shape) =
    foldr f (ppr et) shape <+> text "at" <+> ppr mem <> space' <+> ept'
    where f e s = brackets $ s <> comma <> ppr e
          ept' = case ept of TypeUnsigned -> text " (unsigned)"
                             TypeDirect   -> mempty
          space' = case space of Space s      -> text "@" <> text s
                                 DefaultSpace -> mempty


instance Pretty ExternalValue where
  ppr (TransparentValue v) = ppr v
  ppr (OpaqueValue desc vs) =
    text "opaque" <+> text desc <+>
    nestedBlock "{" "}" (stack $ map ppr vs)

instance Pretty Size where
  ppr (ConstSize x) = ppr x
  ppr (VarSize v)   = ppr v

instance Pretty ArrayContents where
  ppr (ArrayValues vs) = braces (commasep $ map ppr vs)
  ppr (ArrayZeros n) = braces (text "0") <+> text "*" <+> ppr n

instance Pretty op => Pretty (Code op) where
  ppr (Op op) = ppr op
  ppr Skip   = text "skip"
  ppr (c1 :>>: c2) = ppr c1 </> ppr c2
  ppr (For i it limit body) =
    text "for" <+> ppr i <> text ":" <> ppr it <+> langle <+> ppr limit <+> text "{" </>
    indent 2 (ppr body) </>
    text "}"
  ppr (While cond body) =
    text "while" <+> ppr cond <+> text "{" </>
    indent 2 (ppr body) </>
    text "}"
  ppr (DeclareMem name space) =
    text "var" <+> ppr name <> text ": mem" <> parens (ppr space)
  ppr (DeclareScalar name t) =
    text "var" <+> ppr name <> text ":" <+> ppr t
  ppr (DeclareArray name space t vs) =
    text "array" <+> ppr name <> text "@" <> ppr space <+> text ":" <+> ppr t <+>
    equals <+> ppr vs
  ppr (Allocate name e space) =
    ppr name <+> text "<-" <+> text "malloc" <> parens (ppr e) <> ppr space
  ppr (Free name space) =
    text "free" <> parens (ppr name) <> ppr space
  ppr (Write name i bt space vol val) =
    ppr name <> langle <> vol' <> ppr bt <> ppr space <> rangle <> brackets (ppr i) <+>
    text "<-" <+> ppr val
    where vol' = case vol of Volatile -> text "volatile "
                             Nonvolatile -> mempty
  ppr (SetScalar name val) =
    ppr name <+> text "<-" <+> ppr val
  ppr (SetMem dest from space) =
    ppr dest <+> text "<-" <+> ppr from <+> text "@" <> ppr space
  ppr (Assert e msg _) =
    text "assert" <> parens (commasep [ppr msg, ppr e])
  ppr (Copy dest destoffset destspace src srcoffset srcspace size) =
    text "memcpy" <>
    parens (ppMemLoc dest destoffset <> ppr destspace <> comma </>
            ppMemLoc src srcoffset <> ppr srcspace <> comma </>
            ppr size)
    where ppMemLoc base offset =
            ppr base <+> text "+" <+> ppr offset
  ppr (If cond tbranch fbranch) =
    text "if" <+> ppr cond <+> text "then {" </>
    indent 2 (ppr tbranch) </>
    text "} else {" </>
    indent 2 (ppr fbranch) </>
    text "}"
  ppr (Call dests fname args) =
    commasep (map ppr dests) <+> text "<-" <+>
    ppr fname <> parens (commasep $ map ppr args)
  ppr (Comment s code) =
    text "--" <+> text s </> ppr code
  ppr (DebugPrint desc (Just e)) =
    text "debug" <+> parens (commasep [text (show desc), ppr e])
  ppr (DebugPrint desc Nothing) =
    text "debug" <+> parens (text (show desc))

instance Pretty Arg where
  ppr (MemArg m) = ppr m
  ppr (ExpArg e) = ppr e

instance Pretty ExpLeaf where
  ppr (ScalarVar v) =
    ppr v
  ppr (Index v is bt space vol) =
    ppr v <> langle <> vol' <> ppr bt <> space' <> rangle <> brackets (ppr is)
    where space' = case space of DefaultSpace -> mempty
                                 Space s      -> text "@" <> text s
          vol' = case vol of Volatile -> text "volatile "
                             Nonvolatile -> mempty

  ppr (SizeOf t) =
    text "sizeof" <> parens (ppr t)

instance Functor Functions where
  fmap = fmapDefault

instance Foldable Functions where
  foldMap = foldMapDefault

instance Traversable Functions where
  traverse f (Functions funs) =
    Functions <$> traverse f' funs
    where f' (name, fun) = (name,) <$> traverse f fun

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
  traverse f (For i it bound code) =
    For i it bound <$> traverse f code
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
  traverse _ (DeclareScalar name bt) =
    pure $ DeclareScalar name bt
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

declaredIn :: Code a -> Names
declaredIn (DeclareMem name _) = oneName name
declaredIn (DeclareScalar name _) = oneName name
declaredIn (DeclareArray name _ _ _) = oneName name
declaredIn (If _ t f) = declaredIn t <> declaredIn f
declaredIn (x :>>: y) = declaredIn x <> declaredIn y
declaredIn (For i _ _ body) = oneName i <> declaredIn body
declaredIn (While _ body) = declaredIn body
declaredIn (Comment _ body) = declaredIn body
declaredIn _ = mempty

instance FreeIn a => FreeIn (Code a) where
  freeIn' (x :>>: y) =
    fvBind (declaredIn x) $ freeIn' x <> freeIn' y
  freeIn' Skip =
    mempty
  freeIn' (For i _ bound body) =
    fvBind (oneName i) $ freeIn' bound <> freeIn' body
  freeIn' (While cond body) =
    freeIn' cond <> freeIn' body
  freeIn' DeclareMem{} =
    mempty
  freeIn' DeclareScalar{} =
    mempty
  freeIn' DeclareArray{} =
    mempty
  freeIn' (Allocate name size _) =
    freeIn' name <> freeIn' size
  freeIn' (Free name _) =
    freeIn' name
  freeIn' (Copy dest x _ src y _ n) =
    freeIn' dest <> freeIn' x <> freeIn' src <> freeIn' y <> freeIn' n
  freeIn' (SetMem x y _) =
    freeIn' x <> freeIn' y
  freeIn' (Write v i _ _ _ e) =
    freeIn' v <> freeIn' i <> freeIn' e
  freeIn' (SetScalar x y) =
    freeIn' x <> freeIn' y
  freeIn' (Call dests _ args) =
    freeIn' dests <> freeIn' args
  freeIn' (If cond t f) =
    freeIn' cond <> freeIn' t <> freeIn' f
  freeIn' (Assert e _ _) =
    freeIn' e
  freeIn' (Op op) =
    freeIn' op
  freeIn' (Comment _ code) =
    freeIn' code
  freeIn' (DebugPrint _ v) =
    maybe mempty freeIn' v

instance FreeIn ExpLeaf where
  freeIn' (Index v e _ _ _) = freeIn' v <> freeIn' e
  freeIn' (ScalarVar v) = freeIn' v
  freeIn' (SizeOf _) = mempty

instance FreeIn Arg where
  freeIn' (MemArg m) = freeIn' m
  freeIn' (ExpArg e) = freeIn' e

instance FreeIn Size where
  freeIn' (VarSize name) = fvName name
  freeIn' (ConstSize _) = mempty
