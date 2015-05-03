{-# LANGUAGE TupleSections #-}
-- | Inspired by the paper "Defunctionalizing Push Arrays".
module Futhark.CodeGen.ImpCode
  ( Program
  , ProgramT (..)
  , Function
  , FunctionT (..)
  , ValueDecl (..)
  , Param (..)
  , paramName
  , Size (..)
  , MemSize
  , DimSize
  , Type (..)
  , Space
  , Code (..)
  , Exp (..)
  , UnOp (..)
  -- * Re-exports from other modules.
  , module Language.Futhark.Core
  )
  where

import Control.Applicative
import Data.Monoid
import Data.List hiding (foldr)
import Data.Loc
import Data.Traversable
import Data.Foldable
import qualified Data.HashSet as HS

import Prelude hiding (foldr)

import Language.Futhark.Core
import Futhark.Representation.AST.Syntax (BinOp (..))
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Pretty ()

import Text.PrettyPrint.Mainland hiding (space)

data Size = ConstSize Int
          | VarSize VName
          deriving (Eq, Show)

type MemSize = Size
type DimSize = Size

-- | The memory space of a block.  If 'Nothing', this is the "default"
-- space, whatever that is.  The exact meaning of the string depends
-- on the backend used.  On the GPU, for example, this is used to
-- distinguish between constant, global and shared memory spaces.
type Space = Maybe String

data Type = Scalar BasicType | Mem DimSize Space

data Param = MemParam VName DimSize Space
           | ScalarParam VName BasicType
             deriving (Show)

paramName :: Param -> VName
paramName (MemParam name _ _) = name
paramName (ScalarParam name _) = name

newtype ProgramT a = Program [(Name, Function a)]

type Program = ProgramT

data ValueDecl = ArrayValue VName BasicType [DimSize]
               | ScalarValue BasicType VName
               deriving (Show)

data FunctionT a = Function [Param] [Param] (Code a) [ValueDecl] [ValueDecl]
                 deriving (Show)

type Function = FunctionT

data Code a = Skip
            | Code a :>>: Code a
            | For VName Exp (Code a)
            | While Exp (Code a)
            | DeclareMem VName Space
            | DeclareScalar VName BasicType
            | Allocate VName Exp
            | Copy VName Exp VName Exp Exp
              -- ^ Destination, offset in destination, source, offset
              -- in source, number of bytes.
            | Write VName Exp BasicType Space Exp
            | SetScalar VName Exp
            | SetMem VName VName
            | Call [VName] Name [Exp]
            | If Exp (Code a) (Code a)
            | Assert Exp SrcLoc
            | Op a
            deriving (Show)

data Exp = Constant BasicValue
         | BinOp BinOp Exp Exp
         | UnOp UnOp Exp
         | Index VName Exp BasicType Space
         | ScalarVar VName
         | SizeOf BasicType
           deriving (Eq, Show)

data UnOp = Not
          | Negate
            deriving (Eq, Show)

instance Monoid (Code a) where
  mempty = Skip
  Skip `mappend` y    = y
  x    `mappend` Skip = x
  x    `mappend` y    = x :>>: y

-- Prettyprinting definitions.

instance Pretty op => Pretty (ProgramT op) where
  ppr (Program funs) = stack $ intersperse mempty $ map ppFun funs
    where ppFun (name, fun) =
            text "Function " <> ppr name <> colon </> indent 2 (ppr fun)

instance Pretty op => Pretty (FunctionT op) where
  ppr (Function outs ins body results args) =
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
  ppr (MemParam name size space) =
    text "mem" <> parens (ppr size) <> space' <+> ppr name
    where space' = case space of Just s -> text "@" <> text s
                                 Nothing -> mempty

instance Pretty ValueDecl where
  ppr (ScalarValue t name) =
    ppr t <+> ppr name
  ppr (ArrayValue mem et shape) =
    foldr f (ppr et) shape <+> text "at" <+> ppr mem
    where f e s = brackets $ s <> comma <> ppr e

instance Pretty Size where
  ppr (ConstSize x) = ppr x
  ppr (VarSize v)   = ppr v

instance Pretty op => Pretty (Code op) where
  ppr (Op op) = ppr op
  ppr Skip   = text "skip"
  ppr (c1 :>>: c2) = ppr c1 </> ppr c2
  ppr (For i limit body) =
    text "for" <+> ppr i <+> langle <+> ppr limit <+> text "{" </>
    indent 2 (ppr body) </>
    text "}"
  ppr (While cond body) =
    text "while" <+> ppr cond <+> text "{" </>
    indent 2 (ppr body) </>
    text "}"
  ppr (DeclareMem name space) =
    text "declare" <+> ppr name <+> text "as memory block" <>
    case space of Nothing -> mempty
                  Just s  -> text " at" <+> text s
  ppr (DeclareScalar name t) =
    text "declare" <+> ppr name <+> text "as scalar of type" <+> ppr t
  ppr (Allocate name e) =
    ppr name <+> text "<-" <+> text "malloc" <> parens (ppr e)
  ppr (Write name i bt space val) =
    ppr name <> langle <> ppr bt <> space' <> rangle <> brackets (ppr i) <+>
    text "<-" <+> ppr val
    where space' = case space of Nothing -> mempty
                                 Just s -> text "@" <> text s
  ppr (SetScalar name val) =
    ppr name <+> text "<-" <+> ppr val
  ppr (SetMem dest from) =
    ppr dest <+> text "<-" <+> ppr from
  ppr (Assert e _) =
    text "assert" <> parens (ppr e)
  ppr (Copy dest destoffset src srcoffset size) =
    text "memcpy" <> parens (ppMemLoc dest destoffset <> comma </>
                             ppMemLoc src srcoffset <> comma </>
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

instance Pretty Exp where
  ppr = pprPrec (-1)
  pprPrec _ (Constant v) = ppr v
  pprPrec p (BinOp op x y) =
    parensIf (p >= precedence op) $
    pprPrec (precedence op) x <+/>
    ppr op <+>
    pprPrec (rprecedence op) y
  pprPrec _ (UnOp Not x) =
    text "not" <+> ppr x
  pprPrec _ (UnOp Negate x) =
    text "-" <+> ppr x
  pprPrec _ (ScalarVar v) =
    ppr v
  pprPrec _ (Index v is bt space) =
    ppr v <> langle <> ppr bt <> space' <> rangle <> brackets (ppr is)
    where space' = case space of Nothing -> mempty
                                 Just s -> text "@" <> text s
  pprPrec _ (SizeOf t) =
    text "sizeof" <> parens (ppr t)

precedence :: BinOp -> Int
precedence LogAnd = 0
precedence LogOr = 0
precedence Band = 1
precedence Bor = 1
precedence Xor = 1
precedence Equal = 2
precedence Less = 2
precedence Leq = 2
precedence ShiftL = 3
precedence ShiftR = 3
precedence Plus = 4
precedence Minus = 4
precedence Times = 5
precedence Divide = 5
precedence Mod = 5
precedence Pow = 6

rprecedence :: BinOp -> Int
rprecedence Minus = 10
rprecedence Divide = 10
rprecedence op = precedence op

instance Functor ProgramT where
  fmap = fmapDefault

instance Foldable ProgramT where
  foldMap = foldMapDefault

instance Traversable ProgramT where
  traverse f (Program funs) =
    Program <$> traverse f' funs
    where f' (name, fun) = (name,) <$> traverse f fun

instance Functor FunctionT where
  fmap = fmapDefault

instance Foldable FunctionT where
  foldMap = foldMapDefault

instance Traversable FunctionT where
  traverse f (Function outs ins body results args) =
    Function outs ins <$> traverse f body <*> pure results <*> pure args

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
  traverse _ (DeclareScalar name bt) =
    pure $ DeclareScalar name bt
  traverse _ (Allocate name size) =
    pure $ Allocate name size
  traverse _ (Copy dest destoffset src srcoffset size) =
    pure $ Copy dest destoffset src srcoffset size
  traverse _ (Write name i bt val space) =
    pure $ Write name i bt val space
  traverse _ (SetScalar name val) =
    pure $ SetScalar name val
  traverse _ (SetMem dest from) =
    pure $ SetMem dest from
  traverse _ (Assert e loc) =
    pure $ Assert e loc
  traverse _ (Call dests fname args) =
    pure $ Call dests fname args

declaredIn :: Code a -> Names
declaredIn (DeclareMem name _) = HS.singleton name
declaredIn (DeclareScalar name _) = HS.singleton name
declaredIn (If _ t f) = declaredIn t <> declaredIn f
declaredIn (x :>>: y) = declaredIn x <> declaredIn y
declaredIn (For i _ body) = HS.singleton i <> declaredIn body
declaredIn (While _ body) = declaredIn body
declaredIn _ = mempty

instance FreeIn a => FreeIn (Code a) where
  freeIn (x :>>: y) =
    freeIn x <> freeIn y `HS.difference` declaredIn x
  freeIn Skip =
    mempty
  freeIn (For i bound body) =
    i `HS.delete` (freeIn bound <> freeIn body)
  freeIn (While cond body) =
    freeIn cond <> freeIn body
  freeIn (DeclareMem {}) =
    mempty
  freeIn (DeclareScalar {}) =
    mempty
  freeIn (Allocate name size) =
    freeIn name <> freeIn size
  freeIn (Copy dest x src y n) =
    freeIn dest <> freeIn x <> freeIn src <> freeIn y <> freeIn n
  freeIn (SetMem x y) =
    freeIn x <> freeIn y
  freeIn (Write v i _ _ e) =
    freeIn v <> freeIn i <> freeIn e
  freeIn (SetScalar x y) =
    freeIn x <> freeIn y
  freeIn (Call dests _ args) =
    freeIn dests <> freeIn args
  freeIn (If cond t f) =
    freeIn cond <> freeIn t <> freeIn f
  freeIn (Assert e _) =
    freeIn e
  freeIn (Op op) =
    freeIn op

instance FreeIn Exp where
  freeIn (Constant _) = mempty
  freeIn (BinOp _ x y) = freeIn x <> freeIn y
  freeIn (UnOp _ x) = freeIn x
  freeIn (Index v e _ _) = freeIn v <> freeIn e
  freeIn (ScalarVar v) = freeIn v
  freeIn (SizeOf _) = mempty
