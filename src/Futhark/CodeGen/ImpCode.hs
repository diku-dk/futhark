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
  , Code (..)
  , Exp (..)
  , UnOp (..)
  -- * Re-exports from other modules.
  , module Language.Futhark.Core
  )
  where

import Data.Monoid
import Data.List
import Data.Loc
import Data.Traversable

import Language.Futhark.Core
import Futhark.Representation.AST.Syntax (BinOp (..))
import Futhark.Representation.AST.Pretty ()

import Text.PrettyPrint.Mainland

data Size = ConstSize Int
          | VarSize VName
          deriving (Eq, Show)

type MemSize = Size
type DimSize = Size

data Type = Scalar BasicType | Mem DimSize

data Param = MemParam VName DimSize
           | ScalarParam VName BasicType
             deriving (Show)

paramName :: Param -> VName
paramName (MemParam name _) = name
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
            | DeclareMem VName
            | DeclareScalar VName BasicType
            | Allocate VName Exp
            | Copy VName Exp VName Exp Exp
              -- ^ Destination, offset in destination, source, offset
              -- in source, number of bytes.
            | Write VName Exp BasicType Exp
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
         | Index VName Exp BasicType
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
  ppr (MemParam name size) =
    text "mem" <> parens (ppr size) <+> ppr name

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
  ppr (DeclareMem name) =
    text "declare" <+> ppr name <+> text "as memory block"
  ppr (DeclareScalar name t) =
    text "declare" <+> ppr name <+> text "as scalar of type" <+> ppr t
  ppr (Allocate name e) =
    ppr name <+> text "<-" <+> text "malloc" <> parens (ppr e)
  ppr (Write name i bt val) =
    ppr name <> langle <> ppr bt <> rangle <> brackets (ppr i) <+>
    text "<-" <+> ppr val
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
  pprPrec _ (Index v is bt) =
    ppr v <> langle <> ppr bt <> rangle <> brackets (ppr is)
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
  traverse _ (DeclareMem name) =
    pure $ DeclareMem name
  traverse _ (DeclareScalar name bt) =
    pure $ DeclareScalar name bt
  traverse _ (Allocate name size) =
    pure $ Allocate name size
  traverse _ (Copy dest destoffset src srcoffset size) =
    pure $ Copy dest destoffset src srcoffset size
  traverse _ (Write name i bt val) =
    pure $ Write name i bt val
  traverse _ (SetScalar name val) =
    pure $ SetScalar name val
  traverse _ (SetMem dest from) =
    pure $ SetMem dest from
  traverse _ (Assert e loc) =
    pure $ Assert e loc
  traverse _ (Call dests fname args) =
    pure $ Call dests fname args
