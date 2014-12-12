-- | Inspired by the paper "Defunctionalizing Push Arrays".
module Futhark.CodeGen.ImpCode
  ( Program (..)
  , Function (..)
  , ValueDecl (..)
  , Param (..)
  , paramName
  , DimSize (..)
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

import Language.Futhark.Core

import Text.PrettyPrint.Mainland

data DimSize = ConstSize Int
             | VarSize VName
               deriving (Eq, Show)

data Type = Scalar BasicType | Mem DimSize

data Param = MemParam VName DimSize
           | ScalarParam VName BasicType
             deriving (Show)

paramName :: Param -> VName
paramName (MemParam name _) = name
paramName (ScalarParam name _) = name

newtype Program a = Program [(Name, Function a)]

data ValueDecl = ArrayValue VName BasicType [DimSize]
               | ScalarValue BasicType VName
               deriving (Show)

data Function a = Function [Param] [Param] (Code a) [ValueDecl] [ValueDecl]
                  deriving (Show)

data Code a = Skip
            | Code a :>>: Code a
            | For VName Exp (Code a)
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
           deriving (Show)

data UnOp = Not
          | Negate
            deriving (Show)

instance Monoid (Code a) where
  mempty = Skip
  Skip `mappend` y    = y
  x    `mappend` Skip = x
  x    `mappend` y    = x :>>: y

-- Prettyprinting definitions.

instance Pretty (Program op) where
  ppr (Program funs) = stack $ intersperse mempty $ map ppFun funs
    where ppFun (name, fun) =
            text "Function " <> ppr name <> colon </> indent 2 (ppr fun)

instance Pretty (Function op) where
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

instance Pretty DimSize where
  ppr (ConstSize x) = ppr x
  ppr (VarSize v)   = ppr v

instance Pretty (Code op) where
  ppr (Op _) = text "#<foreign operation>"
  ppr Skip   = text "skip"
  ppr (c1 :>>: c2) = ppr c1 </> ppr c2
  ppr (For i limit body) =
    text "for" <+> ppr i <+> langle <+> ppr limit <+> text "{" </>
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
    text "memcpy" <> parens (ppMemLoc dest destoffset <> comma <+>
                             ppMemLoc src srcoffset <> comma <+>
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
    text (opStr op) <+>
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
