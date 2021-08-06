module Futhark.CodeGen.Backends.GenericPython.AST
  ( PyExp (..),
    PyIdx (..),
    PyArg (..),
    PyStmt (..),
    module Language.Futhark.Core,
    PyProg (..),
    PyExcept (..),
    PyFunDef (..),
    PyClassDef (..),
  )
where

import qualified Data.Text as T
import Futhark.Util.Pretty
import Language.Futhark.Core

data UnOp
  = -- | Boolean negation.
    Not
  | -- | Bitwise complement.
    Complement
  | -- | Numerical negation.
    Negate
  | -- | Absolute/numerical value.
    Abs
  deriving (Eq, Show)

data PyExp
  = Integer Integer
  | Bool Bool
  | Float Double
  | String String
  | RawStringLiteral T.Text
  | Var String
  | BinOp String PyExp PyExp
  | UnOp String PyExp
  | Cond PyExp PyExp PyExp
  | Index PyExp PyIdx
  | Call PyExp [PyArg]
  | Cast PyExp String
  | Tuple [PyExp]
  | List [PyExp]
  | Field PyExp String
  | Dict [(PyExp, PyExp)]
  | Lambda String PyExp
  | None
  deriving (Eq, Show)

data PyIdx
  = IdxRange PyExp PyExp
  | IdxExp PyExp
  deriving (Eq, Show)

data PyArg
  = ArgKeyword String PyExp
  | Arg PyExp
  deriving (Eq, Show)

data PyStmt
  = If PyExp [PyStmt] [PyStmt]
  | Try [PyStmt] [PyExcept]
  | While PyExp [PyStmt]
  | For String PyExp [PyStmt]
  | With PyExp [PyStmt]
  | Assign PyExp PyExp
  | AssignOp String PyExp PyExp
  | Comment String [PyStmt]
  | Assert PyExp PyExp
  | Raise PyExp
  | Exp PyExp
  | Return PyExp
  | Pass
  | -- Definition-like statements.
    Import String (Maybe String)
  | FunDef PyFunDef
  | ClassDef PyClassDef
  | -- Some arbitrary string of Python code.
    Escape T.Text
  deriving (Eq, Show)

data PyExcept = Catch PyExp [PyStmt]
  deriving (Eq, Show)

data PyFunDef = Def String [String] [PyStmt]
  deriving (Eq, Show)

data PyClassDef = Class String [PyStmt]
  deriving (Eq, Show)

newtype PyProg = PyProg [PyStmt]
  deriving (Eq, Show)

instance Pretty PyIdx where
  ppr (IdxExp e) = ppr e
  ppr (IdxRange from to) = ppr from <> text ":" <> ppr to

instance Pretty PyArg where
  ppr (ArgKeyword k e) = text k <> equals <> ppr e
  ppr (Arg e) = ppr e

instance Pretty PyExp where
  ppr (Integer x) = ppr x
  ppr (Bool x) = ppr x
  ppr (Float x)
    | isInfinite x = text $ if x > 0 then "float('inf')" else "float('-inf')"
    | otherwise = ppr x
  ppr (String x) = text $ show x
  ppr (RawStringLiteral s) = text "\"\"\"" <> strictText s <> text "\"\"\""
  ppr (Var n) = text $ map (\x -> if x == '\'' then 'm' else x) n
  ppr (Field e s) = ppr e <> text "." <> text s
  ppr (BinOp s e1 e2) = parens (ppr e1 <+> text s <+> ppr e2)
  ppr (UnOp s e) = text s <> parens (ppr e)
  ppr (Cond e1 e2 e3) = ppr e2 <+> text "if" <+> ppr e1 <+> text "else" <+> ppr e3
  ppr (Cast src bt) =
    text "ct.cast"
      <> parens
        ( ppr src <> text ","
            <+> text "ct.POINTER" <> parens (text bt)
        )
  ppr (Index src idx) = ppr src <> brackets (ppr idx)
  ppr (Call fun exps) = ppr fun <> parens (commasep $ map ppr exps)
  ppr (Tuple [dim]) = parens (ppr dim <> text ",")
  ppr (Tuple dims) = parens (commasep $ map ppr dims)
  ppr (List es) = brackets $ commasep $ map ppr es
  ppr (Dict kvs) = braces $ commasep $ map ppElem kvs
    where
      ppElem (k, v) = ppr k <> colon <+> ppr v
  ppr (Lambda p e) = text "lambda" <+> text p <> text ":" <+> ppr e
  ppr None = text "None"

instance Pretty PyStmt where
  ppr (If cond [] []) =
    text "if" <+> ppr cond <> text ":"
      </> indent 2 (text "pass")
  ppr (If cond [] fbranch) =
    text "if" <+> ppr cond <> text ":"
      </> indent 2 (text "pass")
      </> text "else:"
      </> indent 2 (stack $ map ppr fbranch)
  ppr (If cond tbranch []) =
    text "if" <+> ppr cond <> text ":"
      </> indent 2 (stack $ map ppr tbranch)
  ppr (If cond tbranch fbranch) =
    text "if" <+> ppr cond <> text ":"
      </> indent 2 (stack $ map ppr tbranch)
      </> text "else:"
      </> indent 2 (stack $ map ppr fbranch)
  ppr (Try pystms pyexcepts) =
    text "try:"
      </> indent 2 (stack $ map ppr pystms)
      </> stack (map ppr pyexcepts)
  ppr (While cond body) =
    text "while" <+> ppr cond <> text ":"
      </> indent 2 (stack $ map ppr body)
  ppr (For i what body) =
    text "for" <+> ppr i <+> text "in" <+> ppr what <> text ":"
      </> indent 2 (stack $ map ppr body)
  ppr (With what body) =
    text "with" <+> ppr what <> text ":"
      </> indent 2 (stack $ map ppr body)
  ppr (Assign e1 e2) = ppr e1 <+> text "=" <+> ppr e2
  ppr (AssignOp op e1 e2) = ppr e1 <+> text (op ++ "=") <+> ppr e2
  ppr (Comment s body) = text "#" <> text s </> stack (map ppr body)
  ppr (Assert e1 e2) = text "assert" <+> ppr e1 <> text "," <+> ppr e2
  ppr (Raise e) = text "raise" <+> ppr e
  ppr (Exp c) = ppr c
  ppr (Return e) = text "return" <+> ppr e
  ppr Pass = text "pass"
  ppr (Import from (Just as)) =
    text "import" <+> text from <+> text "as" <+> text as
  ppr (Import from Nothing) =
    text "import" <+> text from
  ppr (FunDef d) = ppr d
  ppr (ClassDef d) = ppr d
  ppr (Escape s) = stack $ map strictText $ T.lines s

instance Pretty PyFunDef where
  ppr (Def fname params body) =
    text "def" <+> text fname <> parens (commasep $ map ppr params) <> text ":"
      </> indent 2 (stack (map ppr body))

instance Pretty PyClassDef where
  ppr (Class cname body) =
    text "class" <+> text cname <> text ":"
      </> indent 2 (stack (map ppr body))

instance Pretty PyExcept where
  ppr (Catch pyexp stms) =
    text "except" <+> ppr pyexp <+> text "as e:"
      </> indent 2 (stack $ map ppr stms)

instance Pretty PyProg where
  ppr (PyProg stms) = stack (map ppr stms)
