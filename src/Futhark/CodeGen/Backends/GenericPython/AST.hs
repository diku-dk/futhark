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

import Data.Text qualified as T
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
  | String T.Text
  | RawStringLiteral T.Text
  | Var String
  | BinOp String PyExp PyExp
  | UnOp String PyExp
  | Cond PyExp PyExp PyExp
  | Index PyExp PyIdx
  | Call PyExp [PyArg]
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
  pretty (IdxExp e) = pretty e
  pretty (IdxRange from to) = pretty from <> ":" <> pretty to

instance Pretty PyArg where
  pretty (ArgKeyword k e) = pretty k <> equals <> pretty e
  pretty (Arg e) = pretty e

instance Pretty PyExp where
  pretty (Integer x) = pretty x
  pretty (Bool x) = pretty x
  pretty (Float x)
    | isInfinite x = if x > 0 then "float('inf')" else "float('-inf')"
    | otherwise = pretty x
  pretty (String x) = pretty $ show x
  pretty (RawStringLiteral s) = "\"\"\"" <> pretty s <> "\"\"\""
  pretty (Var n) = pretty $ map (\x -> if x == '\'' then 'm' else x) n
  pretty (Field e s) = pretty e <> "." <> pretty s
  pretty (BinOp s e1 e2) = parens (pretty e1 <+> pretty s <+> pretty e2)
  pretty (UnOp s e) = pretty s <> parens (pretty e)
  pretty (Cond e1 e2 e3) = pretty e2 <+> "if" <+> pretty e1 <+> "else" <+> pretty e3
  pretty (Index src idx) = pretty src <> brackets (pretty idx)
  pretty (Call fun exps) = pretty fun <> parens (commasep $ map pretty exps)
  pretty (Tuple [dim]) = parens (pretty dim <> ",")
  pretty (Tuple dims) = parens (commasep $ map pretty dims)
  pretty (List es) = brackets $ commasep $ map pretty es
  pretty (Dict kvs) = braces $ commasep $ map ppElem kvs
    where
      ppElem (k, v) = pretty k <> colon <+> pretty v
  pretty (Lambda p e) = "lambda" <+> pretty p <> ":" <+> pretty e
  pretty None = "None"

instance Pretty PyStmt where
  pretty (If cond [] []) =
    "if"
      <+> pretty cond
        <> ":"
      </> indent 2 "pass"
  pretty (If cond [] fbranch) =
    "if"
      <+> pretty cond
        <> ":"
      </> indent 2 "pass"
      </> "else:"
      </> indent 2 (stack $ map pretty fbranch)
  pretty (If cond tbranch []) =
    "if"
      <+> pretty cond
        <> ":"
      </> indent 2 (stack $ map pretty tbranch)
  pretty (If cond tbranch fbranch) =
    "if"
      <+> pretty cond
        <> ":"
      </> indent 2 (stack $ map pretty tbranch)
      </> "else:"
      </> indent 2 (stack $ map pretty fbranch)
  pretty (Try pystms pyexcepts) =
    "try:"
      </> indent 2 (stack $ map pretty pystms)
      </> stack (map pretty pyexcepts)
  pretty (While cond body) =
    "while"
      <+> pretty cond
        <> ":"
      </> indent 2 (stack $ map pretty body)
  pretty (For i what body) =
    "for"
      <+> pretty i
      <+> "in"
      <+> pretty what
        <> ":"
      </> indent 2 (stack $ map pretty body)
  pretty (With what body) =
    "with"
      <+> pretty what
        <> ":"
      </> indent 2 (stack $ map pretty body)
  pretty (Assign e1 e2) = pretty e1 <+> "=" <+> pretty e2
  pretty (AssignOp op e1 e2) = pretty e1 <+> pretty (op ++ "=") <+> pretty e2
  pretty (Comment s body) = "#" <> pretty s </> stack (map pretty body)
  pretty (Assert e1 e2) = "assert" <+> pretty e1 <> "," <+> pretty e2
  pretty (Raise e) = "raise" <+> pretty e
  pretty (Exp c) = pretty c
  pretty (Return e) = "return" <+> pretty e
  pretty Pass = "pass"
  pretty (Import from (Just as)) =
    "import" <+> pretty from <+> "as" <+> pretty as
  pretty (Import from Nothing) =
    "import" <+> pretty from
  pretty (FunDef d) = pretty d
  pretty (ClassDef d) = pretty d
  pretty (Escape s) = stack $ map pretty $ T.lines s

instance Pretty PyFunDef where
  pretty (Def fname params body) =
    "def"
      <+> pretty fname
        <> parens (commasep $ map pretty params)
        <> ":"
      </> indent 2 (stack (map pretty body))

instance Pretty PyClassDef where
  pretty (Class cname body) =
    "class"
      <+> pretty cname
        <> ":"
      </> indent 2 (stack (map pretty body))

instance Pretty PyExcept where
  pretty (Catch pyexp stms) =
    "except"
      <+> pretty pyexp
      <+> "as e:"
      </> indent 2 (vsep $ map pretty stms)

instance Pretty PyProg where
  pretty (PyProg stms) = vsep (map pretty stms)
