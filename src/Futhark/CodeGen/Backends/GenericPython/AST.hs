module Futhark.CodeGen.Backends.GenericPython.AST
  ( PyExp(..)
  , PyStmt(..)
  , module Language.Futhark.Core
  , PyFunc(..)
  , PyProg(..)
  )
  where

import Language.Futhark.Core
import Text.PrettyPrint.Mainland hiding (space)

type VariableName = String

data UnOp = Not -- ^ Boolean negation.
          | Complement -- ^ Bitwise complement.
          | Negate -- ^ Numerical negation.
          | Abs -- ^ Absolute/numerical value.
            deriving (Eq, Show)

data PyExp = Constant BasicValue
           | ScalarVar VariableName
           | BinaryOp String PyExp PyExp
           | UnOp String PyExp
           | Cond PyExp PyExp PyExp
             deriving (Eq, Show)

data PyStmt = If PyExp [PyStmt] [PyStmt]
            | While PyExp [PyStmt]
            | For VariableName PyExp [PyStmt]
            | AssignVar VariableName PyExp
            deriving (Eq, Show)

data PyReturn = PyReturnScalar PyExp
              | PyReturnTuple [PyExp]
              | PyReturnList [PyExp]


data PyFunc = PyFunc String [String] [PyStmt] --PyReturn

data PyProg = PyProg [PyFunc]

instance Pretty PyExp where
    ppr (Constant v) = ppr v
    ppr (ScalarVar n) = text n
    ppr (BinaryOp s e1 e2) = ppr e1 <> text s <> ppr e2
    ppr (UnOp s e) = text s <> parens (ppr e)
    ppr (Cond e1 e2 e3) = ppr e2 <+> text "if" <+> ppr e1 <+> text "else" <+> ppr e3

instance Pretty PyStmt where
  ppr (If cond tbranch fbranch) =
    text "if" <+> ppr cond <+> text ":" </>
    indent 2 (stack $ map ppr tbranch) </>
    text "else:" </>
    indent 2 (stack $ map ppr fbranch)
  ppr (While cond body) =
    text "while" <+> ppr cond <+> text ":" </>
    indent 2 (stack $ map ppr body)
  ppr (For i limit body) =
    text  "for" <+> ppr i <+> text "in range" <+> parens (ppr limit) <+> text ":" </>
    indent 2 (stack $ map ppr body)
  ppr (AssignVar v e) = text v <+> text "=" <+> ppr e

instance Pretty PyReturn where
  ppr (PyReturnScalar expr) = ppr expr
  ppr (PyReturnTuple exprs) = parens (stack $ map ppr exprs)
  ppr (PyReturnList exprs) = text "[" <> stack (map ppr exprs) <> text "]"

instance Pretty PyFunc where
  ppr (PyFunc fname params body) =
    text "def" <+> text fname <> parens (commasep $ map ppr params) <> text ":" </>
    indent 2 (stack (map ppr body))

instance Pretty PyProg where
  ppr (PyProg funcs) = stack $ map ppr funcs
