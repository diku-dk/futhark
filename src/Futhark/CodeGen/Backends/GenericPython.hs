module Futhark.CodeGen.Backends.GenericPython
  ( compileProg ) where

import Futhark.CodeGen.ImpCode

import Futhark.CodeGen.Backends.GenericPythonAst

compileProg :: Program () -> String
compileProg p = "print 'Hello World!'\n"

instance Pretty PyStmt where
  ppr (If cond tbranch fbranch) =
    text "if" <+> ppr cond <+> text ":" </>
    indent 2 (ppr tbranch) </>
    text "else:" </>
    indent 2 (ppr fbranch)
  ppr (While cond body) =
    text "while" <+> ppr cond <+> text ":" </>
    indent 2 (ppr body)
  ppr (For i limit body) =
    text  "for" <+> ppr i <+> text "in range(" <+> ppr limit <+> text ")" </>
    indent 2 (ppr body)
