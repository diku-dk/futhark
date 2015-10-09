module Futhark.CodeGen.Backends.GenericPython.AST
  ( PyExp(..) 
  , PyStmt(..)
  , module Language.Futhark.Core
  )
  where

import Language.Futhark.Core
import Text.PrettyPrint.Mainland hiding (space)

type VariableName = String

data PyExp = Constant BasicValue
           | ScalarVar VariableName
           | BinaryOp String PyExp PyExp 
             deriving (Eq, Show)
           
data PyStmt = If PyExp PyStmt PyStmt
            | While PyExp PyStmt
            | For VariableName PyExp PyStmt
            | SetScalar VariableName PyExp
            deriving (Eq, Show)


instance Pretty PyExp where
    ppr (Constant v) = text $ show v 
    ppr (ScalarVar n) = text n  
    ppr (BinaryOp s e1 e2) = ppr e1 <> text s <> ppr e2 
  
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
    text  "for" <+> ppr i <+> text "in range" <+> parens (ppr limit) <+> text ":" </>
    indent 2 (ppr body)
  ppr (SetScalar v e) = text v <> text "=" <> ppr e 

            
            
