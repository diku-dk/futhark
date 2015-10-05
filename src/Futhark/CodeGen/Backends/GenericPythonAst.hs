
module Futhark.CodeGen.Backends.GenericPythonAst
  ( PyExp(..) 
  , PyStmt(..)
  , module Language.Futhark.Core
  )
  where

import Language.Futhark.Core
import Text.PrettyPrint.Mainland hiding (space)

type VariableName = String

data PyExp = Constant BasicValue
           | ScalarVariable VariableName
           | BinaryOp String PyExp PyExp 
           | SetScalar VariableName PyExp
             deriving (Eq, Show)
           
data PyStmt = If PyExp PyStmt PyStmt
            | While PyExp PyStmt
            | For VariableName PyExp PyStmt
            deriving (Eq, Show)


instance Pretty PyExp where
    ppr (Constant v) = text $ show v 
    ppr (ScalarVariable n) = text n  
    ppr (BinaryOp s e1 e2) = ppr e1 <> text s <> ppr e2 
    ppr (SetScalar v e) = text v <> text "=" <> ppr e 
  


            
            
