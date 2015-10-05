
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



            
            
