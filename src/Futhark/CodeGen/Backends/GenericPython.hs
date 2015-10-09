module Futhark.CodeGen.Backends.GenericPython
  ( compileProg, 
    compileExp, 
    compileCode) where

import qualified Futhark.CodeGen.ImpCode as Imp

import Futhark.Representation.AST.Syntax (BinOp (..))

import Futhark.CodeGen.Backends.GenericPython.AST

compileProg :: Imp.Program () -> String
compileProg _ = "print 'Hello World!'\n"


compileBinOp :: BinOp -> String
compileBinOp op =
  case op of
    Plus -> "+"
    Minus -> "-" 
    _     -> undefined

compileExp :: Imp.Exp -> PyExp
compileExp (Imp.Constant v) = Constant v 
compileExp (Imp.ScalarVar vname) = ScalarVar $ baseString vname
compileExp (Imp.BinOp op exp1 exp2) = BinaryOp (compileBinOp op) (compileExp exp1) (compileExp exp2)  
compileExp _ = undefined


compileCode :: Imp.Code a -> PyStmt
compileCode (Imp.If cond tb fb) = If (compileExp cond) (compileCode tb) (compileCode fb)
compileCode (Imp.While cond body) = While (compileExp cond) (compileCode body)
compileCode (Imp.For i bound body) = For (baseString i) (compileExp bound) (compileCode body)
compileCode (Imp.SetScalar vname exp1) = SetScalar (baseString vname) (compileExp exp1)
compileCode _ = undefined



