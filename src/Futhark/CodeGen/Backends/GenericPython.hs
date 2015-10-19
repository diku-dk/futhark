{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.Backends.GenericPython
  ( compileProg,
    compileExp,
    compileCode,
    CompilerEnv(..),
    CompilerState(..)
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax (BinOp (..))
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.Backends.GenericPython.AST


data CompilerEnv = CompilerEnv {
  envFtable     :: HM.HashMap Name [Imp.Type]
}


data CompilerState = CompilerState {
  compNameSrc :: VNameSource
}

newtype CompilerM a = CompilerM (RWS
                                  CompilerEnv
                                  [PyStmt]
                                  CompilerState a)
  deriving (Functor, Applicative, Monad,
            MonadState CompilerState,
            MonadReader CompilerEnv,
            MonadWriter [PyStmt])


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
