module Futhark.CodeGen.Backends.GenericPython
  ( compileProg ) where

import Futhark.CodeGen.ImpCode

import Futhark.CodeGen.Backends.GenericPythonAst

compileProg :: Program () -> String
compileProg p = "print 'Hello World!'\n"
