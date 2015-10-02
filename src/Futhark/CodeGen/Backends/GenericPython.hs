module Futhark.CodeGen.Backends.GenericPython
  ( compileProg ) where

import Futhark.CodeGen.ImpCode

compileProg :: Program () -> String
compileProg p = "print 'Hello World!'\n"
