module Futhark.CodeGen.ImpGen.Sequential
  ( compileProg
  )
  where

import Futhark.Representation.ExplicitMemory

import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen

compileProg :: Prog -> Either String Imp.Program
compileProg = ImpGen.compileProg ImpGen.defaultOperations Imp.DefaultSpace
