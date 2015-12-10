{-# LANGUAGE TypeFamilies #-}
module Futhark.CodeGen.ImpGen.Sequential
  ( compileProg
  )
  where

import Futhark.Representation.ExplicitMemory

import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen

compileProg :: Prog -> Either String Imp.Program
compileProg = ImpGen.compileProg ops Imp.DefaultSpace
  where ops = ImpGen.defaultOperations { ImpGen.opsExpCompiler = expCompiler }
        expCompiler (ImpGen.Destination [ImpGen.MemoryDestination mem size]) (Op (Alloc e space)) = do
          ImpGen.emit $ Imp.Allocate mem (Imp.bytes e') space
          case size of Just size' -> ImpGen.emit $ Imp.SetScalar size' e'
                       Nothing    -> return ()
          return ImpGen.Done
            where e' = ImpGen.compileSubExp e
        expCompiler _ e =
          return $ ImpGen.CompileExp e
