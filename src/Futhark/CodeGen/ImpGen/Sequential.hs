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
        expCompiler (ImpGen.Destination [d]) (Op (SizeOf bt)) = do
          ImpGen.writeExp d $ Imp.SizeOf bt
          return ImpGen.Done
        expCompiler _ (Op (ParExp e)) =
          return $ ImpGen.CompileExp e
        expCompiler _ e =
          return $ ImpGen.CompileExp e
