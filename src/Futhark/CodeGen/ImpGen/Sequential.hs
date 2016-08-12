{-# LANGUAGE TypeFamilies #-}
module Futhark.CodeGen.ImpGen.Sequential
  ( compileProg
  )
  where

import Control.Monad.Except
import Futhark.Representation.ExplicitMemory

import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either String Imp.Program)
compileProg = ImpGen.compileProg ops Imp.DefaultSpace
  where ops = ImpGen.defaultOperations opCompiler
        opCompiler dest (Alloc e space) =
          ImpGen.compileAlloc dest e space
        opCompiler _ (Inner _) =
          throwError "Cannot handle kernels in sequential code generator."
