{-# LANGUAGE TypeFamilies #-}
module Futhark.CodeGen.ImpGen.Sequential
  ( compileProg
  )
  where

import Futhark.Error
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError Imp.Program)
compileProg = ImpGen.compileProg ops Imp.DefaultSpace
  where ops = ImpGen.defaultOperations opCompiler
        opCompiler :: ImpGen.OpCompiler ExplicitMemory Imp.Sequential
        opCompiler dest (Alloc e space) =
          ImpGen.compileAlloc dest e space
        opCompiler _ (Inner _) =
          compilerBugS "Cannot handle kernels in sequential code generator."
