{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Compile Futhark to sequential imperative code.
module Futhark.CodeGen.ImpGen.Sequential
  ( compileProg,
    ImpGen.Warnings,
  )
where

import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames

-- | Compile a 'SeqMem' program to sequential imperative code.
compileProg :: MonadFreshNames m => Prog SeqMem -> m (ImpGen.Warnings, Imp.Program)
compileProg = ImpGen.compileProg () ops Imp.DefaultSpace
  where
    ops = ImpGen.defaultOperations opCompiler
    opCompiler dest (Alloc e space) =
      ImpGen.compileAlloc dest e space
    opCompiler _ (Inner ()) = pure ()
