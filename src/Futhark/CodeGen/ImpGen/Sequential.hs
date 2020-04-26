{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Sequential
  ( compileProg
  )
  where

import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.Representation.SeqMem
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog SeqMem -> m Imp.Program
compileProg = ImpGen.compileProg () ops Imp.DefaultSpace
  where ops = ImpGen.defaultOperations opCompiler
        opCompiler dest (Alloc e space) =
          ImpGen.compileAlloc dest e space
        opCompiler _ (Inner ()) = pure ()
