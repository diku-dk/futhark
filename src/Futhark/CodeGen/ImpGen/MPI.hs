{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.CodeGen.ImpGen.MPI
  ( Futhark.CodeGen.ImpGen.MPI.compileProg,
    Warnings,
  )
where

import qualified Futhark.CodeGen.ImpCode.MPI as Imp
import Futhark.CodeGen.ImpGen
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
import Prelude hiding (quot, rem)

-- If we need to add more context to the code generation, we can put
-- it in this datatype.
data Env = Env

compileProg ::
  MonadFreshNames m =>
  Prog MCMem ->
  m (Warnings, Imp.Definitions Imp.MPIOp)
compileProg = Futhark.CodeGen.ImpGen.compileProg Env ops Imp.DefaultSpace
  where
    ops = defaultOperations opCompiler
    opCompiler dest (Alloc e space) = compileAlloc dest e space
    opCompiler dest (Inner op) = compileMCOp dest op

compileMCOp ::
  Pattern MCMem ->
  MCOp MCMem () ->
  ImpM MCMem Env Imp.MPIOp ()
compileMCOp _ (OtherOp ()) = pure ()
compileMCOp pat (ParOp _par_op op) =
  -- For a start, ignore the _par_op, which may contain nested
  -- parallelism.
  emit $ Imp.Op $ Imp.CrashWithThisMessage $ "Code for the expression with pattern " ++ pretty pat
