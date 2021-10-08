{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.CodeGen.ImpGen.MPI
  ( Futhark.CodeGen.ImpGen.MPI.compileProg,
    Warnings,
  )
where

import qualified Futhark.CodeGen.ImpCode.MPI as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.MPI.Base
import Futhark.CodeGen.ImpGen.MPI.SegMap
import Futhark.CodeGen.ImpGen.MPI.SegRed
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
import Prelude hiding (quot, rem)

-- Compile inner code
compileProg ::
  MonadFreshNames m =>
  Prog MCMem ->
  m (Warnings, Imp.Definitions Imp.MPIOp)
compileProg = Futhark.CodeGen.ImpGen.compileProg Env ops Imp.DefaultSpace
  where
    ops = defaultOperations opCompiler
    opCompiler dest (Alloc e space) = compileAlloc dest e space
    opCompiler dest (Inner op) = compileMCOp dest op

-- Compile seg
compileMCOp ::
  Pat MCMem ->
  MCOp MCMem () ->
  ImpM MCMem Env Imp.MPIOp ()
compileMCOp _ (OtherOp ()) = error "Unknow op"
compileMCOp pat (ParOp _par_op op) = do
  -- Contains the arrray size
  let space = getSpace op

  dPrimV_ (segFlat space) (0 :: Imp.TExp Int64)
  seq_code <- compileSegOp pat op
  retvals <- getReturnParams pat op
  iterations <- getIterationDomain op space

  let non_free = map Imp.paramName retvals

  s <- segOpString op
  free_params <- freeParams seq_code non_free
  emit $ Imp.Op $ Imp.Segop s free_params seq_code retvals (untyped iterations)

compileSegOp ::
  Pat MCMem ->
  SegOp () MCMem ->
  ImpM MCMem Env Imp.MPIOp Imp.Code
compileSegOp pat (SegMap _ space _ kbody) = compileSegMap pat space kbody
compileSegOp pat (SegRed _ space reds _ kbody) = compileSegRed pat space reds kbody
compileSegOp _ _ = error "SegOp not implemented yet"

getSpace :: SegOp () MCMem -> SegSpace
getSpace (SegHist _ space _ _ _) = space
getSpace (SegRed _ space _ _ _) = space
getSpace (SegScan _ space _ _ _) = space
getSpace (SegMap _ space _ _) = space
