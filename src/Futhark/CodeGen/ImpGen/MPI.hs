{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.CodeGen.ImpGen.MPI
  ( Futhark.CodeGen.ImpGen.MPI.compileProg,
    Warnings,
  )
where

import qualified Futhark.CodeGen.ImpCode.MPI as Imp
--import Futhark.CodeGen.ImpGen.MPI.Base
import Futhark.CodeGen.ImpGen
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
import Futhark.CodeGen.ImpGen.MPI.Base
import Futhark.CodeGen.ImpGen.MPI.SegMap
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
  Pattern MCMem ->
  MCOp MCMem () ->
  ImpM MCMem Env Imp.MPIOp ()
compileMCOp _ (OtherOp ()) = pure ()
compileMCOp pat (ParOp _par_op op) = do
  -- Contains the arrray size
  let space = getSpace op
  -- Declare a Int64 with the value 0, the name is in the segFlat part of the arg space ("flat_tid").
  -- I comment it for now I don't ("flat_tid") for now.
  -- dPrimV_ (segFlat space) (0 :: Imp.TExp Int64)

  seq_code <- compileSegOp pat op
  retvals <- getReturnParams pat op
  iterations <- getIterationDomain op space

  let non_free = map Imp.paramName retvals

  s <- segOpString op
  free_params <- freeParams seq_code non_free
  emit $ Imp.Op $ Imp.Segop s free_params seq_code retvals (untyped iterations)
  pure ()
  {-- For a start, ignore the _par_op, which may contain nested
  -- parallelism.
  traceShow op
  emit $ Imp.Op $ Imp.CrashWithThisMessage $ "Code for the expression with pattern " ++ pretty pat
  --}


compileSegOp ::
  Pattern MCMem ->
  SegOp () MCMem ->
  ImpM MCMem Env Imp.MPIOp Imp.Code
  
compileSegOp pat (SegMap _ space _ kbody) = compileSegMap pat space kbody
compileSegOp _ _ = pure Imp.Skip


getSpace :: SegOp () MCMem -> SegSpace
getSpace (SegHist _ space _ _ _) = space
getSpace (SegRed _ space _ _ _) = space
getSpace (SegScan _ space _ _ _) = space
getSpace (SegMap _ space _ _) = space
