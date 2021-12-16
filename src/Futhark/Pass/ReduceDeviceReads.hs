module Futhark.Pass.ReduceDeviceReads (reduceDeviceReads) where

import Futhark.Analysis.MigrationTable
import Futhark.IR.GPU
import Futhark.Pass

reduceDeviceReads :: Pass GPU GPU
reduceDeviceReads =
  Pass
    "reduce device reads"
    "Move host statements to device to reduce data transfers from device."
    reduceProgram

reduceProgram :: Prog GPU -> PassM (Prog GPU)
reduceProgram prog@(Prog consts funs) = do
  let mt = analyseProg prog
  consts' <- reduceStms mt consts
  funs' <- parPass (reduceFunDef mt) funs
  pure (Prog consts' funs')

reduceFunDef :: MigrationTable -> FunDef GPU -> PassM (FunDef GPU)
reduceFunDef mt fd = do
  let body = funDefBody fd
  stms' <- reduceStms mt (bodyStms body)
  pure $ fd {funDefBody = body {bodyStms = stms'}}

reduceStms :: MigrationTable -> Stms GPU -> PassM (Stms GPU)
reduceStms mt stms = pure stms

-- TODO: Run ormolu and hlint
