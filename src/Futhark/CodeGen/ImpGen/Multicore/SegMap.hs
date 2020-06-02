module Futhark.CodeGen.ImpGen.Multicore.SegMap
  (compileSegMap
  )
  where

import Control.Monad

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.IR.MCMem
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.Transform.Rename

writeResult :: [VName]
            -> PatElemT dec
            -> KernelResult
            -> MulticoreGen ()
writeResult is pe (Returns _ se) =
  copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
writeResult _ pe (WriteReturns rws _ idx_vals) = do
  let (iss, vs) = unzip idx_vals
  rws' <- mapM toExp rws
  forM_ (zip iss vs) $ \(slice, v) -> do
    slice' <- mapM (traverse toExp) slice
    let condInBounds (DimFix i) rw =
          0 .<=. i .&&. i .<. rw
        condInBounds (DimSlice i n s) rw =
          0 .<=. i .&&. i+n*s .<. rw
        in_bounds = foldl1 (.&&.) $ zipWith condInBounds slice' rws'
        when_in_bounds = copyDWIM (patElemName pe) slice' v []
    sWhen in_bounds when_in_bounds
writeResult _ _ res =
  error $ "writeResult: cannot handle " ++ pretty res



compileSegMapBody :: VName
                  -> Pattern MCMem
                  -> SegSpace
                  -> KernelBody MCMem
                  -> MulticoreGen Imp.Code
compileSegMapBody flat_idx pat space (KernelBody _ kstms kres) = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  kstms' <- mapM renameStm kstms
  collect $ do
    emit $ Imp.DebugPrint "SegMap fbody" Nothing
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    compileStms (freeIn kres) kstms' $
      zipWithM_ (writeResult is) (patternElements pat) kres


compileSegMap :: Pattern MCMem
              -> SegSpace
              -> KernelBody MCMem
              -> MulticoreGen ()
compileSegMap pat space kbody = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  dPrimV_ (segFlat space) 0
  flat_idx <- dPrim "iter" int32

  par_task_code <- collect $ do
    body <- compileSegMapBody flat_idx pat space kbody
    free_params <- freeParams body [segFlat space, flat_idx]
    let (body_allocs, body') = extractAllocations body
        sched = decideScheduling body'
    emit $ Imp.DebugPrint "SegMap parallel" Nothing
    ntasks <- dPrim "num_tasks" $ IntType Int32
    emit $ Imp.Op $ Imp.MCFunc flat_idx body_allocs body' free_params $
      Imp.MulticoreInfo ntasks sched (segFlat space)

  seq_task_code <- collect $ localMode ModeSequential $ do
    emit $ Imp.DebugPrint "SegMap sequential" Nothing
    body <- compileSegMapBody flat_idx pat space kbody
    let (body_allocs, body') = extractAllocations body
    emit $ Imp.Op $ Imp.SeqCode flat_idx body_allocs body'

  mode <- askEnv

  if mode == ModeSequential
    then emit seq_task_code
    else do
    free_params_task <- freeParams (par_task_code <> seq_task_code) [flat_idx]
    emit $ Imp.Op $ Imp.Task free_params_task (product ns') par_task_code seq_task_code (segFlat space) []
