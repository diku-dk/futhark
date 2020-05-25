module Futhark.CodeGen.ImpGen.Multicore.SegMap
  (compileSegMap
  )
  where

import Control.Monad

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.IR.MCMem
import Futhark.CodeGen.ImpGen.Multicore.Base



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
                  -> MulticoreGen (Imp.Code, Imp.Code)
compileSegMapBody flat_idx pat space (KernelBody _ kstms kres) = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns


  prebody <- collect $
    dPrimV_ (segFlat space) 0

  body <- collect $ do
    emit $ Imp.DebugPrint "SegMap fbody" Nothing
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    compileStms (freeIn kres) kstms $
      zipWithM_ (writeResult is) (patternElements pat) kres


  return (prebody, body)




compileSegMap :: Pattern MCMem
              -> SegSpace
              -> KernelBody MCMem
              -> MulticoreGen ()
compileSegMap pat space kbody = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  flat_idx <- dPrim "iter" int32
  (prebody, body) <- compileSegMapBody flat_idx pat space kbody
  let freeVariables = namesToList (freeIn body `namesSubtract` namesFromList [segFlat space, flat_idx])
  ts <- mapM lookupType freeVariables
  let freeParams = zipWith toParam freeVariables ts

  par_task_code <- collect $ do
    emit $ Imp.DebugPrint "SegMap parallel" Nothing
    ntasks <- dPrim "num_tasks" $ IntType Int32
    emit $ Imp.Op $ Imp.MCFunc freeParams ntasks flat_idx mempty body (segFlat space)

  seq_task_code <- collect $ do
    emit $ Imp.DebugPrint "SegMap sequential" Nothing
    emit $ Imp.Op $ Imp.SeqCode flat_idx mempty body

  let freeVariables_task = namesToList $ freeIn (prebody <> par_task_code <> seq_task_code) `namesSubtract` namesFromList [flat_idx]
  ts_task <- mapM lookupType freeVariables_task
  let freeParams_task = zipWith toParam freeVariables_task ts_task


  emit $ Imp.Op $ Imp.ParLoop freeParams_task (product ns') (prebody <> par_task_code) (prebody <> seq_task_code)
