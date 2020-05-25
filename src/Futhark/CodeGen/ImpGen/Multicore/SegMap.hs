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



compileSegMap :: Pattern MCMem
              -> SegSpace
              -> KernelBody MCMem
              -> MulticoreGen ()
compileSegMap pat space (KernelBody _ kstms kres) = do
  emit $ Imp.DebugPrint "SegMap " Nothing

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  num_tasks <- dPrim "ntask" $ IntType Int32

  flat_idx <- dPrim "iter" int32

  tid <- dPrim "tid" int32
  tid' <- toExp $ Var tid

  dPrimV_ (segFlat space) 0

  par_body <- collect $ do
   emit $ Imp.DebugPrint "SegMap fbody" Nothing
   dPrimV_ (segFlat space) tid'
   zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
   compileStms (freeIn kres) kstms $
     zipWithM_ (writeResult is) (patternElements pat) kres


  let freeVariables = namesToList (freeIn par_body `namesSubtract` namesFromList [tid, flat_idx])
  ts <- mapM lookupType freeVariables
  let freeParams = zipWith toParam freeVariables ts
      scheduling = decideScheduling par_body

  let (body_allocs, par_body') = extractAllocations par_body

  emit $ Imp.Op $ Imp.ParLoop scheduling num_tasks flat_idx (product ns') freeParams
                             (Imp.MulticoreFunc body_allocs par_body' tid)
                             (Imp.SequentialFunc body_allocs par_body')
