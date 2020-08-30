module Futhark.CodeGen.ImpGen.Multicore.SegMap
  (
    compileSegMap
  , compileSequentialSegMap
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
      rws' = map toInt32Exp rws
  forM_ (zip iss vs) $ \(slice, v) -> do
    let slice' = map (fmap toInt32Exp) slice
        condInBounds (DimFix i) rw =
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
      ns' = map toInt32Exp ns
  kstms' <- mapM renameStm kstms
  collect $ do
    emit $ Imp.DebugPrint "SegMap fbody" Nothing
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    compileStms (freeIn kres) kstms' $
      zipWithM_ (writeResult is) (patternElements pat) kres

compileSegMap :: Pattern MCMem
              -> SegSpace
              -> KernelBody MCMem
              -> MulticoreGen Imp.Code
compileSegMap pat space kbody =
  collect $ do
    flat_par_idx <- dPrim "iter" int64
    body <- compileSegMapBody flat_par_idx pat space kbody
    free_params <- freeParams body [segFlat space, flat_par_idx]
    let (body_allocs, body') = extractAllocations body
    emit $ Imp.Op $ Imp.ParLoop "segmap" flat_par_idx body_allocs body' free_params $ segFlat space

compileSequentialSegMap :: Pattern MCMem
                        -> SegSpace
                        -> KernelBody MCMem
                        -> MulticoreGen Imp.Code
compileSequentialSegMap pat space kbody = do
  let ns = map snd $ unSegSpace space
      ns' = map toInt32Exp ns
  collect $ do
    emit $ Imp.DebugPrint "SegMap sequential" Nothing
    flat_seq_idx <- dPrim "seq_iter" int32
    body <- compileSegMapBody flat_seq_idx pat space kbody
    let (body_allocs, body') = extractAllocations body
    emit body_allocs
    sFor "i" (product ns') $ \i -> do
      flat_seq_idx <-- i
      emit body'
