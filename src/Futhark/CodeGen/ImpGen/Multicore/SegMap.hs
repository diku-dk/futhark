-- | Multicore code generation for 'SegMap'.
module Futhark.CodeGen.ImpGen.Multicore.SegMap
  ( compileSegMap,
  )
where

import Control.Monad
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Transform.Rename

writeResult ::
  [VName] ->
  PatElemT dec ->
  KernelResult ->
  MulticoreGen ()
writeResult is pe (Returns _ _ se) =
  copyDWIMFix (patElemName pe) (map Imp.le64 is) se []
writeResult _ pe (WriteReturns _ (Shape rws) _ idx_vals) = do
  let (iss, vs) = unzip idx_vals
      rws' = map toInt64Exp rws
  forM_ (zip iss vs) $ \(slice, v) -> do
    let slice' = fmap toInt64Exp slice
        when_in_bounds = copyDWIM (patElemName pe) (unSlice slice') v []
    sWhen (inBounds slice' rws') when_in_bounds
writeResult _ _ res =
  error $ "writeResult: cannot handle " ++ pretty res

compileSegMapBody ::
  TV Int64 ->
  Pat MCMem ->
  SegSpace ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
compileSegMapBody flat_idx pat space (KernelBody _ kstms kres) = do
  let (is, ns) = unzip $ unSegSpace space
      ns' = map toInt64Exp ns
  kstms' <- mapM renameStm kstms
  collect $ do
    emit $ Imp.DebugPrint "SegMap fbody" Nothing
    dIndexSpace (zip is ns') $ tvExp flat_idx
    compileStms (freeIn kres) kstms' $
      zipWithM_ (writeResult is) (patElems pat) kres

compileSegMap ::
  Pat MCMem ->
  SegSpace ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
compileSegMap pat space kbody =
  collect $ do
    flat_par_idx <- dPrim "iter" int64
    body <- compileSegMapBody flat_par_idx pat space kbody
    free_params <- freeParams body [segFlat space, tvVar flat_par_idx]
    let (body_allocs, body') = extractAllocations body
    emit $ Imp.Op $ Imp.ParLoop "segmap" (tvVar flat_par_idx) body_allocs body' mempty free_params $ segFlat space
