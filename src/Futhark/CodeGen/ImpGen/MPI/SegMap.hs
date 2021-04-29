module Futhark.CodeGen.ImpGen.MPI.SegMap
  ( compileSegMap,
  )
where

import Control.Monad
import qualified Futhark.CodeGen.ImpCode.MPI as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.MPI.Base
import Futhark.IR.MCMem
import Futhark.Transform.Rename

writeResult ::
  [VName] ->
  PatElemT dec ->
  KernelResult ->
  MPIGen ()
writeResult is pe (Returns _ se) =
  copyDWIMFix (patElemName pe) (map Imp.vi64 is) se []
writeResult _ _ res =
  error $ "writeResult: cannot handle " ++ pretty res

compileSegMapBody ::
  TV Int64 ->
  Pattern MCMem ->
  SegSpace ->
  KernelBody MCMem ->
  MPIGen Imp.Code
compileSegMapBody flat_idx pat space (KernelBody _ kstms kres) = do
  --ns = nb iterations
  let (is, ns) = unzip $ unSegSpace space
      ns' = map toInt64Exp ns
  --Rename statements to give an unique name to each statement
  kstms' <- mapM renameStm kstms
  collect $ do
    emit $ Imp.DebugPrint "SegMap fbody" Nothing
    zipWithM_ dPrimV_ is $ map sExt64 $ unflattenIndex ns' $ tvExp flat_idx
    compileStms (freeIn kres) kstms' $
      zipWithM_ (writeResult is) (patternElements pat) kres

compileSegMap ::
  Pattern MCMem ->
  SegSpace ->
  KernelBody MCMem ->
  MPIGen Imp.Code
compileSegMap pat space kbody = do
  collect $ do
    --iteration variable
    flat_par_idx <- dPrim "iter" int64
    --Loop body
    body <- compileSegMapBody flat_par_idx pat space kbody
    free_params <- freeParams body [segFlat space, tvVar flat_par_idx]
    -- Moove allocs outside of body
    let (body_allocs, body') = extractAllocations body
    
    -- Get the output array
    let (out_pt, out_name) = extractOutputMem pat
    emit $ Imp.Op $ Imp.DistributedLoop "segmap" (tvVar flat_par_idx) body_allocs body' mempty free_params $ segFlat space
    gather out_name out_pt


extractOutputMem :: PatternT (MemInfo d u MemBind) -> (PrimType, VName)
extractOutputMem pat = do
  let Pattern _ pat_val = pat
  let pat_elem = head pat_val
  let PatElem _ (MemArray out_pt _ _ (ArrayIn out_name _)) = pat_elem
  (out_pt, out_name)
