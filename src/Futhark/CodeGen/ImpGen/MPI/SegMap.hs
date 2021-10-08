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
writeResult is pe (Returns _ _ se) =
  copyDWIMFix (patElemName pe) (map Imp.vi64 is) se []
writeResult _ _ (WriteReturns (Shape _) _ _) = do
    error "writeResult: not implemented yet"
writeResult _ _ res =
  error $ "writeResult: cannot handle " ++ pretty res

compileSegMapBody ::
  TV Int64 ->
  Pat MCMem ->
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
      zipWithM_ (writeResult is) (patElems pat) kres

compileSegMap ::
  Pat MCMem ->
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
    
    -- Get the output arrays
    let out_arrays = extractOutputMem pat
    emit $ Imp.Op $ Imp.DistributedLoop "segmap" (tvVar flat_par_idx) body_allocs body' mempty free_params $ segFlat space
    forM_ out_arrays $ \(pt, name) -> gather name $ Prim pt 
    


extractOutputMem :: PatternT (MemInfo d u MemBind) -> [(PrimType, VName)]
extractOutputMem pat = map (\(PatElem _ (MemArray out_pt _ _ (ArrayIn out_name _))) -> (out_pt, out_name)) pat_val
  where Pattern _ pat_val = pat