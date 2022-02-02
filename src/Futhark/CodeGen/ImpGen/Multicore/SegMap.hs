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
    sWhen (inBounds slice' rws') $
      copyDWIM (patElemName pe) (unSlice slice') v []
writeResult _ _ res =
  error $ "writeResult: cannot handle " ++ pretty res

compileSegMapBody ::
  Pat MCMem ->
  SegSpace ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
compileSegMapBody pat space (KernelBody _ kstms kres) = collect $ do
  let (is, ns) = unzip $ unSegSpace space
      ns' = map toInt64Exp ns
  kstms' <- mapM renameStm kstms
  generateChunkLoop "SegMap" $ \i -> do
    dIndexSpace (zip is ns') i
    compileStms (freeIn kres) kstms' $
      zipWithM_ (writeResult is) (patElems pat) kres

compileSegMap ::
  Pat MCMem ->
  SegSpace ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
compileSegMap pat space kbody = collect $ do
  body <- compileSegMapBody pat space kbody
  free_params <- freeParams body [segFlat space]
  emit $ Imp.Op $ Imp.ParLoop "segmap" body free_params $ segFlat space
