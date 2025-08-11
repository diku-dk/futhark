-- | Multicore code generation for 'SegMap'.
module Futhark.CodeGen.ImpGen.Multicore.SegMap
  ( compileSegMap,
  )
where

import Control.Monad
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Transform.Rename

writeResult ::
  [VName] ->
  PatElem dec ->
  KernelResult ->
  MulticoreGen ()
writeResult is pe (Returns _ _ se) =
  copyDWIMFix (patElemName pe) (map Imp.le64 is) se []
writeResult _ _ res =
  error $ "writeResult: cannot handle " ++ prettyString res

compileSegMapBody ::
  Pat LetDecMem ->
  SegSpace ->
  KernelBody MCMem ->
  MulticoreGen Imp.MCCode
compileSegMapBody pat space (Body _ kstms kres) = collect $ do
  let (is, ns) = unzip $ unSegSpace space
      ns' = map pe64 ns
  dPrim_ (segFlat space) int64
  sOp $ Imp.GetTaskId (segFlat space)
  kstms' <- mapM renameStm kstms
  inISPC $
    generateChunkLoop "SegMap" Vectorized $ \i -> do
      dIndexSpace (zip is ns') i
      compileStms (freeIn kres) kstms' $
        zipWithM_ (writeResult is) (patElems pat) kres

compileSegMap ::
  Pat LetDecMem ->
  SegSpace ->
  KernelBody MCMem ->
  MulticoreGen Imp.MCCode
compileSegMap pat space kbody = collect $ do
  body <- compileSegMapBody pat space kbody
  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "segmap" body free_params
