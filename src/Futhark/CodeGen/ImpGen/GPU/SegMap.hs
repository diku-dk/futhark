{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegMap' is quite straightforward.  The only
-- trick is virtualisation in case the physical number of threads is
-- not sufficient to cover the logical thread space.  This is handled
-- by having actual workgroups run a loop to imitate multiple workgroups.
module Futhark.CodeGen.ImpGen.GPU.SegMap (compileSegMap) where

import Control.Monad
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpGen.GPU.Group
import Futhark.IR.GPUMem
import Futhark.Util.IntegralExp (divUp)
import Prelude hiding (quot, rem)

-- | Compile 'SegMap' instance code.
compileSegMap ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegMap pat lvl space kbody = do
  attrs <- lvlKernelAttrs lvl

  let (is, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      group_size' = pe64 <$> kAttrGroupSize attrs

  emit $ Imp.DebugPrint "\n# SegMap" Nothing
  case lvl of
    SegThread {} -> do
      virt_num_groups <- dPrimVE "virt_num_groups" $ sExt32 $ product dims' `divUp` unCount group_size'
      sKernelThread "segmap" (segFlat space) attrs $
        virtualiseGroups (segVirt lvl) virt_num_groups $ \group_id -> do
          local_tid <- kernelLocalThreadId . kernelConstants <$> askEnv

          global_tid <-
            dPrimVE "global_tid" $
              sExt64 group_id * sExt64 (unCount group_size')
                + sExt64 local_tid

          dIndexSpace (zip is dims') global_tid

          sWhen (isActive $ unSegSpace space) $
            compileStms mempty (kernelBodyStms kbody) $
              zipWithM_ (compileThreadResult space) (patElems pat) $
                kernelBodyResult kbody
    SegGroup {} -> do
      pc <- precomputeConstants group_size' $ kernelBodyStms kbody
      virt_num_groups <- dPrimVE "virt_num_groups" $ sExt32 $ product dims'
      sKernelGroup "segmap_intragroup" (segFlat space) attrs $ do
        precomputedConstants pc $
          virtualiseGroups (segVirt lvl) virt_num_groups $ \group_id -> do
            dIndexSpace (zip is dims') $ sExt64 group_id

            compileStms mempty (kernelBodyStms kbody) $
              zipWithM_ (compileGroupResult space) (patElems pat) $
                kernelBodyResult kbody
    SegThreadInGroup {} ->
      error "compileSegMap: SegThreadInGroup"
  emit $ Imp.DebugPrint "" Nothing
