{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegMap' is quite straightforward.  The only
-- trick is virtualisation in case the physical number of threads is
-- not sufficient to cover the logical thread space.  This is handled
-- by having actual workgroups run a loop to imitate multiple workgroups.
module Futhark.CodeGen.ImpGen.GPU.SegMap (compileSegMap) where

import Control.Monad.Except
import qualified Futhark.CodeGen.ImpCode.GPU as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.IR.GPUMem
import Futhark.Util.IntegralExp (divUp)
import Prelude hiding (quot, rem)

-- | Compile 'SegMap' instance code.
compileSegMap ::
  Pat GPUMem ->
  SegLevel ->
  SegSpace ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegMap pat lvl space kbody = do
  let (is, dims) = unzip $ unSegSpace space
      dims' = map toInt64Exp dims
      num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size' = toInt64Exp <$> segGroupSize lvl

  emit $ Imp.DebugPrint "\n# SegMap" Nothing
  case lvl of
    SegThread {} -> do
      let virt_num_groups =
            sExt32 $ product dims' `divUp` unCount group_size'
      sKernelThread "segmap" (segFlat space) (defKernelAttrs num_groups' group_size') $
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
      sKernelGroup "segmap_intragroup" (segFlat space) (defKernelAttrs num_groups' group_size') $ do
        let virt_num_groups = sExt32 $ product dims'
        precomputedConstants pc $
          virtualiseGroups (segVirt lvl) virt_num_groups $ \group_id -> do
            dIndexSpace (zip is dims') $ sExt64 group_id

            compileStms mempty (kernelBodyStms kbody) $
              zipWithM_ (compileGroupResult space) (patElems pat) $
                kernelBodyResult kbody
  emit $ Imp.DebugPrint "" Nothing
