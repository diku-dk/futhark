{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegMap' is quite straightforward.  The only
-- trick is virtualisation in case the physical number of threads is
-- not sufficient to cover the logical thread space.  This is handled
-- by having actual threadblocks run a loop to imitate multiple threadblocks.
module Futhark.CodeGen.ImpGen.GPU.SegMap (compileSegMap) where

import Control.Monad
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpGen.GPU.Block
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
      tblock_size' = pe64 <$> kAttrBlockSize attrs

  emit $ Imp.DebugPrint "\n# SegMap" Nothing
  case lvl of
    SegThread {} -> do
      virt_num_tblocks <- dPrimVE "virt_num_tblocks" $ sExt32 $ product dims' `divUp` unCount tblock_size'
      sKernelThread "segmap" (segFlat space) attrs $
        virtualiseBlocks (segVirt lvl) virt_num_tblocks $ \tblock_id -> do
          local_tid <- kernelLocalThreadId . kernelConstants <$> askEnv

          global_tid <-
            dPrimVE "global_tid" $
              sExt64 tblock_id * sExt64 (unCount tblock_size')
                + sExt64 local_tid

          dIndexSpace (zip is dims') global_tid

          sWhen (isActive $ unSegSpace space) $
            compileStms mempty (kernelBodyStms kbody) $
              zipWithM_ (compileThreadResult space) (patElems pat) $
                kernelBodyResult kbody
    SegBlock {} -> do
      pc <- precomputeConstants tblock_size' $ kernelBodyStms kbody
      virt_num_tblocks <- dPrimVE "virt_num_tblocks" $ sExt32 $ product dims'
      sKernelBlock "segmap_intrablock" (segFlat space) attrs $ do
        precomputedConstants pc $
          virtualiseBlocks (segVirt lvl) virt_num_tblocks $ \tblock_id -> do
            dIndexSpace (zip is dims') $ sExt64 tblock_id

            compileStms mempty (kernelBodyStms kbody) $
              zipWithM_ (compileBlockResult space) (patElems pat) $
                kernelBodyResult kbody
    SegThreadInBlock {} ->
      error "compileSegMap: SegThreadInBlock"
  emit $ Imp.DebugPrint "" Nothing
