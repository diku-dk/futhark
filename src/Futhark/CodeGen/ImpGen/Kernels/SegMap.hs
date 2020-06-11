{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Kernels.SegMap
  ( compileSegMap )
where

import Control.Monad.Except

import Prelude hiding (quot, rem)

import Futhark.IR.KernelsMem
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.CodeGen.ImpGen
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.Util.IntegralExp (divUp)

-- | Compile 'SegMap' instance code.
compileSegMap :: Pattern KernelsMem
              -> SegLevel
              -> SegSpace
              -> KernelBody KernelsMem
              -> CallKernelGen ()

compileSegMap pat lvl space kbody = do
  let (is, dims) = unzip $ unSegSpace space
  dims' <- mapM toExp dims

  num_groups' <- traverse toExp $ segNumGroups lvl
  group_size' <- traverse toExp $ segGroupSize lvl

  case lvl of
    SegThread{} -> do
      emit $ Imp.DebugPrint "\n# SegMap" Nothing
      let virt_num_groups = product dims' `divUp` unCount group_size'
      sKernelThread "segmap" num_groups' group_size' (segFlat space) $
        virtualiseGroups (segVirt lvl) virt_num_groups $ \group_id -> do
        local_tid <- kernelLocalThreadId . kernelConstants <$> askEnv
        let global_tid = Imp.vi32 group_id * unCount group_size' + local_tid

        zipWithM_ dPrimV_ is $ unflattenIndex dims' global_tid

        sWhen (isActive $ unSegSpace space) $
          compileStms mempty (kernelBodyStms kbody) $
          zipWithM_ (compileThreadResult space) (patternElements pat) $
          kernelBodyResult kbody

    SegGroup{} ->
      sKernelGroup "segmap_intragroup" num_groups' group_size' (segFlat space) $ do
      let virt_num_groups = product dims'
      precomputeSegOpIDs (kernelBodyStms kbody) $
        virtualiseGroups (segVirt lvl) virt_num_groups $ \group_id -> do

        zipWithM_ dPrimV_ is $ unflattenIndex dims' $ Imp.vi32 group_id

        compileStms mempty (kernelBodyStms kbody) $
          zipWithM_ (compileGroupResult space) (patternElements pat) $
          kernelBodyResult kbody
