-- | Reuse the memory blocks of arrays.
--
-- Enable by setting the environment variable MEMORY_BLOCK_MERGING_REUSE=1.
module Futhark.Optimise.MemoryBlockMerging.Reuse
  ( reuseInProg
  ) where

import Futhark.Pass

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.AuxiliaryInfo
import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizeMovingUp
import Futhark.Optimise.MemoryBlockMerging.Reuse.Core

reuseInProg :: Prog ExplicitMemory -> PassM (Prog ExplicitMemory)
reuseInProg = intraproceduralTransformation reuseInFunDef

reuseInFunDef :: MonadFreshNames m
                 => FunDef ExplicitMemory
                 -> m (FunDef ExplicitMemory)
reuseInFunDef fundef0 = do
  let fundef1 = moveUpAllocSizesFunDef fundef0
      aux1 = getAuxiliaryInfo fundef1
  coreReuseFunDef fundef1
    (auxFirstUses aux1) (auxInterferences aux1)
    (auxPotentialKernelDataRaceInterferences aux1) (auxVarMemMappings aux1)
    (auxActualVariables aux1) (auxExistentials aux1)
