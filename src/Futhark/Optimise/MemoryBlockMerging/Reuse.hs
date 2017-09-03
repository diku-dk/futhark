-- | Reuse the memory blocks of arrays.
--
-- Enable by setting the environment variable MEMORY_BLOCK_MERGING_REUSE=1.
module Futhark.Optimise.MemoryBlockMerging.Reuse
  ( reuseInProg
  ) where

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.AuxiliaryInfo
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous

import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizeHoisting
import Futhark.Optimise.MemoryBlockMerging.Reuse.Core


reuseInProg :: MonadFreshNames m
            => Prog ExplicitMemory
            -> m (Prog ExplicitMemory, Log)
reuseInProg = intraproceduralTransformationWithLog reuseInFunDef

reuseInFunDef :: MonadFreshNames m
                 => FunDef ExplicitMemory
                 -> m (FunDef ExplicitMemory, Log)
reuseInFunDef fundef0 = do
  let aux0 = getAuxiliaryInfo fundef0
      debug0 = debugAuxiliaryInfo aux0 "Before reuse"
      fundef1 = hoistAllocSizesFunDef fundef0

      aux1 = getAuxiliaryInfo fundef1
      debug1 = debugAuxiliaryInfo aux1 "After allocation size hoisting"
  (fundef2, proglog) <- coreReuseFunDef fundef1
    (auxFirstUses aux1) (auxInterferences aux1) (auxVarMemMappings aux1)
    (auxActualVariables aux1) (auxExistentials aux1)

  let debug = debug0 >> debug1
  withDebug debug $ return (fundef2, proglog)
