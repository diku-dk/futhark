-- | Coalesce the memory blocks of arrays.
--
-- Enable by setting the environment variable MEMORY_BLOCK_MERGING_COALESCING=1.
module Futhark.Optimise.MemoryBlockMerging.Coalescing
  ( coalesceInProg
  ) where

import Futhark.Pass

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.AuxiliaryInfo
import Futhark.Optimise.MemoryBlockMerging.Types

import Futhark.Optimise.MemoryBlockMerging.Coalescing.AllocationMovingUp
import Futhark.Optimise.MemoryBlockMerging.Coalescing.Core


coalesceInProg :: Prog ExplicitMemory -> PassM (Prog ExplicitMemory)
coalesceInProg = intraproceduralTransformation coalesceInFunDef

coalesceInFunDef :: MonadFreshNames m
                 => FunDef ExplicitMemory
                 -> m (FunDef ExplicitMemory)
coalesceInFunDef fundef0 = do
  let fundef1 = moveUpAllocsFunDef fundef0
      aux1 = getAuxiliaryInfo fundef1
  coreCoalesceFunDef fundef1
    (auxVarMemMappings aux1) (auxMemAliases aux1)
    (auxVarAliases aux1) (auxFirstUses aux1) (auxLastUses aux1)
    (auxActualVariables aux1) (auxExistentials aux1)
