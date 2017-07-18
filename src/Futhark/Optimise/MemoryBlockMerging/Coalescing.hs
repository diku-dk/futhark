-- | Coalesce the memory blocks of arrays.
--
-- Enable by setting the environment variable MEMORY_BLOCK_MERGING_COALESCING=1.
module Futhark.Optimise.MemoryBlockMerging.Coalescing
  ( coalesceInProg
  ) where

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Tools

import Futhark.Optimise.MemoryBlockMerging.AuxiliaryInfo
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous

import Futhark.Optimise.MemoryBlockMerging.Coalescing.AllocHoisting
import Futhark.Optimise.MemoryBlockMerging.Coalescing.Core


coalesceInProg :: MonadFreshNames m
               => Prog ExplicitMemory
               -> m (Prog ExplicitMemory)
coalesceInProg = intraproceduralTransformation coalesceInFunDef

coalesceInFunDef :: MonadFreshNames m
                 => FunDef ExplicitMemory
                 -> m (FunDef ExplicitMemory)
coalesceInFunDef fundef0 =
  let aux0 = getAuxiliaryInfo fundef0
      debug0 = debugAuxiliaryInfo aux0 "Before coalescing"
      fundef1 = hoistAllocsFunDef fundef0

      aux1 = getAuxiliaryInfo fundef1
      debug1 = debugAuxiliaryInfo aux1 "After allocation hoisting"
      fundef2 = coreCoalesceFunDef fundef1
                (auxVarMemMappings aux1) (auxMemAliases aux1)
                (auxVarAliases aux1) (auxFirstUses aux1) (auxLastUses aux1)

      debug = print fundef0 >> debug0 >> debug1
  in withDebug debug $ return fundef2
