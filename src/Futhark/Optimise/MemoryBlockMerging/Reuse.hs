-- | Reuse the memory blocks of arrays.
--
-- Enable by setting the environment variable MEMORY_BLOCK_MERGING_REUSE=1.
module Futhark.Optimise.MemoryBlockMerging.Reuse
  ( reuseInProg
  ) where

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Tools

import Futhark.Optimise.MemoryBlockMerging.AuxiliaryInfo
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous

import Futhark.Optimise.MemoryBlockMerging.Reuse.Core


reuseInProg :: MonadFreshNames m
            => Prog ExplicitMemory
            -> m (Prog ExplicitMemory)
reuseInProg = intraproceduralTransformation reuseInFunDef

reuseInFunDef :: MonadFreshNames m
                 => FunDef ExplicitMemory
                 -> m (FunDef ExplicitMemory)
reuseInFunDef fundef0 =
  let aux0 = getAuxiliaryInfo fundef0
      debug0 = debugAuxiliaryInfo aux0 "Before reuse"
      fundef1 = coreReuseFunDef fundef0
                (auxFirstUses aux0) (auxInterferences aux0) (auxVarMemMappings aux0)
                (auxActualVariables aux0) (auxExistentials aux0)

      debug = debug0
  in withDebug debug $ return fundef1
