module Futhark.Optimise.MemoryBlockMerging
  ( memoryBlockMergingCoalescing
  , memoryBlockMergingReuse
  ) where

import Futhark.Pass
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.Coalescing (coalesceInProg)
import Futhark.Optimise.MemoryBlockMerging.Reuse (reuseInProg)

-- FIXME: Make all of this work for GPU code generation as well.


-- | Apply the coalescing part of the memory block merging optimisation.
memoryBlockMergingCoalescing :: Pass ExplicitMemory ExplicitMemory
memoryBlockMergingCoalescing =
  simplePass
  "Memory block merging (coalescing)"
  "Coalesce the memory blocks of arrays"
  coalesceInProg

-- | Apply the coalescing part of the memory block merging optimisation.
memoryBlockMergingReuse :: Pass ExplicitMemory ExplicitMemory
memoryBlockMergingReuse =
  simplePass
  "Memory block merging (reuse)"
  "Reuse the memory blocks of arrays"
  reuseInProg
