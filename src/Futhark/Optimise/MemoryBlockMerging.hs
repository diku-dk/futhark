module Futhark.Optimise.MemoryBlockMerging
  ( memoryBlockMergingCoalescing
  , memoryBlockMergingReuse
  ) where

import Futhark.Pass
import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Util (isEnvVarSet)

import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Coalescing (coalesceInProg)
import Futhark.Optimise.MemoryBlockMerging.Reuse (reuseInProg)
import Futhark.Optimise.MemoryBlockMerging.OverviewPrint (overviewPrintProg)


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
  (maybeOverviewPrint reuseInProg)

maybeOverviewPrint :: MonadFreshNames m =>
                      (Prog ExplicitMemory -> m (Prog ExplicitMemory, Log)) ->
                      (Prog ExplicitMemory -> m (Prog ExplicitMemory))
maybeOverviewPrint f prog
  | usesMemoryBlockMergingOverviewPrint = do
      proglog <- snd <$> f prog
      -- Print the most important parts of the program.  Will not result in a
      -- *valid* program, but might give a better overview of the main structure
      -- of the program.
      overviewPrintProg proglog prog
  | otherwise = fst <$> f prog

-- Do we print an overview of the program with all "filler" statements filtered
-- out (based on what is most relevant for memory block merging)?  Currently
-- disabled by default.  Enable by setting the environment variable
-- MEMORY_BLOCK_MERGING_OVERVIEW_PRINT=1.
usesMemoryBlockMergingOverviewPrint :: Bool
usesMemoryBlockMergingOverviewPrint =
  isEnvVarSet "MEMORY_BLOCK_MERGING_OVERVIEW_PRINT" False
