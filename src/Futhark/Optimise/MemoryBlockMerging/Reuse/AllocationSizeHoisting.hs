-- | Move size variables used in allocation statements upwards in the bodies of
-- a program to enable more memory block reuses.
--
-- This should be run *before* the reuse pass, as it enables more optimisations.
module Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizeHoisting
  ( hoistAllocSizesFunDef
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.CrudeHoisting

import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes


findAllocSizeHoistees :: Body ExplicitMemory -> Maybe [FParam ExplicitMemory]
                      -> [VName]
findAllocSizeHoistees body params =
  let subexps = map fst $ M.elems
                $ memBlockSizesParamsBodyNonRec (fromMaybe [] params) body
  in mapMaybe fromVar subexps

hoistAllocSizesFunDef :: FunDef ExplicitMemory
                      -> FunDef ExplicitMemory
hoistAllocSizesFunDef fundef =
  hoistInFunDef fundef findAllocSizeHoistees
