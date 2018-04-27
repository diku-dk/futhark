{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Helper information for the main optimisation passes.
module Futhark.Optimise.MemoryBlockMerging.AuxiliaryInfo
  ( AuxiliaryInfo(..), getAuxiliaryInfo)
where

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.Types

import Futhark.Optimise.MemoryBlockMerging.VariableMemory
import Futhark.Optimise.MemoryBlockMerging.MemoryAliases
import Futhark.Optimise.MemoryBlockMerging.VariableAliases
import Futhark.Optimise.MemoryBlockMerging.Liveness.FirstUse
import Futhark.Optimise.MemoryBlockMerging.Liveness.LastUse
import Futhark.Optimise.MemoryBlockMerging.Liveness.Interference
import Futhark.Optimise.MemoryBlockMerging.ActualVariables
import Futhark.Optimise.MemoryBlockMerging.Existentials

-- Information needed by multiple transformations.
data AuxiliaryInfo = AuxiliaryInfo
  { auxName :: Name -- For debugging.
  , auxVarMemMappings :: VarMemMappings MemorySrc
  , auxMemAliases :: MemAliases
  , auxVarAliases :: VarAliases
  , auxFirstUses :: FirstUses
  , auxLastUses :: LastUses
  , auxInterferences :: Interferences
  , auxPotentialKernelDataRaceInterferences
    :: PotentialKernelDataRaceInterferences
  , auxActualVariables :: ActualVariables
  , auxExistentials :: Names
  }
  deriving (Show)

getAuxiliaryInfo :: FunDef ExplicitMemory -> AuxiliaryInfo
getAuxiliaryInfo fundef =
  let name = funDefName fundef
      var_to_mem = findVarMemMappings fundef
      mem_aliases = findMemAliases fundef var_to_mem
      var_aliases = findVarAliases fundef
      first_uses = findFirstUses var_to_mem mem_aliases fundef
      last_uses = findLastUses var_to_mem mem_aliases first_uses existentials
                  fundef
      (interferences, potential_kernel_interferences) =
        findInterferences var_to_mem mem_aliases first_uses last_uses
        existentials fundef
      actual_variables = findActualVariables var_to_mem first_uses fundef
      existentials = findExistentials fundef
  in AuxiliaryInfo
     { auxName = name
     , auxVarMemMappings = var_to_mem
     , auxMemAliases = mem_aliases
     , auxVarAliases = var_aliases
     , auxFirstUses = first_uses
     , auxLastUses = last_uses
     , auxInterferences = interferences
     , auxPotentialKernelDataRaceInterferences = potential_kernel_interferences
     , auxActualVariables = actual_variables
     , auxExistentials = existentials
     }
