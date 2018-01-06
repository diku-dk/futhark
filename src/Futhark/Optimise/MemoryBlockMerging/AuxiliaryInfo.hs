{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Helper information for the main optimisation passes.
module Futhark.Optimise.MemoryBlockMerging.AuxiliaryInfo where

import qualified Data.Map.Strict as M
import qualified Data.List as L

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous

import Futhark.Optimise.MemoryBlockMerging.VariableMemory
import Futhark.Optimise.MemoryBlockMerging.MemoryAliases
import Futhark.Optimise.MemoryBlockMerging.VariableAliases
import Futhark.Optimise.MemoryBlockMerging.Liveness.FirstUse
import Futhark.Optimise.MemoryBlockMerging.Liveness.LastUse
import Futhark.Optimise.MemoryBlockMerging.Liveness.Interference
import Futhark.Optimise.MemoryBlockMerging.ActualVariables
import Futhark.Optimise.MemoryBlockMerging.Existentials


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

debugAuxiliaryInfo :: AuxiliaryInfo -> String -> IO ()
debugAuxiliaryInfo aux desc =
  aux `seq` auxVarMemMappings aux `seq` auxMemAliases aux `seq`
  auxVarAliases aux `seq` auxFirstUses aux `seq` auxLastUses aux `seq`
  auxInterferences aux `seq` auxPotentialKernelDataRaceInterferences `seq`
  auxActualVariables aux `seq` auxExistentials aux `seq`
  putBlock [ desc ++ ": Helper info in " ++ pretty (auxName aux) ++ ":"
           , replicate 70 '-'
           , "Variable-to-memory mappings:"
           , L.intercalate "\n" $ flip map (M.assocs $ auxVarMemMappings aux)
             $ \(var, memloc) ->
               "For " ++ pretty var ++ ": " ++
               pretty (memSrcName memloc) ++ ", " ++ show (memSrcIxFun memloc)
           , replicate 70 '-'
           , "Memory aliases:"
           , L.intercalate "\n" $ flip map (M.assocs $ auxMemAliases aux)
             $ \(mem, aliases) ->
               "For " ++ pretty mem ++ ": " ++ prettySet aliases
           , replicate 70 '-'
           , "Variables aliases:"
           , L.intercalate "\n" $ flip map (M.assocs $ auxVarAliases aux)
             $ \(var, aliases) ->
               "For " ++ pretty var ++ ": " ++ prettySet aliases
           , replicate 70 '-'
           , "First uses of memory:"
           , L.intercalate "\n" $ flip map (M.assocs $ auxFirstUses aux)
             $ \(stmt_var, mems) ->
               "In " ++ pretty stmt_var ++ ": " ++ prettySet mems
           , replicate 70 '-'
           , "Last uses of memory:"
           , L.intercalate "\n" $ flip map (M.assocs $ auxLastUses aux)
             $ \(var, mems) ->
               let pret = case var of
                     FromStm stmt_var -> "stm@" ++ pretty stmt_var
                     FromRes res_var -> "res@" ++ pretty res_var
               in "In " ++ pret ++ ": " ++ prettySet mems
           , replicate 70 '-'
           , "Interferences of memory blocks:"
           , L.intercalate "\n" $ flip map (M.assocs $ auxInterferences aux)
             $ \(mem, mems) ->
               "In " ++ pretty mem ++ ": " ++ prettySet mems
           , replicate 70 '-'
           , "Potential kernel data race interferences of memory blocks:"
           , L.intercalate "\n" $ map show (auxPotentialKernelDataRaceInterferences aux)
           , replicate 70 '-'
           , "Actual variables:"
           , L.intercalate "\n" $ flip map (M.assocs $ auxActualVariables aux)
             $ \(var, vars) ->
               "For " ++ pretty var ++ ": " ++ prettySet vars
           , replicate 70 '-'
           , "Existentials:"
           , prettySet $ auxExistentials aux
           ]
