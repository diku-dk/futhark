{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Helper information for the main optimisation passes.
module Futhark.Optimise.MemoryBlockMerging.AuxiliaryInfo where

import qualified Data.Map.Strict as M
import Control.Monad

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
      last_uses = findLastUses var_to_mem mem_aliases first_uses existentials fundef
      interferences = findInterferences var_to_mem mem_aliases first_uses last_uses existentials fundef
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
     , auxActualVariables = actual_variables
     , auxExistentials = existentials
     }

debugAuxiliaryInfo :: AuxiliaryInfo -> String -> IO ()
debugAuxiliaryInfo aux desc =
  aux `seq` auxVarMemMappings aux `seq` auxMemAliases aux `seq`
  auxVarAliases aux `seq` auxFirstUses aux `seq` auxLastUses aux `seq`
  auxInterferences aux `seq` auxActualVariables aux `seq`
  auxExistentials aux `seq` do
  putStrLn $ replicate 70 '='
  putStrLn (desc ++ ": Helper info in " ++ pretty (auxName aux) ++ ":")
  putStrLn $ replicate 70 '-'
  putStrLn "Variable-to-memory mappings:"
  forM_ (M.assocs $ auxVarMemMappings aux) $ \(var, memloc) ->
    putStrLn ("For " ++ pretty var ++ ": " ++
              pretty (memSrcName memloc) ++ ", " ++ show (memSrcIxFun memloc))
  putStrLn $ replicate 70 '-'
  putStrLn "Memory aliases:"
  forM_ (M.assocs $ auxMemAliases aux) $ \(mem, aliases) ->
    putStrLn ("For " ++ pretty mem ++ ": " ++ prettySet aliases)
  putStrLn $ replicate 70 '-'
  putStrLn "Variables aliases:"
  forM_ (M.assocs $ auxVarAliases aux) $ \(var, aliases) ->
    putStrLn ("For " ++ pretty var ++ ": " ++ prettySet aliases)
  putStrLn $ replicate 70 '-'
  putStrLn "First uses of memory:"
  forM_ (M.assocs $ auxFirstUses aux) $ \(stmt_var, mems) ->
    putStrLn ("In " ++ pretty stmt_var ++ ": " ++ prettySet mems)
  putStrLn $ replicate 70 '-'
  putStrLn "Last uses of memory:"
  forM_ (M.assocs $ auxLastUses aux) $ \(var, mems) -> do
    let pret = case var of
          FromStm stmt_var -> "stm@" ++ pretty stmt_var
          FromRes res_var -> "res@" ++ pretty res_var
    putStrLn ("In " ++ pret ++ ": " ++ prettySet mems)
  putStrLn $ replicate 70 '-'
  putStrLn "Interferences of memory blocks:"
  forM_ (M.assocs $ auxInterferences aux) $ \(mem, mems) ->
    putStrLn ("In " ++ pretty mem ++ ": " ++ prettySet mems)
  putStrLn $ replicate 70 '-'
  putStrLn "Actual variables:"
  forM_ (M.assocs $ auxActualVariables aux) $ \(var, vars) ->
    putStrLn ("For " ++ pretty var ++ ": " ++ prettySet vars)
  putStrLn $ replicate 70 '-'
  putStrLn "Existentials:"
  putStrLn $ prettySet $ auxExistentials aux
  putStrLn $ replicate 70 '='
  putStrLn ""
