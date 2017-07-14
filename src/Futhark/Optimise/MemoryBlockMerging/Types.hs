module Futhark.Optimise.MemoryBlockMerging.Types where

import qualified Data.Map.Strict as M

import Futhark.Representation.AST
import qualified Futhark.Representation.ExplicitMemory as ExpMem



data MemorySrc = MemorySrc
  { memSrcName :: VName -- ^ the memory block name
  , memSrcIxFun :: ExpMem.IxFun -- ^ the index function into the memory
  , memSrcShape :: Shape -- ^ the shape of the memory block
  }
  deriving (Show)

data MemoryLoc = MemoryLoc
  { memLocName :: VName -- ^ the memory block name
  , memLocIxFun :: ExpMem.IxFun -- ^ the index function into the memory
  }
  deriving (Show)

-- A mapping from variable names to memory blocks (with varying details)
type VarMemMappings t = M.Map VName t

-- Aliasing of memory blocks, meaning multiple memory blocks refer to the same
-- actualy memory.  Aliasing is not commutative.
type MemAliases = M.Map VName Names

-- Aliasing of variables, meaning the use the same memory blocks.  Aliasing is
-- commutative?
type VarAliases = M.Map VName Names

type FirstUses = M.Map VName Names
type LastUses = M.Map VName Names

type Interferences = M.Map VName Names

-- Information needed by multiple transformations.
data AuxiliaryInfo = AuxiliaryInfo
  { auxName :: Name -- For debugging.
  , auxVarMemMappings :: VarMemMappings MemorySrc
  , auxMemAliases :: MemAliases
  , auxVarAliases :: VarAliases
  , auxFirstUses :: FirstUses
  , auxLastUses :: LastUses
  , auxInterferences :: Interferences
  }
  deriving (Show)
