module Futhark.Optimise.MemoryBlockMerging.Types
  ( MName
  , MNames
  , MemorySrc(..)
  , MemoryLoc(..)
  , VarMemMappings
  , MemAliases
  , VarAliases
  , FirstUses
  , StmOrRes(..)
  , LastUses
  , Interferences
  , ActualVariables
  , PotentialKernelDataRaceInterferences
  , PotentialKernelDataRaceInterferenceGroup
  , KernelFirstUse
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Semigroup as Sem

import Futhark.Representation.AST
import qualified Futhark.Representation.ExplicitMemory as ExpMem


-- | Memory block VName.
type MName = VName

-- | Memory block names.
type MNames = Names

data MemorySrc = MemorySrc
  { memSrcName :: MName -- ^ the memory block name
  , memSrcIxFun :: ExpMem.IxFun -- ^ the index function into the memory
  , memSrcShape :: Shape -- ^ the shape of the original array
  }
  deriving (Show, Eq)

data MemoryLoc = MemoryLoc
  { memLocName :: MName -- ^ the memory block name
  , memLocIxFun :: ExpMem.IxFun -- ^ the index function into the memory
  }
  deriving (Show, Eq)

-- A mapping from variable names to memory blocks (with varying details)
type VarMemMappings t = M.Map VName t

-- Aliasing of memory blocks, meaning multiple memory blocks refer to the same
-- actualy memory.  Aliasing is not commutative.
type MemAliases = M.Map MName MNames

-- Aliasing of variables, meaning the use the same memory blocks.  Aliasing is
-- commutative?
type VarAliases = M.Map VName Names

-- First uses of memory blocks in statement denoted by variable name.
type FirstUses = M.Map VName MNames

-- A last use can occur in a statement OR in a body result.
data StmOrRes = FromStm VName
              | FromRes VName
  deriving (Show, Eq, Ord)
type LastUses = M.Map StmOrRes MNames

-- Interferences between memory blocks.
type Interferences = M.Map MName MNames

-- Sets of potential interferences inside kernels because of potential data
-- races.  For each set, every memory block *can* interfere with every other
-- memory block, but only in dire edge cases.  Usually some of them can be said
-- to not interfere, and sometimes array creation statements can be modified to
-- have fewer interferences.  See Reuse/Core.hs.
type PotentialKernelDataRaceInterferences =
  [PotentialKernelDataRaceInterferenceGroup]
type PotentialKernelDataRaceInterferenceGroup = [KernelFirstUse]
type KernelFirstUse = (MName, VName, PrimType, ExpMem.IxFun)

-- "Links" for handling how variables belong together.
type ActualVariables = M.Map VName Names

-- Log keeping.  Statement variable names to a list of topic-content-mappings.
newtype Log = Log (M.Map VName [(String, String)])
  deriving (Show, Eq, Ord)

instance Sem.Semigroup Log where
  Log a <> Log b = Log $ M.unionWith (++) a b

instance Monoid Log where
  mempty = Log M.empty
  mappend = (Sem.<>)
