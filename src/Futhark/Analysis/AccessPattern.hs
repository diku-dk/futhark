module Futhark.Analysis.AccessPattern
  ( analyzeMemoryAccessPatterns,
    analyseStm,
    ArrayIndexDescriptors,
  )
where

import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.IR.GPU

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType = Sequential | Parallel
  deriving (Eq, Ord, Show)

-- | Pattern determines what kind of physical pattern is used to index.
-- This is subject to get removed.
data Pattern = Linear | Random
  deriving (Eq, Ord, Show)

-- | Variance represents whether the index is variant, or invariant to the outer
-- kernel/iteration function.
data Variance = Variant | Invariant
  deriving (Eq, Ord, Show)

-- | Collect all features of memory access together
data MemoryAccessPattern = MemoryAccessPattern
  { -- | Expression reference that is used to index into a given dimension
    idxExpr :: VName,
    iterationType :: IterationType,
    pattern :: Pattern,
    variance :: Variance
  }
  deriving (Eq, Ord, Show)

-- | Each element in the list corresponds to a dimension in the given array
type MemoryEntry = [MemoryAccessPattern]

-- | We map variable names of arrays to lists of memory access patterns.
type ArrayIndexDescriptors = M.Map VName [MemoryEntry]

type FunAids = S.Set (Name, ArrayIndexDescriptors)

-- | For each `entry` we return a tuple of (function-name and AIDs)
analyzeMemoryAccessPatterns :: Prog GPU -> FunAids -- FunAids -- M.Map VName ArrayIndexDescriptors
-- analyzeMemoryAccessPatterns (Prog{progTypes = _, progConsts = _, progFuns = funs}) = M.empty
analyzeMemoryAccessPatterns prog =
  -- We map over the program functions (usually always entries)
  -- Then fold them together to a singular map.
  -- foldl' mergeAids M.empty .
  foldl' S.union S.empty $ getAids <$> progFuns prog

getAids :: FunDef GPU -> FunAids
getAids f =
  S.singleton
    ( funDefName f,
      -- merge results
      foldl' mergeMemAccTable M.empty
        -- map analyzation over stmts
        . fmap analyseStm
        -- functionBody -> [stm]
        . stmsToList
        . bodyStms
        . funDefBody $ f
    )

-- Concat the list off array access (note, access != dimensions)
mergeMemAccTable :: ArrayIndexDescriptors -> ArrayIndexDescriptors -> ArrayIndexDescriptors
mergeMemAccTable = M.unionWith (++)

-- TODO:
-- Add patterns here
analyseStm :: Stm GPU -> ArrayIndexDescriptors
analyseStm _ = M.empty
