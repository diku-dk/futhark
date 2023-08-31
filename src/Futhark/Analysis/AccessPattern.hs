module Futhark.Analysis.AccessPattern
  ( analyzeMemoryAccessPatterns
  , MemoryAccessPattern
  )
where

import Data.Map.Strict qualified as M
import Futhark.IR.Syntax
import Futhark.IR.GPU
import Futhark.Tools

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType =
  Sequential | Parallel
  deriving (Eq, Ord, Show)

-- | Pattern determines what kind of physical pattern is used to index.
-- This is subject to get removed.
data Pattern = Linear | Random
  deriving (Eq, Ord, Show)

-- | Variance represents whether the index is variant, or invariant to the outer
-- kernel/iteration function.
data Variance = Variant | Invariant
  deriving (Eq, Ord, Show)

data MemoryAccessPattern =
  MemoryAccessPattern
    { iterationType :: IterationType
    , pattern :: Pattern
    , variance :: Variance
    }

data MemoryEntry = MemoryEntry
    { idxExpr :: VName
    , memAccessPatterns :: [MemoryAccessPattern]
    }

-- Theres probably a more readable way of doing this.
type ArrayIndexDescriptors = M.Map VName [ MemoryEntry ]
  --deriving (Eq, Ord, Show)


analyzeMemoryAccessPatterns :: Prog GPU -> ArrayIndexDescriptors
--analyzeMemoryAccessPatterns (Prog {}) = []
analyzeMemoryAccessPatterns _ = M.empty
