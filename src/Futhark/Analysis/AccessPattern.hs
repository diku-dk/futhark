module Futhark.Analysis.AccessPattern ( analyseMemoryAccessPatterns, analyseMemoryAccessVariance )
where

import Futhark.IR.GPU

data Pattern = Sequential
             | Parallel
             | Linear
             | Scatter
             | Random


data Variance = Variant Pattern
              | Invariant Pattern


analyseMemoryAccessPatterns :: GPU -> GPU
analyseMemoryAccessPatterns = id


analyseMemoryAccessVariance :: GPU -> GPU
analyseMemoryAccessVariance = id
