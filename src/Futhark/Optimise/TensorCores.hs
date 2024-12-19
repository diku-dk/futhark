module Futhark.Optimise.TensorCores
  (tensorCoreMemFixup, extractTensorCores)
where

import Control.Monad
import Futhark.Pass
  ( Pass (..),
    intraproceduralTransformationWithConsts,
  )
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.Pass.Simplify
import Futhark.Optimise.TensorCores.ExtractTensorCores (transformProg)
import Futhark.Optimise.TensorCores.TensorCoreMemFixup (fixFuns)


-- | Transforms intragroup kernels corresponding to matrix multiplication into
-- function calls that use the Tensor Cores.
extractTensorCores :: Pass GPU GPU
extractTensorCores =
  Pass
    "tensor-mma"
    "Extracts NVIDIA tensor core MMA operations"
    transformProg

-- | Fixes up the memory allocation caused by inserting function calls for
-- tensor core operations.
tensorCoreMemFixup :: Pass GPUMem GPUMem
tensorCoreMemFixup =
  Pass
    "mma-fixup"
    "Extracts NVIDIA tensor core MMA operations"
    $ intraproceduralTransformationWithConsts pure fixFuns
      >=> passFunction simplifyGPUMem
