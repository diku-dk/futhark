module Futhark.Optimise.ArrayLayout
  ( optimiseArrayLayoutGPU,
    optimiseArrayLayoutMC,
  )
where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.AnalysePrimExp
import Futhark.Builder
import Futhark.IR.Aliases
import Futhark.IR.GPU (GPU)
import Futhark.IR.MC (MC)
import Futhark.Optimise.ArrayLayout.Layout
import Futhark.Optimise.ArrayLayout.Transform
import Futhark.Pass

optimiseArrayLayout :: (Transform rep, BuilderOps rep) => String -> Pass rep rep
optimiseArrayLayout s =
  Pass
    ("optimise array layout " <> s)
    "Transform array layout for locality optimisations."
    $ \prog -> do
      -- Analyse the program
      let index_table = analyseDimAccesss prog
      -- Compute primExps for all variables
      let primExpTable = primExpAnalysis prog
      -- Compute permutations to acheive coalescence for all arrays
      let permutation_table = layoutTableFromIndexTable primExpTable index_table
      -- Insert permutations in the AST
      intraproceduralTransformation (onStms permutation_table) prog
  where
    onStms layout_table scope stms = do
      let m = localScope scope $ transformStms layout_table mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)

-- | The optimisation performed on the GPU representation.
optimiseArrayLayoutGPU :: Pass GPU GPU
optimiseArrayLayoutGPU = optimiseArrayLayout "gpu"

-- | The optimisation performed on the MC representation.
optimiseArrayLayoutMC :: Pass MC MC
optimiseArrayLayoutMC = optimiseArrayLayout "mc"
