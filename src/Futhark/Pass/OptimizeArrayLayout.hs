module Futhark.Pass.OptimizeArrayLayout (optimizeArrayLayout, printAST) where

import Control.Monad.State.Strict
import Data.Foldable
import Data.Map.Strict qualified as M
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.AnalyzePrimExp
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.Aliases
import Futhark.Pass
import Futhark.Pass.OptimizeArrayLayout.Layout
import Futhark.Pass.OptimizeArrayLayout.Transform

printAST :: (RepTypes rep) => Pass rep rep
printAST =
  Pass
    "pretty print ast"
    "Pretty-print the ast at current stage in pipeline"
    $ pure . pTraceShowId

-- | The pass definition.
optimizeArrayLayout :: (Transform rep, BuilderOps rep) => Pass rep rep
optimizeArrayLayout =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- return
    $ \prog -> do
      -- Analyse the program
      -- let indexTable = analysisPropagateByTransitivity $ analyzeDimAccesss prog
      let indexTable = analyzeDimAccesss prog
      -- Compute primExps for all variables
      let primExpTable = primExpAnalysis prog
      -- Compute permutations to acheive coalescence for all arrays
      let permutationTable = permutationTableFromIndexTable primExpTable indexTable
      -- Insert permutations in the AST
      intraproceduralTransformation (onStms permutationTable) prog
  where
    onStms permutationTable scope stms = do
      let m = localScope scope $ transformStms permutationTable mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)
