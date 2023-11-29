module Futhark.Pass.OptimizeArrayLayout (optimizeArrayLayout, printAST) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.AnalyzePrimExp
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
    $ \prog -> do
      -- Analyse the program
      let index_table = analysisPropagateByTransitivity $ analyzeDimAccesss prog
      -- Compute primExps for all variables
      let primExpTable = primExpAnalysis prog
      -- Compute permutations to acheive coalescence for all arrays
      let permutation_table = permutationTableFromIndexTable primExpTable index_table
      -- Insert permutations in the AST
      intraproceduralTransformation (onStms permutation_table) prog
  where
    onStms permutationTable scope stms = do
      let m = localScope scope $ transformStms permutationTable mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)
