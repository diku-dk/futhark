module Futhark.Pass.OptimizeMemLayout (optimizeMemLayout, printAST) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Builder
import Futhark.IR.Aliases
import Futhark.Pass
import Futhark.Pass.OptimizeMemLayout.Layout
import Futhark.Pass.OptimizeMemLayout.Transform

printAST :: (RepTypes rep) => Pass rep rep
printAST =
  Pass
    "pretty print ast"
    "Pretty-print the ast at current stage in pipeline"
    $ pure . pTraceShowId

-- | The pass definition.
optimizeMemLayout :: (Transform rep, BuilderOps rep) => Pass rep rep
optimizeMemLayout =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- return
    $ \prog -> do
      -- Analyse the program
      let analysisRes = analysisPropagateByTransitivity $ analyzeDimIdxPats prog
      -- Compute permutations to acheive coalescence for all arrays
      let permutationTable = permutationTableFromIndexTable analysisRes
      -- Insert permutations in the AST
      intraproceduralTransformation (onStms permutationTable) prog
  where
    onStms permutationTable scope stms = do
      let m = localScope scope $ transformStms permutationTable mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)
