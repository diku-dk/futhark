module Futhark.Pass.GALOP (galop, printAST) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.AnalysePrimExp
import Futhark.Builder
import Futhark.IR.Aliases
import Futhark.Pass
import Futhark.Pass.GALOP.Layout
import Futhark.Pass.GALOP.Transform

printAST :: (RepTypes rep) => Pass rep rep
printAST =
  Pass
    "pretty print ast"
    "Pretty-print the ast at current stage in pipeline"
    $ pure . pTraceShowId

-- | The pass definition.
galop :: (Transform rep, BuilderOps rep) => Pass rep rep
galop =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
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
