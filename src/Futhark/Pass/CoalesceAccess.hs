{-# LANGUAGE TypeFamilies #-}

-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict qualified as M
import Data.Sequence.Internal qualified as S
import Debug.Pretty.Simple
import Debug.Trace
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.Pretty
import Futhark.Builder
import Futhark.IR.GPU
import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass

type CoalesceM = ReaderT (Scope GPU) (State VNameSource)

printAST :: Pass GPU GPU
printAST =
  Pass
    "pretty print ast"
    "Pretty-print the ast at current stage in pipeline"
    $ pure . pTraceShowId

-- | The pass definition.
coalesceAccess :: Pass GPU GPU
coalesceAccess =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- \$ return
    $ \prog ->
      let analysisRes = analyzeDimIdxPats prog
       in -- pTraceShow prog $ pTrace "\n------------------------\n" $ pTraceShow analysisRes $ pTrace "\n------------------------\n"
          intraproceduralTransformation (onStms analysisRes) prog
  where
    onStms analysisRes scope stms =
      modifyNameSource $ runState $ runReaderT (transformStms analysisRes stms) scope

-- TODO: coalesce things here
-- 1. Recurse through IR
-- 2. For each segOp:
--  1. For each array:
--    1. Find optimal permutation
--    2. If it doesn't already have optimal permutation, replace it with
--       a manifest (not that simple tho! Figure out what to do with an
--       array that should have multiple different manifestations)

type Ctx = ArrayIndexDescriptors

transformStms :: Ctx -> Stms GPU -> CoalesceM (Stms GPU)
transformStms ctx stms = do
  (_, stms') <- foldM foldfun (ctx, mempty) $ stmsToList stms
  pure stms'
  where
    foldfun :: (Ctx, Stms GPU) -> Stm GPU -> CoalesceM (Ctx, Stms GPU)
    foldfun (c, ss) s = do
      (c', s') <- transformStm c s
      pure (c', ss <> s')

transformStm :: Ctx -> Stm GPU -> CoalesceM (Ctx, Stms GPU)
transformStm ctx stm@(Let pat aux (Op (SegOp op))) = do
  let stm' = stm
  let ctx' = ctx
  pure (ctx', S.singleton stm')
transformStm ctx stm@(Let pat aux e) = do
  let stm' = stm
  let ctx' = ctx
  pure (ctx', S.singleton stm')
