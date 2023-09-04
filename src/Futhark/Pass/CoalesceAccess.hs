{-# LANGUAGE TypeFamilies #-}

-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.CoalesceAccess (coalesceAccess) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Builder
import Futhark.IR.GPU
import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.Rename

type CoalesceM = ReaderT (Scope GPU) (State VNameSource)

-- | The pass definition.
coalesceAccess :: Pass GPU GPU
coalesceAccess =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- \$ return
    $ \prog ->
      let prog' = analysis prog
       in intraproceduralTransformation onStms prog'
  where
    onStms scope stms =
      let a = undefined
       in modifyNameSource $
            runState $
              runReaderT (analyseStms (M.empty, M.empty) stms) scope

analysis :: Prog GPU -> Prog GPU
analysis prog = do
  let funaids = analyzeMemoryAccessPatterns prog
   in prog

-- Code below is temporary and only to make the compiler happy.

analyseBody :: Env -> Body GPU -> CoalesceM (Body GPU)
analyseBody env (Body () stms res) =
  Body () <$> analyseStms env stms <*> pure res

analyseStms :: Env -> Stms GPU -> CoalesceM (Stms GPU)
analyseStms env stms =
  localScope (scopeOf stms) $ do
    (_, stms') <- foldM foldfun (env, mempty) $ stmsToList stms
    pure stms'
  where
    foldfun :: (Env, Stms GPU) -> Stm GPU -> CoalesceM (Env, Stms GPU)
    foldfun (e, ss) s = do
      (e', s') <- analyseStm_ e s
      pure (e', ss <> s')

analyseStm_ :: Env -> Stm GPU -> CoalesceM (Env, Stms GPU)
-- analyseStm env stm@(Let _ _ (Op (SegOp (SegMap SegThread {} _ _ _)))) = do
--   res_genred_opt <- genRedOpts env stm
--   let stms' =
--         case res_genred_opt of
--           Just stms -> stms
--           Nothing -> oneStm stm
--   pure (env, stms')
analyseStm_ env (Let pat aux e) = do
  env' <- changeEnv env (head $ patNames pat) e
  e' <- mapExpM (analyse env') e
  -- pTrace "\nenv':\n" $ pTraceShow env' $ pTrace "\ne':\n" $ pTraceShow e' $
  pure (env', oneStm $ Let pat aux e')
  where
    analyse env' = identityMapper {mapOnBody = \scope -> localScope scope . analyseBody env'}