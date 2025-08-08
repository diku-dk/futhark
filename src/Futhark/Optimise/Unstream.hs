{-# LANGUAGE TypeFamilies #-}

-- | Sequentialise any remaining SOACs.  It is very important that
-- this is run *after* any access-pattern-related optimisation,
-- because this pass will destroy information.
--
-- This pass conceptually contains three subpasses:
--
-- 1. Sequentialise 'Stream' operations, leaving other SOACs intact.
--
-- 2. Apply whole-program simplification.
--
-- 3. Sequentialise remaining SOACs.
--
-- This is because sequentialisation of streams creates many SOACs
-- operating on single-element arrays, which can be efficiently
-- simplified away, but only *before* they are turned into loops.  In
-- principle this pass could be split into multiple, but for now it is
-- kept together.
module Futhark.Optimise.Unstream (unstreamGPU, unstreamMC) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Futhark.IR.GPU
import Futhark.IR.GPU qualified as GPU
import Futhark.IR.GPU.Simplify (simplifyGPU)
import Futhark.IR.MC
import Futhark.IR.MC qualified as MC
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT

-- | The pass for GPU kernels.
unstreamGPU :: Pass GPU GPU
unstreamGPU = unstream onHostOp simplifyGPU

-- | The pass for multicore.
unstreamMC :: Pass MC MC
unstreamMC = unstream onMCOp MC.simplifyProg

data Stage = SeqStreams | SeqAll

unstream ::
  (ASTRep rep) =>
  (Stage -> OnOp rep) ->
  (Prog rep -> PassM (Prog rep)) ->
  Pass rep rep
unstream onOp simplify =
  Pass "unstream" "sequentialise remaining SOACs" $
    intraproceduralTransformation (optimise SeqStreams)
      >=> simplify
      >=> intraproceduralTransformation (optimise SeqAll)
  where
    optimise stage scope stms =
      modifyNameSource $
        runState $
          runReaderT (optimiseStms (onOp stage) stms) scope

type UnstreamM rep = ReaderT (Scope rep) (State VNameSource)

type OnOp rep =
  Pat (LetDec rep) -> StmAux (ExpDec rep) -> Op rep -> UnstreamM rep [Stm rep]

optimiseStms ::
  (ASTRep rep) =>
  OnOp rep ->
  Stms rep ->
  UnstreamM rep (Stms rep)
optimiseStms onOp stms =
  localScope (scopeOf stms) $
    stmsFromList . concat <$> mapM (optimiseStm onOp) (stmsToList stms)

optimiseBody ::
  (ASTRep rep) =>
  OnOp rep ->
  GBody rep res ->
  UnstreamM rep (GBody rep res)
optimiseBody onOp (Body aux stms res) =
  Body aux <$> optimiseStms onOp stms <*> pure res

optimiseLambda ::
  (ASTRep rep) =>
  OnOp rep ->
  Lambda rep ->
  UnstreamM rep (Lambda rep)
optimiseLambda onOp lam = localScope (scopeOfLParams $ lambdaParams lam) $ do
  body <- optimiseBody onOp $ lambdaBody lam
  pure lam {lambdaBody = body}

optimiseStm ::
  (ASTRep rep) =>
  OnOp rep ->
  Stm rep ->
  UnstreamM rep [Stm rep]
optimiseStm onOp (Let pat aux (Op op)) =
  onOp pat aux op
optimiseStm onOp (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where
    optimise =
      identityMapper
        { mapOnBody = \scope ->
            localScope scope . optimiseBody onOp
        }

optimiseSegOp ::
  (ASTRep rep) =>
  OnOp rep ->
  SegOp lvl rep ->
  UnstreamM rep (SegOp lvl rep)
optimiseSegOp onOp op =
  localScope (scopeOfSegSpace $ segSpace op) $ mapSegOpM optimise op
  where
    optimise =
      identitySegOpMapper
        { mapOnSegOpBody = optimiseBody onOp,
          mapOnSegOpLambda = optimiseLambda onOp
        }

onMCOp :: Stage -> OnOp MC
onMCOp stage pat aux (ParOp par_op op) = do
  par_op' <- traverse (optimiseSegOp (onMCOp stage)) par_op
  op' <- optimiseSegOp (onMCOp stage) op
  pure [Let pat aux $ Op $ ParOp par_op' op']
onMCOp stage pat aux (MC.OtherOp soac)
  | sequentialise stage soac = do
      stms <- runBuilder_ $ auxing aux $ FOT.transformSOAC pat soac
      fmap concat . localScope (scopeOf stms) $
        mapM (optimiseStm (onMCOp stage)) (stmsToList stms)
  | otherwise =
      -- Still sequentialise whatever's inside.
      pure <$> (Let pat aux . Op . MC.OtherOp <$> mapSOACM optimise soac)
  where
    optimise =
      identitySOACMapper
        { mapOnSOACLambda = optimiseLambda (onMCOp stage)
        }

sequentialise :: Stage -> SOAC rep -> Bool
sequentialise SeqStreams Stream {} = True
sequentialise SeqStreams _ = False
sequentialise SeqAll _ = True

onHostOp :: Stage -> OnOp GPU
onHostOp stage pat aux (GPU.OtherOp soac)
  | sequentialise stage soac = do
      stms <- runBuilder_ $ auxing aux $ FOT.transformSOAC pat soac
      fmap concat . localScope (scopeOf stms) $
        mapM (optimiseStm (onHostOp stage)) (stmsToList stms)
  | otherwise =
      -- Still sequentialise whatever's inside.
      pure <$> (Let pat aux . Op . GPU.OtherOp <$> mapSOACM optimise soac)
  where
    optimise =
      identitySOACMapper
        { mapOnSOACLambda = optimiseLambda (onHostOp stage)
        }
onHostOp stage pat aux (SegOp op) =
  pure <$> (Let pat aux . Op . SegOp <$> optimiseSegOp (onHostOp stage) op)
onHostOp _ pat aux op = pure [Let pat aux $ Op op]
