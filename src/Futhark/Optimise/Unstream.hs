{-# LANGUAGE FlexibleContexts #-}
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
module Futhark.Optimise.Unstream
  (unstreamKernels, unstreamMC) where

import Control.Monad.State
import Control.Monad.Reader

import Futhark.MonadFreshNames
import Futhark.IR.Kernels
import qualified Futhark.IR.Kernels as Kernels
import Futhark.IR.Kernels.Simplify (simplifyKernels)
import Futhark.IR.MC
import qualified Futhark.IR.MC as MC
import Futhark.Pass
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT

-- | The pass for GPU kernels.
unstreamKernels :: Pass Kernels Kernels
unstreamKernels = unstream onHostOp simplifyKernels

-- | The pass for multicore.
unstreamMC :: Pass MC MC
unstreamMC = unstream onMCOp MC.simplifyProg

data Stage = SeqStreams | SeqAll

unstream :: ASTLore lore =>
            (Stage -> OnOp lore) -> (Prog lore -> PassM (Prog lore))
         -> Pass lore lore
unstream onOp simplify =
  Pass "unstream" "sequentialise remaining SOACs" $
  intraproceduralTransformation (optimise SeqStreams)
  >=> simplify
  >=> intraproceduralTransformation (optimise SeqAll)
  where optimise stage scope stms =
          modifyNameSource $ runState $
          runReaderT (optimiseStms (onOp stage) stms) scope

type UnstreamM lore = ReaderT (Scope lore) (State VNameSource)

type OnOp lore =
  Pattern lore -> StmAux (ExpDec lore) -> Op lore -> UnstreamM lore [Stm lore]

optimiseStms :: ASTLore lore =>
                OnOp lore -> Stms lore -> UnstreamM lore (Stms lore)
optimiseStms onOp stms =
  localScope (scopeOf stms) $
  stmsFromList . concat <$> mapM (optimiseStm onOp) (stmsToList stms)

optimiseBody :: ASTLore lore =>
                OnOp lore -> Body lore -> UnstreamM lore (Body lore)
optimiseBody onOp (Body aux stms res) =
  Body aux <$> optimiseStms onOp stms <*> pure res

optimiseKernelBody :: ASTLore lore =>
                      OnOp lore -> KernelBody lore
                   -> UnstreamM lore (KernelBody lore)
optimiseKernelBody onOp (KernelBody attr stms res) =
  localScope (scopeOf stms) $
  KernelBody attr <$>
  (stmsFromList . concat <$> mapM (optimiseStm onOp) (stmsToList stms)) <*>
  pure res

optimiseLambda :: ASTLore lore =>
                  OnOp lore -> Lambda lore -> UnstreamM lore (Lambda lore)
optimiseLambda onOp lam = localScope (scopeOfLParams $ lambdaParams lam) $ do
  body <- optimiseBody onOp $ lambdaBody lam
  return lam { lambdaBody = body}

optimiseStm :: ASTLore lore =>
               OnOp lore -> Stm lore -> UnstreamM lore [Stm lore]

optimiseStm onOp (Let pat aux (Op op)) =
  onOp pat aux op

optimiseStm onOp (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = \scope ->
                                      localScope scope . optimiseBody onOp
                                  }

optimiseSegOp :: ASTLore lore =>
                 OnOp lore -> SegOp lvl lore
              -> UnstreamM lore (SegOp lvl lore)
optimiseSegOp onOp op =
  localScope (scopeOfSegSpace $ segSpace op) $ mapSegOpM optimise op
  where optimise = identitySegOpMapper { mapOnSegOpBody = optimiseKernelBody onOp
                                       , mapOnSegOpLambda = optimiseLambda onOp
                                       }

onMCOp :: Stage -> OnOp MC
onMCOp stage pat aux (ParOp par_op op) = do
  par_op' <- traverse (optimiseSegOp (onMCOp stage)) par_op
  op' <- optimiseSegOp (onMCOp stage) op
  pure [Let pat aux $ Op $ ParOp par_op' op']
onMCOp stage pat aux (MC.OtherOp soac)
  | sequentialise stage soac = do
      stms <- runBinder_ $ FOT.transformSOAC pat soac
      fmap concat $ localScope (scopeOf stms) $
        mapM (optimiseStm (onMCOp stage)) $ stmsToList stms
  | otherwise =
      -- Still sequentialise whatever's inside.
      pure <$> (Let pat aux . Op . MC.OtherOp <$> mapSOACM optimise soac)
        where optimise = identitySOACMapper
                         { mapOnSOACLambda = optimiseLambda (onMCOp stage) }

sequentialise :: Stage -> SOAC lore -> Bool
sequentialise SeqStreams Stream{} = True
sequentialise SeqStreams _ = False
sequentialise SeqAll _ = True

onHostOp :: Stage -> OnOp Kernels

onHostOp stage pat aux (Kernels.OtherOp soac)
  | sequentialise stage soac = do
      stms <- runBinder_ $ FOT.transformSOAC pat soac
      fmap concat $ localScope (scopeOf stms) $
        mapM (optimiseStm (onHostOp stage)) $ stmsToList stms
  | otherwise =
      -- Still sequentialise whatever's inside.
      pure <$> (Let pat aux . Op . Kernels.OtherOp <$> mapSOACM optimise soac)
        where optimise = identitySOACMapper
                         { mapOnSOACLambda = optimiseLambda (onHostOp stage) }

onHostOp stage pat aux (SegOp op) =
  pure <$> (Let pat aux . Op . SegOp <$> optimiseSegOp (onHostOp stage) op)

onHostOp _ pat aux op = return [Let pat aux $ Op op]
