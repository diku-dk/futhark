{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Sequentialise any remaining SOACs.  It is very important that
-- this is run *after* any access-pattern-related optimisation,
-- because this pass will destroy information.
module Futhark.Optimise.Unstream
  (unstreamKernels, unstreamMC) where

import Control.Monad.State
import Control.Monad.Reader

import Futhark.MonadFreshNames
import Futhark.IR.Kernels
import Futhark.IR.MC
import Futhark.Pass
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT

unstreamKernels :: Pass Kernels Kernels
unstreamKernels = unstream onHostOp

unstreamMC :: Pass MC MC
unstreamMC = unstream onSegOp

unstream :: ASTLore lore => OnOp lore -> Pass lore lore
unstream onOp = Pass "unstream" "sequentialise remaining SOACs" $
                intraproceduralTransformation optimise
  where optimise scope stms =
          modifyNameSource $ runState $ runReaderT (optimiseStms onOp stms) scope

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

onSegOp :: OnOp MC
onSegOp pat aux op =
  pure <$> (Let pat aux . Op <$> optimiseSegOp onSegOp op)

onHostOp :: OnOp Kernels

onHostOp pat aux (OtherOp soac) = do
  stms <- runBinder_ $ FOT.transformSOAC pat soac
  fmap concat $ localScope (scopeOf stms) $
    mapM (optimiseStm onHostOp) $ stmsToList $
    certify (stmAuxCerts aux) <$> stms

onHostOp pat aux (SegOp op) =
  pure <$> (Let pat aux . Op . SegOp <$> optimiseSegOp onHostOp op)

onHostOp pat aux op = return [Let pat aux $ Op op]
