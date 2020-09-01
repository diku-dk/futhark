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
module Futhark.Optimise.Unstream (unstream) where

import Control.Monad.Reader
import Control.Monad.State
import Futhark.IR.Kernels
import Futhark.IR.Kernels.Simplify (simplifyKernels)
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT

data Stage = SeqStreams | SeqAll

-- | The pass definition.
unstream :: Pass Kernels Kernels
unstream =
  Pass "unstream" "sequentialise remaining SOACs" $
    intraproceduralTransformation (optimise SeqStreams)
      >=> simplifyKernels
      >=> intraproceduralTransformation (optimise SeqAll)
  where
    optimise stage scope stms =
      modifyNameSource $ runState $ runReaderT (optimiseStms stage stms) scope

type UnstreamM = ReaderT (Scope Kernels) (State VNameSource)

optimiseStms :: Stage -> Stms Kernels -> UnstreamM (Stms Kernels)
optimiseStms stage stms =
  localScope (scopeOf stms) $
    stmsFromList . concat <$> mapM (optimiseStm stage) (stmsToList stms)

optimiseBody :: Stage -> Body Kernels -> UnstreamM (Body Kernels)
optimiseBody stage (Body () stms res) =
  Body () <$> optimiseStms stage stms <*> pure res

optimiseKernelBody :: Stage -> KernelBody Kernels -> UnstreamM (KernelBody Kernels)
optimiseKernelBody stage (KernelBody () stms res) =
  localScope (scopeOf stms) $
    KernelBody ()
      <$> (stmsFromList . concat <$> mapM (optimiseStm stage) (stmsToList stms))
      <*> pure res

optimiseLambda :: Stage -> Lambda Kernels -> UnstreamM (Lambda Kernels)
optimiseLambda stage lam = localScope (scopeOfLParams $ lambdaParams lam) $ do
  body <- optimiseBody stage $ lambdaBody lam
  return lam {lambdaBody = body}

sequentialise :: Stage -> SOAC Kernels -> Bool
sequentialise SeqStreams Stream {} = True
sequentialise SeqStreams _ = False
sequentialise SeqAll _ = True

optimiseStm :: Stage -> Stm Kernels -> UnstreamM [Stm Kernels]
optimiseStm stage (Let pat aux (Op (OtherOp soac)))
  | sequentialise stage soac = do
      stms <- runBinder_ $ FOT.transformSOAC pat soac
      fmap concat $ localScope (scopeOf stms) $ mapM (optimiseStm stage) $ stmsToList stms
  | otherwise =
      -- Still sequentialise whatever's inside.
      pure <$> (Let pat aux . Op . OtherOp <$> mapSOACM optimise soac)
        where optimise = identitySOACMapper { mapOnSOACLambda = optimiseLambda stage }
optimiseStm stage (Let pat aux (Op (SegOp op))) =
  localScope (scopeOfSegSpace $ segSpace op) $
    pure <$> (Let pat aux . Op . SegOp <$> mapSegOpM optimise op)
  where
    optimise =
      identitySegOpMapper
        { mapOnSegOpBody = optimiseKernelBody stage,
          mapOnSegOpLambda = optimiseLambda stage
        }
optimiseStm stage (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where
    optimise = identityMapper {mapOnBody = \scope -> localScope scope . optimiseBody stage}
