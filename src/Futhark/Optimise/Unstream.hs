{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Sequentialise any remaining SOACs.  It is very important that
-- this is run *after* any access-pattern-related optimisation,
-- because this pass will destroy information.
module Futhark.Optimise.Unstream (unstream) where

import Control.Monad.State
import Control.Monad.Reader

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Pass
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT

unstream :: Pass Kernels Kernels
unstream = Pass "unstream" "sequentialise remaining SOACs" $
           intraproceduralTransformation optimiseFunDef

optimiseFunDef :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
optimiseFunDef fundec = do
  body' <- modifyNameSource $ runState $
           runReaderT m (scopeOfFParams (funDefParams fundec))
  return fundec { funDefBody = body' }
  where m = optimiseBody $ funDefBody fundec

type UnstreamM = ReaderT (Scope Kernels) (State VNameSource)

optimiseBody :: Body Kernels -> UnstreamM (Body Kernels)
optimiseBody (Body () stms res) =
  localScope (scopeOf stms) $
  Body () <$> (stmsFromList . concat <$> mapM optimiseStm (stmsToList stms)) <*> pure res

optimiseKernelBody :: KernelBody Kernels -> UnstreamM (KernelBody Kernels)
optimiseKernelBody (KernelBody () stms res) =
  localScope (scopeOf stms) $
  KernelBody () <$> (stmsFromList . concat <$> mapM optimiseStm (stmsToList stms)) <*> pure res

optimiseLambda :: Lambda Kernels -> UnstreamM (Lambda Kernels)
optimiseLambda lam = localScope (scopeOfLParams $ lambdaParams lam) $ do
  body <- optimiseBody $ lambdaBody lam
  return lam { lambdaBody = body}

optimiseStm :: Stm Kernels -> UnstreamM [Stm Kernels]

optimiseStm (Let pat _ (Op (OtherOp soac))) = do
  stms <- runBinder_ $ FOT.transformSOAC pat soac
  fmap concat $ localScope (scopeOf stms) $ mapM optimiseStm $ stmsToList stms

optimiseStm (Let pat aux (Op (Husk space lam nes ts body))) =
  localScope (scopeOfHuskSpace space) $
  pure <$> (Let pat aux . Op <$>
            (Husk space <$> optimiseLambda lam <*>
             pure nes <*> pure ts <*> optimiseBody body))

optimiseStm (Let pat aux (Op (SegOp op))) =
  localScope (scopeOfSegSpace $ segSpace op) $
  pure <$> (Let pat aux . Op . SegOp <$> mapSegOpM optimise op)
  where optimise = identitySegOpMapper { mapOnSegOpBody = optimiseKernelBody
                                       , mapOnSegOpLambda = optimiseLambda
                                       }

optimiseStm (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = \scope -> localScope scope . optimiseBody }
