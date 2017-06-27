{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Turn GroupStreams that operate on entire input into do-loops,
-- thus aiding subsequent optimisation.  It is very important that
-- this is run *after* any access-pattern-related optimisation,
-- because this pass will destroy information.
module Futhark.Optimise.Unstream
       ( unstream )
       where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.List

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels

import Futhark.Pass
import Futhark.Tools

unstream :: Pass Kernels Kernels
unstream =
  Pass { passName = "unstream"
       , passDescription = "Remove whole-array streams in kernels"
       , passFunction = intraproceduralTransformation optimiseFunDef
       }

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
  Body () <$> (concat <$> mapM optimiseStm stms) <*> pure res

optimiseStm :: Stm Kernels -> UnstreamM [Stm Kernels]
optimiseStm (Let pat () (Op (Kernel desc cs space ts body))) = do
  stms' <- localScope (scopeOfKernelSpace space) $
           runBinder_ $ optimiseInKernelStms $ kernelBodyStms body
  return [Let pat () $ Op $ Kernel desc cs space ts $ body { kernelBodyStms = stms' }]
optimiseStm (Let pat () e) =
  pure <$> (Let pat () <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = \scope -> localScope scope . optimiseBody }

type InKernelM = Binder InKernel

optimiseInKernelStms :: [Stm InKernel] -> InKernelM ()
optimiseInKernelStms = mapM_ optimiseInKernelStm

optimiseInKernelStm :: Stm InKernel -> InKernelM ()
optimiseInKernelStm (Let pat () (Op (GroupStream w max_chunk lam accs arrs)))
  | max_chunk == w = do
      let GroupStreamLambda chunk_size chunk_offset acc_params arr_params body = lam
      letBindNames'_ [chunk_size] $ BasicOp $ SubExp $ constant (1::Int32)

      loop_body <- insertStmsM $ do
        forM_ (zip arr_params arrs) $ \(p,a) ->
          letBindNames'_ [paramName p] $ BasicOp $ Index [] a $
          fullSlice (paramType p)
          [DimSlice (Var chunk_offset) (Var chunk_size) (constant (1::Int32))]
        optimiseInBody body

      -- Accumulators are updated in-place and must hence be unique.
      let merge = zip (map (fmap (`toDecl` Unique)) acc_params) accs
      letBind_ pat $ DoLoop [] merge (ForLoop chunk_offset Int32 w []) loop_body
optimiseInKernelStm (Let pat () e) =
  addStm =<< (Let pat () <$> mapExpM optimise e)
  where optimise = identityMapper
          { mapOnBody = \scope -> localScope scope . optimiseInBody }

optimiseInBody :: Body InKernel -> InKernelM (Body InKernel)
optimiseInBody body = do
  stms' <- collectStms_ $ optimiseInKernelStms $ bodyStms body
  return body { bodyStms = stms' }
