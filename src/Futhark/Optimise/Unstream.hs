{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Turn GroupStreams that operate on entire input or thread-variant
-- sizes into do-loops, thus aiding subsequent optimisation.  It is
-- very important that this is run *after* any access-pattern-related
-- optimisation, because this pass will destroy information.
module Futhark.Optimise.Unstream
       ( unstream )
       where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map as M

import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.Alias as Alias
import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Pass
import Futhark.Tools

unstream :: Pass Kernels Kernels
unstream = Pass "unstream" "Remove whole-array streams in kernels" $
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

optimiseStm :: Stm Kernels -> UnstreamM [Stm Kernels]
optimiseStm (Let pat aux (Op (HostOp (Kernel desc space ts body)))) = do
  inv <- S.fromList . M.keys <$> askScope
  stms' <- localScope (scopeOfKernelSpace space) $
           runBinder_ $ optimiseInKernelStms inv $ kernelBodyStms body
  return [Let pat aux $ Op $ HostOp $
          Kernel desc space ts $ body { kernelBodyStms = stms' }]
optimiseStm (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = \scope -> localScope scope . optimiseBody }

type Invariant = S.Set VName

type InKernelM = Binder InKernel

optimiseInKernelStms :: Invariant -> Stms InKernel -> InKernelM ()
optimiseInKernelStms inv = mapM_ (optimiseInKernelStm inv) . stmsToList

optimiseInKernelStm :: Invariant -> Stm InKernel -> InKernelM ()
optimiseInKernelStm inv (Let pat aux (Op (GroupStream w max_chunk lam accs arrs)))
  | max_chunk == w || maybe False (`S.notMember` inv) (subExpVar w) = do
      let GroupStreamLambda chunk_size chunk_offset acc_params arr_params body = lam
      letBindNames_ [chunk_size] $ BasicOp $ SubExp $ constant (1::Int32)

      loop_body <- insertStmsM $ do
        forM_ (zip arr_params arrs) $ \(p,a) ->
          letBindNames_ [paramName p] $
          BasicOp $ Index a $ fullSlice (paramType p)
          [DimSlice (Var chunk_offset) (Var chunk_size) (constant (1::Int32))]
        localScope (scopeOfLParams acc_params) $ optimiseInBody inv body

      -- Some accumulators may be updated in-place and must hence be unique.
      let lam_consumed = consumedInBody $ Alias.analyseBody $ groupStreamLambdaBody lam
          uniqueIfConsumed p | paramName p `S.member` lam_consumed =
                                 fmap (`toDecl` Unique) p
                             | otherwise = fmap (`toDecl` Nonunique) p
          merge = zip (map uniqueIfConsumed acc_params) accs
      certifying (stmAuxCerts aux) $
        letBind_ pat $ DoLoop [] merge (ForLoop chunk_offset Int32 w []) loop_body
optimiseInKernelStm inv (Let pat aux e) =
  addStm =<< (Let pat aux <$> mapExpM optimise e)
  where optimise = identityMapper
          { mapOnBody = \scope -> localScope scope . optimiseInBody inv }

optimiseInBody :: Invariant -> Body InKernel -> InKernelM (Body InKernel)
optimiseInBody inv body = do
  stms' <- collectStms_ $ optimiseInKernelStms inv $ bodyStms body
  return body { bodyStms = stms' }
