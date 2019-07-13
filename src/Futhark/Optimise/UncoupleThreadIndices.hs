{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Uncouple local thread indices from their SegOp.  This enables
-- hoisting out of the SegOp.  It is very important that this is run
-- *after* any access-pattern-related optimisation, because this pass
-- will destroy information.
module Futhark.Optimise.UncoupleThreadIndices (uncoupleThreadIndices) where

import Control.Monad.State
import Control.Monad.Reader

import Futhark.Analysis.PrimExp.Convert
import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Pass
import Futhark.Tools

computeSegSpaceIndices :: SegSpace -> SubExp -> Binder Kernels ()
computeSegSpaceIndices space ltid_flat = do
  let (tids, dims) = unzip $ unSegSpace space
      computed = unflattenIndex (map (primExpFromSubExp int32) dims) $
                 primExpFromSubExp int32 ltid_flat
  forM_ (zip tids computed) $ \(tid, e) ->
    letBindNames_ [tid] =<< toExp e

dummyNamesForSegSpace :: MonadFreshNames m => SegSpace -> m SegSpace
dummyNamesForSegSpace (SegSpace flat tids_and_dims) =
  fmap (SegSpace flat) $ forM tids_and_dims $ \(tid, dim) -> do
    tid' <- newVName $ baseString tid ++ "_dummy"
    return (tid', dim)

data SegInfo = SegInfo VName SegLevel

type UncoupleM = ReaderT (Scope Kernels) (State VNameSource)

optimiseBody :: Maybe SegInfo -> Body Kernels -> UncoupleM (Body Kernels)
optimiseBody info (Body () stms res) =
  localScope (scopeOf stms) $
  Body ()
  <$> (mconcat <$> mapM (optimiseStm info) (stmsToList stms))
  <*> pure res

optimiseKernelBody :: Maybe SegInfo -> KernelBody Kernels -> UncoupleM (KernelBody Kernels)
optimiseKernelBody info (KernelBody () stms res) =
  localScope (scopeOf stms) $
  KernelBody ()
  <$> (mconcat <$> mapM (optimiseStm info) (stmsToList stms))
  <*> pure res

optimiseLambda :: Maybe SegInfo -> Lambda Kernels -> UncoupleM (Lambda Kernels)
optimiseLambda info lam = localScope (scopeOfLParams $ lambdaParams lam) $ do
  body <- optimiseBody info $ lambdaBody lam
  return lam { lambdaBody = body }

optimiseStm :: Maybe SegInfo -> Stm Kernels -> UncoupleM (Stms Kernels)

optimiseStm info (Let pat aux (Op (SegOp op)))
  | Just (SegInfo phys (SegGroup _num_groups group_size _)) <- info,
    segVirt (segLevel op) == SegNoVirt = runBinder_ $ do
      ltid_flat <- letSubExp "ltid_flat" $ BasicOp $ BinOp (SRem Int32) (Var phys) (unCount group_size)
      computeSegSpaceIndices (segSpace op) ltid_flat
      dummy_space <- dummyNamesForSegSpace $ segSpace op
      addStm $ Let pat aux $ Op $ SegOp $ setSegSpace dummy_space op
  | otherwise =
      oneStm . Let pat aux . Op . SegOp <$>
      localScope (scopeOfSegSpace (segSpace op)) (mapSegOpM mapper op)
  where op_info = Just (SegInfo (segFlat (segSpace op)) (segLevel op))
        info' = maybe op_info Just info
        mapper = identitySegOpMapper { mapOnSegOpLambda = optimiseLambda info'
                                     , mapOnSegOpBody = optimiseKernelBody info'
                                     }

optimiseStm info (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = \scope ->
                                      localScope scope . optimiseBody info
                                  }

optimiseFunDef :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
optimiseFunDef fundec = do
  body' <- modifyNameSource $ runState $
           runReaderT m (scopeOfFParams (funDefParams fundec))
  return fundec { funDefBody = body' }
  where m = optimiseBody Nothing $ funDefBody fundec

uncoupleThreadIndices :: Pass Kernels Kernels
uncoupleThreadIndices = Pass "uncouple" "uncouple local thread indices" $
                        intraproceduralTransformation optimiseFunDef
