{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Multiversion segmented reduction.
module Futhark.Pass.ExtractKernels.SegmentedReduce
       ( regularSegmentedReduce
       , regularSegmentedReduceAsScan
       )
       where

import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Lazy as HM

import Prelude

import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.Kernels
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass.ExtractKernels.BlockedKernel

regularSegmentedReduce :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                          SubExp
                       -> SubExp
                       -> Pattern Kernels
                       -> Certificates
                       -> Commutativity
                       -> LambdaT InKernel
                       -> [(SubExp, VName)]
                       -> m ()
regularSegmentedReduce segment_size num_segments pat cs comm lam reduce_inps = do
  let (nes, full_arrs) = unzip reduce_inps
  i <- newVName "i"

  loop_vals <- zipWithM mergeParam (patternValueNames pat) acct
  loop_vals_init <- forM acct $ \t ->
    letSubExp "segmented_reduce_scratch" $ PrimOp $ Scratch (elemType t) (num_segments : arrayDims t)

  segmented_arrs <- forM (zip full_arrs acct) $ \(arr, t) ->
    let newshape = map DimNew $ num_segments : segment_size : arrayDims t
    in letExp (baseString arr ++ "_segmented") $ PrimOp $ Reshape [] newshape arr

  loop_body <- runBodyBinder $ localScope (scopeOfFParams loop_vals) $ do
    red_pat <- Pattern [] <$> zipWithM redPatElem (patternValueNames pat) acct

    arrs <- forM segmented_arrs $ \segmented_arr ->
      letExp (baseString segmented_arr ++ "_indexed") $
      PrimOp $ Index [] segmented_arr [DimFix $ Var i]

    mapM_ addBinding =<< blockedReduction red_pat cs segment_size comm lam lam nes arrs

    loop_vals' <- forM (zip loop_vals $ patternValueNames red_pat) $ \(loop_param, red_out) ->
      letInPlace (baseString (paramName loop_param)) [] (paramName loop_param) [Var i] $
      PrimOp $ SubExp $ Var red_out

    return $ resultBody $ map Var loop_vals'

  flat_pat <- Pattern [] <$> zipWithM loopPatElem (patternValueNames pat) acct

  addBinding $ Let flat_pat () $ DoLoop [] (zip loop_vals loop_vals_init)
    (ForLoop i num_segments) loop_body

  forM_ (zip (patternValueElements pat) (patternValueNames flat_pat)) $ \(to, from) ->
    letBind_ (Pattern [] [to]) $ PrimOp $
    Reshape [] (map DimNew $ arrayDims $ patElemType to) from

  where redPatElem pe_name t = do
          name <- newVName $ baseString pe_name
          return $ PatElem name BindVar t
        mergeParam pe_name t = do
          name <- newVName $ baseString pe_name
          return $ Param name $ toDecl (t `arrayOfRow` num_segments) Unique
        loopPatElem pe_name t = do
          name <- newVName $ baseString pe_name
          return $ PatElem name BindVar $ t `arrayOfRow` num_segments

        acct = lambdaReturnType lam

regularSegmentedReduceAsScan :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                                SubExp
                             -> SubExp
                             -> [SubExp]
                             -> Pattern Kernels
                             -> Pattern Kernels
                             -> Certificates
                             -> SubExp
                             -> Lambda InKernel
                             -> [(SubExp, VName)]
                             -> m ()
regularSegmentedReduceAsScan segment_size num_segments nest_sizes flat_pat pat cs w lam reduce_inps = do
  blockedSegmentedScan segment_size flat_pat cs w lam reduce_inps

  is <- replicateM (length nest_sizes) $ newVName "i"

  body <- runBodyBinder $ localScope (HM.fromList $ zip is $ repeat IndexInfo) $ do
    let segment_id = flattenIndex
                     (map SE.intSubExpToScalExp nest_sizes)
                     (map (SE.intSubExpToScalExp . Var) is)
        offset = (segment_id + 1) * SE.intSubExpToScalExp segment_size - 1
    j <- letSubExp "j" =<< SE.fromScalExp offset
    vals <- forM (patternValueIdents flat_pat) $ \arr ->
      letSubExp "v" $ PrimOp $ Index [] (identName arr) $
      fullSlice (identType arr) [DimFix j]
    return $ resultBody vals

  (mapk_bnds, mapk) <- mapKernelFromBody [] num_segments (zip is nest_sizes) [] acct body
  mapM_ addBinding mapk_bnds
  letBind_ pat $ Op mapk
  where acct = lambdaReturnType lam
