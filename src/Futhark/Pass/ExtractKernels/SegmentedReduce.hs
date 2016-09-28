{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Multiversion segmented reduction.
module Futhark.Pass.ExtractKernels.SegmentedReduce
       ( regularSegmentedRedomapAsScan
       )
       where

import Control.Monad
import qualified Data.HashMap.Lazy as HM

import Prelude

import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.Kernels
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass.ExtractKernels.BlockedKernel

regularSegmentedRedomapAsScan :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                                SubExp
                             -> SubExp
                             -> [SubExp]
                             -> Pattern Kernels
                             -> Pattern Kernels
                             -> Certificates
                             -> SubExp
                             -> Lambda InKernel
                             -> Lambda InKernel
                             -> [SubExp] -> [VName]
                             -> m ()
regularSegmentedRedomapAsScan segment_size num_segments nest_sizes flat_pat pat cs w lam fold_lam nes arrs = do
  blockedSegmentedScan segment_size flat_pat cs w lam fold_lam nes arrs

  let (acc_arrs, map_arrs) = splitAt (length nes) $ patternValueIdents flat_pat
      (acc_pes, map_pes) = splitAt (length nes) $ patternValueElements pat
      acc_ts = lambdaReturnType lam
      acc_pat = Pattern [] acc_pes

  is <- replicateM (length nest_sizes) $ newVName "i"

  body <- runBodyBinder $ localScope (HM.fromList $ zip is $ repeat IndexInfo) $ do
    let segment_id = flattenIndex
                     (map SE.intSubExpToScalExp nest_sizes)
                     (map (SE.intSubExpToScalExp . Var) is)
        offset = (segment_id + 1) * SE.intSubExpToScalExp segment_size - 1
    j <- letSubExp "j" =<< SE.fromScalExp offset
    vals <- forM acc_arrs $ \arr ->
      letSubExp "v" $ BasicOp $ Index [] (identName arr) $
      fullSlice (identType arr) [DimFix j]
    return $ resultBody vals

  (mapk_bnds, mapk) <-
    mapKernelFromBody [] num_segments (zip is nest_sizes) [] acc_ts body
  mapM_ addBinding mapk_bnds
  letBind_ acc_pat $ Op mapk

  forM_ (zip map_pes map_arrs) $ \(pe,arr) ->
    letBind_ (Pattern [] [pe]) $
    BasicOp $ Reshape [] (map DimNew $ arrayDims $ typeOf pe) $ identName arr
