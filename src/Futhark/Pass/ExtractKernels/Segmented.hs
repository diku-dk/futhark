{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Multiversion segmented reduction.
module Futhark.Pass.ExtractKernels.Segmented
       ( regularSegmentedRedomapAsScan
       , regularSegmentedScan
       )
       where

import Control.Monad
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Transform.Rename
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
                             -> [(VName, SubExp)]
                             -> [KernelInput]
                             -> [SubExp] -> [VName]
                             -> m ()
regularSegmentedRedomapAsScan segment_size num_segments nest_sizes flat_pat
                              pat cs w lam fold_lam ispace inps nes arrs = do
  regularSegmentedScan segment_size flat_pat cs w lam fold_lam ispace inps nes arrs

  let (acc_arrs, map_arrs) = splitAt (length nes) $ patternValueIdents flat_pat
      (acc_pes, map_pes) = splitAt (length nes) $ patternValueElements pat
      acc_ts = lambdaReturnType lam
      acc_pat = Pattern [] acc_pes

  is <- replicateM (length nest_sizes) $ newVName "i"

  body <- runBodyBinder $ localScope (HM.fromList $ zip is $ repeat $ IndexInfo Int32) $ do
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
    mapKernelFromBody [] num_segments (FlatThreadSpace $ zip is nest_sizes) [] acc_ts body
  mapM_ addStm mapk_bnds
  letBind_ acc_pat $ Op mapk

  forM_ (zip map_pes map_arrs) $ \(pe,arr) ->
    letBind_ (Pattern [] [pe]) $
    BasicOp $ Reshape [] (map DimNew $ arrayDims $ typeOf pe) $ identName arr

addFlagToLambda :: (MonadBinder m, Lore m ~ Kernels) =>
                   [SubExp] -> Lambda InKernel -> m (Lambda InKernel)
addFlagToLambda nes lam = do
  let num_accs = length nes
  x_flag <- newVName "x_flag"
  y_flag <- newVName "y_flag"
  let x_flag_param = Param x_flag $ Prim Bool
      y_flag_param = Param y_flag $ Prim Bool
      (x_params, y_params) = splitAt num_accs $ lambdaParams lam
      params = [x_flag_param] ++ x_params ++ [y_flag_param] ++ y_params

  body <- runBodyBinder $ localScope (scopeOfLParams params) $ do
    new_flag <- letSubExp "new_flag" $
                BasicOp $ BinOp LogOr (Var x_flag) (Var y_flag)
    lhs <- fmap (map Var) $ letTupExp "seg_lhs" $ If (Var y_flag)
      (resultBody nes)
      (resultBody $ map (Var . paramName) x_params)
      (staticShapes $ map paramType x_params)
    let rhs = map (Var . paramName) y_params

    lam' <- renameLambda lam -- avoid shadowing
    res <- eLambda lam' $ lhs ++ rhs

    return $ resultBody $ new_flag : res

  return Lambda { lambdaParams = params
                , lambdaBody = body
                , lambdaReturnType = Prim Bool : lambdaReturnType lam
                }

regularSegmentedScan :: (MonadBinder m, Lore m ~ Kernels) =>
                        SubExp
                     -> Pattern Kernels
                     -> Certificates
                     -> SubExp
                     -> Lambda InKernel
                     -> Lambda InKernel
                     -> [(VName, SubExp)] -> [KernelInput]
                     -> [SubExp] -> [VName]
                     -> m ()
regularSegmentedScan segment_size pat cs w lam fold_lam ispace inps nes arrs = do
  flags_i <- newVName "flags_i"

  unused_flag_array <- newVName "unused_flag_array"
  flags_body <-
    runBodyBinder $ localScope (HM.singleton flags_i $ IndexInfo Int32) $ do
      segment_index <- letSubExp "segment_index" $
                       BasicOp $ BinOp (SRem Int32) (Var flags_i) segment_size
      start_of_segment <- letSubExp "start_of_segment" $
                          BasicOp $ CmpOp (CmpEq int32) segment_index zero
      flag <- letSubExp "flag" $
              If start_of_segment (resultBody [true]) (resultBody [false]) [Prim Bool]
      return $ resultBody [flag]
  (mapk_bnds, mapk) <- mapKernelFromBody [] w (FlatThreadSpace [(flags_i, w)]) [] [Prim Bool] flags_body
  mapM_ addStm mapk_bnds
  flags <- letExp "flags" $ Op mapk

  lam' <- addFlagToLambda nes lam
  fold_lam' <- addFlagToLambda nes fold_lam

  let pat' = pat { patternValueElements = PatElem unused_flag_array BindVar
                                          (arrayOf (Prim Bool) (Shape [w]) NoUniqueness) :
                                          patternValueElements pat
                 }
  blockedScan pat' cs w lam' fold_lam' segment_size ispace inps (false:nes) (flags:arrs)
  where zero = constant (0 :: Int32)
        true = constant True
        false = constant False
