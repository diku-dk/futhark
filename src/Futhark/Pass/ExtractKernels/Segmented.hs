{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Segmented scan.
module Futhark.Pass.ExtractKernels.Segmented
       ( regularSegmentedScan
       )
       where

import Control.Monad
import qualified Data.Map.Strict as M

import Futhark.Transform.Rename
import Futhark.Representation.Kernels
import Futhark.Representation.SOACS.SOAC (nilFn)
import Futhark.MonadFreshNames
import Futhark.Tools hiding (true, false)
import Futhark.Pass.ExtractKernels.BlockedKernel

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
           (resultBody $ map (Var . paramName) x_params) $
           ifCommon $ map paramType x_params
    let rhs = map (Var . paramName) y_params

    lam' <- renameLambda lam -- avoid shadowing
    res <- eLambda lam' $ map eSubExp $ lhs ++ rhs

    return $ resultBody $ new_flag : res

  return Lambda { lambdaParams = params
                , lambdaBody = body
                , lambdaReturnType = Prim Bool : lambdaReturnType lam
                }

regularSegmentedScan :: (MonadBinder m, Lore m ~ Kernels) =>
                        SubExp
                     -> Pattern Kernels
                     -> SubExp
                     -> Lambda InKernel
                     -> Lambda InKernel
                     -> [(VName, SubExp)] -> [KernelInput]
                     -> [SubExp] -> [VName]
                     -> m ()
regularSegmentedScan segment_size pat w lam map_lam ispace inps nes arrs = do
  flags_i <- newVName "flags_i"

  unused_flag_array <- newVName "unused_flag_array"
  flags_body <-
    runBodyBinder $ localScope (M.singleton flags_i $ IndexInfo Int32) $ do
      segment_index <- letSubExp "segment_index" $
                       BasicOp $ BinOp (SRem Int32) (Var flags_i) segment_size
      start_of_segment <- letSubExp "start_of_segment" $
                          BasicOp $ CmpOp (CmpEq int32) segment_index zero
      let flag = start_of_segment
      return $ resultBody [flag]
  (mapk_bnds, mapk) <- mapKernelFromBody w (FlatThreadSpace [(flags_i, w)]) [] [Prim Bool] flags_body
  addStms mapk_bnds
  flags <- letExp "flags" $ Op mapk

  lam' <- addFlagToLambda nes lam

  flag_p <- newParam "flag" $ Prim Bool
  let map_lam' = map_lam { lambdaParams = flag_p : lambdaParams map_lam
                         , lambdaBody = (lambdaBody map_lam)
                           { bodyResult = Var (paramName flag_p) : bodyResult (lambdaBody map_lam) }
                         , lambdaReturnType = Prim Bool : lambdaReturnType map_lam
                         }

  let pat' = pat { patternValueElements = PatElem unused_flag_array
                                          (arrayOf (Prim Bool) (Shape [w]) NoUniqueness) :
                                          patternValueElements pat
                 }
  void $ blockedScan pat' w (lam', false:nes) (Commutative, nilFn, mempty) map_lam' segment_size ispace inps (flags:arrs)
  where zero = constant (0 :: Int32)
        false = constant False
