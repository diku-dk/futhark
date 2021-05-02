{-# LANGUAGE TupleSections #-}
module Futhark.CodeGen.ImpGen.Multicore.SegStencil
  ( compileSegStencil
  , tileDims
  )
where

import Control.Monad
import Data.List (transpose, zip4, zipWith4)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.MonadFreshNames (newVName)

import Debug.Trace (trace)

compileSegStencil ::
  Pattern MCMem ->
  SegSpace ->
  StencilOp MCMem ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
compileSegStencil pat space sten kbody = collect $ do
    iter <- dPrim "iter" $ IntType Int64

    body <- collect $ do
      zipWithM_ dPrimV_ is $ unflattenIndex loopDimsI64 $ tvExp (iter :: TV Int64)
      sComment "kernel body" $
        compileStms mempty (kernelBodyStms kbody) $ do

          dLParams const_params
          forM_ (zip const_params kbres) $ \(param, r) -> do
            copyDWIMFix (paramName param) [] r []

          dLParams sten_params
          innerLoop (zip3 tileDimsI64 (map (toInt64Exp . Var) is) dimsI64) []
    free_params <- freeParams body [segFlat space, tvVar iter]
    let (body_allocs, body') = extractAllocations body
    emit $ Imp.Op $ Imp.ParLoop "stencil" (tvVar iter) body_allocs body' mempty free_params $ segFlat space
  where
    sten_idxs = stencilIndexes sten
    sten_lam = stencilOp sten
    arrs = stencilArrays sten
    kbres = map kernelResultSubExp $ kernelBodyResult kbody
    (const_params, sten_params) = splitAt (length kbres) (lambdaParams sten_lam)
    (is, ns) = unzip $ unSegSpace space
    dimsI64 = map toInt64Exp ns
    tileDimsI64 = map toInt64Exp (tileDims $ length dimsI64)
    loopDimsI64 = zipWith (\d t -> TPrimExp $ BinOpExp (SDiv Int64 Unsafe)
                                                    (untyped $ d + t - 1)
                                                    (untyped t))
                              dimsI64 tileDimsI64
    innerLoop ::
      [(Imp.TExp Int64, Imp.TExp Int64, Imp.TExp Int64)] ->
      [Imp.TExp Int64] ->
      MulticoreGen ()
    innerLoop [] ts = do
      let ts' = reverse ts
          idxs = map (uncurry4 stencilIdxs) $ zip4 is sten_idxs dimsI64 ts'
          idxs_with_vnames = concatMap (\vn -> map (vn,) (transpose idxs)) arrs

      forM_ (zip sten_params idxs_with_vnames) $ \(param, (arr, idxs')) ->
        copyDWIMFix (paramName param) [] (Var arr) idxs'

      flatName <- newVName "idx_out_flat"

      out_ns <- replicateM (length is) (newVName "idx_out")
      let is' = zipWith4 (\i t n d -> inBounds' (toInt64Exp (Var i) * d + t) n) is ts' dimsI64 tileDimsI64
          spaceOut = SegSpace flatName (zip out_ns (map Var out_ns))
      zipWithM_ dPrimV_ out_ns is'

      compileStms mempty (bodyStms $ lambdaBody sten_lam) $
        zipWithM_ (compileThreadResult spaceOut) (patternElements pat) $
          map (Returns ResultMaySimplify) $ bodyResult $ lambdaBody sten_lam
    innerLoop ((d, i, a):ds) ts = do
      iters <- newVName "seq_iters"
      let prev_iters = i * d
          rem_iters = a - prev_iters
      dPrimV_ iters $ sMin64 rem_iters d
      sFor "t" (toInt64Exp $ Var iters) $ \t ->
        innerLoop ds (t:ts)
    
    stencilIdxs ::
      VName ->
      [Integer] ->
      Imp.TExp Int64 ->
      Imp.TExp Int64 ->
      [Imp.TExp Int64]
    stencilIdxs iter idxs dim tile_iter =
      let iter' = (toInt64Exp (Var iter)) + tile_iter in
        flip map idxs $ \idx -> inBounds' (iter' + fromInteger idx) dim

    inBounds' ::
      Imp.TExp Int64 ->
      Imp.TExp Int64 ->
      Imp.TExp Int64
    inBounds' x n = sMin64 (n - 1) (sMax64 0 x)

uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t5) -> (t1, t2, t3, t4) -> t5
uncurry4 f (a, b, c, d) = f a b c d
