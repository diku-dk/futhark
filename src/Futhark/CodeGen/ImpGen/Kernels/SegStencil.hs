{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegStencil' is quite straightforward.
module Futhark.CodeGen.ImpGen.Kernels.SegStencil (compileSegStencil) where

import Control.Monad.Except
import Data.List (transpose)
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.IR.KernelsMem
import Futhark.Util.IntegralExp
import Prelude hiding (quot, rem, div)

-- | Compile 'SegMap' instance code.
compileSegStencil ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegStencil pat lvl space op kbody =
  compileBigTileFlat pat lvl space op kbody

compileGlobalReadFlat ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileGlobalReadFlat pat lvl space op kbody = do
  let dims = map (toInt64Exp . snd) $ unSegSpace space
      num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size' = toInt64Exp <$> segGroupSize lvl

  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing
      let virt_num_groups =
            sExt32 $ product dims `divUp` unCount group_size'
      sKernelThread "segstencil" num_groups' group_size' (segFlat space) $
        virtualiseGroups (segVirt lvl) virt_num_groups $ \group_id ->
          let group_id' = sExt64 group_id
              group_size'' = sExt64 (unCount group_size')
           in compileGlobalReadFlatBody pat space op kbody group_id' group_size''
    SegGroup {} ->
      error "not implemented"

compileGlobalReadFlatBody ::
  Pattern KernelsMem ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  InKernelGen ()
compileGlobalReadFlatBody pat space op kbody group_id group_size =
  let (is, dims') = unzip $ unSegSpace space
      dims = map toInt64Exp dims'
      stencil_ixss = map (map fromInteger) $ stencilIndexes op
      invarElems = map kernelResultSubExp $ kernelBodyResult kbody
      lam = stencilOp op
      lamBody = lambdaBody lam
      (invariantParams, variantParams) =
        splitAt (length invarElems) $ lambdaParams lam
   in do
        local_tid <- sExt64 . kernelLocalThreadId . kernelConstants <$> askEnv
        let global_tid = group_id * group_size + local_tid

        -- create global ids for each axis
        let gids = map sExt64 $ unflattenIndex (map sExt64 dims) global_tid
        zipWithM_ dPrimV_ is gids

        -- check for out of bound on global id for each axis
        sWhen (isActive $ unSegSpace space) $ do
          -- compile invariant elements
          compileStms mempty (kernelBodyStms kbody) $ pure ()

          -- declare and attach invariant elements to invariantParams
          zipWithM_ dPrimV_ (map paramName invariantParams) . map TPrimExp
            =<< mapM toExp invarElems

          -- create max indexes for each axis, and the bound macro
          max_idxs <- forM dims (dPrimVE "max_idx_" . (+ (-1)))
          let bound_idx = zipWith sMin64 max_idxs . map (sMax64 0)

          -- calculate the unrolled variants parameter indexers
          let bounded_ixs = flip map (zip gids stencil_ixss) $
                \(axis_gid, axis_ixs) -> bound_idx $ map (axis_gid +) axis_ixs
          let vname_ixs_for_tup =
                concatMap (mapM (,) (transpose bounded_ixs)) $ stencilArrays op

          -- declare lambda variant parameters
          dLParams variantParams

          -- load variants into lambda variant parameters
          forM_ (zip variantParams vname_ixs_for_tup) $
            \(vparam, (ixs_tup, input_arr)) -> do
              let pname = paramName vparam
              copyDWIMFix pname [] (Var input_arr) ixs_tup

          -- compile lambda function and designate output style
          compileStms mempty (bodyStms lamBody) $
            zipWithM_ (compileThreadResult space) (patternElements pat) $
              map (Returns ResultMaySimplify) $ bodyResult lamBody


compileBigTileFlat ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileBigTileFlat pat lvl space op kbody = do
  let
      num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size_flat_c = toInt64Exp <$> segGroupSize lvl
      group_size_flat = (unCount group_size_flat_c)
      group_sizes_exp = -- assumes group_size_flat >= 64
        case length dims of
          1 -> [group_size_flat]
          2 -> [group_size_flat `div` 32, 32]
          3 -> [2, (group_size_flat `div` 64), 32]
          _ -> error "not valid dimensions"
      (is, dims') = unzip $ unSegSpace space
      dims = map toInt64Exp dims'
      stencil_ixss = map (map fromInteger) $ stencilIndexes op
      invarElems = map kernelResultSubExp $ kernelBodyResult kbody
      lam = stencilOp op
      lamBody = lambdaBody lam
      lamPar = lambdaParams lam
      (invariantParams, variantParams) =
        splitAt (length invarElems) lamPar
      a_mins = map (fromInteger . (\x -> if x > 0 then error "invalid axis min" else x) . minimum) $ stencilIndexes op
      a_maxs = map (fromInteger . (\x -> if x < 0 then error "invalid axis max" else x) . maximum) $ stencilIndexes op
      shared_sizes_exp = zipWith (+) group_sizes_exp $ zipWith (\x y -> x - y) a_maxs a_mins
      chunksOf _   [] = []
      chunksOf len xs =
        let (left, right) = splitAt len xs
         in left : chunksOf len right
      n_point_stencil = length $ head stencil_ixss
      lamParTypes = map (elemType . paramType) $ map head $ chunksOf n_point_stencil variantParams
      unflattenIx _ [] _ = pure []
      unflattenIx base (x:xs) i = do
        dimIx <- dPrimVE base $ i `quot` x
        (dimIx:) <$> unflattenIx base xs (i - (dimIx * x))

  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing

      -- Host side evaluated variables
      max_idxs <- mapM (dPrimVE "max_idx" . (+ (-1))) dims
      shared_sizes <- mapM (dPrimVE "shared_size") shared_sizes_exp
      shared_size_flat_var <- dPrimV "sh_flat" $ product shared_sizes
      group_sizes <- mapM (dPrimVE "group_sizes") group_sizes_exp
      readSet_iters <- mapM (dPrimV "readSet_iters") (zipWith divUp shared_sizes group_sizes)
      grid_sizes <- mapM (dPrimVE "grid_sizes") $ zipWith divUp dims group_sizes_exp
      inner_grid <- mapM (dPrimVE "grid_sizes_inner") $ tail $ scanr (*) 1 grid_sizes
      inner_group <- mapM (dPrimVE "group_sizes_inner") $ tail $ scanr (*) 1 group_sizes

      let dims_round_up = zipWith (*) grid_sizes group_sizes
          virt_num_groups =
            sExt32 $ product dims_round_up `divUp` unCount group_size_flat_c
      sKernelThread "segstencil" num_groups' group_size_flat_c (segFlat space) $
        virtualiseGroups (segVirt lvl) virt_num_groups $ \group_id_flat_exp -> do
             group_id_flat <- dPrimVE "group_id_flat" $ sExt64 group_id_flat_exp
             local_id_flat <- dPrimVE "local_id_flat" . sExt64 . kernelLocalThreadId . kernelConstants =<< askEnv
             local_ids <- unflattenIx "local_id" (map sExt64 inner_group) local_id_flat
             group_ids <- unflattenIx "group_id" (map sExt64 inner_grid) group_id_flat

             tiles <- forM (lamParTypes) $ \ptype -> sAllocArray "tile" ptype (Shape [Var $ tvVar shared_size_flat_var]) (Space "local")
             -- create max indexes for each axis, and the bound macro
             let bound_idxs = zipWith sMin64 max_idxs . map (sMax64 0)

             -- create writeSet offSets
             writeSet_offsets <-
               mapM (dPrimVE "writeSet_offset") $ zipWith (*) group_ids group_sizes
             -- create offsets for the readSet
             readSet_offsets <- mapM (dPrimVE "readSet_offset") $ zipWith (+) writeSet_offsets a_mins

             sLoopNest (Shape $ map (Var . tvVar) readSet_iters) $ \ix_list -> do
               tile_locals <- mapM (dPrimVE "tile_local") $ zipWith (+) local_ids $ zipWith (*) ix_list group_sizes
               tile_read_gids <- mapM (dPrimVE "tile_read_gid") $ bound_idxs $ zipWith (+) readSet_offsets tile_locals
               flat_local <- dPrimVE "tile_local_flat" $ flattenIndex shared_sizes tile_locals
               sWhen (foldl1 (.&&.) (zipWith (.<.) tile_locals shared_sizes)) $ do
                 forM_ (zip tiles $ stencilArrays op) $ \(tile, input_arr) -> do
                     copyDWIMFix tile [flat_local] (Var input_arr) tile_read_gids

             -- group syncronize
             sOp $ Imp.Barrier Imp.FenceLocal

             let gids = zipWith (+) writeSet_offsets local_ids
             zipWithM_ dPrimV_ is gids
             -- check for out of bound on global id for each axis
             sWhen (isActive $ unSegSpace space) $ do
               -- compile invariant elements
               compileStms mempty (kernelBodyStms kbody) $ pure ()

               -- declare and attach invariant elements to invariantParams
               zipWithM_ dPrimV_ (map paramName invariantParams) . map TPrimExp
                 =<< mapM toExp invarElems

               sh_offs <- mapM (dPrimVE "tile_offset") $ zipWith (-) local_ids a_mins
               dLParams variantParams

               let param_ixs = transpose $ zipWith (mapM (+)) stencil_ixss sh_offs
               let params_ixs_ordered = transpose $ chunksOf n_point_stencil variantParams

               ---- load variants into lambda variant parameters
               forM_ (zip param_ixs params_ixs_ordered) $ \(parix, pars) -> do
                   tile_ix <- dPrimVE "tile_ix" $ flattenIndex shared_sizes parix
                   forM_ (zip pars tiles) $ \(par, tile) ->
                     copyDWIMFix (paramName par) [] (Var tile) [tile_ix]

               -- compile lambda function and designate output style
               compileStms mempty (bodyStms lamBody) $
                 zipWithM_ (compileThreadResult space) (patternElements pat) $
                   map (Returns ResultMaySimplify) $ bodyResult lamBody

    SegGroup {} ->
      error "not implemented"

