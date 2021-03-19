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
import Prelude hiding (div, quot, rem)

-- | Compile 'SegMap' instance code.
compileSegStencil ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegStencil = compileBigTile

-- the provided one has a ton of common subexpressions so a new one was made
-- !!!! It does however require the variables of dim be subjected to
-- tail . scanr1 (*)
unflattenIx ::
  IntExp t =>
  String ->
  [Imp.TExp t] ->
  Imp.TExp t ->
  ImpM lore r op [Imp.TExp t]
unflattenIx base [] i = (: []) <$> dPrimVE base i
unflattenIx base (x : xs) i = do
  dimIx <- dPrimVE base $ i `quot` x
  rem_val <- dPrimVE "rem_val" (i - (dimIx * x))
  (dimIx :) <$> unflattenIx base xs rem_val

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf len xs =
  let (left, right) = splitAt len xs
   in left : chunksOf len right

compileGlobalReadFlat ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileGlobalReadFlat pat lvl space op kbody = do
  let (is, dims') = unzip $ unSegSpace space
      dims = map toInt64Exp dims'
      num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size_flat_c = toInt64Exp <$> segGroupSize lvl
      group_size_flat = unCount group_size_flat_c
      stencil_ixss = map (map fromInteger) $ stencilIndexes op
      invarElems = map kernelResultSubExp $ kernelBodyResult kbody
      lam = stencilOp op
      lamBody = lambdaBody lam
      (invariantParams, variantParams) =
        splitAt (length invarElems) $ lambdaParams lam
      n_point_stencil = length $ head stencil_ixss

  case lvl of
    SegThread {} -> do
      -- Host side evaluated variables
      max_idxs <- mapM (dPrimVE "max_idx" . (+ (-1))) dims
      inner_dims <- mapM (dPrimVE "dims_inner") $ tail $ scanr1 (*) dims

      emit $ Imp.DebugPrint "\n# SegStencil" Nothing
      let virt_num_groups =
            sExt32 $ product dims `divUp` group_size_flat

      virt_num_groups_var <- dPrimVE "virt_num_groups" virt_num_groups

      sKernelThread "segstencil" num_groups' group_size_flat_c (segFlat space) $
        virtualiseGroups (segVirt lvl) virt_num_groups_var $ \group_id_flat_exp -> do
          group_id_flat <- dPrimVE "group_id_flat" $ sExt64 group_id_flat_exp
          local_id_flat <- dPrimVE "local_id_flat" . sExt64 . kernelLocalThreadId . kernelConstants =<< askEnv

          -- create global ids for each axis
          gid_flat <- dPrimVE "global_id_flat" $ group_id_flat * group_size_flat + local_id_flat
          gids <- unflattenIx "global_id" (map sExt64 inner_dims) gid_flat
          zipWithM_ dPrimV_ is gids

          -- check for out of bound on global id for each axis
          sWhen (isActive $ unSegSpace space) $ do
            -- compile invariant elements
            compileStms mempty (kernelBodyStms kbody) $ pure ()

            -- declare and attach invariant elements to invariantParams
            zipWithM_ dPrimV_ (map paramName invariantParams) . map TPrimExp
              =<< mapM toExp invarElems

            -- create max indexes for each axis, and the bound macro
            let bound_ixs = zipWith sMin64 max_idxs . map (sMax64 0)

            let param_ixs = transpose $ zipWith (mapM (+)) stencil_ixss gids
            let params_ixs_ordered = transpose $ chunksOf n_point_stencil variantParams

            dLParams variantParams

            -- load variants into lambda variant parameters
            forM_ (zip param_ixs params_ixs_ordered) $ \(parix, pars) -> do
              read_ixs <- mapM (dPrimVE "read_ix") $ bound_ixs parix
              forM_ (zip pars $ stencilArrays op) $ \(par, src) ->
                copyDWIMFix (paramName par) [] (Var src) read_ixs

            -- compile lambda function and designate output style
            compileStms mempty (bodyStms lamBody) $
              zipWithM_ (compileThreadResult space) (patternElements pat) $
                map (Returns ResultMaySimplify) $ bodyResult lamBody
    SegGroup {} ->
      error "not implemented"

compileBigTile ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileBigTile pat lvl space op kbody = do
  let num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size_flat_c = toInt64Exp <$> segGroupSize lvl
      group_size_flat = unCount group_size_flat_c
      group_sizes_exp =
        -- assumes group_size_flat >= 64
        case length dims of
          1 -> [group_size_flat]
          2 -> [group_size_flat `div` 32, 32]
          3 -> [2, group_size_flat `div` 64, 32]
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
      shared_sizes_exp = zipWith (+) group_sizes_exp $ zipWith (-) a_maxs a_mins
      n_point_stencil = length $ head stencil_ixss
      lamParTypes = map (elemType . paramType . head) $ chunksOf n_point_stencil variantParams

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
      inner_grid <- mapM (dPrimVE "grid_sizes_inner") $ tail . scanr1 (*) $ grid_sizes
      inner_group <- mapM (dPrimVE "group_sizes_inner") $ tail . scanr1 (*) $ group_sizes

      let dims_round_up = zipWith (*) grid_sizes group_sizes
          virt_num_groups =
            sExt32 $ product dims_round_up `divUp` unCount group_size_flat_c

      virt_num_groups_var <- dPrimVE "virt_num_groups" virt_num_groups

      sKernelThread "segstencil" num_groups' group_size_flat_c (segFlat space) $
        virtualiseGroups (segVirt lvl) virt_num_groups_var $ \group_id_flat_exp -> do
          group_id_flat <- dPrimVE "group_id_flat" $ sExt64 group_id_flat_exp
          -- move the localId stuff outside of the virtualisegroup loop if possible.
          local_id_flat <- dPrimVE "local_id_flat" . sExt64 . kernelLocalThreadId . kernelConstants =<< askEnv
          local_ids <- unflattenIx "local_id" (map sExt64 inner_group) local_id_flat
          group_ids <- unflattenIx "group_id" (map sExt64 inner_grid) group_id_flat

          tiles <- forM lamParTypes $ \ptype -> sAllocArray "tile" ptype (Shape [Var $ tvVar shared_size_flat_var]) (Space "local")
          -- create max indexes for each axis, and the bound macro
          let bound_idxs = zipWith sMin64 max_idxs . map (sMax64 0)

          -- create writeSet offSets
          writeSet_offsets <-
            mapM (dPrimVE "writeSet_offset") $ zipWith (*) group_ids group_sizes
          -- create offsets for the readSet
          readSet_offsets <- mapM (dPrimVE "readSet_offset") $ zipWith (+) writeSet_offsets a_mins
          
          --We need to unroll this with a #pragma unroll (However, some of the variables are not garuanteed to be constant at compile-time
          --, how do we fix it?)
          --Also optimize the flattenIndex such that we use fewer multiplications z*a*b + y*b + x = (z*a+y)*b + x
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
