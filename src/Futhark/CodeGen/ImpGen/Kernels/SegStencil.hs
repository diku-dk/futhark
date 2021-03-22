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
  ImpM lore r op [TV t]
unflattenIx base [] i = (: []) <$> dPrimV base i
unflattenIx base (x : xs) i = do
  dimIx <- dPrimV base $ i `quot` x
  rem_val <- dPrimV "rem_val" (i - (tvExp dimIx * x))
  (dimIx :) <$> unflattenIx base xs (tvExp rem_val)

-- works better for 3d+ than the provided by using that multiplication is distributive over addition
flattenIx :: Num b => [b] -> [b] -> b
flattenIx dims ixs = foldl (\acc (d,n) -> acc*d + n) (head ixs) $ zip (tail dims) (tail ixs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf len xs =
  let (left, right) = splitAt len xs
   in left : chunksOf len right

dPrimVEC ::
  String ->
  TPrimExp t Imp.ExpLeaf ->
  ImpM lore r op (TPrimExp t Imp.ExpLeaf)
dPrimVEC _ (x@(TPrimExp (ValueExp _))) = pure x
dPrimVEC name x = dPrimVE name x

propagateConst ::
  String ->
  [TPrimExp t Imp.ExpLeaf] ->
  ImpM lore r op [TPrimExp t Imp.ExpLeaf]
propagateConst name = mapM (dPrimVEC name)

createSpans :: Num a => [a] -> [a]
createSpans = scanr1 (*) . tail

virtualiseGroupsHigherDim ::
  SegVirt ->
  Imp.TExp Int32 ->
  [Imp.TExp Int64] ->
  ([Imp.TExp Int64] -> InKernelGen ()) ->
  InKernelGen ()
virtualiseGroupsHigherDim SegVirt required_groups grid_sizes_exp m = do
  constants <- kernelConstants <$> askEnv
  phys_group_id <- dPrim "phys_group_id" int32
  sOp $ Imp.GetGroupId (tvVar phys_group_id) 0

  let iterations =
        (sExt64 required_groups - tvExp phys_group_id)
          `divUp` sExt64 (kernelNumGroups constants)

  grid_sizes <- propagateConst "grid_sizes" grid_sizes_exp
  grid_spans_tail <- propagateConst "grid_sizes_outer" $ createSpans grid_sizes_exp
  num_iterations <- dPrimVEC "iterations" iterations
  base_group_ids <- unflattenIx "base_group_id" grid_spans_tail . sExt64 . tvExp $ phys_group_id
  unflat_ng <- unflattenIx "unflat_num_groups" grid_spans_tail . sExt64 . kernelNumGroups $ constants

  sFor "i" num_iterations $ \_ -> do
    carry <- dPrimV "carry" 0
    mapM_ (\(bi, gz)-> do
            bi <-- tvExp bi + tvExp carry
            sIf (tvExp bi .>=. gz) ((bi <-- (tvExp bi - gz)) *> (carry <-- 1)) (carry <-- 0)
          ) (reverse $ zip base_group_ids grid_sizes)
    m . map tvExp $ base_group_ids
    zipWithM_ (\b ng -> b <-- (tvExp b + tvExp ng)) base_group_ids unflat_ng
    sOp $ Imp.Barrier Imp.FenceGlobal
virtualiseGroupsHigherDim _ _ grid_sizes_exp m = do
  gid <- sExt64 . Imp.vi32 . kernelGroupIdVar . kernelConstants <$> askEnv
  gids <- unflattenIx "group_id" (createSpans grid_sizes_exp) gid
  m . map tvExp $ gids

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
      inner_dims <- mapM (dPrimVE "dims_inner") $ createSpans dims

      emit $ Imp.DebugPrint "\n# SegStencil" Nothing
      let virt_num_groups =
            sExt32 $ product dims `divUp` group_size_flat

      virt_num_groups_var <- dPrimVE "virt_num_groups" virt_num_groups

      sKernelThread "segstencil" num_groups' group_size_flat_c (segFlat space) $ do
        local_id_flat <- dPrimVE "local_id_flat" . sExt64 . kernelLocalThreadId . kernelConstants =<< askEnv

        virtualiseGroups (segVirt lvl) virt_num_groups_var $ \group_id_flat_exp -> do
          group_id_flat <- dPrimVE "group_id_flat" $ sExt64 group_id_flat_exp

          -- create global ids for each axis
          gid_flat <- dPrimVE "global_id_flat" $ group_id_flat * group_size_flat + local_id_flat
          gids <- map tvExp <$> unflattenIx "global_id" (map sExt64 inner_dims) gid_flat
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
      group_size_flat_exp = unCount group_size_flat_c
      group_sizes_exp =
        -- assumes group_size_flat >= 256
        case length dims of
          1 -> [group_size_flat_exp]
          2 -> [group_size_flat_exp `quot` 32, 32]
          3 -> [group_size_flat_exp `quot` 256, 8, 32]
          _ -> error "not valid dimensions"
      group_sizes_exp_inner_scan =
        -- assumes group_size_flat >= 256
        case length dims of
          1 -> []
          2 -> [32]
          3 -> [256, 32]
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
      n_point_stencil = length $ head stencil_ixss
      lamParTypes = map (elemType . paramType . head) $ chunksOf n_point_stencil variantParams
      grid_sizes_exp = zipWith divUp dims group_sizes_exp
      shared_sizes_exp = zipWith (+) group_sizes_exp $ zipWith (-) a_maxs a_mins

  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing

      -- Host side evaluated variables
      let dims_round_up = zipWith (*) grid_sizes_exp group_sizes_exp
          virt_num_groups =
            sExt32 $ product dims_round_up `divUp` unCount group_size_flat_c

      virt_num_groups_var <- dPrimVE "virt_num_groups" virt_num_groups
      shared_size_flat_var <- dPrimV "sh_flat" $ product shared_sizes_exp
      max_idxs <- mapM (dPrimVE "max_idx" . (+ (-1))) dims
      let bound_idxs = zipWith sMin64 max_idxs . map (sMax64 0)

      phys_num_groups <- dPrimVE "phys_num_groups" $ unCount num_groups'
      sKernelThread "segstencil" (Count phys_num_groups) group_size_flat_c (segFlat space) $ do

        -- large parts of these lists are constants so propagate that.
        shared_sizes <- propagateConst "shared_size_outer" shared_sizes_exp
        group_sizes <- propagateConst "group_sizes_outer" group_sizes_exp
        readSet_iters <- mapM (dPrimV "readSet_iters") $ zipWith divUp shared_sizes group_sizes

        local_id_flat <- dPrimVE "local_id_flat" . sExt64 . kernelLocalThreadId . kernelConstants =<< askEnv
        local_ids <- map tvExp <$> unflattenIx "local_id" (group_sizes_exp_inner_scan) local_id_flat

        local_id_sh_flat <- dPrimVE "local_id_sh_flat" $ flattenIx shared_sizes local_ids
        let p2 = map (flattenIx shared_sizes) $ transpose $ zipWith (mapM (-)) stencil_ixss a_mins
        let p3 = mapM (+) p2 local_id_sh_flat

        let params_ixs_ordered = transpose $ chunksOf n_point_stencil variantParams
        --tile_ixs <- forM p3 (dPrimVE "tile_ix")
        let tile_ixs = p3

        -- virtualiseGroups (segVirt lvl) virt_num_groups_var $ \group_id_flat_exp -> do
        --  group_id_flat <- dPrimVE "group_id_flat" $ sExt64 group_id_flat_exp
        --  group_ids <- unflattenIx "group_id" (map sExt64 inner_grid) group_id_flat

        virtualiseGroupsHigherDim (segVirt lvl) virt_num_groups_var grid_sizes_exp $ \group_ids_exp -> do
          group_ids <- mapM (dPrimVE "group_id") group_ids_exp

          tiles <- forM lamParTypes $ \ptype -> sAllocArray "tile" ptype (Shape [Var $ tvVar shared_size_flat_var]) (Space "local")
          -- create max indexes for each axis, and the bound macro

          -- create writeSet offSets
          writeSet_offsets <- mapM (dPrimVE "writeSet_offset") $ zipWith (*) group_ids group_sizes
          -- create offsets for the readSet
          readSet_offsets <- mapM (dPrimVE "readSet_offset") $ zipWith (+) writeSet_offsets a_mins

          --We need to unroll this with a #pragma unroll (However, some of the variables are not garuanteed to be constant at compile-time
          --, how do we fix it?)
          sLoopNest (Shape $ map (Var . tvVar) readSet_iters) $ \ix_list -> do
            tile_locals <- mapM (dPrimVE "tile_local") $ zipWith (+) local_ids $ zipWith (*) ix_list group_sizes
            tile_read_gids <- mapM (dPrimVE "tile_read_gid") $ bound_idxs $ zipWith (+) readSet_offsets tile_locals
            flat_local <- dPrimVE "tile_local_flat" $ flattenIx shared_sizes tile_locals
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

            dLParams variantParams

            ---- load variants into lambda variant parameters
            forM_ (zip tile_ixs params_ixs_ordered) $ \(tile_ix, pars) -> do
              forM_ (zip pars tiles) $ \(par, tile) ->
                copyDWIMFix (paramName par) [] (Var tile) [tile_ix]

            -- compile lambda function and designate output style
            compileStms mempty (bodyStms lamBody) $
              zipWithM_ (compileThreadResult space) (patternElements pat) $
                map (Returns ResultMaySimplify) $ bodyResult lamBody
    SegGroup {} ->
      error "not implemented"

compileBigTileFlat ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileBigTileFlat pat lvl space op kbody = do
  let num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size_flat_c = toInt64Exp <$> segGroupSize lvl
      group_size_flat_exp = unCount group_size_flat_c
      group_sizes_exp =
        -- assumes group_size_flat >= 256
        case length dims of
          1 -> [group_size_flat_exp]
          2 -> [group_size_flat_exp `quot` 32, 32]
          3 -> [group_size_flat_exp `quot` 256, 8, 32]
          _ -> error "not valid dimensions"
      group_sizes_exp_inner_scan =
        -- assumes group_size_flat >= 256
        case length dims of
          1 -> []
          2 -> [32]
          3 -> [256, 32]
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
      n_point_stencil = length $ head stencil_ixss
      lamParTypes = map (elemType . paramType . head) $ chunksOf n_point_stencil variantParams
      grid_sizes_exp = zipWith divUp dims group_sizes_exp
      shared_sizes_exp = zipWith (+) group_sizes_exp $ zipWith (-) a_maxs a_mins

  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing

      -- Host side evaluated variables
      let dims_round_up = zipWith (*) grid_sizes_exp group_sizes_exp
          virt_num_groups =
            sExt32 $ product dims_round_up `divUp` unCount group_size_flat_c

      virt_num_groups_var <- dPrimVE "virt_num_groups" virt_num_groups
      shared_size_flat_var <- dPrimV "sh_flat" $ product shared_sizes_exp
      max_idxs <- mapM (dPrimVE "max_idx" . (+ (-1))) dims
      let bound_idxs = zipWith sMin64 max_idxs . map (sMax64 0)

      phys_num_groups <- dPrimVE "phys_num_groups" $ unCount num_groups'
      sKernelThread "segstencil" (Count phys_num_groups) group_size_flat_c (segFlat space) $ do

        -- large parts of these lists are constants so propagate that.
        shared_sizes <- propagateConst "shared_size_outer" shared_sizes_exp
        shared_size_flat <- dPrimVEC "shared_size_flat" $ product shared_sizes
        shared_spans <- propagateConst "shared_spans" $ createSpans shared_sizes
        group_sizes <- propagateConst "group_sizes_outer" group_sizes_exp
        group_size_flat <- dPrimVEC "group_size_flat" group_size_flat_exp

        local_id_flat <- dPrimVE "local_id_flat" . sExt64 . kernelLocalThreadId . kernelConstants =<< askEnv
        local_ids <- map tvExp <$> unflattenIx "local_id" (group_sizes_exp_inner_scan) local_id_flat

        local_id_sh_flat <- dPrimVE "local_id_sh_flat" $ flattenIx shared_sizes local_ids
        let p2 = map (flattenIx shared_sizes) $ transpose $ zipWith (mapM (-)) stencil_ixss a_mins
        let p3 = mapM (+) p2 local_id_sh_flat

        let params_ixs_ordered = transpose $ chunksOf n_point_stencil variantParams
        --tile_ixs <- forM p3 (dPrimVE "tile_ix")
        let tile_ixs = p3

        -- virtualiseGroups (segVirt lvl) virt_num_groups_var $ \group_id_flat_exp -> do
        --  group_id_flat <- dPrimVE "group_id_flat" $ sExt64 group_id_flat_exp
        --  group_ids <- unflattenIx "group_id" (map sExt64 inner_grid) group_id_flat

        virtualiseGroupsHigherDim (segVirt lvl) virt_num_groups_var grid_sizes_exp $ \group_ids_exp -> do
          group_ids <- mapM (dPrimVE "group_id") group_ids_exp

          tiles <- forM lamParTypes $ \ptype -> sAllocArray "tile" ptype (Shape [Var $ tvVar shared_size_flat_var]) (Space "local")
          -- create max indexes for each axis, and the bound macro

          -- create writeSet offSets
          writeSet_offsets <- mapM (dPrimVE "writeSet_offset") $ zipWith (*) group_ids group_sizes
          -- create offsets for the readSet
          readSet_offsets <- mapM (dPrimVE "readSet_offset") $ zipWith (+) writeSet_offsets a_mins

          flatIters <- dPrimVEC "flat_iters" $ shared_size_flat `divUp` group_size_flat
          sFor "i" flatIters $ \i -> do
            tile_ix_flat <- dPrimVEC "tile_ix_flat" $ (i * group_size_flat) + local_id_flat
            tile_idxs <- map tvExp <$> unflattenIx "tile_ix" shared_spans tile_ix_flat
            read_gids <- propagateConst "read_gid" $ bound_idxs $ zipWith (+) readSet_offsets tile_idxs
            sWhen (tile_ix_flat .<. shared_size_flat) $ do
              forM_ (zip tiles $ stencilArrays op) $ \(tile, input_arr) -> do
                copyDWIMFix tile [tile_ix_flat] (Var input_arr) read_gids

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

            dLParams variantParams

            ---- load variants into lambda variant parameters
            forM_ (zip tile_ixs params_ixs_ordered) $ \(tile_ix, pars) -> do
              forM_ (zip pars tiles) $ \(par, tile) ->
                copyDWIMFix (paramName par) [] (Var tile) [tile_ix]

            -- compile lambda function and designate output style
            compileStms mempty (bodyStms lamBody) $
              zipWithM_ (compileThreadResult space) (patternElements pat) $
                map (Returns ResultMaySimplify) $ bodyResult lamBody
    SegGroup {} ->
      error "not implemented"
