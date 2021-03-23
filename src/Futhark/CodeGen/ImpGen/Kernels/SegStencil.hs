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
compileSegStencil =
  --compileGlobalReadFlat
  --compileBigTile tile_loader_cube
  compileBigTile tile_loader_flat

-- the provided one has a ton of common subexpressions so a new one was made
-- !!!! It does however need the span of the inner dimensions, so use
-- scanr1 (*) . tail
unflattenIx ::
  IntExp t =>
  String ->
  [Imp.TExp t] ->
  Imp.TExp t ->
  ImpM lore r op [TV t]
unflattenIx base [] i = (: []) <$> dPrimV base i
unflattenIx base (x : xs) i = do
  dimIx <- dPrimV base $ i `quot` x
  rem_val <- dPrimV "rem_val" $ i `rem` x
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
dPrimVEC _ (x@(TPrimExp (LeafExp _ _))) = pure x
dPrimVEC name x = dPrimVE name x

propagateConst ::
  String ->
  [TPrimExp t Imp.ExpLeaf] ->
  ImpM lore r op [TPrimExp t Imp.ExpLeaf]
propagateConst name = mapM (dPrimVEC name)

createSpans :: Num a => [a] -> [a]
createSpans = scanr1 (*) . tail

sForUnflat ::
  TPrimExp Int64 Imp.ExpLeaf
  -> [TPrimExp Int64 Imp.ExpLeaf]
  -> TV Int64
  -> TPrimExp Int64 Imp.ExpLeaf
  -> ImpM lore r op ()
  -> ([Imp.TExp Int64] -> ImpM lore r op ())
  -> ImpM lore r op ()
sForUnflat num_iterations sizes_exp start_flat added_flat loop_tail m = do
  sizes <- propagateConst "size" sizes_exp
  size_span <- propagateConst "size_span" $ createSpans sizes_exp
  iterations <- dPrimVEC "iterations" num_iterations
  starts <- unflattenIx "start" size_span . sExt64 . tvExp $ start_flat
  adds <- unflattenIx "added" size_span . sExt64 $ added_flat

  let add_carry (bi, gz, bn) = do
        cond <- dPrimVEC "cond" (tvExp bi .>=. gz)
        sWhen cond $ do
          bi <-- (tvExp bi - gz)
          bn <-- (tvExp bn + 1)
      stls = reverse starts
      szls = reverse sizes
      ls = zip3 stls szls (tail stls)
      prepareNextIter = do
        zipWithM_ (\b ng -> b <-- (tvExp b + tvExp ng)) starts adds
        forM_ ls add_carry
  sFor "i" iterations $ \_ -> do
    (m . map tvExp $ starts)
    loop_tail
    prepareNextIter

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

  let added = kernelNumGroups constants
  let loop_tail_eval = sOp $ Imp.Barrier Imp.FenceGlobal
  sForUnflat iterations grid_sizes_exp phys_group_id added loop_tail_eval m
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

      emit $ Imp.DebugPrint "\n# SegStencil" Nothing

      -- Host side evaluated variables
      max_idxs <- mapM (dPrimVE "max_idx" . (+ (-1))) dims
      virt_num_groups_var <- dPrimVE "virt_num_groups" $
        sExt32 $ product dims `divUp` group_size_flat
      dim_span <- mapM (dPrimVE "dim_span" . sExt64) $ createSpans dims

      sKernelThread "segstencil" num_groups' group_size_flat_c (segFlat space) $ do
        -- device side variable that is independent of virtual kernel id
        local_id_flat <- dPrimVEC "local_id_flat" . sExt64 . kernelLocalThreadId . kernelConstants =<< askEnv
        virtualiseGroups (segVirt lvl) virt_num_groups_var $ \group_id_flat_exp -> do
          group_id_flat <- dPrimVE "group_id_flat" $ sExt64 group_id_flat_exp
          gid_flat <- dPrimVE "global_id_flat" $ group_id_flat * group_size_flat + local_id_flat
          gids <- map tvExp <$> unflattenIx "global_id" dim_span gid_flat
          zipWithM_ dPrimV_ is gids

          -- check for out of bound on global id for each axis
          sWhen (isActive $ unSegSpace space) $ do
            -- compile invariant elements
            compileStms mempty (kernelBodyStms kbody) $ pure ()

            -- declare and attach invariant elements to invariantParams
            zipWithM_ dPrimV_ (map paramName invariantParams) . map TPrimExp
              =<< mapM toExp invarElems

            let bound_ixs = zipWith sMin64 max_idxs . map (sMax64 0)
                param_ixs = transpose $ zipWith (mapM (+)) stencil_ixss gids
                params_ixs_ordered = transpose $ chunksOf n_point_stencil variantParams

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

type BigTileLoader = BigTileLoaderT KernelsMem KernelEnv Imp.KernelOp
type BigTileLoaderT lore r op =
  [Imp.TExp Int64]
  -> ([Imp.TExp Int64] -> [Imp.TExp Int64])
  -> [VName]
  -> [VName]
  -> [Imp.TExp Int64]
  -> [Imp.TExp Int64]
  -> ([Imp.TExp Int64], Imp.TExp Int64)
  -> ImpM lore r op ()

load_tuple_srcs_to_tiles ::
  [VName]
  -> [VName]
  -> Imp.TExp Int64
  -> [Imp.TExp Int64]
  -> ImpM lore r op ()
load_tuple_srcs_to_tiles tiles srcs flat_local read_gids = do
  forM_ (zip tiles srcs) $ \(tile, input_arr) -> do
    copyDWIMFix tile [flat_local] (Var input_arr) read_gids

tile_loader_cube :: BigTileLoader
tile_loader_cube readSet_offsets bound_ixs tiles srcs shared_sizes group_sizes (local_ids, _) = do
  readSet_iters <- mapM (dPrimV "readSet_iters") $ zipWith divUp shared_sizes group_sizes
  sLoopNest (Shape $ map (Var . tvVar) readSet_iters) $ \ix_list -> do
    tile_locals <- mapM (dPrimVE "tile_local") $ zipWith (+) local_ids $ zipWith (*) ix_list group_sizes
    read_gids <- mapM (dPrimVE "read_gid") $ bound_ixs $ zipWith (+) readSet_offsets tile_locals
    flat_local <- dPrimVE "tile_local_flat" $ flattenIx shared_sizes tile_locals
    sWhen (foldl1 (.&&.) (zipWith (.<.) tile_locals shared_sizes)) $ do
      load_tuple_srcs_to_tiles tiles srcs flat_local read_gids

tile_loader_flat :: BigTileLoader
tile_loader_flat readSet_offsets bound_ixs tiles srcs shared_sizes group_sizes (_, local_id_flat) = do
  shared_size_flat <- dPrimVEC "shared_size_flat" $ product shared_sizes
  group_size_flat <- dPrimVEC "group_size_flat" $ product group_sizes
  flatIters <- dPrimVEC "flat_iters" $ shared_size_flat `divUp` group_size_flat
  shared_spans <- propagateConst "shared_spanss" $ createSpans shared_sizes
  sFor "i" flatIters $ \i -> do
    tile_ix_flat <- dPrimVEC "tile_ix_flat" $ (i * group_size_flat) + local_id_flat
    tile_idxs <- map tvExp <$> unflattenIx "tile_ix" shared_spans tile_ix_flat
    read_gids <- propagateConst "read_gid" $ bound_ixs $ zipWith (+) readSet_offsets tile_idxs
    sWhen (tile_ix_flat .<. shared_size_flat) $ do
      load_tuple_srcs_to_tiles tiles srcs tile_ix_flat read_gids

compileBigTile ::
  BigTileLoader ->
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileBigTile loader pat lvl space op kbody = do
  let num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size_flat_c = toInt64Exp <$> segGroupSize lvl
      group_size_flat_exp = unCount group_size_flat_c
      group_sizes_exp =
        case length dims of
          1 -> [group_size_flat_exp]
          2 -> [group_size_flat_exp `quot` 32, 32]
          3 -> [group_size_flat_exp `quot` 256, 8, 32]
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
      virt_num_groups = sExt32 $ product grid_sizes_exp
  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing

      -- host side variables
      virt_num_groups_var <- dPrimVEC "virt_num_groups" virt_num_groups
      tile_len_flat <- dPrimV "sh_flat_len" $ product shared_sizes_exp
      max_idxs <- mapM (dPrimVEC "max_idx" . (+ (-1))) dims
      num_launched_groups <- dPrimVEC "num lauched groups" $ unCount num_groups'

      sKernelThread "segstencil" (Count num_launched_groups) group_size_flat_c (segFlat space) $ do
        -- device side variables that are independent of group id
        tiles <- forM lamParTypes $ \ptype -> sAllocArray "tile" ptype (Shape [Var $ tvVar tile_len_flat]) (Space "local")
        constants <- kernelConstants <$> askEnv
        shared_sizes <- propagateConst "shared_size_outer" shared_sizes_exp
        group_sizes <- propagateConst "group_size" group_sizes_exp
        group_spans <- propagateConst "group_span" $ createSpans group_sizes
        local_id_flat <- dPrimVEC "local_id_flat" . sExt64 . kernelLocalThreadId $ constants
        local_ids <- map tvExp <$> unflattenIx "local_id" group_spans local_id_flat
        local_id_sh_flat <- dPrimVEC "local_id_sh_flat" $ flattenIx shared_sizes local_ids

        let tile_offsets = map (flattenIx shared_sizes) $ transpose $ zipWith (mapM (-)) stencil_ixss a_mins
            tile_ixs = mapM (+) tile_offsets local_id_sh_flat
            variant_params_tuples = transpose $ chunksOf n_point_stencil variantParams
            bound_idxs = zipWith sMin64 max_idxs . map (sMax64 0)

        virtualiseGroupsHigherDim (segVirt lvl) virt_num_groups_var grid_sizes_exp $ \group_ids_exp -> do
          group_ids <- propagateConst "group_id" group_ids_exp

          -- create writeSet offSets
          writeSet_offsets <- propagateConst "writeSet_offset" $ zipWith (*) group_ids group_sizes
          -- create offsets for the readSet
          readSet_offsets <- propagateConst "readSet_offset" $ zipWith (+) writeSet_offsets a_mins

          -- run the data loader
          loader readSet_offsets bound_idxs tiles (stencilArrays op)
            shared_sizes group_sizes (local_ids, local_id_flat)

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
            forM_ (zip tile_ixs variant_params_tuples) $ \(tile_ix, pars) -> do
              forM_ (zip pars tiles) $ \(par, tile) ->
                copyDWIMFix (paramName par) [] (Var tile) [tile_ix]

            -- compile lambda function and designate output style
            compileStms mempty (bodyStms lamBody) $
              zipWithM_ (compileThreadResult space) (patternElements pat) $
                map (Returns ResultMaySimplify) $ bodyResult lamBody
    SegGroup {} ->
      error "not implemented"

