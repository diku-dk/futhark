{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegStencil' is quite straightforward.
module Futhark.CodeGen.ImpGen.Kernels.SegStencil (compileSegStencil) where

import Control.Monad.Except
import Data.List (transpose, minimumBy)
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.IR.KernelsMem
import Futhark.Util.IntegralExp
import Prelude hiding (div, quot, rem)
import qualified Prelude (quot)

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
  --compileBigTileSingleDim tile_loader_flat
  compileBigTileStripMinedSingleDim

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

createSpans :: Num a => [a] -> [a]
createSpans = scanr1 (*) . tail

sForUnflatten ::
  IntExp t =>
  [Imp.TExp t] ->
  Imp.TExp t ->
  Imp.TExp t ->
  (([Imp.TExp t], Imp.TExp t)-> ImpM lore r op ()) ->
  ImpM lore r op ()
sForUnflatten sizes start_flat added_flat m = do
  size_span <- mapM (dPrimVE "size_span") $ createSpans sizes
  iterations <- dPrimVE "iterations" $ divUp (product sizes) added_flat
  starts <- unflattenIx "start" size_span start_flat
  adds <- unflattenIx "added" size_span added_flat
  sizes_const <- mapM (dPrimVE "size") sizes
  added_flat_var <- dPrimVE "added_flat_var" added_flat
  start_flat_var <- dPrimV "start_flat_var" start_flat
  let add_carry (bi, gz, bn) = do
        cond <- dPrimVE "cond" (tvExp bi .>=. gz)
        sWhen cond $ do
          bi <-- (tvExp bi - gz)
          bn <-- (tvExp bn + 1)
      stls = reverse starts
      szls = reverse sizes_const
      ls = zip3 stls szls (tail stls)
      prepareNextIter = do
        start_flat_var <-- (tvExp start_flat_var + added_flat_var)
        zipWithM_ (\b ng -> b <-- (tvExp b + tvExp ng)) starts adds
        forM_ ls add_carry
  sFor "i" iterations $ \_ -> do
    () <- m (map tvExp starts, tvExp start_flat_var)
    prepareNextIter

dPrimVEC ::
  String ->
  TPrimExp t Imp.ExpLeaf ->
  ImpM lore r op (TPrimExp t Imp.ExpLeaf)
dPrimVEC _ x@(TPrimExp (ValueExp _)) = pure x
dPrimVEC _ x@(TPrimExp (LeafExp _ _)) = pure x
dPrimVEC name x = dPrimVE name x

propagateConst ::
  String ->
  [TPrimExp t Imp.ExpLeaf] ->
  ImpM lore r op [TPrimExp t Imp.ExpLeaf]
propagateConst name = mapM (dPrimVEC name)

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
      size_span <- mapM (dPrimVE "size_span" . sExt64) $ createSpans dims

      sKernelThread "segstencil" num_groups' group_size_flat_c (segFlat space) $ do
        -- device side variable that is independent of virtual kernel id
        local_id_flat <- dPrimVEC "local_id_flat" . sExt64 . kernelLocalThreadId . kernelConstants =<< askEnv

        phys_group_id <- dPrim "phys_group_id" int32
        sOp $ Imp.GetGroupId (tvVar phys_group_id) 0

        gid_flat <- dPrimVE "global_id_flat" $ tvExp phys_group_id * group_size_flat + local_id_flat
        gids <- map tvExp <$> unflattenIx "global_id" size_span gid_flat
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
  -> [Imp.TExp Int32]
  -> [Imp.TExp Int32]
  -> ([Imp.TExp Int32], Imp.TExp Int32)
  -> ImpM lore r op ()

loadTupleSrcsToTiles ::
  [VName]
  -> [VName]
  -> Imp.TExp Int64
  -> [Imp.TExp Int64]
  -> ImpM lore r op ()
loadTupleSrcsToTiles tiles srcs flat_local read_gids = do
  forM_ (zip tiles srcs) $ \(tile, input_arr) -> do
    copyDWIMFix tile [flat_local] (Var input_arr) read_gids

tileLoaderFlat :: BigTileLoader
tileLoaderFlat readSet_offsets bound_ixs tiles srcs shared_sizes group_sizes (_, local_id_flat) = do
  shared_size_flat <- dPrimVEC "shared_size_flat" $ product shared_sizes
  group_size_flat <- dPrimVEC "group_size_flat" $ product group_sizes
  flatIters <- dPrimVEC "flat_iters" $ shared_size_flat `divUp` group_size_flat
  shared_spans <- propagateConst "shared_spanss" $ createSpans shared_sizes
  sFor "i" flatIters $ \i -> do
    tile_ix_flat <- dPrimVEC "tile_ix_flat" $ (i * group_size_flat) + local_id_flat
    tile_idxs <- map tvExp <$> unflattenIx "tile_ix" shared_spans tile_ix_flat
    read_gids <- propagateConst "read_gid" $ bound_ixs $ zipWith (+) readSet_offsets (map sExt64 tile_idxs)
    sWhen (tile_ix_flat .<. shared_size_flat) $ do
      load_tuple_srcs_to_tiles tiles srcs (sExt64 tile_ix_flat) read_gids

-- Still need to find a way to choose the maximum possible block size.
compileBigTileSingleDim ::
  BigTileLoader ->
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileBigTileSingleDim loader pat lvl space op kbody = do
  let group_size_flat_c = toInt64Exp <$> segGroupSize lvl
      group_size_flat_exp = sExt32 $ unCount group_size_flat_c
      (is, dims') = unzip $ unSegSpace space
      dims = map toInt64Exp dims'
      -- It is possible to use int32, since a value larger than a maximum int32 would surely make the program
      -- run out of shared memory
      stencil_ixss :: [[TPrimExp Int32 Imp.ExpLeaf]]
      stencil_ixss = map (map fromInteger) $ stencilIndexes op
      invarElems = map kernelResultSubExp $ kernelBodyResult kbody
      lam = stencilOp op
      lamBody = lambdaBody lam
      lamPar = lambdaParams lam
      (invariantParams, variantParams) =
        splitAt (length invarElems) lamPar
      a_mins :: [TPrimExp Int32 Imp.ExpLeaf]
      a_mins = map (fromInteger . (\x -> if x > 0 then error "invalid axis min" else x) . minimum) $ stencilIndexes op
      a_maxs :: [TPrimExp Int32 Imp.ExpLeaf]
      a_maxs = map (fromInteger . (\x -> if x < 0 then error "invalid axis max" else x) . maximum) $ stencilIndexes op
      n_point_stencil = length $ head stencil_ixss
      lamParTypes = map (elemType . paramType . head) $ chunksOf n_point_stencil variantParams

  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing

      let group_sizes_exp :: [TPrimExp Int32 Imp.ExpLeaf]
          group_sizes_exp =
            case length dims of
              1 -> [group_size_flat_exp]
              2 -> [group_size_flat_exp `quot` 32, 32]
              3 -> [group_size_flat_exp `quot` 256, 8, 32]
              _ -> error "not valid dimensions"

          grid_sizes_exp = map sExt64 $ zipWith divUp dims (map sExt64 group_sizes_exp)
          shared_sizes_exp = zipWith (+) group_sizes_exp $ zipWith (-) a_maxs a_mins
          virt_num_groups = product grid_sizes_exp

      -- host side variables
      sizes <- propagateConst "size" grid_sizes_exp
      size_span <- propagateConst "size_span" $ createSpans sizes
      virt_num_groups_var <- dPrimVEC "virt_num_groups" virt_num_groups
      tile_len_flat <- dPrimV "sh_flat_len" $ product shared_sizes_exp
      max_idxs <- mapM (dPrimVEC "max_idx" . (+ (-1))) dims

      sKernelThread "segstencil" (Count virt_num_groups_var) group_size_flat_c (segFlat space) $ do
        -- device side variables that are independent of group id
        tiles <- forM lamParTypes $ \ptype -> sAllocArray "tile" ptype (Shape [Var $ tvVar tile_len_flat]) (Space "local")
        constants <- kernelConstants <$> askEnv
        shared_sizes <- propagateConst "shared_size_outer" shared_sizes_exp
        group_sizes <- propagateConst "group_size" group_sizes_exp
        group_spans <- propagateConst "group_span" $ createSpans group_sizes
        local_id_flat <- dPrimVEC "local_id_flat" . kernelLocalThreadId $ constants
        local_ids <- map tvExp <$> unflattenIx "local_id" group_spans local_id_flat
        local_id_sh_flat <- dPrimVEC "local_id_sh_flat" $ flattenIx shared_sizes local_ids

        let tile_offsets = map (flattenIx shared_sizes) $ transpose $ zipWith (mapM (-)) stencil_ixss a_mins
            tile_ixs = mapM (+) tile_offsets local_id_sh_flat
            variant_params_tuples = transpose $ chunksOf n_point_stencil variantParams
            bound_idxs = zipWith sMin64 max_idxs . map (sMax64 0)



        phys_group_id <- dPrim "phys_group_id" int32
        sOp $ Imp.GetGroupId (tvVar phys_group_id) 0

        group_ids <- map tvExp <$> (unflattenIx "group_id" size_span . tvExp $ phys_group_id)

        -- create writeSet offSets
        writeSet_offsets <- propagateConst "writeSet_offset" $ zipWith (*) (map sExt64 group_ids) (map sExt64 group_sizes)
        -- create offsets for the readSet
        readSet_offsets <- propagateConst "readSet_offset" $ zipWith (+) writeSet_offsets (map sExt64 a_mins)

        -- run the data loader
        loader readSet_offsets bound_idxs tiles (stencilArrays op)
          shared_sizes group_sizes (local_ids, local_id_flat)

        -- group syncronize
        sOp $ Imp.Barrier Imp.FenceLocal

        let gids = zipWith (+) writeSet_offsets (map sExt64 local_ids)
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
              copyDWIMFix (paramName par) [] (Var tile) [sExt64 tile_ix]

          -- compile lambda function and designate output style
          compileStms mempty (bodyStms lamBody) $
            zipWithM_ (compileThreadResult space) (patternElements pat) $
              map (Returns ResultMaySimplify) $ bodyResult lamBody
    SegGroup {} ->
      error "not implemented"

calculateStripSetup
  :: [Integer]
  -> [Integer]
  -> Integer
  -> Integer
  -> [Integer]
calculateStripSetup group_sizes halo_widths max_memory memory_per_elem =
  let max_flat_length = max_memory `Prelude.quot` memory_per_elem
      inverse_weights = zipWith (\h g -> if h == 0 then max_flat_length else g) halo_widths group_sizes
      go sm =
        let sl = zipWith (*) sm inverse_weights
            min_ix = snd $ minimumBy (\(x,_) (y,_) -> x `compare` y) $ zip sl [0 :: Integer ..]
            next_sm = zipWith (\m i-> if i == min_ix then m*2 else m) sm [0..]
            flat_length = product $ zipWith (*) next_sm group_sizes
         in if flat_length > max_flat_length then sm else go next_sm
   in go $ map (const 1) group_sizes

compileBigTileStripMinedSingleDim ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileBigTileStripMinedSingleDim pat lvl space op kbody = do
  let group_size_flat_c = TPrimExp . toExp' int32 <$> segGroupSize lvl
      group_size_flat_exp = unCount group_size_flat_c
      (is, dims') = unzip $ unSegSpace space
      dims = map toInt64Exp dims'
      stencil_ixss :: [[Imp.TExp Int32]]
      stencil_ixss = map (map fromInteger) $ stencilIndexes op
      invarElems = map kernelResultSubExp $ kernelBodyResult kbody
      lam = stencilOp op
      lamBody = lambdaBody lam
      lamPar = lambdaParams lam
      (invariantParams, variantParams) =
        splitAt (length invarElems) lamPar
      a_mins_i = map minimum $ stencilIndexes op
      a_maxs_i = map maximum $ stencilIndexes op
      a_mins :: [Imp.TExp Int32]
      a_mins = map fromInteger a_mins_i
      a_maxs :: [Imp.TExp Int32]
      a_maxs = map fromInteger a_maxs_i
      n_point_stencil = length $ head stencil_ixss
      lamParTypes = map (elemType . paramType . head) $ chunksOf n_point_stencil variantParams
      group_sizes_exp :: [Imp.TExp Int32]
      group_sizes_exp =
        case length dims of
          1 -> [group_size_flat_exp]
          2 -> [group_size_flat_exp `quot` 32, 32]
          3 -> [group_size_flat_exp `quot` 256, 8, 32]
          _ -> error "not valid dimensions"

      -- width of the zone of indexes that only read from and not writen to.
      halo_widths = map (\x-> x-1) $ zipWith (-) a_maxs_i a_mins_i
      -- maximum possible block size on any configuration.
      -- This is valid as a smaller blocksize than that would then have it
      -- use less shared memory that at max size.
      max_possible_group_sizes_exp =
        case length dims of
          1 -> [1024]
          2 -> [32, 32]
          3 -> [4, 8, 32]
          _ -> error "not valid dimensions"
      -- This is lowest limit of allowed max shared memory per block for the
      -- supported Cuda compute capabilities (>= 3.0).
      -- This may be too much for certain AMD/OpenCl configurations but we
      -- check for that before reaching this point. (TODO)
      max_memory = 49152
      -- Amount of Bytes required for a single index in tile(s).
      memory_per_elem = sum $ map primByteSize lamParTypes

      strip_multiples_exp :: [Integer]
      strip_multiples_exp = calculateStripSetup max_possible_group_sizes_exp halo_widths max_memory memory_per_elem
      strip_multiples :: [Imp.TExp Int32]
      strip_multiples = map fromInteger strip_multiples_exp

      bound_idxs = zipWith sMin64 (map (\x->x-1) dims) . map (sMax64 0)

  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing

      -- constexpr variables
      let strip_sizes = zipWith (*) strip_multiples group_sizes_exp
          shared_sizes = zipWith (+) strip_sizes $ zipWith (-) a_maxs a_mins
          shared_size_flat = product shared_sizes
          group_sizes = group_sizes_exp
          group_spans = createSpans group_sizes
          group_size_flat = product group_sizes

      -- host side variables
      strip_grid <- propagateConst "strip_grid" $ map sExt32 $ zipWith divUp dims $ map sExt64 strip_sizes
      strip_grid_spans <- propagateConst "strip_grid_spans" $ createSpans strip_grid

      tile_length <- dPrimV "tile_length_flat" $ product shared_sizes

      num_groups <- dPrimVEC "num_groups" $ sExt64 $ product strip_grid

      sKernelThread "segstencil" (Count num_groups) (sExt64 <$> group_size_flat_c) (segFlat space) $ do
        tiles <- forM lamParTypes $ \ptype -> sAllocArray "tile" ptype (Shape [Var $ tvVar tile_length]) (Space "local")
        constants <- kernelConstants <$> askEnv
        local_id_flat <- dPrimVEC "local_id_flat" . kernelLocalThreadId $ constants
        local_ids <- map tvExp <$> unflattenIx "local_id" group_spans local_id_flat

        strip_id_flat <- dPrimVEC "strip_id_flat" . kernelGroupId $ constants
        strip_ids <- map tvExp <$> unflattenIx "strip_id" strip_grid_spans strip_id_flat

        writeSet_offsets <- propagateConst "writeSet_offset" $ zipWith (*) (map sExt64 strip_ids) (map sExt64 strip_sizes)
        -- run the data loader
        sForUnflatten shared_sizes local_id_flat group_size_flat $ \(loader_ids, loader_ids_flat) -> do
          loader_gids <- propagateConst "loader_gid" $
            bound_idxs $ zipWith (+) writeSet_offsets $ map sExt64 $ zipWith (+) loader_ids a_mins
          sWhen (loader_ids_flat .<. shared_size_flat) $
            forM_ (zip tiles (stencilArrays op)) $ \(tile, input_arr) ->
              copyDWIMFix tile [sExt64 loader_ids_flat] (Var input_arr) loader_gids

        sOp $ Imp.Barrier Imp.FenceLocal
        -- loader finished

        let nest_shape_const = Shape $ map (Constant . IntValue . intValue Int64) strip_multiples_exp
        sLoopNest nest_shape_const $ \local_strip_ids -> do
          tile_local_ids <- propagateConst "tile_local_id" $ zipWith (+) local_ids $ zipWith (*) (map sExt32 local_strip_ids) group_sizes
          tile_local_id_flat <- dPrimVEC "tile_local_id_flat" $ flattenIx shared_sizes tile_local_ids
          let gids = zipWith (+) writeSet_offsets (map sExt64 tile_local_ids)
          zipWithM_ dPrimV_ is gids

          let tile_offsets = map (flattenIx shared_sizes) $ transpose $ zipWith (mapM (-)) stencil_ixss a_mins
              tile_ixs = mapM (+) tile_offsets tile_local_id_flat
              variant_params_tuples = transpose $ chunksOf n_point_stencil variantParams

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
                copyDWIMFix (paramName par) [] (Var tile) [sExt64 tile_ix]

            -- compile lambda function and designate output style
            compileStms mempty (bodyStms lamBody) $
              zipWithM_ (compileThreadResult space) (patternElements pat) $
                map (Returns ResultMaySimplify) $ bodyResult lamBody
    SegGroup {} ->
      error "not implemented"
