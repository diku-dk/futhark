{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Futhark.Optimise.TCTiling (doTCTiling) where

import Control.Monad
import Data.Char
import Data.List (zip5)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.GPU
-- import Futhark.IR.Mem.LMAD qualified as LMAD
-- import Futhark.MonadFreshNames
import Futhark.Optimise.BlkRegTiling (matchCodeStreamCode, processIndirections)
import Futhark.Optimise.TileLoops.Shared
import Futhark.Tools
-- import Futhark.Util (splitFromEnd)



-- forM2 :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m [c]
-- forM2 xs ys f = zipWithM f xs ys

-- forM2_ :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m ()
-- forM2_ xs ys f = forM2 xs ys f >> pure ()
--
forM3 :: Monad m => [a] -> [b] -> [c] -> (a -> b -> c -> m d) -> m [d]
forM3 xs ys zs f = forM (zip3 xs ys zs) (\(a, b, c) -> f a b c)
--
-- forM3_ :: Monad m => [a] -> [b] -> [c] -> (a -> b -> c -> m d) -> m ()
-- forM3_ xs ys zs f = forM3 xs ys zs f >> pure ()

se1 :: SubExp
se1 = intConst Int64 1

doTCTiling :: Env -> Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
doTCTiling _env _orig_stm@(Let pat aux (Op (SegOp (SegMap SegThread {} seg_space ts old_kbody))))
  | KernelBody () kstms [Returns ResultMaySimplify certs (Var _res_name)] <- old_kbody,

    -- we don't want to tile the kernel if it is going to have expensive
    -- boundary checks.
    -- TODO: why, though? what else do we do in this case?
    certs == mempty,

    -- the kernel should have exactly one primtyped result.
    [res_type] <- ts,
    primType res_type,

    all_gtids_dims <- unSegSpace seg_space,

    -- TODO: for now, I test only source programs with no outer parallel
    --       dimensions, ie. all dims in the segspace pertain to the
    --       contraction.
    -- TODO: find out how to reliably extract the inner dims of the segspace.
    --       perhaps inner dims are all those onto which the kernel result is
    --       variant and at least (or exactly) one redomap array is variant?
    -- (rem_outer_gtids_dims, inner_gtids_dims) <- undefined -- splitFromEnd n_inner_dims all_gtids_dims,
    (rem_outer_gtids_dims, inner_gtids_dims) <- ([], all_gtids_dims), -- TODO: placeholder.
    (gtids, inner_dims) <- unzip inner_gtids_dims,

    -- check that the kernel fits the pattern:
    -- some code1; one Screma SOAC; some code2,
    -- where code2 may contain additional Scremas but code1 may not.
    -- TODO: do we assume only one Screma in kstms? does it even matter?
    Just (code1, screma_stmt@(Let pat_redomap _ (Op _)), code2') <-
      matchCodeStreamCode kstms,

    -- checks that the Screma SOAC is actually a redomap and normalizes it
    Just (_common_dim, redomap_arrs, (_is_comm, _red_lam, red_nes, _map_lam)) <- isTileableRedomap screma_stmt,

    -- TODO: Cosmin's implementation mentioned rearranging the below couple of
    --       conditions. better look into this.
    -- check that exactly two 1D arrays are streamed through redomap,
    -- and the result of redomap is one scalar
    length redomap_arrs == 2,
    [red_ne] <- red_nes,
    -- [map_t1t, map_t2t] <- map paramDec $ lambdaParams map_lam,
    -- [red_t1, _] <- map paramDec $ lambdaParams red_lam,
    -- primType map_t1t && primType map_t2t && primType red_t1,
    -- _map_t1_0 <- elemType map_t1t,
    -- _map_t2_0 <- elemType map_t2t,

    initial_variance <- M.map mempty $ scopeOfSegSpace seg_space,
    variance <- varianceInStms initial_variance kstms,
    -- assert that all redomap arrays are variant to some, but not all innermost
    -- dimensions of the kernel.
    -- TODO: find out whether/where/how to use the returned information.
    -- Just var_dims_per_arr <- variantInnerDimsPerArr variance redomap_arrs gtids,
    -- Just (var_dims_A : var_dims_B : _) <- variantInnerDimsPerArr variance redomap_arrs gtids,
    Just var_dims_per_arr <- variantInnerDimsPerArr variance redomap_arrs gtids,

    -- TODO: all of the below guards are lifted from Cosmin's code.
    --       find out which of them are relevant here, and whether they need to
    --       be changed/generalized.
    --       as far as I can tell, it all pertains to the handling of `code2`,
    --       so I'll let it sit for now.
    -- get the variables on which the first result of redomap depends on
    (redomap_orig_res : _) <- patNames pat_redomap,
    Just red_res_variance <- M.lookup redomap_orig_res variance, -- variance of the reduce result

    -- we furthermore check that code1 is only formed by
    -- 1. statements that slice some globally-declared arrays
    --    to produce the input for the redomap, and
    -- 2. potentially some statements on which the redomap
    --    is independent; these are recorded in `code2''`
    Just (code2'', table_load_stms) <- processIndirections code1 redomap_arrs red_res_variance,

    -- extract the stms loading slices from redomap arrays and check that there
    -- is one such stm for each redomap array.
    Just load_stms <- mapM (`M.lookup` table_load_stms) redomap_arrs,

    True = do

      let _code2 = code2' <> code2''

      myDebugM $
        "kstms:\n"
          ++ prettyString kstms

      (new_kernel, host_stms) <- runBuilder $ do

        host_info@( HostInfo
                      inner_dim_names
                      tiles_T
                      tiles_R
                      tiles_TR
                      tile_seq
                      grid_dims
                      grid_size_flat
                      tblock_dims
                      tblock_size_flat
                      tbids
                      tbid_flat
                    ) <-
          makeHostInfo inner_dims

        (ret_seggroup, stms_seggroup) <- runBuilder $ do
          KernelInfo tb_offsets arrs_params <-
            makeKernelInfo host_info inner_dims load_stms var_dims_per_arr

          -- Assuming exactly two input arrays.
          let (info_A : info_B : _) = arrs_params

          -- Zero-initialize register tile.
          let segthd_lvl = SegThreadInBlock $ SegNoVirtFull $ SegSeqDims []
          reg_tiles <- segMapND_ "reg_tiles" segthd_lvl ResultPrivate tiles_T $ \_ -> do
            reg_tile_init <- scratch "reg_tile_init" (elemType res_type) tiles_R
            fmap varsRes $
              forLoopNest tiles_R [reg_tile_init] $ \loop_inds [merge] ->
                (: []) <$> update "reg_tile" merge loop_inds red_ne


          let regtile_ret_dims =
                map ((,se1,se1) . snd) rem_outer_gtids_dims
                  ++ zip3 inner_dims tiles_T tiles_R
          pure [RegTileReturns mempty regtile_ret_dims reg_tiles]
        -- END KERNEL BUILDER

        let grid = KernelGrid (Count grid_size_flat) (Count tblock_size_flat)
            level' = SegBlock SegNoVirt (Just grid)
            space' = SegSpace tbid_flat (rem_outer_gtids_dims ++ zip tbids grid_dims)
            kbody' = KernelBody () stms_seggroup ret_seggroup
        pure $ Let pat aux $ Op $ SegOp $ SegMap level' space' ts kbody'

      -- END HOST BUILDER
      pure $ Just (host_stms, new_kernel)
  where

doTCTiling _seg_space _kstms = pure Nothing

-- | Given a variance table, a list of array names, and a list of inner dims
-- (actually, the list of gtids for said inner dims); asserts that each array is
-- variant to at least 1 and not all inner dims, and that at least one array is
-- variant to each inner dim. If these assertions hold; returns list of indices
-- of variant dims for each array.
-- TODO: instead of the indices of variant dims, should this function return
--       simply the variant dims?
variantInnerDimsPerArr ::
  VarianceTable ->
  [VName] ->
  [VName] ->
  Maybe [[VName]]
variantInnerDimsPerArr variance arrs inner_dims
  -- TODO: what is this check (lifted from Cosmin's original implementation) and
  -- is it relevant here?
  -- all (`notNameIn` branch_variant) inner_dims,

  | arr_variances <- map variantInnerDimsForArr arrs,
    -- assert that all arrays are variant to some, but not all inner_dims.
    -- TODO: is below check sufficient to check this assertion?
    --       perhaps this assertion should be (or already is) made elsewhere.
    all ((`elem` [1 .. n_dims - 1]) . length) arr_variances,
    -- assert that at least one array is variant to each inner dim.
    -- TODO: is there a better, more correct, or safer way to assert this?
    all (`elem` concat arr_variances) inner_dims,
    -- assert no overlap in variance between arrays.
    -- TODO: is this check necessary?
    allUnique $ concat arr_variances =
      pure arr_variances
  | otherwise = Nothing
  where
    n_dims = length inner_dims
    variantInnerDimsForArr arr =
      let arr_variance = M.findWithDefault mempty arr variance
       in filter (`nameIn` arr_variance) inner_dims
    allUnique (x : xs) = x `notElem` xs && allUnique xs
    allUnique _ = True

-- | All the various kernel parameters and related information we need to
-- declare and/or compute in host code.
-- TODO: give a proper name to this one.
data TCTilingHostInfo = HostInfo
  { -- not strictly necessary, but nice to have for consistent names throughout
    -- the generated code.
    innerDimNames :: [String],
    -- T, R, and TR tiles for each dim in the result.
    tilesT :: [SubExp],
    tilesR :: [SubExp],
    tilesTR :: [SubExp],
    -- sequential tile (tiling the sequential/reduction dimension).
    tileSeq :: SubExp,
    gridDims :: [SubExp],
    gridSizeFlat :: SubExp,
    tblockDims :: [SubExp],
    tblockSizeFlat :: SubExp,
    -- tblock indices.
    tbidVns :: [VName],
    tbidFlatVn :: VName
  }
  deriving (Show)

-- | Holds the various kernel information which needs to be declared and/or
-- computed in kernel code.
-- TODO: give a proper name to this one.
data TCTilingKernelInfo = KernelInfo
  { tbOffsets :: [SubExp],
    arrsParams :: [ArrInfo]
  }
  deriving (Show)

-- | All the information needed to handle reading from an operand array.
-- TODO: give a proper name to this one.
data ArrInfo = ArrInfo
  { baseArrDims :: [SubExp],
    tileDims :: [SubExp],
    tbOffsets :: [SubExp],
    variantGtids :: [VName],
    arrLoadStm :: Stm GPU
  }
  deriving (Show)

makeHostInfo ::
  [SubExp] ->
  Builder GPU TCTilingHostInfo
makeHostInfo inner_dims_se = do
  -- various names.
  tile_common_dim_vn <- newVName "T_common_dim"
  tile_T_vns <- newPrefixedVNames "T_" inner_dim_names
  tile_R_vns <- newPrefixedVNames "R_" inner_dim_names
  tbid_vns <- newPrefixedVNames "tbid_" inner_dim_names
  tbid_flat_vn <- newVName "tbid_flat"

  -- tile sizes.
  tile_seq <- getTileSE SizeTile tile_common_dim_vn
  tiles_T <- mapM (getTileSE SizeTile) tile_T_vns
  tiles_R <- mapM (getTileSE SizeRegTile) tile_R_vns
  tiles_TR <-
    zipWithM (\t r -> toExp $ pe64 t * pe64 r) tiles_T tiles_R
      >>= zipWithM letSubExp (map ("TR_" ++) inner_dim_names)

  -- grid and tblock stuff.
  grid_dims <-
    zipWithM ceilDiv inner_dims_se tiles_TR
      >>= zipWithM letSubExp (map ("grid_dim_" ++) inner_dim_names)

  let tblock_dims = tiles_T
  grid_size_flat <- letSubExp "grid_size_flat" =<< toExp (product $ map pe64 grid_dims)
  tblock_size_flat <- letSubExp "tblock_size_flat" =<< toExp (product $ map pe64 tiles_T)

  pure $
    HostInfo
      inner_dim_names
      tiles_T
      tiles_R
      tiles_TR
      tile_seq
      grid_dims
      grid_size_flat
      tblock_dims
      tblock_size_flat
      tbid_vns
      tbid_flat_vn
  where
    inner_dim_names
      | Just name_strs <- mapM getNameStrFor inner_dims_se = name_strs
      | otherwise = map show $ indices inner_dims_se

    getNameStrFor (Var v) = Just $ filter isAscii $ baseString v
    getNameStrFor _ = Nothing

    newPrefixedVNames s = mapM (newVName . (s ++))

    getTileSE tile_type v =
      letSubExp (baseString v) $ Op $ SizeOp $ GetSize (baseName v) tile_type

makeKernelInfo ::
  TCTilingHostInfo ->
  [SubExp] ->
  [Stm GPU] ->
  [[VName]] ->
  Builder GPU TCTilingKernelInfo
makeKernelInfo host_stuff inner_dims_se load_stms var_gtids_per_arr = do
  -- We need to extract the dimensions of each input array, and unfortunately
  -- the Redomap passed into this module only indirectly carries this
  -- information, as part of the kernel stms loading each redomap input slice.
  -- It'd be more convenient if the Redomap carried not only the VNames of its
  -- operands slices, but also the base arrays (if any) whence each slice comes,
  -- or at least the dimensionality of.
  --
  -- In any case, this information is necessary in order to match tile dims with
  -- the dims of each input array -- since these are not simply (M: (Ty, Ry))
  -- and (N: (Tx, Rx)) as in the 2D case -- as well as to generate boundary
  -- checks later on.
  let base_arrs = map getArrayFromLoadStm load_stms
  dims_per_arr <- mapM getArrDims base_arrs

  tb_offsets <-
    forM3 inner_dim_names tbids tiles_TR $
      \dim_name tbid tile_TR ->
        letSubExp ("tb_offset_" ++ dim_name)
          =<< toExp (le64 tbid * pe64 tile_TR)

  -- It's not pretty, but we somehow need to extract the tile dimensions and
  -- tblock offsets corresponding to each dimension of each array, and below
  -- mess accomplishes this.
  --
  -- First, map each dimension to its corresponding TR tile and tblock offset.
  let dims_to_tiles = M.fromList $ zip inner_dims_se tiles_TR
  let dims_to_tb_offsets = M.fromList $ zip inner_dims_se tb_offsets

  -- Then, for each dimension of each array, extract the TR tile and tblock
  -- offset corresponding to this dimension. Each array has a common
  -- dimension which is not represented in `inner_dims_se`; here we insert the
  -- sequential tile size, but do obviously not compute a tblock offset.
  let (tile_dims_per_arr, tb_offsets_per_arr) =
        unzip $
          map
            ( \dims ->
                ( mapMaybe (`M.lookup` dims_to_tb_offsets) dims,
                  map (flip (M.findWithDefault tile_seq) dims_to_tiles) dims
                )
            )
            dims_per_arr

  let arrs_params =
        map
          ( \(dim, tile_dim, tb_offset, var_gtids, load_stm) ->
              ArrInfo dim tile_dim tb_offset var_gtids load_stm
          )
          $ zip5 dims_per_arr tile_dims_per_arr tb_offsets_per_arr var_gtids_per_arr load_stms

  pure $
    KernelInfo
      tb_offsets
      arrs_params
  where
    getArrayFromLoadStm :: Stm GPU -> VName
    getArrayFromLoadStm (Let _ _ (BasicOp (Index arr _))) = arr
    getArrayFromLoadStm stm =
      error $
        "getArrayFromLoadStm error: expected a BasicOp Index stm, got: "
          ++ prettyString stm

    getArrDims :: VName -> Builder GPU [SubExp]
    -- TODO: can also use this non-throwing definition using Types.arrayDims (?)
    -- getArrDims = pure . arrayDims <=< lookupType
    getArrDims = pure . arrDims <=< lookupType
      where
        arrDims (Array _ shape _) = shapeDims shape
        arrDims tp =
          error $ "getTileDimsForArr error: expected array type, got: " ++ prettyString tp

    inner_dim_names = innerDimNames host_stuff
    tbids = tbidVns host_stuff
    tiles_TR = tilesTR host_stuff
    tile_seq = tileSeq host_stuff
