{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Futhark.Optimise.TCTiling (doTCTiling) where

import Control.Monad
import Data.Char
import Data.List -- (zip5, findIndices, elemIndex)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.GPU
-- import Futhark.IR.Mem.LMAD qualified as LMAD
-- import Futhark.MonadFreshNames
import Futhark.Optimise.BlkRegTiling (matchCodeStreamCode, processIndirections)
import Futhark.Optimise.TileLoops.Shared
import Futhark.Tools
import Futhark.Transform.Rename
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

se42 :: SubExp
se42 = intConst Int64 42

seglvl_thd :: SegLevel
seglvl_thd = SegThreadInBlock $ SegNoVirtFull $ SegSeqDims []

gather :: [a] -> [Int] -> [a]
gather xs = map (xs !!) . filter (`elem` indices xs)

gather_ :: [a] -> a -> [Maybe Int] -> [a]
gather_ xs x = map (maybe x (xs !!) . checkIdx)
  where
    checkIdx (Just i)
      | i `elem` indices xs = Just i
    checkIdx _ = Nothing

reductionLoopBody ::
  TCTilingHostInfo ->
  TCTilingKernelInfo ->
  VName ->
  VName ->
  VName ->
  VName ->
  Builder GPU [VName]
reductionLoopBody host_info kernel_info qq0 reg_tiles_in shr_A_in shr_B_in = do
  qq <- letExp "qq" =<< toExp (le64 qq0 * pe64 tq)

  -- TODO: finish copyGlb2Shr.
  let shr_A_withacc_inputs = [(shr_A_shape, [shr_A_in], Nothing)]

  cert_param <- newParam "cert_p" $ Prim Unit
  t <- stripArray (shapeRank shr_A_shape) <$> lookupType shr_A_in
  acc_param <- newParam "acc_p" $ Acc (paramName cert_param) shr_A_shape [t] NoUniqueness

  iters <- letSubExp "iters" =<< toExp se42
  lam_load_A <-
    -- mkLambda [cert_param, acc_param] $ varsRes [paramName acc_param]
    mkLambda [cert_param, acc_param] $
      fmap varsRes $
        forLoop iters [paramName acc_param] $ \i [acc_merge] -> do
          let gtids = gather (innerGtids host_info) $ catMaybes var_dims_A
          forM_ gtids $ \gtid ->
            letBindNames [gtid] =<< toExp (le64 i)
          addStm load_A
          val <- index "A_elem" slice_A [i]
          letTupExp "acc_out" $
            BasicOp $
              UpdateAcc
                acc_merge
                [Var i, Var i]
                [Var val]

  shr_A_out <- letExp "shr_A_new" $ WithAcc shr_A_withacc_inputs lam_load_A

  shr_B <- pure shr_B_in -- TODO

  reg_tiles_out <- forLoop_ tq [reg_tiles_in] $ \q [reg_tiles_merge] -> do
    letTupExp "reg_tiles"
      =<< eIf
        (toExp $ le64 qq + le64 q .<. pe64 len_q)
        (accumulateRegTile q reg_tiles_merge shr_A_out shr_B)
        (resultBodyM [Var reg_tiles_merge])

  pure [reg_tiles_out, shr_A_out, shr_B]
  where
    accumulateRegTile q reg_tiles_in shr_A shr_B = do
      reg_tiles_out <- segMapND_ "reg_tile_res" seglvl_thd ResultPrivate tiles_T $ \ltids -> do
        reg_tile_in <- index "reg_tile_in" reg_tiles_in ltids
        fmap varsRes $ forLoopNest tiles_R [reg_tile_in] $ \loop_inds [reg_tile_merge] -> do

          let inds_A = gather_ loop_inds q var_dims_A
          let inds_B = gather_ loop_inds q var_dims_B

          a <- index "elem_A" shr_A inds_A
          b <- index "elem_B" shr_B inds_B
          acc <- index "acc" reg_tile_merge loop_inds

          map_f <- renameLambda $ mapLam host_info
          red_op <- renameLambda $ redLam host_info

          map_res <- eLambda map_f $ map (eSubExp . Var) [a, b]
          red_res <- eLambda red_op $ map eSubExp $ Var acc : map resSubExp map_res
          res <- update "res" reg_tile_merge loop_inds $ resSubExp $ head red_res
          pure [res]

      resultBodyM [Var reg_tiles_out]

    copyGlb2Shr glb_X shr_X tile_dims = do
      pure undefined

    tq = tileSeq host_info
    tiles_T = tilesT host_info
    tiles_R = tilesR host_info
    len_q = commonDim host_info
    gtids = innerGtids host_info

    arr_infos = arrsInfo kernel_info
    [slice_A, slice_B] = map slice arr_infos
    [shr_A_shape, shr_B_shape] = map (Shape . tileDims) arr_infos
    [var_dims_A, var_dims_B] = map variantDims arr_infos
    [load_A, load_B] = map arrLoadStm arr_infos

doTCTiling :: Env -> Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
doTCTiling _env (Let pat aux (Op (SegOp (SegMap SegThread {} seg_space ts old_kbody))))
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
    Just (common_dim, redomap_arrs, (_is_comm, red_lam, red_nes, map_lam)) <- isTileableRedomap screma_stmt,

    -- TODO: Cosmin's implementation mentioned rearranging the below couple of
    --       conditions. better look into this.
    -- check that exactly two 1D arrays are streamed through redomap,
    -- and the result of redomap is one scalar
    length redomap_arrs == 2,
    [red_ne] <- red_nes,
    map_ts <- map paramDec $ lambdaParams map_lam,
    [map_t1, map_t2] <- map elemType map_ts,
    [red_t, _] <- map paramDec $ lambdaParams red_lam,
    all primType map_ts,
    primType red_t,

    initial_variance <- M.map mempty $ scopeOfSegSpace seg_space,
    variance <- varianceInStms initial_variance kstms,
    -- assert that all redomap arrays are variant to some, but not all innermost
    -- dimensions of the kernel.
    -- TODO: find out whether/where/how to use the returned information.
    Just (var_dims_per_arr, var_inds_per_arr) <- variantInnerDimsPerArr variance redomap_arrs gtids,

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

      (new_kernel, host_stms) <- runBuilder $ do

        host_info@( HostInfo
                      _gtids
                      _inner_dims
                      _common_dim
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
                      _map_lam
                      _red_lam
                    ) <-
          makeHostInfo gtids inner_dims common_dim map_lam red_lam

        (ret_seggroup, stms_seggroup) <- runBuilder $ do
          kernel_info@(KernelInfo tb_offsets arrs_params) <-
            makeKernelInfo host_info inner_dims load_stms

          -- Assuming exactly two input arrays.
          let (info_A : info_B : _) = arrs_params
          let shr_A_size = tileDims info_A
          let shr_B_size = tileDims info_B

          -- Zero-initialize register tile.
          reg_tiles_init <- segMapND_ "reg_tiles" seglvl_thd ResultPrivate tiles_T $ \_ -> do
            reg_tile_init <- scratch "reg_tile_init" (elemType res_type) tiles_R
            css <- forLoopNest tiles_R [reg_tile_init] $ \loop_inds [merge] ->
              (: []) <$> update "reg_tile" merge loop_inds red_ne
            pure $ varsRes css

          shr_A_init <- scratch "A_shr" map_t1 shr_A_size
          shr_B_init <- scratch "B_shr" map_t2 shr_B_size

          num_seq_tiles <- letSubExp "num_seq_tiles" =<< ceilDiv common_dim tile_seq

          reduction_loop_res <-
            forLoop num_seq_tiles [reg_tiles_init, shr_A_init, shr_B_init] $
              \qq [reg_tile_merge, shr_A_merge, shr_B_merge] ->
                reductionLoopBody host_info kernel_info qq reg_tile_merge shr_A_merge shr_B_merge

          let [reg_tiles_res, _shr_A, _shr_B] = reduction_loop_res

          let regtile_ret_dims =
                map ((,se1,se1) . snd) rem_outer_gtids_dims
                  ++ zip3 inner_dims tiles_T tiles_R
          pure [RegTileReturns mempty regtile_ret_dims reg_tiles_res]
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
  Maybe ([[VName]], [[Int]])
variantInnerDimsPerArr variance arrs inner_dims
  -- TODO: what is this check (lifted from Cosmin's original implementation) and
  -- is it relevant here?
  -- all (`notNameIn` branch_variant) inner_dims,

  | var_inds_per_arr <- map variantInnerDimsForArr arrs,
    var_dims_per_arr <- map (gather inner_dims) var_inds_per_arr,
    -- assert that all arrays are variant to some, but not all inner_dims.
    -- TODO: is below check sufficient to check this assertion?
    --       perhaps this assertion should be (or already is) made elsewhere.
    all ((`elem` [1 .. n_dims - 1]) . length) var_dims_per_arr,
    -- assert that at least one array is variant to each inner dim.
    -- TODO: is there a better, more correct, or safer way to assert this?
    all (`elem` concat var_dims_per_arr) inner_dims,
    -- assert no overlap in variance between arrays.
    -- TODO: is this check necessary?
    allUnique $ concat var_dims_per_arr =
      pure (var_dims_per_arr, var_inds_per_arr)
  | otherwise = Nothing
  where
    n_dims = length inner_dims
    variantInnerDimsForArr arr =
      let arr_variance = M.findWithDefault mempty arr variance
       in findIndices (`nameIn` arr_variance) inner_dims
    allUnique (x : xs) = x `notElem` xs && allUnique xs
    allUnique _ = True

-- | All the various kernel parameters and related information we need to
-- declare and/or compute in host code.
-- TODO: give a proper name to this one.
data TCTilingHostInfo = HostInfo
  { innerGtids :: [VName],
    innerDims :: [SubExp],
    commonDim :: SubExp,
    -- not strictly necessary, but nice to have for consistent names throughout
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
    tbidFlatVn :: VName,
    mapLam :: Lambda GPU,
    redLam :: Lambda GPU
  }
  deriving (Show)

-- | Holds the various kernel information which needs to be declared and/or
-- computed in kernel code.
-- TODO: give a proper name to this one.
data TCTilingKernelInfo = KernelInfo
  { tbOffsets :: [SubExp],
    arrsInfo :: [ArrInfo]
  }
  deriving (Show)

-- | All the information needed to handle reading from an operand array.
-- TODO: give a proper name to this one.
data ArrInfo = ArrInfo
  { slice :: VName,
    baseArrDims :: [SubExp],
    tileDims :: [SubExp],
    tbOffsets :: [SubExp],
    -- variantGtids :: [VName],
    variantDims :: [Maybe Int],
    arrLoadStm :: Stm GPU
  }
  deriving (Show)

makeHostInfo ::
  [VName] ->
  [SubExp] ->
  SubExp ->
  Lambda GPU ->
  Lambda GPU ->
  Builder GPU TCTilingHostInfo
makeHostInfo gtids inner_dims_se common_dim map_lam red_lam = do
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
      gtids
      inner_dims_se
      common_dim
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
      map_lam
      red_lam
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
  Builder GPU TCTilingKernelInfo
makeKernelInfo host_stuff inner_dims load_stms = do
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
  let (redomap_slices, base_arrs) = unzip $ map getArraysFromLoadStm load_stms
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
  -- First, for each dimension in each array, find the index in inner_dims of
  -- this dimension. These indices can then be used to extract the corresponding
  -- tblock offsets and tile dims, which will be ordered by the inner
  -- dimensions. Note that for each array, the indices computed here are the
  -- same as those returned by `variantInnerDimsPerArr`, but in different order.
  let var_inds_per_arr = map (map (flip elemIndex inner_dims)) dims_per_arr

  -- Then, for each dimension of each array, extract the TR tile and tblock
  -- offset corresponding to this dimension.
  -- For the tile dims, we insert tile_seq in the index of the array dim not
  -- represented in inner_dims.
  let tile_dims_per_arr = map (gather_ tiles_TR tile_seq) var_inds_per_arr
  let tb_offsets_per_arr = map (gather tb_offsets) $ map catMaybes var_inds_per_arr

  let arrs_params =
        map
          ( \(redomap_slice, dim, tile_dim, tb_offset, var_gtids, load_stm) ->
              ArrInfo redomap_slice dim tile_dim tb_offset var_gtids load_stm
          )
          $ zip6 redomap_slices dims_per_arr tile_dims_per_arr tb_offsets_per_arr var_inds_per_arr load_stms

  pure $ KernelInfo tb_offsets arrs_params
  where
    getArraysFromLoadStm :: Stm GPU -> (VName, VName)
    getArraysFromLoadStm (Let pat _ (BasicOp (Index arr _))) =
      (patElemName $ head $ patElems pat, arr)
    getArraysFromLoadStm stm =
      error $
        "getArraysFromLoadStm error: expected a BasicOp Index stm, got: "
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
