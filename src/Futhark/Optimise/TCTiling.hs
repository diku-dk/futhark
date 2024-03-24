{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.TCTiling (doTCTiling) where

import Control.Monad
import Data.Char
import Data.List qualified as L
import Data.Map.Strict qualified as M
-- import Data.Maybe
-- import Data.Sequence qualified as Seq
import Futhark.IR.GPU
-- import Futhark.IR.Mem.LMAD qualified as LMAD
-- import Futhark.MonadFreshNames

import Futhark.Optimise.BlkRegTiling (matchCodeStreamCode)
import Futhark.Optimise.TileLoops.Shared
import Futhark.Tools
import Futhark.Util (splitFromEnd)

-- processIndirections ::
--   Names -> -- input arrays to redomap
--   Names -> -- variables on which the result of redomap depends on.
--   Maybe (Stms GPU, M.Map VName (Stm GPU)) ->
--   Stm GPU ->
--   Maybe (Stms GPU, M.Map VName (Stm GPU))
-- processIndirections _ _ _ _ = Nothing

se1 :: SubExp
se1 = intConst Int64 1

doTCTiling :: Env -> Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
doTCTiling _env _orig_stm@(Let pat aux (Op (SegOp (SegMap SegThread {} seg_space ts old_kbody))))
  | KernelBody () kstms [Returns ResultMaySimplify certs (Var _res_name)] <- old_kbody,

    -- we don't want to tile the kernel if it is going to have expensive
    -- boundary checks.
    -- TODO: why, though? what else can we do?
    certs == mempty,

    -- the kernel should have exactly one primtyped result.
    [res_type] <- ts,
    primType res_type,

    all_dims <- unSegSpace seg_space,
    -- TODO: replace dummy `6`.
    (rem_outer_gtids_dims, inner_gtids_dims) <- splitFromEnd 6 all_dims,
    (gtids, inner_dims) <- unzip inner_gtids_dims,

    -- check that the kernel fits the pattern:
    -- some code1; one Screma SOAC; some code2,
    -- where code2 may contain additional Scremas but code1 may not.
    -- TODO: do we assume only one Screma in kstms? does it even matter?
    Just (_code1, screma_stmt@(Let pat_redomap _ (Op _)), _code2) <-
      matchCodeStreamCode kstms,

    -- checks that the Screma SOAC is actually a redomap and normalizes it
    Just (common_dim, arrs, (_, red_lam, red_nes, map_lam)) <- isTileableRedomap screma_stmt,
    -- check that exactly two 1D arrays are streamed through redomap,
    -- and the result of redomap is one scalar

    -- TODO: Cosmin's implementation mentioned rearranging the below couple of
    --       conditions. better look into this.
    length arrs == 2,
    [red_ne] <- red_nes,
    -- [map_t1t, map_t2t] <- map paramDec $ lambdaParams map_lam,
    -- [red_t1, _] <- map paramDec $ lambdaParams red_lam,
    -- primType map_t1t && primType map_t2t && primType red_t1,
    -- _map_t1_0 <- elemType map_t1t,
    -- _map_t2_0 <- elemType map_t2t,

    -- assert that all redomap arrays are variant to some, but not all innermost
    -- dimensions of the kernel.
    -- TODO: find out whether/where/how to use the returned information.
    initial_variance <- M.map mempty $ scopeOfSegSpace seg_space,
    variance <- varianceInStms initial_variance kstms,
    Just _variant_inner_dims_per_arr <- variantInnerDimsPerArr variance arrs gtids,

    -- TODO: all of the below guards are lifted from Cosmin's code.
    --       find out which of them are relevant here, and whether they need to
    --       be changed/generalized.
    --
    -- get the variables on which the first result of redomap depends on
    -- [_redomap_orig_res] <- patNames pat_redomap,
    -- Just _red_res_variance <- M.lookup _redomap_orig_res variance, -- variance of the reduce result

    -- we furthermore check that code1 is only formed by
    -- 1. statements that slice some globally-declared arrays
    --    to produce the input for the redomap, and
    -- 2. potentially some statements on which the redomap
    --    is independent; these are recorded in `code2''`

    -- Just (code2'', tab_inv_stm) <-
    --   foldl
    --     (processIndirections (namesFromList arrs) red_res_variance)
    --     (Just (Seq.empty, M.empty))
    --     code1,

    -- identify load_A, load_B
    -- tmp_stms <- mapMaybe (`M.lookup` tab_inv_stm) arrs,
    -- length tmp_stms == length arrs

    True = do
      myDebugM $
        "Matches TCTiling! :)\n"
          ++ "kstms:\n"
          ++ prettyString kstms

      (new_kernel, host_stms) <- runBuilder $ do
        ( tiles_T,
          tiles_R,
          _tiles_TR,
          _tile_common_dim,
          grid_dims,
          grid_size_flat,
          tblock_size,
          tbids,
          tbid_flat
          ) <-
          makeTileSizesAndKernelParams inner_dims common_dim

        (ret_seggroup, stms_seggroup) <- runBuilder $ do
          let segthd_lvl = SegThreadInBlock (SegNoVirtFull (SegSeqDims []))
          reg_tiles <- fmap head $ segMapND "reg_tiles" segthd_lvl ResultPrivate tiles_T $ const $ do
            reg_tile_init <- scratch "reg_tile_init" (elemType res_type) tiles_R

            reg_tile <- forLoopNest tiles_R [reg_tile_init] $ \loop_inds [merge] ->
              (: []) <$> update "reg_tile" merge loop_inds red_ne
            pure [varRes $ reg_tile]

          -- segMapND "foo" (SegThreadInBlock (SegNoVirtFull (SegSeqDims [])))
          -- forM_ (zip gtids [0..]) $ \(gtid, i) -> do
          --   myDebugM $ "gtid: " ++ prettyString gtid
          --   letBindNames [gtid] =<< toExp (intConst Int64 i)
          -- pure [RegTileReturns mempty [] foo]

          -- is_in_bounds <-
          --   letSubExp "is_in_bounds"
          --     =<< toExp
          --       ( foldr1 (.&&.) $
          --           map (\(gtid, dim_len) -> le64 gtid .<. pe64 dim_len) inner_dims
          --       )
          let regtile_ret_dims =
                map ((,se1,se1) . snd) rem_outer_gtids_dims
                  ++ zip3 inner_dims tiles_T tiles_R
          pure [RegTileReturns mempty regtile_ret_dims reg_tiles]

        let grid = KernelGrid (Count grid_size_flat) (Count tblock_size)
            level' = SegBlock SegNoVirt (Just grid)
            space' = SegSpace tbid_flat (rem_outer_gtids_dims ++ zip tbids grid_dims)
            kbody' = KernelBody () stms_seggroup ret_seggroup
        pure $ Let pat aux $ Op $ SegOp $ SegMap level' space' ts kbody'
      pure $ Just (host_stms, new_kernel)
doTCTiling _seg_space _kstms = pure Nothing

-- | Given a variance table for, a list of array names, and a list of inner dims
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
  Maybe [[Int]]
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
    all (`elem` concat arr_variances) [0 .. n_dims - 1],
    -- assert no overlap in variance between arrays.
    -- TODO: is this check necessary?
    allUnique $ concat arr_variances =
      pure arr_variances
  | otherwise = Nothing
  where
    n_dims = length inner_dims
    variantInnerDimsForArr arr =
      let arr_variance = M.findWithDefault mempty arr variance
       in L.findIndices (`nameIn` arr_variance) inner_dims
    allUnique (x : xs) = x `notElem` xs && allUnique xs
    allUnique _ = True

makeTileSizesAndKernelParams ::
  [SubExp] ->
  SubExp ->
  Builder
    GPU
    ( [SubExp], -- T tiles
      [SubExp], -- R tiles
      [SubExp], -- TR tiles
      SubExp, -- common dim (sequential) tile
      [SubExp], -- grid dims
      SubExp, -- flat grid size
      SubExp, -- tblock size
      [VName], -- tblock indices
      VName -- flat tblock index
    )
makeTileSizesAndKernelParams inner_dims common_dim = do
  -- various names.
  tile_common_dim_vn <- newVName "T_common_dim"
  tile_T_vns <- newPrefixedVNames "T_" inner_dim_names
  tile_R_vns <- newPrefixedVNames "R_" inner_dim_names
  tile_TR_vns <- newPrefixedVNames "TR_" inner_dim_names
  grid_dim_vns <- newPrefixedVNames "grid_dim_" inner_dim_names
  tbids <- newPrefixedVNames "tbid_" inner_dim_names
  tbid_flat <- newVName "tbid_flat"

  -- tile sizes.
  tiles_T <- mapM (getTileSE SizeTile) tile_T_vns
  tiles_R <- mapM (getTileSE SizeRegTile) tile_R_vns
  tiles_TR <-
    zipWithM (\t r -> toExp $ pe64 t * pe64 r) tiles_T tiles_R
      >>= zipWithM letSubExp (map baseString tile_TR_vns)
  tile_common_dim <- getTileSE SizeTile tile_common_dim_vn

  -- grid and tblock stuff.
  grid_dims <-
    zipWithM ceilDiv inner_dims tiles_TR
      >>= zipWithM letSubExp (map baseString grid_dim_vns)
  grid_size_flat <- letSubExp "grid_size_flat" =<< toExp (product $ map pe64 grid_dims)
  tblock_size <- letSubExp "tblock_size" =<< toExp (product $ map pe64 tiles_T)

  pure
    ( tiles_T,
      tiles_R,
      tiles_TR,
      tile_common_dim,
      grid_dims,
      grid_size_flat,
      tblock_size,
      tbids,
      tbid_flat
    )
  where
    inner_dim_names =
      case mapM getNameStrFor inner_dims of
        Just dim_names -> dim_names
        _ -> map show $ indices inner_dims
      where
        getNameStrFor (Var v) = Just $ filter isAscii $ baseString v
        getNameStrFor _ = Nothing

    newPrefixedVNames s = mapM (newVName . (s ++))
    getTileSE tile_type v =
      letSubExp (baseString v) $ Op $ SizeOp $ GetSize (baseName v) tile_type
