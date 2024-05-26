{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.TCTiling (doTCTiling) where

import Control.Monad
import Data.Char
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Futhark.Analysis.PrimExp
import Futhark.IR.GPU
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Optimise.BlkRegTiling (matchCodeStreamCode, processIndirections)
import Futhark.Optimise.TileLoops.Shared
import Futhark.Tools
import Futhark.Transform.Rename

forM2 :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m [c]
forM2 xs ys f = zipWithM f xs ys

forM3 :: Monad m => [a] -> [b] -> [c] -> (a -> b -> c -> m d) -> m [d]
forM3 xs ys zs f = forM (zip3 xs ys zs) (\(a, b, c) -> f a b c)

se0, se1, se2 :: SubExp
se0 = intConst Int64 0
se1 = intConst Int64 1
se2 = intConst Int64 2

seglvl_thd :: SegLevel
seglvl_thd = SegThreadInBlock $ SegNoVirtFull $ SegSeqDims []

reductionLoopBody ::
  TCEnv ->
  VName ->
  VName ->
  [VName] ->
  Bool ->
  Builder GPU [VName]
reductionLoopBody tc_env qq0 reg_tiles_in shr_arrs_in is_prologue = do
  qq <- letExp "qq" =<< toExp (le64 qq0 * pe64 tile_Q)

  redomap_inputs_shr <- forM2 shr_arrs_in arr_infos $ copyGlb2Shr qq
  reg_tiles_out <- accumulateRegTile qq redomap_inputs_shr
  pure $ reg_tiles_out : redomap_inputs_shr
  where
    arr_infos = arrsInfo tc_env
    kernel_params = kernelParams tc_env
    tile_Q = tileQ kernel_params
    tiles_T = tilesT kernel_params
    tiles_R = tilesR kernel_params
    common_dim = commonDim kernel_params
    tblock_size_flat = tblockSizeFlat kernel_params

    copyGlb2Shr :: VName -> VName -> ArrInfo -> Builder GPU VName
    copyGlb2Shr qq shr_arr arr_info = do
      -- Setup parameters for the WithAcc.
      cert_p <- newParam "cert_p" $ Prim Unit
      t <- stripArray (shapeRank smem_shape) <$> lookupType shr_arr
      acc_p <-
        newParam (baseString shr_arr) $
          Acc (paramName cert_p) smem_shape [t] NoUniqueness

      tile_size_flat <-
        letSubExp "tile_size_flat"
          =<< toExp (product $ map pe64 tile_dims)
      -- The strategy is to flatten the tblock and then unflatten it to fit the
      -- dimensions of the array in shared memory, using a virtualization loop
      -- in case the tile is larger than the tblock, and a boundary guard for
      -- the converse. This is easily achieved using SegVirt, but whereas
      -- SegVirt wraps the entire loop body in an `if (i < tile_size_flat)`
      -- guard, we want that guard only on the write to shared memory. Hence
      -- we must manually build the virtualization loop, which unfortunately
      -- bloats the code a bit here.
      iters <- letSubExp "virt_iters" =<< ceilDiv tile_size_flat tblock_size_flat
      lam <- mkLambda [cert_p, acc_p] $ do
        fmap (varsRes . (: [])) $
          segMapND_ "foo" seglvl_thd ResultNoSimplify [tile_size_flat] $ \[ltid] ->
            fmap (varsRes . (: [])) $
              forLoop_ iters (paramName acc_p) $ \i0 acc_merge -> do
                i <- letExp "flat_virttid" =<< toExp (le64 i0 * pe64 tblock_size_flat + le64 ltid)
                copyLoopBody tile_size_flat acc_merge i

      letExp (baseString shr_arr) $
        WithAcc [(smem_shape, [shr_arr], Nothing)] lam
      where
        smem_strides = smemStrides arr_info
        copyLoopBody :: SubExp -> VName -> VName -> Builder GPU VName
        copyLoopBody tile_size_flat acc i = do
          unflat_inds <-
            forM (unflattenIndex tile_dims' $ le64 i) $
              letSubExp "unflat_ind" <=< toExp

          shr_ind_flat <-
            letTupExp' "shr_ind_flat" <=< toExp . sum $
              zipWith
                (\ind s -> pe64 ind * pe64 s)
                unflat_inds
                smem_strides

          -- The shared mem indices are simply the unflattened indices, while
          -- the global mem indices need to have tblock offsets added onto them.
          glb_inds <-
            forM2 tblock_offsets unflat_inds $ \tb_offset ind ->
              letExp "glb_ind" =<< toExp (pe64 tb_offset + pe64 ind)

          -- Perform a boundary check and read from the global mem array!
          in_bounds <-
            letExp "in_bounds"
              =<< toExp
                ( foldr (.&&.) true $
                    zipWith
                      (\ind dim -> le64 ind .<. pe64 dim)
                      glb_inds
                      base_arr_dims
                )

          -- We initially permuted base array dimensions to match the actual
          -- layout in memory, such that we were able to map it to the thread
          -- block. However, we must make to sure re-permute it before executing
          -- the read, since the `index` function assumes the indices are given
          -- in order of the *rearranged* array. Insane, I know.
          let glb_inds_perm = arrPerm arr_info glb_inds
          glb_elem <-
            letExp (baseString base_arr)
              =<< eIf
                (toExp in_bounds)
                ( index "glb_elem" base_arr glb_inds_perm
                    >>= resultBodyM . (: []) . Var
                )
                -- Here, we simply insert a zero (or zero-like value) into
                -- smem whenever we are outside bounds. However, this only
                -- succeeds in certain cases, unless we explicitly handle
                -- residual tiles in an epilogue (which we do).
                -- See note [SmemZeroPaddingOnGlobalMemOOB].
                (eBody [eBlank $ Prim $ smemElemType arr_info])

          -- Finally, update shared mem array accumulator.
          letExp "acc_out"
            =<< eIf
              (toExp $ le64 i .<. pe64 tile_size_flat)
              ( resultBodyM . map Var <=< letTupExp "acc_updated" . BasicOp $
                  UpdateAcc
                    Unsafe
                    acc
                    shr_ind_flat
                    [Var glb_elem]
              )
              (resultBodyM [Var acc])

        smem_shape = Shape [smemSizeFlat arr_info]
        tile_dims = tileDims arr_info
        tile_dims' = map pe64 tile_dims
        tblock_offsets = arrGather_ arr_info (tblockOffsets tc_env) (Var qq)
        base_arr_dims = baseArrDims arr_info
        base_arr = baseArr arr_info

    accumulateRegTile :: VName -> [VName] -> Builder GPU VName
    accumulateRegTile qq redomap_inputs_shr =
      segMapND_ "reg_tiles_out" seglvl_thd ResultPrivate tiles_T $ \ltids -> do
        reg_tile_in <- index "reg_tile_in" reg_tiles_in ltids
        fmap ((: []) . varRes) $
          forLoop_ tile_Q reg_tile_in $ \q reg_tile_in' ->
            letExp "reg_tile_acc"
              =<< eIf
                ( toExp $
                    -- if we are in the prologue, accumulate unconditionally!
                    fromBool is_prologue
                      .||. le64 qq + le64 q .<. pe64 common_dim
                )
                ( resultBody . (: []) . Var
                    <$> accumulateRegTileInnerLoopNest ltids q reg_tile_in'
                )
                (resultBodyM [Var reg_tile_in'])
      where
        accumulateRegTileInnerLoopNest :: [VName] -> VName -> VName -> Builder GPU VName
        accumulateRegTileInnerLoopNest ltids q reg_tile_in =
          forLoopNest_ tiles_R reg_tile_in $ \loop_inds reg_tile_merge -> do
            -- Compute lists of indices for each redomap operand. For each
            -- dimension, we need an index of the form `ltid * reg_tile +
            -- loop_ind`, so for the reduction dimension, use a dummy ltid and
            -- reg_tile.
            dummy_ltid <- letExp "dummy_ltid_q" =<< toExp se0
            let dummy_regtile = se1
            shr_inds_flat <- forM arr_infos $ \arr -> do
              let ltids' = arrGather_ arr ltids dummy_ltid
              let tiles_R' = arrGather_ arr tiles_R dummy_regtile
              let loop_inds' = arrGather_ arr loop_inds q
              inds <-
                forM3 ltids' tiles_R' loop_inds' $ \ltid tile loop_ind ->
                  letSubExp "shr_ind" =<< toExp (le64 ltid * pe64 tile + le64 loop_ind)
              letTupExp "shr_ind_flat" <=< toExp . sum $
                zipWith
                  (\ind s -> le64 ind * le64 s)
                  inds
                  (smemStrides arr)

            -- Compute map and reduction results and update the register tile.
            map_f <- renameLambda $ mapLam tc_env
            red_op <- renameLambda $ redLam tc_env

            map_operands <- forM2 redomap_inputs_shr shr_inds_flat $ \arr inds ->
              eSubExp . Var <$> index (baseString arr ++ "_elem") arr inds
            map_res <- eLambda map_f map_operands

            acc <- eSubExp . Var <$> index "acc" reg_tile_merge loop_inds
            red_res <- eLambda red_op $ acc : map (eSubExp . resSubExp) map_res

            update "res" reg_tile_merge loop_inds $ resSubExp $ head red_res

doTCTiling :: Env -> Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
doTCTiling env (Let pat aux (Op (SegOp (SegMap SegThread {} seg_space ts old_kbody))))
  | KernelBody () kstms [Returns ResultMaySimplify certs (Var _res_name)] <- old_kbody,
    -- we don't want to tile the kernel if it is going to have expensive
    -- boundary checks.
    -- TODO: why, though? what else do we do in this case?
    certs == mempty,
    -- the kernel should have exactly one primtyped result.
    [res_t] <- ts,
    primType res_t,
    all_gtids_dims <- unSegSpace seg_space,
    -- TODO: for now, I test only source programs with no outer parallel
    --       dimensions, ie. all dims in the segspace pertain to the
    --       contraction.
    --       find out how to reliably extract the inner dims of the segspace.
    --       perhaps inner dims are all those onto which the kernel result is
    --       variant and at least (or exactly) one redomap array is variant?
    (rem_outer_gtids_dims, inner_gtids_dims) <- ([], all_gtids_dims), -- TODO: placeholder.
    (gtids, inner_dims) <- unzip inner_gtids_dims,
    -- check that the kernel fits the pattern:
    -- some code1; one Screma SOAC; some code2,
    -- where code2 may contain additional Scremas but code1 may not.
    -- TODO: do we assume only one Screma in kstms? does it even matter?
    Just (code1, screma_stmt@(Let pat_redomap _ (Op _)), code2') <-
      matchCodeStreamCode kstms,
    -- checks that the Screma SOAC is actually a redomap and normalizes it
    Just (common_dim, redomap_arrs, (_is_comm, red_lam, red_nes, map_lam)) <-
      isTileableRedomap screma_stmt,
    -- TODO: Cosmin's implementation mentioned rearranging the below couple of
    --       conditions. better look into this.
    -- check that exactly two 1D arrays are streamed through redomap,
    -- and the result of redomap is one scalar
    length redomap_arrs == 2,
    [red_ne] <- red_nes,
    [red_t, _] <- map paramDec $ lambdaParams red_lam,
    primType red_t,
    map_ts@[_, _] <- map paramDec $ lambdaParams map_lam,
    all primType map_ts,
    initial_variance <- M.map mempty $ scopeOfSegSpace seg_space,
    variance <- varianceInStms initial_variance kstms,
    -- assert that all redomap arrays are variant to some, but not all innermost
    -- dimensions of the kernel.
    -- TODO: find out whether/where/how to use the returned information.
    Just _var_inds_per_arr <- variantDimsPerArr variance redomap_arrs gtids,
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
    Just (code2'', table_load_stms) <-
      processIndirections code1 redomap_arrs red_res_variance,
    -- extract the stms loading slices from redomap arrays and check that there
    -- is one such stm for each redomap array.
    Just load_stms <- mapM (`M.lookup` table_load_stms) redomap_arrs = do
      let _code2 = code2' <> code2''
      let map_prim_ts = map elemType map_ts

      -- TODO: for now, we manually disable the prologue/epilogue treatment when
      -- suitable. However, ideally this would be done automatically, or not at
      -- all, if there turns out to be a better method, or if the epilogue is
      -- not sufficiently detrimental to performance that it is necessary.
      let use_epilogue = not $ AttrName "no_epilogue" `inAttrs` stmAuxAttrs aux

      (new_kernel, host_stms) <- runBuilder $ do
        kernel_params@( TCKernelParams
                          _gtids
                          _inner_dims
                          _common_dim
                          _inner_dim_names
                          tiles_T
                          tiles_R
                          _tiles_TR
                          tile_Q
                          grid_dims
                          grid_size_flat
                          _tblock_dims
                          tblock_size_flat
                          tbids
                          tbid_flat
                        ) <-
          makeTCKernelParams gtids inner_dims common_dim

        (ret_seggroup, stms_seggroup) <- runBuilder $ do
          tc_env <- makeTCEnv env kernel_params load_stms map_lam red_lam map_prim_ts

          -- Zero-initialize register tile.
          reg_tiles_init <- segMapND_ "reg_tiles" seglvl_thd ResultPrivate tiles_T $ \_ -> do
            reg_tile_init <- scratch "reg_tile_init" (elemType res_t) tiles_R
            css <- forLoopNest_ tiles_R reg_tile_init $ \loop_inds merge ->
              update "reg_tile" merge loop_inds red_ne
            pure [varRes css]

          -- Declare shared memory arrays.
          shr_arrs_init <-
            forM (arrsInfo tc_env) $ \arr ->
              scratch
                ("shr_" ++ baseString (baseArr arr))
                (smemElemType arr)
                [smemSizeFlat arr]

          ~(reg_tiles_res : _) <-
            case use_epilogue of
              True -> do
                myDebugM "Compiling TC expression WITH epilogue"
                num_full_Q_tiles <-
                  letExp "num_full_Q_tiles" . BasicOp $
                    BinOp (SQuot Int64 Unsafe) common_dim tile_Q
                residual_input <-
                  letExp "residual_input" . BasicOp $
                    BinOp (SRem Int64 Unsafe) common_dim tile_Q

                ~prologue_res@(reg_tiles' : shr_arrs') <-
                  forLoop (Var num_full_Q_tiles) (reg_tiles_init : shr_arrs_init) $
                    \qq0 (reg_tiles_merge : shr_arrs_merge) ->
                      reductionLoopBody tc_env qq0 reg_tiles_merge shr_arrs_merge True

                letTupExp "reduction_res"
                  =<< eIf
                    (toExp $ le64 residual_input .==. 0)
                    (resultBodyM $ map Var prologue_res)
                    ( resultBody . map Var
                        <$> reductionLoopBody tc_env num_full_Q_tiles reg_tiles' shr_arrs' False
                    )
              _ -> do
                myDebugM "Compiling TC expression WITHOUT epilogue"
                num_q_tiles <- letSubExp "num_Q_tiles" =<< ceilDiv common_dim tile_Q
                forLoop num_q_tiles (reg_tiles_init : shr_arrs_init) $
                  \qq0 (reg_tiles_merge : shr_arrs_merge) ->
                    reductionLoopBody tc_env qq0 reg_tiles_merge shr_arrs_merge True

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
doTCTiling _seg_space _kstms = pure Nothing

-- | Given a variance table, a list of array names, and a list of inner dims
-- (actually, the list of gtids for said inner dims); asserts that each array is
-- variant to at least 1 and not all inner dims, and that at least one array is
-- variant to each inner dim. If these assertions hold; returns list of indices
-- of variant dims for each array.
-- TODO: Dimensions on which all redomap arrays are variant should be
-- interchanged outwards.
variantDimsPerArr ::
  VarianceTable ->
  [VName] ->
  [VName] ->
  Maybe [[Int]]
variantDimsPerArr variance arrs gtids = do
  let var_inds_per_arr = map variantInnerDimsForArr arrs
  let var_gtids_per_arr = map (gather gtids) var_inds_per_arr

  -- Interchange those dimensions of the segspace on which all redomap arrays
  -- are variant outwards.
  let (outer_dims, tc_dims) =
        L.partition
          -- Check that given dim is in var_dims of all arrays.
          (\dim -> all (elem dim) var_gtids_per_arr)
          gtids
  let segspace_dims' = outer_dims ++ tc_dims
  let segspace_perm = gtids `isPermutationOf` segspace_dims'

  -- myDebugM $
  --   "variantDimsPerArr\nsegspace_dims:\n"
  --     ++ prettyString gtids
  --     ++ "\nsegspace_dims':\n"
  --     ++ show segspace_dims'
  --     ++ "\nperm:\n"
  --     ++ show segspace_perm
  --     ++ "\nvar_inds_per_arr:\n"
  --     ++ show var_inds_per_arr

  -- assert that all arrays are variant to some, but not all dims.
  -- TODO: is below check sufficient to check this assertion?
  --       perhaps this assertion should be (or already is) made elsewhere.
  guard $ all ((`elem` [1 .. n_dims - 1]) . length) var_inds_per_arr

  -- for each dim, assert that at least one array is variant to this dim.
  -- TODO: is there a better, more correct, or safer way to assert this?
  -- Actually, I think this can safely be assumed to already hold, due to these
  -- parallel dimensions already having been interchanged outwards in an earlier
  -- compiler stage, but I might be wrong on this.
  guard $ all (`elem` concat var_gtids_per_arr) gtids

  -- assert no overlap in variance between arrays.
  -- TODO: is this check necessary or even desired? for exactly 2 redomap
  -- arrays, overlap in variance means all redomap arrays are variant to the
  -- given parallel dimension, and thus it would have been interchanged outwards
  -- (given the above TODO is implemented).
  -- guard $ allUnique $ concat var_inds_per_arr

  pure var_inds_per_arr
  where
    n_dims = length gtids
    variantInnerDimsForArr arr =
      let arr_variance = M.findWithDefault mempty arr variance
       in L.findIndices (`nameIn` arr_variance) gtids
    -- allUnique (x : xs) = x `notElem` xs && allUnique xs
    -- allUnique _ = True

-- | All the various kernel parameters and related information we need to
-- declare and/or compute in host code.
data TCKernelParams = TCKernelParams
  { -- Gtids and sizes of those dimensions of the inner segspace which we are tiling.
    innerGtids :: [VName],
    innerDims :: [SubExp],
    commonDim :: SubExp,
    -- Not strictly necessary, but nice to have for consistent names throughout
    -- the generated code.
    innerDimNames :: [String],
    -- T, R, and TR tiles for each inner dimension.
    tilesT :: [SubExp],
    tilesR :: [SubExp],
    tilesTR :: [SubExp],
    -- Tile size for the sequential (reduction) dimension.
    tileQ :: SubExp,
    -- Grid and tblock parameters.
    gridDims :: [SubExp],
    gridSizeFlat :: SubExp,
    tblockDims :: [SubExp],
    tblockSizeFlat :: SubExp,
    -- VNames for the tblock id's.
    tbidVns :: [VName],
    tbidFlatVn :: VName
  }
  deriving (Show)

-- | All of the information needed for code generation in kernel scope. Also
-- carries the kernel parameters declared in host scope.
data TCEnv = TCEnv
  { kernelParams :: TCKernelParams,
    -- Block offset for each dimension in the result.
    tblockOffsets :: [SubExp],
    -- Lambdas for the map function and reduction operators for the contraction.
    mapLam :: Lambda GPU,
    redLam :: Lambda GPU,
    -- For each reduction array, the information needed to handle this
    -- particular array during code generation.
    arrsInfo :: [ArrInfo]
  }
  deriving (Show)

-- | All the information needed to handle a given operand array.
-- TODO: give a proper name to this one.
data ArrInfo = ArrInfo
  { baseArr :: VName,
    baseArrDims :: [SubExp],
    arrLoadStm :: Stm GPU,
    lmadPerm :: [Int],
    varDimInds :: [Maybe Int],
    tileDims :: [SubExp],
    smemSizeFlat :: SubExp,
    smemStrides :: [SubExp],
    smemElemType :: PrimType
  }
  deriving (Show)

gather :: [a] -> [Int] -> [a]
gather xs = map (xs !!) . filter (`elem` indices xs)

gather_ :: [a] -> a -> [Maybe Int] -> [a]
gather_ xs x = map (maybe x (xs !!) . checkIdx)
  where
    checkIdx i
      | Just j <- i, j `elem` indices xs = i
      | otherwise = Nothing

arrGather_ :: ArrInfo -> [a] -> a -> [a]
arrGather_ info src x = gather_ src x $ varDimInds info

arrPerm :: ArrInfo -> [a] -> [a]
arrPerm info xs = gather xs $ lmadPerm info

makeTCKernelParams ::
  [VName] ->
  [SubExp] ->
  SubExp ->
  Builder GPU TCKernelParams
makeTCKernelParams gtids inner_dims_se common_dim_se = do
  -- various names.
  tile_common_dim_vn <- newVName $ "T_" ++ common_dim_name
  tile_T_vns <- mapM (newVName . ("T_" ++)) inner_dim_names
  tile_R_vns <- mapM (newVName . ("R_" ++)) inner_dim_names
  tbids <- mapM (newVName . ("tbid_" ++)) inner_dim_names
  tbid_flat <- newVName "tbid_flat"

  -- tile sizes.
  tile_Q <- letTileSE SizeTile tile_common_dim_vn
  tiles_T <- mapM (letTileSE SizeTile) tile_T_vns
  tiles_R <- mapM (letTileSE SizeRegTile) tile_R_vns
  tiles_TR <-
    zipWithM (\t r -> toExp $ pe64 t * pe64 r) tiles_T tiles_R
      >>= zipWithM letSubExp (map ("TR_" ++) inner_dim_names)

  -- grid and tblock stuff.
  grid_dims <-
    zipWithM ceilDiv inner_dims_se tiles_TR
      >>= zipWithM letSubExp (map ("grid_dim_" ++) inner_dim_names)
  grid_size_flat <-
    letSubExp "grid_size_flat"
      =<< toExp (product $ map pe64 grid_dims)

  let tblock_dims = tiles_T
  tblock_size_flat <-
    letSubExp "tblock_size_flat"
      =<< toExp (product $ map pe64 tiles_T)

  pure $
    TCKernelParams
      gtids
      inner_dims_se
      common_dim_se
      inner_dim_names
      tiles_T
      tiles_R
      tiles_TR
      tile_Q
      grid_dims
      grid_size_flat
      tblock_dims
      tblock_size_flat
      tbids
      tbid_flat
  where
    inner_dim_names
      | Just name_strs <- mapM getNameStrFor inner_dims_se = name_strs
      | otherwise = map show $ indices inner_dims_se
    common_dim_name = maybe "Q" id $ getNameStrFor common_dim_se

    getNameStrFor (Var v) = Just $ filter isAscii $ baseString v
    getNameStrFor _ = Nothing

    letTileSE tile_type v =
      letSubExp (baseString v) $ Op $ SizeOp $ GetSize (baseName v) tile_type


data FlatPrimExp = Product [FlatPrimExp] | OpaquePrimExp (PrimExp VName)
  deriving (Eq, Ord)

-- TODO: should rewrite this to not use L.permutations, since it is O(n!) for
-- arrays of `n` dims. For n <= 6 dims this is fine-ish, but for ~7 and up it
-- quickly becomes a problem. Can also find the correct permutation using
-- iterative search in quadratic-ish time.
findLMADPerm :: Env -> VName -> Builder GPU [Int]
findLMADPerm (_, ixfn_env) arr = do
  case maybe_lmad_perm of
    Just res -> pure res
    _ -> indices . arrayDims <$> lookupType arr
  where
    maybe_lmad_perm = do
      lmad <- LMAD.dims <$> M.lookup arr ixfn_env
      let shape = map (untyped . LMAD.ldShape) lmad
          strides0 = map (toFlatPrimExp . untyped . LMAD.ldStride) lmad
      -- Test each permutation against the known strides; pick first succeeding.
      msum $ map (isPermutationOf strides0 . strides) $ L.permutations shape

    strides = map toFlatPrimExp . (++ [val1]) . scanr1 binopMul . tail
    binopMul = BinOpExp $ Mul Int64 OverflowUndef
    val1 = ValueExp $ IntValue $ Int64Value 1

    -- Flattens a nested PrimExp (if that PrimExp happens to represent a simple
    -- product) to a [FlatPrimExp], which can then be sorted to check for
    -- equality. Used to more reliably check equality between LMAD strides.
    -- See note [FlattenPrimExps].
    toFlatPrimExp :: PrimExp VName -> FlatPrimExp
    toFlatPrimExp = Product . L.sort . flattenProducts . flattenMulOps
      where
        flattenMulOps (BinOpExp Mul {} e1 e2) = Product $ map toFlatPrimExp [e1, e2]
        flattenMulOps e = OpaquePrimExp e

        flattenProducts (Product es) = concatMap flattenProducts es
        flattenProducts e = [e]

makeTCEnv ::
  Env ->
  TCKernelParams ->
  [Stm GPU] ->
  Lambda GPU ->
  Lambda GPU ->
  [PrimType] ->
  Builder GPU TCEnv
makeTCEnv env kernel_params load_stms map_lam red_lam _map_ts = do

  tblock_offsets <-
    forM3 inner_dim_names tbids tiles_TR $
      \dim_name tbid tile_TR ->
        letSubExp ("tb_offset_" ++ dim_name)
          =<< toExp (le64 tbid * pe64 tile_TR)

  fmap (TCEnv kernel_params tblock_offsets map_lam red_lam)
    $ forM
      load_stms
    $ \load_stm -> do
      -- TODO: should probably gather all of the comments made here in a note.

      -- We need to extract the dimensions of each input array, and
      -- unfortunately the Redomap passed into this module only indirectly
      -- carries this information, as part of the kernel stms loading each
      -- redomap input slice. It'd be more convenient if the Redomap carried not
      -- only the VNames of its operands slices, but also the base arrays (if
      -- any) whence each slice comes, or at least the layout thereof.
      --
      -- In any case, knowledge of the actual layout of a given input array is
      -- necessary in order to correctly map the global-to-smem tile copy to
      -- the tblock dimensions (to obtain coalesced access on both smem
      -- inputs), as well as to generate the boundary guard on the read, and to
      -- match tile sizes to each input array, since these are not simply
      -- (M: (Ty, Ry)), (N: (Tx, Rx)), and (U: Tk) as in the 2D case.
      --
      -- Additionally, as it turned out, it was simply more convenient to load
      -- directly from these base arrays, rather than binding computed indices
      -- to gtids and inserting load statements.
      let base_arr = getArrayFromLoadStm load_stm
      arr_t <- lookupType base_arr
      let dims' = arrayDims arr_t
      let smem_elem_type = elemType arr_t

      -- In fact, we need not only the layout for each array, but also the index
      -- in the segspace of each dimension, s.t. later we may extract tblock
      -- offsets, loop variables, and other information associated with this
      -- given smem array. Below mess accomplishes this:
      --
      -- First, for each dimension in the array, determine the index into
      -- inner_dims of this dimension. Note that the indices computed here are
      -- the same as those returned by `variantDimsPerArr` for the given array,
      -- but in different order -- those computed by `variantDimsPerArr` are
      -- ordered by their occurence in the map nest (outermost first), while
      -- these are ordered by the array layout (outermost first).
      --
      -- Then, later in code generation, when we compute e.g. a set of tblock
      -- offsets or a set of loop indices based on the segspace, we can, for
      -- each input array, extract the tblock offsets and loop indices
      -- corresponding to this particular array.
      --
      -- Unfortunately, it is not quite as simple as that. If the array layout
      -- has been rearranged at some point before reaching this module, then we
      -- must reverse-engineer the original array layout from associated LMAD
      -- information. However, since LMADs do not carry permutation information,
      -- we must reverse-engineer it by trying all possible permutations of the
      -- known dimensions for the array (see function `findLMADPerm`). Again,
      -- none of this would be necessary if information on the base array was
      -- available somehow.
      -- If an array has not been rearranged, the identity permutation is
      -- recorded.
      lmad_perm <- findLMADPerm env base_arr
      let inv_lmad_perm = map snd $ L.sort $ zip lmad_perm [0 ..]

      let base_arr_dims = gather dims' inv_lmad_perm
      -- TODO: handle the case where multiple dimensions have the same name.
      let var_inds = map (`L.elemIndex` inner_dims) base_arr_dims

      -- Then, for each dimension of each array, extract the TR tile and
      -- tblock offset corresponding to this dimension. For the tile
      -- dims, we insert tile_Q in the index of the array dim not
      -- represented in inner_dims.
      let tile_dims = gather_ tiles_TR tile_Q var_inds
      let tile_dims_pe = map pe64 tile_dims

      -- Determine whether this array is a candidate for padding. If so, we need
      -- to take this into account in its flat size and the computed strides.
      let innerProducts = scanr (*) 1 . tail
      ~(smem_size_flat', smem_strides') <-

        -- An array is candidate for padding if one of its dimensions is indexed
        -- by the inner thread index UNLESS that dimension happens to also be
        -- innermost on the shared array.
        case Just inner_dim_ind `L.elemIndex` init var_inds of
          Just i -> do
            -- Split on the index at which the inner tiles need padding.
            let (outer_smem_dims, inner_smem_dims) = splitAt (i + 1) tile_dims_pe

            -- We only need padding when the inner size is a multiple of 2, so
            -- the padding term is `1 - (size_pre_pad % 2)`.
            -- TODO: I bind these two because I can't seem to get `rem` to work
            -- with TPrimExps. is there a better way?
            size_pre_pad <- letSubExp "size_pre_pad" =<< toExp (product inner_smem_dims)
            tmp <- letSubExp "tmp" $ BasicOp $ BinOp (SRem Int64 Unsafe) size_pre_pad se2
            pad_term <- letSubExp "pad_term" =<< toExp (1 - pe64 tmp)

            let inner_size_flat = pe64 size_pre_pad + pe64 pad_term

                outer_strides = init $ innerProducts $ outer_smem_dims ++ [inner_size_flat]
                inner_strides = innerProducts inner_smem_dims

                size_flat = product outer_smem_dims * inner_size_flat
            pure (size_flat, outer_strides ++ inner_strides)

          _ -> pure (product tile_dims_pe, innerProducts tile_dims_pe)

      smem_size_flat <- letSubExp "smem_size_flat" =<< toExp smem_size_flat'
      smem_strides <- mapM (letSubExp "smem_stride" <=< toExp) smem_strides'

      pure $
        ArrInfo
          base_arr
          base_arr_dims
          load_stm
          lmad_perm
          var_inds
          tile_dims
          smem_size_flat
          smem_strides
          smem_elem_type

  where
    getArrayFromLoadStm :: Stm GPU -> VName
    getArrayFromLoadStm (Let _ _ (BasicOp (Index arr _))) = arr
    getArrayFromLoadStm stm =
      error $
        "getArrayFromLoadStm error: expected a BasicOp Index stm, got: "
          ++ prettyString stm

    tbids = tbidVns kernel_params
    tiles_TR = tilesTR kernel_params
    tile_Q = tileQ kernel_params
    inner_dim_names = innerDimNames kernel_params
    inner_dims = innerDims kernel_params
    inner_dim_ind = length inner_dims - 1


-- Note [SmemZeroPaddingOnGlobalMemOOB]
-- When copying from global to shared memory, we need to handle out-of-bounds
-- reads from global memory. For the time being, we write a padding value to
-- shared memory. This padding value is a zero (or zero-like) value from the
-- corresponding element type.
--
-- However, this "solution" succeeds only when the following condition holds:
--
-- `f zero_0 _ = f _ zero_1 = red_ne`
--
-- where `f` is the map function; `zero_0` and `zero_1` are the zero-like values
-- for the two given smem array element types; and `red_ne` is the reduce
-- neutral element.
--
-- This is seldom the case in general, however it happens to hold for regular
-- tensor contraction and MM, hence it is used for testing for the time being.
--
-- The simple solution (and the one implemented) is the prologue/epilogue
-- treatment, in which the last iteration of the main reduction loop is unrolled
-- and a boundary guard corresponding to the one we had on global memory is
-- inserted into the register tile accumulation step s.t. we never process
-- garbage values in the reduction (or, at least, they do not affect those
-- entries of the register tile which are eventually written to global mem).
-- However, this will inevitably affect performance, and the difference is more
-- noticeable the less full tiles we have in the common dimension.
--
-- As an example, for regular MM of 2000x2000 matrices with a reduction dim tile
-- of Tk = 32, we will have floor(2000 / 32) = 62 full tiles and 1 partial tile,
-- so here the epilogue is largely amortized by the size of the prologue. But
-- for tensor contractions of higher-rank tensors, each dimension typically is
-- not very large. If we have, say, 30x30x30x30 tensors and a reduction dim tile
-- of Tk = 16, then we will have 1 full tile and 1 partial tile, and now the
-- epilogue dominates.
--
--
-- Another solution is to statically examine whether `zero_0` and `zero_1` exist
-- s.t. the above condition holds, but this analysis can be difficult or
-- impossible, and the values may not even exist.
--
-- Alternatively (on Cosmin's suggestion), the user can manually pass a padding
-- value as an attribute in the Futhark source code. Personally, I think this is
-- very hacky, obscure to most users, error-prone, and an anti-pattern. Also,
-- attributes only support passing integral values, not float values.

-- There is a big TODO in figuring out the best solution to this problem which
-- will also generalize best to arbitrary contractions.

-- Note [FlattenPrimExps]
-- In reverse-engineering LMAD permutations, we need to check equality between
-- LMAD strides. To do so, we in turn need to check equality between product
-- expressions. From commutativity and distributivity of multiplication, we of
-- course expect the two strides lists:
--
-- `[(a * b) * c, b * c, c, 1]`
--
-- and
--
-- `[(c * b) * a, c * b, c, 1]`
--
-- to be equal, since we have (a * b) * c = (c * b) * a, and so on.
--
-- However, the `Eq` instance for `PrimExp`s is not quite so sophisticated, so
-- we need a way to "normalize" product `PrimExp`s. To accomplish this, we
-- "flatten" nested `Mul` expressions and sort them (using the `Ord` instance
-- for `PrimExp`).
--
-- Example: Before flattening, the three `PrimExp` expressions:
--
-- exp1 = `BinOpExp Mul (BinOpExp Mul a b) c`
-- exp2 = `BinOpExp Mul (BinOpExp Mul c b) a`
-- exp3 = `BinOpExp Mul a (BinOpExp Mul b c)`
--
-- where a, b, c are `PrimExp`, would not test equal. However, all three
-- expressions flatten to:
--
-- `Product [OpaquePrimExp a, OpaquePrimExp b, OpaquePrimExp c]`
--
-- and hence we have `(exp1 == exp2) && (exp2 == exp3)`. Yay!
--
--
-- Note that if any of the expressions `a, b, c` are nested non-`Mul` `PrimExp`s
-- where ordering matters, then the flattening and sorting is not reliable.
-- As an example, the two expressions:
--
-- exp4 = `BinOpExp Mul (BinOpExp Add a (BinOpExp Mul b c)) d`
-- exp5 = `BinOpExp Mul (BinOpExp Add (BinOpExp Mul b c) a) d`
--
-- would "flatten" to
--
-- `Product [OpaquePrimExp (BinOpExp Add a (BinOpExp Mul b c)), OpaquePrimExp d]`
-- and
-- `Product [OpaquePrimExp (BinOpExp Add (BinOpExp Mul b c) a), OpaquePrimExp d]`
--
-- respectively, which would not test equal, meaning that in terms of testing
-- equality, this flattening is only reliable for simple product `PrimExp`s.
--
-- Hence it should be considered a proof of concept, and there is a big TODO in
-- making this reliable.
