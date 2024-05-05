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

se0 :: SubExp
se0 = intConst Int64 0

se1 :: SubExp
se1 = intConst Int64 1

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
  qq <- letExp "qq" =<< toExp (le64 qq0 * pe64 tile_seq)

  redomap_inputs_shr <- forM2 shr_arrs_in arr_metas $ copyGlb2Shr qq
  reg_tiles_out <- accumulateRegTile qq redomap_inputs_shr
  pure $ reg_tiles_out : redomap_inputs_shr
  where
    arr_metas = arrsInfo tc_env
    kernel_params = kernelParams tc_env
    tile_seq = tileSeq kernel_params
    tiles_T = tilesT kernel_params
    tiles_R = tilesR kernel_params
    common_dim = commonDim kernel_params

    copyGlb2Shr :: VName -> VName -> ArrMeta -> Builder GPU VName
    copyGlb2Shr qq shr_arr arr_meta = do
      -- Setup parameters for the WithAcc.
      cert_p <- newParam "cert_p" $ Prim Unit
      t <- stripArray (shapeRank shmem_shape) <$> lookupType shr_arr
      acc_p <-
        newParam (baseString shr_arr) $
          Acc (paramName cert_p) shmem_shape [t] NoUniqueness

      -- The strategy is to flatten the tblock and then unflatten it to fit the
      -- dimensions of the array in shared memory, using a virtualization loop
      -- in case the tile is larger than the tblock, and a boundary guard for
      -- the converse. This is easily achieved using SegVirt.
      lam <- mkLambda [cert_p, acc_p] $ do
        fmap varsRes $
          segMapND "foo" (SegThreadInBlock SegVirt) ResultNoSimplify tile_dims $
            copyLoopBody (paramName acc_p)

      letExp (baseString shr_arr) $
        WithAcc [(shmem_shape, [shr_arr], Nothing)] lam
      where
        copyLoopBody :: VName -> [VName] -> Builder GPU Result
        copyLoopBody acc ltids = do

          -- The shared mem indices are simply the unflattened indices, while
          -- the global mem indices need to have tblock offsets added onto them.
          let shr_arr_inds = map Var ltids
          glb_arr_inds <-
            forM2 block_offsets ltids $ \tb_offset ltid -> do
              letExp "glb_ind" =<< toExp (pe64 tb_offset + le64 ltid)

          -- Perform a boundary check and read from the global mem array!
          in_bounds <-
            letExp "in_bounds"
              =<< toExp
                ( foldr (.&&.) true $
                    zipWith
                      (\ind dim -> le64 ind .<. pe64 dim)
                      glb_arr_inds
                      base_arr_dims
                )

          -- Repermute the global array indices to match.
          let glb_arr_inds_perm = arrPerm arr_meta glb_arr_inds
          glb_elem <-
            letExp (baseString base_arr)
              =<< eIf
                (toExp in_bounds)
                ( index "glb_elem" base_arr glb_arr_inds_perm
                    >>= resultBodyM . (: []) . Var
                )
                -- TODO: here, we simply insert a zero (or zero-like value) into
                -- shmem whenever we are outside bounds. However, this only
                -- suceeds whenever:
                --
                -- `f zero_0 _ = f _ zero_1 = red_ne`
                --
                -- where `f` is the map function; `zero_0` and `zero_1` are the
                -- zero-like values for the two given array element types; and
                -- `red_ne` is the reduce neutral element.
                --
                -- This is seldom the case in general, however it happens to
                -- hold for regular tensor contraction and MM, hence it is used
                -- for testing for the time being.
                --
                -- There is a big TODO in figuring out the best solution to this
                -- problem. The simplest solution is to simply have a
                -- corresponding boundary guard in the register tile
                -- accumulation step, however this greatly and negatively
                -- affects performance.
                --
                -- Another solution is to statically examine whether `zero_0`
                -- and `zero_1` exist s.t. the above condition holds, but this
                -- analysis can be difficult or impossible, and the values may
                -- not even exist.
                --
                -- Alternatively (on Cosmin's suggestion), the user can manually
                -- pass a padding value as an attribute in the Futhark source
                -- code. Personally, I think this is very hacky, obscure to most
                -- users, error-prone, and an anti-pattern. Also, attributes
                -- only support passing integral values, not float values.
                (eBody [eBlank $ Prim $ shmemElemType arr_meta])

          -- Finally, update shared mem array accumulator.
          fmap varsRes $
            letTupExp "acc_out" $
              BasicOp $
                UpdateAcc
                  Unsafe
                  acc
                  shr_arr_inds
                  [Var glb_elem]

        shmem_shape = Shape $ shmemDims arr_meta
        tile_dims = tileDims arr_meta
        block_offsets = arrGather_ arr_meta (blockOffsets tc_env) (Var qq)
        base_arr_dims = baseArrDims arr_meta
        base_arr = baseArr arr_meta

    accumulateRegTile :: VName -> [VName] -> Builder GPU VName
    accumulateRegTile qq redomap_inputs_shr =
      segMapND_ "reg_tiles_out" seglvl_thd ResultPrivate tiles_T $ \ltids -> do
        reg_tile_in <- index "reg_tile_in" reg_tiles_in ltids
        fmap ((: []) . varRes) $
          forLoop_ tile_seq reg_tile_in $ \q reg_tile_in' ->
            letExp "reg_tile_acc"
              =<< eIf
                ( toExp $
                    -- if we are in the prologue, accumulate unconditionally!
                    fromBool is_prologue
                      .||. le64 qq + le64 q .<. pe64 common_dim
                )
                ( accumulateRegTileInnerLoopNest ltids q reg_tile_in'
                    >>= resultBodyM . (: []) . Var
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
            shr_inds <- forM arr_metas $ \meta -> do
              let ltids' = arrGather_ meta ltids dummy_ltid
              let tiles_R' = arrGather_ meta tiles_R dummy_regtile
              let loop_inds' = arrGather_ meta loop_inds q
              forM3 ltids' tiles_R' loop_inds' $ \ltid tile loop_ind ->
                letExp "shr_ind" =<< toExp (le64 ltid * pe64 tile + le64 loop_ind)

            -- Compute map and reduction results and update the register tile.
            map_f <- renameLambda $ mapLam tc_env
            red_op <- renameLambda $ redLam tc_env

            map_operands <- forM2 redomap_inputs_shr shr_inds $ \arr inds ->
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
    -- TODO: find out how to reliably extract the inner dims of the segspace.
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
    Just var_inds_per_arr <- variantDimsPerArr variance redomap_arrs gtids,
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
                          tile_seq
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
                (shmemElemType arr)
                (shmemDims arr)

          ~(reg_tiles_res : _shr_arrs_out) <-
            case use_epilogue of
              True -> do
                myDebugM "Compiling TC expression WITH epilogue"
                num_full_tiles <-
                  letExp "num_full_tiles" . BasicOp $
                    BinOp (SQuot Int64 Unsafe) common_dim tile_seq

                ~(reg_tiles' : shr_arrs') <-
                  forLoop (Var num_full_tiles) (reg_tiles_init : shr_arrs_init) $
                    \qq0 (reg_tiles_merge : shr_arrs_merge) ->
                      reductionLoopBody tc_env qq0 reg_tiles_merge shr_arrs_merge True

                reductionLoopBody tc_env num_full_tiles reg_tiles' shr_arrs' False
              _ -> do
                myDebugM "Compiling TC expression WITHOUT epilogue"
                num_seq_tiles <- letSubExp "num_seq_tiles" =<< ceilDiv common_dim tile_seq
                forLoop num_seq_tiles (reg_tiles_init : shr_arrs_init) $
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
variantDimsPerArr variance arrs dims = do
  let var_inds_per_arr = map variantInnerDimsForArr arrs
  let var_dims_per_arr = map (gather dims) var_inds_per_arr

  -- Interchange those dimensions of the segspace on which all redomap arrays
  -- are variant outwards.
  let segspace_dims_perm =
        uncurry (++) $
          L.partition
            -- Check that given dim is in var_dims of all arrays.
            (\dim -> all (elem dim) var_dims_per_arr)
            dims
  let segspace_perm = dims `isPermutationOf` segspace_dims_perm

  myDebugM $
    "variantDimsPerArr\ndims:\n"
      ++ prettyString dims
      ++ "\ndims':\n"
      ++ show segspace_dims_perm
      ++ "\nperm:\n"
      ++ show segspace_perm
      ++ "\nvar_inds_per_arr:\n"
      ++ show var_inds_per_arr

  -- assert that all arrays are variant to some, but not all dims.
  -- TODO: is below check sufficient to check this assertion?
  --       perhaps this assertion should be (or already is) made elsewhere.
  guard $ all ((`elem` [1 .. n_dims - 1]) . length) var_inds_per_arr

  -- for each dim, assert that at least one array is variant to this dim.
  -- TODO: is there a better, more correct, or safer way to assert this?
  -- Actually, I think this can safely be assumed to already hold, due to these
  -- parallel dimensions already having been interchanged outwards in an earlier
  -- compiler stage, but I might be wrong on this.
  guard $ all (`elem` concat var_dims_per_arr) dims

  -- assert no overlap in variance between arrays.
  -- TODO: is this check necessary or even desired? for exactly 2 redomap
  -- arrays, overlap in variance means all redomap arrays are variant to the
  -- given parallel dimension, and thus it would have been interchanged outwards
  -- (given the above TODO is implemented).
  -- guard $ allUnique $ concat var_inds_per_arr

  pure var_inds_per_arr
  where
    n_dims = length dims
    variantInnerDimsForArr arr =
      let arr_variance = M.findWithDefault mempty arr variance
       in L.findIndices (`nameIn` arr_variance) dims
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
    tileSeq :: SubExp,
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
    blockOffsets :: [SubExp],
    -- Lambdas for the map function and reduction operators for the contraction.
    mapLam :: Lambda GPU,
    redLam :: Lambda GPU,
    -- For each reduction array, the information needed to handle this
    -- particular array during code generation.
    arrsInfo :: [ArrMeta]
  }
  deriving (Show)

-- | All the information needed to handle a given operand array.
-- TODO: give a proper name to this one.
data ArrMeta = ArrMeta
  { baseArr :: VName,
    baseArrDims :: [SubExp],
    tileDims :: [SubExp],
    shmemDims :: [SubExp],
    shmemElemType :: PrimType,
    variantDims :: [Maybe Int],
    arrLoadStm :: Stm GPU,
    lmadPerm :: Maybe [Int]
  }
  deriving (Show)

gather :: [a] -> [Int] -> [a]
gather xs = map (xs !!) . filter (`elem` indices xs)

gather_ :: [a] -> a -> [Maybe Int] -> [a]
gather_ xs x = map (maybe x (xs !!) . checkIdx)
  where
    checkIdx (Just i)
      | i `elem` indices xs = Just i
    checkIdx _ = Nothing

arrGather_ :: ArrMeta -> [a] -> a -> [a]
arrGather_ meta src x = gather_ src x $ variantDims meta

arrPerm :: ArrMeta -> [a] -> [a]
arrPerm meta xs = maybe xs (gather xs) (lmadPerm meta)

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
  tile_seq <- letTileSE SizeTile tile_common_dim_vn
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
      tile_seq
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
    common_dim_name = maybe "seq" id $ getNameStrFor common_dim_se

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
findLMADPerm :: Env -> VName -> Maybe [Int]
findLMADPerm (_, ixfn_env) arr = do
  lmad <- LMAD.dims <$> M.lookup arr ixfn_env
  let shape0 = map (untyped . LMAD.ldShape) lmad
      strides0 = map (toFlatPrimExp . untyped . LMAD.ldStride) lmad

  -- For each permutation of the LMAD shape; compute the strides for this
  -- permutation and test them against the known strides. Then, pick the first
  -- succeeding set of strides.
  msum $ map (isPermutationOf strides0 . stridesFor) $ L.permutations shape0
  where
    stridesFor = map toFlatPrimExp . (++ [val1]) . (scanr1 binopMul) . tail
    binopMul = BinOpExp (Mul Int64 OverflowUndef)
    val1 = ValueExp (IntValue (Int64Value 1))

    -- Flattens a nested PrimExp if that PrimExp happens to represent a simple
    -- product. Used to more reliably check equality between LMADs (in
    -- isPermutationOf).
    toFlatPrimExp :: PrimExp VName -> FlatPrimExp
    toFlatPrimExp = Product . L.sort . flattenProducts . flattenMulOps
      where
        flattenMulOps (BinOpExp Mul {} e1 e2) =
          Product $ map toFlatPrimExp [e1, e2]
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
makeTCEnv env kernel_params load_stms map_lam red_lam map_ts = do
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

  block_offsets <-
    forM3 inner_dim_names tbids tiles_TR $
      \dim_name tbid tile_TR ->
        letSubExp ("tb_offset_" ++ dim_name)
          =<< toExp (le64 tbid * pe64 tile_TR)

  fmap (TCEnv kernel_params block_offsets map_lam red_lam)
    $ forM3
      base_arrs
      load_stms
      map_ts
    $ \base_arr load_stm shmem_elem_type -> do

      dims <- getArrDims base_arr

      -- It's not pretty, but we somehow need to extract the tile
      -- dimensions and tblock offsets corresponding to each dimension
      -- of each array, and below mess accomplishes this.
      --
      -- First, for each dimension in each array, find the index in
      -- inner_dims of this dimension. These indices can then be used to
      -- extract the corresponding tblock offsets and tile dims, which
      -- will be ordered by the inner dimensions. Note that for each
      -- array, the indices computed here are the same as those returned
      -- by `variantDimsPerArr`, but in different order.
      -- TODO: document why perm is necessary.
      let lmad_perm = findLMADPerm env base_arr
      let inv_lmad_perm = map snd . L.sort . (`zip` [0 ..]) <$> lmad_perm

      let dims' = maybe dims (gather dims) inv_lmad_perm
      let var_gtids = map (`L.elemIndex` inner_dims) dims'

      -- Then, for each dimension of each array, extract the TR tile and
      -- tblock offset corresponding to this dimension. For the tile
      -- dims, we insert tile_seq in the index of the array dim not
      -- represented in inner_dims.
      let tile_dims = gather_ tiles_TR tile_seq var_gtids

      -- TODO: decide pad_term dynamically, based on the given array.
      let pad_term = pe64 se1
      let inner_dim = last tile_dims
      inner_dim_pad <-
        -- TODO: give a better name to the padded dimension.
        -- letSubExp (baseString inner_dim ++ "_pad")
        letSubExp "inner_dim_pad"
          =<< toExp (pe64 inner_dim + pad_term)
      let shmem_dims = init tile_dims ++ [inner_dim_pad]

      pure $
        ArrMeta
          base_arr
          dims'
          tile_dims
          shmem_dims
          shmem_elem_type
          var_gtids
          load_stm
          lmad_perm
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

    tbids = tbidVns kernel_params
    tiles_TR = tilesTR kernel_params
    tile_seq = tileSeq kernel_params
    inner_dim_names = innerDimNames kernel_params
    inner_dims = innerDims kernel_params
