{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegStencil'.
module Futhark.CodeGen.ImpGen.Kernels.SegStencil (compileSegStencil) where

import Control.Monad.Except
import Data.List (transpose, nub, sortOn, group, sort)
import Data.Maybe (fromJust)
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.IR.KernelsMem
import Futhark.Util.IntegralExp
import Prelude hiding (div, quot, rem)
import qualified Prelude (div)

-- | Compile 'SegMap' instance code.
compileSegStencil ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegStencil pat lvl space op kbody =
  case lvl of
    SegGroup {} -> error "cannot happen"
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing

      let dims = map (TPrimExp . toExp' int32 . snd) $ unSegSpace space
          stencil_indexes = stencilIndexes op
          dimentionality = length stencil_indexes
          group_size_flat_subexp = unCount $
            segGroupSize lvl
          group_size_flat_exp = TPrimExp .
            toExp' int32 $ group_size_flat_subexp
          -- stuff to reshape the group-size depending on the macro variable 'groupsize'
          flNumT2 x = 1 +
            sMin32 1 (group_size_flat_exp `quot` x)
          fl64t2 = flNumT2 64
          fl128t2 = flNumT2 128
          fl256t2 = flNumT2 256
          fl512t2 = flNumT2 512
          fl1024t2 = flNumT2 1024
          group_sizes_exp ::
            [Imp.TExp Int32]
          group_sizes_exp =
              case dimentionality of
                  1 -> [group_size_flat_exp]
                  2 -> [fl64t2 *fl128t2
                       *fl256t2*fl512t2
                       *fl1024t2, 32]
                  3 -> [ fl256t2*fl1024t2
                        ,fl64t2*fl128t2*fl512t2
                        , 32]
                  _ -> error "not valid dimensions"
          n_point_stencil = length $ head stencil_indexes
          n_invarElems = length $ map kernelResultSubExp $ kernelBodyResult kbody
          lambdaInvarTypes =
            map (elemType . paramType . head)
            $ chunksOf n_point_stencil
            $ drop n_invarElems
            $ lambdaParams $ stencilOp op
          a_mins_i = map minimum stencil_indexes
          a_maxs_i = map maximum stencil_indexes
          -- width of the zone of indexes that only read from and not writen to.
          halo_widths = zipWith (-) a_maxs_i a_mins_i
          halo_widths_exp = map fromInteger halo_widths

          size_per_invars :: [Integer]
          size_per_invars = map primByteSize lambdaInvarTypes
          -- Amount of Bytes required for a single index in tile(s).
          memory_per_elem :: Integer
          memory_per_elem = sum size_per_invars
          rescale_on_byte_size :: Integer -> Integer
          rescale_on_byte_size mult =
            let largest_type_size = maximum size_per_invars
            in max (1 :: Integer) ((mult * 4) `Prelude.div` largest_type_size)
          work_multiples :: [Integer]
          work_multiples =
            case dimentionality of
              1 -> [rescale_on_byte_size 4]
              2 -> [2, rescale_on_byte_size 2] 
              3 -> [2, 2, rescale_on_byte_size 1]
              _ -> error "not valid dimensions"
          work_multiples_exp :: [Imp.TExp Int32]
          work_multiples_exp = map fromInteger work_multiples
          debugPrintf :: String -> Imp.TExp t -> ImpM lore r op ()
          debugPrintf text = emit . Imp.DebugPrint text . Just . untyped
          computeFlatReuse works hws =
            let unique_ixs = fromIntegral . length . nub . transpose $ stencil_indexes
                flat_tile_reads = product works * unique_ixs
                flat_tile_writes = product $ zipWith (+) works hws
            in dPrimVE "flat_reuse_int" $ flat_tile_reads `quot` flat_tile_writes
      max_shared_bytes <- do
        name <- dPrim "max_shared_bytes" int32
        sOp $ Imp.GetSizeMax (tvVar name) Imp.SizeLocalMemory
        pure $ tvExp name
      can_and_should_run_stripTile <- do
        host_strip_sizes <- mapM (dPrimVE "host_strip_sizes") $ zipWith (*) group_sizes_exp work_multiples_exp
        stripTileElems <- dPrimVE "stripTileElems" $ product $ zipWith (+) halo_widths_exp host_strip_sizes
        stripTileBytes <- dPrimVE "stripTileBytes" $ stripTileElems * fromInteger memory_per_elem
        -- There should also be a check for whether the amount of shared
        --   memory required per thread-block would harm the occupancy too
        --   much for the stripmineBigTile design to be meaningfully run.
        --   However as of writing this comment, there is no support for this
        --   in the compiler, so it was simply (but incorrectly) assumed that
        --   this was of no concern.
        canRunStripTile <- dPrimVE "canRunStripTile" $ stripTileBytes .<=. (max_shared_bytes `div` 2)
        reuseIsHighEnough <- (dPrimVE "reuseIsHighEnoughST" . (2 .<=.)) =<< computeFlatReuse host_strip_sizes halo_widths_exp
        isNotSkewed <- dPrimVE "isNotSkewedST" $ foldl1 (.&&.) $ zipWith (.<=.) host_strip_sizes dims
        forM_ group_sizes_exp $ debugPrintf "group_sizes"
        debugPrintf "shared_size_max" max_shared_bytes
        debugPrintf "stripTileBytes" stripTileBytes
        debugPrintf "can_run_strips" canRunStripTile
        debugPrintf "reuseIsHighEnough_strips" reuseIsHighEnough
        debugPrintf "isNotSkewed_strips" isNotSkewed
        dPrimVE "can_and_should_run_stripTile" $
          canRunStripTile
            .&&. reuseIsHighEnough
            .&&. isNotSkewed
      debugPrintf "can_and_should_run_stripTile" can_and_should_run_stripTile
      sIf can_and_should_run_stripTile
        (compileBigTileStripMinedSingleDim pat lvl space op kbody group_sizes_exp work_multiples)
        (compileGlobalReadFlat pat lvl space op kbody)

-- Creates the size spans given a list of sizes.
createSpans :: Num a => [a] -> [a]
createSpans = scanr1 (*) . tail


-- 'unflattenIndex' has a ton of common subexpressions,
-- and manully calculate 'rem' so a new one was made.
-- Use as:
--   indexes <- unflattenIx name_base (createSpans sizes) flat_index
unflattenIx ::
  IntExp t =>
  String ->
  [Imp.TExp t] ->
  Imp.TExp t ->
  ImpM lore r op [TV t]
unflattenIx name [] i = (: []) <$> dPrimV name i
unflattenIx name (x : xs) i = do
  dimIx <- dPrimV name $ i `quot` x
  rem_val <- dPrimV "rem_val" $ i `rem` x
  (dimIx :) <$> unflattenIx name xs (tvExp rem_val)

-- Splits the lists 'xs' into chunks of 'len' size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf len xs =
  let (left, right) = splitAt len xs
   in left : chunksOf len right

-- A rewrite of the pattern:
--  iterations <- dPrimVE "iters" $ divUp (product sizes) added_flat
--  sFor "i" iterations $ \i ->
--    flat_ix <- dPrimVE "flat_ix" $ (i * added_flat) + start_flat
--    ixs <- unflattenIndex "ixs" sizes flat_ix
--    () <- body (ixs, flat_ix)
--
-- assumes that (0 < product sizes) aka. not an empty range.
sForUnflatten ::
  IntExp t =>
  [Imp.TExp t] ->
  Imp.TExp t ->
  Imp.TExp t ->
  (([Imp.TExp t], Imp.TExp t, Imp.TExp Bool)-> ImpM lore r op ()) ->
  ImpM lore r op ()
sForUnflatten sizes start_flat added_flat body = do
  size_span <- mapM (dPrimVE "size_span") $ createSpans sizes
  flat_size <- dPrimVE "flat_size" $ product sizes
  iterations <- dPrimVE "iterations" ((divUp flat_size added_flat) - 1)
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
    () <- body (map tvExp starts, tvExp start_flat_var, toBoolExp (Constant (BoolValue True)))
    prepareNextIter
  void $ body (map tvExp starts, tvExp start_flat_var, toBoolExp (Constant (BoolValue False)))

-- Kernel for evaluating a single stencil.
-- This is equivalent to just making a nest of maps and finding the neighbours from there.
-- This kernel is primarily used as a fallback in case the others can't meaningfully be run
compileGlobalReadFlat ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileGlobalReadFlat pat lvl space op kbody = do
  let (gids_vn, dims') = unzip $ unSegSpace space
      dims = map toInt64Exp dims'
      group_size_flat_c = toInt64Exp <$> segGroupSize lvl
      group_size_flat = unCount group_size_flat_c
      invarElems = map kernelResultSubExp $ kernelBodyResult kbody
      lam = stencilOp op
      lamBody = lambdaBody lam
      (invariantParams, variantParams) =
        splitAt (length invarElems) $ lambdaParams lam
      n_point_stencil = length $ head $ stencilIndexes op
      param_tups = transpose $ chunksOf n_point_stencil variantParams
      par_relIxs_pairs = zip (transpose (stencilIndexes op)) param_tups
      (sorted_ixs, sorted_params_tup) = unzip $ sortOn fst par_relIxs_pairs
      unique_ixs = map (map head . group . sort) . transpose $ sorted_ixs

  -- Host side evaluated variables
  size_span <- mapM (dPrimVE "size_span" . sExt64) $ createSpans dims
  grid_flat <- Count <$> dPrimVE "grid_flat" (divUp (product dims) group_size_flat)
  max_ixs <- mapM (dPrimVE "max_ixs" . (\x->x-1)) dims

  sKernelThread "segstencil-globalRead" grid_flat group_size_flat_c (segFlat space) $ do
    constants <- kernelConstants <$> askEnv
    zipWithM_ dPrimV_ gids_vn =<< (do
      local_id_flat <- dPrimVE "local_id_flat" . sExt64 . kernelLocalThreadId $ constants
      group_id_flat <- dPrimVE "group_id_flat" . sExt64 . kernelGroupId $ constants
      gid_flat <- dPrimVE "global_id_flat" $ group_id_flat * group_size_flat + local_id_flat
      map tvExp <$> unflattenIx "global_id" size_span gid_flat
      )
    let gids = map (toInt64Exp . Var) gids_vn

    -- check for out of bound on global id for each axis
    sWhen (foldl1 (.&&.) $ zipWith (.<=.) gids max_ixs) $ do
      -- compile invariant elements
      compileStms mempty (kernelBodyStms kbody) $ pure ()

      -- declare and attach invariant elements to invariantParams
      zipWithM_ dPrimV_ (map paramName invariantParams) . map TPrimExp
        =<< mapM toExp invarElems
      -- declare variant elements
      dLParams variantParams

      -- create and bound each index for each axis only once, and create lookup-list.
      -- The manual bounding is valid as the gids themselves are inbounds.
      ixs_lookup <-
        forM (zip3 unique_ixs gids max_ixs) (\(ixs_tup, gid, maxix) ->
          forM ixs_tup (\relix ->
            let read_ix = gid + fromInteger relix in
            let bounded
                  | relix > 0 = sMin64 maxix read_ix
                  | relix < 0 = sMax64 0 read_ix
                  | otherwise = read_ix
            in do
              gix <- dPrimVE "bound_ix" bounded
              pure (relix, gix)
            )
          )

      -- load variants into lambda variant parameters
      forM_ (zip sorted_ixs sorted_params_tup) $ \(rel_ixs, pars) -> do
        let read_ixs = map fromJust $ zipWith lookup rel_ixs ixs_lookup
        forM_ (zip pars $ stencilArrays op) $ \(par, src) ->
          copyDWIMFix (paramName par) [] (Var src) read_ixs

      -- compile lambda function and designate output style
      compileStms mempty (bodyStms lamBody) $
        zipWithM_ (compileThreadResult space) (patternElements pat) $
          map (Returns ResultMaySimplify) $ bodyResult lamBody

-- Kernel for evaluating a single stencil.
-- This kernel uses shared memory to store read elements,
--   and does multiple writes per thread.
-- The write-set of a thread-block is the set of indexes of the output array that are written to.
-- The read-set of a thread-block is the set of indexes of the input array that it reads.
--
-- The approach is to find the the smallest rectangular shape containing all
--   the required elements to do all writes of the write-set.
--   This is then the read-set.
-- There are 2 phases.
--   In phase 1 it loads the entire read-set into the tile(s).
--   In phase 2 it iterates through the write-set and evaluated the lambda
--     function and performs writes.
compileBigTileStripMinedSingleDim ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  [Imp.TExp Int32] ->
  [Integer] ->
  CallKernelGen ()
compileBigTileStripMinedSingleDim pat _ space op kbody group_sizes_exp work_multiples =
  let (gids_vn, dims') = unzip $ unSegSpace space
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
      work_multiples_exp :: [Imp.TExp Int32]
      work_multiples_exp = map fromInteger work_multiples
      work_sizes_exp = zipWith (*) work_multiples_exp group_sizes_exp
      shared_sizes_exp = zipWith (+) work_sizes_exp $ zipWith (-) a_maxs a_mins
      shared_size_flat_exp = product shared_sizes_exp
  in do
  -- host side evaluated variables
  host_work_sizes <- mapM (dPrimVE "host_work_sizes") work_sizes_exp
  work_grid <- mapM (dPrimVE "work_grid" . sExt32) $ zipWith divUp dims $ map sExt64 host_work_sizes
  work_grid_spans <- mapM (dPrimVE "work_grid_spans") $ createSpans work_grid
  tile_length <- dPrimV "tile_length_flat" shared_size_flat_exp
  num_groups <- Count <$> dPrimVE "num_groups" (sExt64 $ product work_grid)
  max_ixs <- mapM (dPrimVE "max_ixs" . (\x->x-1)) dims
  blocksize <- dPrimVE "blocksize" $ sExt64 $ product group_sizes_exp

  sKernelThread "multiWriteBigTile" num_groups (Count blocksize) (segFlat space) $ do
    -- declaration of shared tile(s)
    tiles <- forM lamParTypes $ \ptype ->
      sAllocArray "tile" ptype
        (Shape [Var $ tvVar tile_length])
          (Space "local")

    constants <- kernelConstants <$> askEnv
    -- compile-time constant variables
    group_sizes <- mapM (dPrimVE "group_sizes")
      group_sizes_exp
    group_spans <- mapM (dPrimVE "group_spans") $
      createSpans group_sizes
    group_size_flat <- dPrimVE "group_size_flat" $
      product group_sizes

    work_sizes <- mapM (dPrimVE "work_sizes")
      $ zipWith (*) work_multiples_exp group_sizes

    shared_sizes <- mapM (dPrimVE "shared_sizes") $
      zipWith (+) work_sizes $ zipWith (-) a_maxs a_mins

    shared_size_flat <- dPrimVE "shared_size_flat" $ product shared_sizes
    -- threadId/groupId variables
    local_id_flat <- dPrimVE "local_id_flat" . kernelLocalThreadId $ constants
    local_ids <- map tvExp <$> unflattenIx "local_id" group_spans local_id_flat
    work_id_flat <- dPrimVE "work_id_flat" . kernelGroupId $ constants
    work_ids <- map tvExp <$> unflattenIx "work_id" work_grid_spans work_id_flat

    -- Phase 1: load the read-set into the tile(s).
    let bound_idxs = zipWith sMin64 max_ixs . map (sMax64 0)
    writeSet_offsets <- mapM (dPrimVE "writeSet_offset") $
      zipWith (*) (map sExt64 work_ids) (map sExt64 work_sizes)
    -- iterate through the read-set and do reads.
    sForUnflatten shared_sizes local_id_flat
      group_size_flat
        $ \(loader_ids,
            loader_ids_flat,
            isNotLastIter) -> do

      loader_gids <-
        mapM (dPrimVE "loader_gid") $
          bound_idxs $
            zipWith (+) writeSet_offsets
              $ map sExt64 $
                zipWith (+) loader_ids
                  a_mins

      sWhen (isNotLastIter .||.
             loader_ids_flat .<.
             shared_size_flat) $
        forM_ (zip tiles (stencilArrays op))
        $ \(tile, input_arr) ->
          copyDWIMFix tile
            [sExt64 loader_ids_flat]
            (Var input_arr)
            loader_gids

    -- end of phase 1. Wait for all elements to be loaded.
    sOp $ Imp.Barrier Imp.FenceLocal

    -- Phase 2: Iterate through the write-set and evaluate the lambda function and do the writes.
    base_write_gid <- mapM (dPrimVE "base_write_gid")
      $ zipWith (+) writeSet_offsets $ map sExt64 local_ids
    local_id_shared_flat <- dPrimVE "local_id_shared_flat" $ flattenIndex shared_sizes local_ids

    let nest_shape = map (Constant . IntValue . intValue Int32) work_multiples
    sLoopNestSE nest_shape $ \local_work_ids -> do
      -- create the centre index used to read from the tile(s).
      tile_ids_offs <- mapM (dPrimVE "tile_ids_offs")
                      $ zipWith (*) group_sizes local_work_ids
      tile_ids_offs_flat <- dPrimVE "tile_ids_offs_flat" $ flattenIndex shared_sizes tile_ids_offs
      -- assign the relevant values to the global-ids.
      zipWithM_ dPrimV_ gids_vn $ zipWith (+) base_write_gid $ map sExt64 tile_ids_offs
      let gids = map (toInt64Exp . Var) gids_vn

      -- create the individual indexes used to load from the tile(s).
      let tile_offsets = map (flattenIndex shared_sizes) $ transpose $ zipWith (mapM (-)) stencil_ixss a_mins
          variant_params_tuples = transpose $ chunksOf n_point_stencil variantParams
      tile_ixs <- mapM (dPrimVE "tile_ixs" . (+ local_id_shared_flat) . (+ tile_ids_offs_flat)) tile_offsets

      -- check for out of bound on global id for each axis
      sWhen (foldl1 (.&&.) $ zipWith (.<=.) gids max_ixs) $ do
        -- compile invariant elements
        compileStms mempty (kernelBodyStms kbody) $ pure ()

        -- declare and attach invariant elements to invariantParams
        zipWithM_ dPrimV_ (map paramName invariantParams) . map TPrimExp
          =<< mapM toExp invarElems

        -- declare the lambda function variant paramets and set them.
        dLParams variantParams
        forM_ (zip tile_ixs variant_params_tuples) $ \(tile_ix, pars) ->
          forM_ (zip pars tiles) $ \(par, tile) ->
            copyDWIMFix (paramName par) [] (Var tile) [sExt64 tile_ix]

        -- compile lambda function and designate output style
        compileStms mempty (bodyStms lamBody) $
          zipWithM_ (compileThreadResult space) (patternElements pat) $
            map (Returns ResultMaySimplify) $ bodyResult lamBody

