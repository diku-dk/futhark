{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegStencil' is quite straightforward.
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

      -- constexpr and constants
      let dims = map (TPrimExp . toExp' int32 . snd) $ unSegSpace space
          stencil_indexes = stencilIndexes op
          dimentionality = length stencil_indexes
          group_size_flat_subexp = unCount $ segGroupSize lvl
          group_size_flat_exp = TPrimExp . toExp' int32 $ group_size_flat_subexp
          flNumT2 x = 1 + (sMin32 1 (group_size_flat_exp `quot` x))
          fl256t2 = flNumT2 256
          fl512t2 = flNumT2 512
          fl1024t2 = flNumT2 1024
          group_sizes_exp :: [Imp.TExp Int32]
          group_sizes_exp =
              case dimentionality of
                  1 -> [group_size_flat_exp]
                  2 -> [group_size_flat_exp `quot` 32, 32]
                  3 -> [2*fl512t2,2*fl256t2*fl1024t2,32]
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
          halo_widths = map (\x-> x-1) $ zipWith (-) a_maxs_i a_mins_i
          halo_widths_exp = map fromInteger halo_widths
          -- Amount of Bytes required for a single index in tile(s).
          memory_per_elem = sum $ map primByteSize lambdaInvarTypes

          strip_multiples :: [Integer]
          strip_multiples =
            case dimentionality of
              1 -> [8]
              2 -> [2, 2]
              3 -> [2, 1, 2]
              _ -> error "not valid dimensions"
          strip_multiples_exp :: [Imp.TExp Int32]
          strip_multiples_exp = map fromInteger strip_multiples
          uses = map (length . nub) stencil_indexes
          uses_exp :: [Imp.TExp Int32]
          uses_exp = map fromIntegral uses
          printf :: String -> Imp.TExp t -> ImpM lore r op ()
          printf text = emit . Imp.DebugPrint text . Just . untyped
          computeReuses works hws = do
            let tileReads = zipWith (*) uses_exp works
                weights = 1 : repeat 3
            tileReadsWeighed <- mapM (dPrimVE "tileReadsWeighed") $ reverse $ zipWith (*) weights $ reverse tileReads
            let tileWrites = zipWith (+) hws works
            weighedReuseFactors <- mapM (dPrimVE "weighedReuseFactors") $ zipWith div tileReadsWeighed tileWrites
            pure (     foldl1 (.||.) (map (5 .<=.) weighedReuseFactors)
                  .&&. foldl1 (.&&.) (map (1 .<=.) weighedReuseFactors)
                 )

      max_shared_bytes <- do
        name <- dPrim "max_shared_bytes" int32
        sOp $ Imp.GetSizeMax (tvVar name) Imp.SizeLocalMemory
        pure $ tvExp name
      aboveMinBlock <- dPrimVE "aboveMinBlock" ((128 .<=. group_size_flat_exp))
      printf "aboveMinBlock" aboveMinBlock
      can_and_should_run_stripTile <- do
        host_strip_sizes <- mapM (dPrimVE "host_strip_sizes") $ zipWith (*) group_sizes_exp strip_multiples_exp
        stripTileElems <- dPrimVE "stripTileElems" $ product $ zipWith (+) halo_widths_exp host_strip_sizes
        stripTileBytes <- dPrimVE "stripTileBytes" $ stripTileElems * fromInteger memory_per_elem
        printf "shared_size_max" max_shared_bytes
        forM_ group_sizes_exp $ printf "group_sizes"
        printf "stripTileBytes" stripTileBytes
        canRunStripTile <- dPrimVE "canRunStripTile" $ stripTileBytes .<=. max_shared_bytes
        reuseIsHighEnough <- dPrimVE "reuseIsHighEnoughST" =<< computeReuses host_strip_sizes halo_widths_exp
        isNotSkewed <- dPrimVE "isNotSkewedST" $ foldl1 (.&&.) $ zipWith (.<=.) host_strip_sizes dims
        printf "can_run_strips" canRunStripTile
        printf "reuseIsHighEnough_strips" reuseIsHighEnough
        printf "isNotSkewed_strips" isNotSkewed
        dPrimVE "can_and_should_run_stripTile" $ canRunStripTile .&&. reuseIsHighEnough .&&. isNotSkewed .&&. aboveMinBlock

      printf "group_size_flat" $ product group_sizes_exp
      printf "can_and_should_run_stripTile" can_and_should_run_stripTile
      emit $ Imp.DebugPrint "\n" Nothing
      sIf can_and_should_run_stripTile
        (compileBigTileStripMinedSingleDim pat lvl space op kbody group_sizes_exp strip_multiples)
        (compileGlobalReadFlat pat lvl space op kbody)

createSpans :: Num a => [a] -> [a]
createSpans = scanr1 (*) . tail

-- the provided one has a ton of common subexpressions so a new one was made
-- !!!! It does however need the span of the inner dimensions, so use
-- createSpans
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

sForUnflatten ::
  IntExp t =>
  [Imp.TExp t] ->
  Imp.TExp t ->
  Imp.TExp t ->
  (([Imp.TExp t], Imp.TExp t, Imp.TExp Bool)-> ImpM lore r op ()) ->
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
  sFor "i" iterations $ \i -> do
    () <- m (map tvExp starts, tvExp start_flat_var, i .==. (iterations-1))
    prepareNextIter

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
    local_id_flat <- dPrimVE "local_id_flat" . sExt64 . kernelLocalThreadId $ constants
    group_id_flat <- dPrimVE "group_id_flat" . sExt64 . kernelGroupId $ constants
    gid_flat <- dPrimVE "global_id_flat" $ group_id_flat * group_size_flat + local_id_flat
    gids <- map tvExp <$> unflattenIx "global_id" size_span gid_flat
    zipWithM_ dPrimV_ is gids

    -- check for out of bound on global id for each axis
    sWhen (isActive $ unSegSpace space) $ do
      -- compile invariant elements
      compileStms mempty (kernelBodyStms kbody) $ pure ()

      -- declare and attach invariant elements to invariantParams
      zipWithM_ dPrimV_ (map paramName invariantParams) . map TPrimExp
        =<< mapM toExp invarElems
      -- declare variant elements
      dLParams variantParams

      let fetch_ixs =
            zipWithM (\maxix ->
              mapM (dPrimVE "bound_ix" . sMin64 maxix . sMax64 0)
              ) max_ixs
            $ flip (zipWith (mapM (+))) gids
            $ map (map fromInteger)
            $ unique_ixs
      read_ixs_all <- fetch_ixs
      let ixs_lookup = zipWith (zipWith (,)) unique_ixs read_ixs_all

      -- load variants into lambda variant parameters
      forM_ (zip sorted_ixs sorted_params_tup) $ \(rel_ixs, pars) -> do
        let read_ixs = map fromJust $ zipWith lookup rel_ixs ixs_lookup
        forM_ (zip pars $ stencilArrays op) $ \(par, src) ->
          copyDWIMFix (paramName par) [] (Var src) read_ixs

      -- compile lambda function and designate output style
      compileStms mempty (bodyStms lamBody) $
        zipWithM_ (compileThreadResult space) (patternElements pat) $
          map (Returns ResultMaySimplify) $ bodyResult lamBody

compileBigTileStripMinedSingleDim ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  [Imp.TExp Int32] ->
  [Integer] ->
  CallKernelGen ()
compileBigTileStripMinedSingleDim pat _ space op kbody group_sizes_exp strip_multiples =
  let (is, dims') = unzip $ unSegSpace space
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

      strip_multiples_exp :: [Imp.TExp Int32]
      strip_multiples_exp = map fromInteger strip_multiples

      -- constexpr variables
      strip_sizes_exp = zipWith (*) strip_multiples_exp group_sizes_exp
      shared_sizes_exp = zipWith (+) strip_sizes_exp $ zipWith (-) a_maxs a_mins
      shared_size_flat_exp = product shared_sizes_exp
      block_size_c = Count $ sExt64 $ product group_sizes_exp
  in do
  -- host side evaluated variables
  host_strip_sizes <- mapM (dPrimVE "host_strip_sizes") strip_sizes_exp
  strip_grid <- mapM (dPrimVE "strip_grid") $ map sExt32 $ zipWith divUp dims $ map sExt64 host_strip_sizes
  strip_grid_spans <- mapM (dPrimVE "strip_grid_spans") $ createSpans strip_grid
  tile_length <- dPrimV "tile_length_flat" shared_size_flat_exp
  num_groups <- Count <$> dPrimVE "num_groups" (sExt64 $ product strip_grid)
  max_ixs <- mapM (dPrimVE "max_ixs" . (\x->x-1)) dims

  sKernelThread "segstencil-stripbigtile" num_groups block_size_c (segFlat space) $ do
    -- declaration of shared tile(s)
    tiles <- forM lamParTypes $ \ptype ->
      sAllocArray "tile" ptype (Shape [Var $ tvVar tile_length]) (Space "local")
    constants <- kernelConstants <$> askEnv
    -- constexpr variables
    group_sizes <- mapM (dPrimVE "group_sizes") group_sizes_exp
    group_spans <- mapM (dPrimVE "group_spans") $ createSpans group_sizes
    group_size_flat <- dPrimVE "group_size_flat" $ product group_sizes
    strip_sizes <- mapM (dPrimVE "strip_sizes") $ map sExt64 strip_sizes_exp
    shared_sizes <- mapM (dPrimVE "shared_sizes") shared_sizes_exp
    shared_size_flat <- dPrimVE "shared_size_flat" $ product shared_sizes
    -- runtime variables
    local_id_flat <- dPrimVE "local_id_flat" . kernelLocalThreadId $ constants
    local_ids <- map tvExp <$> unflattenIx "local_id" group_spans local_id_flat
    strip_id_flat <- dPrimVE "strip_id_flat" . kernelGroupId $ constants
    strip_ids <- map tvExp <$> unflattenIx "strip_id" strip_grid_spans strip_id_flat

    let bound_idxs = zipWith sMin64 max_ixs . map (sMax64 0)
    writeSet_offsets <- mapM (dPrimVE "writeSet_offset") $
      zipWith (*) (map sExt64 strip_ids) (map sExt64 strip_sizes)
    -- run the data loader
    sForUnflatten shared_sizes local_id_flat group_size_flat $ \(loader_ids, loader_ids_flat, isLastIter) -> do
      loader_gids <- mapM (dPrimVE "loader_gid") $
         bound_idxs $ zipWith (+) writeSet_offsets $ map sExt64 $ zipWith (+) loader_ids a_mins
      sWhen ((bNot isLastIter) .||. loader_ids_flat .<. shared_size_flat) $
        forM_ (zip tiles (stencilArrays op)) $ \(tile, input_arr) ->
          copyDWIMFix tile [sExt64 loader_ids_flat] (Var input_arr) loader_gids

    sOp $ Imp.Barrier Imp.FenceLocal
    -- loader finished

    let nest_shape = Shape $ map (Constant . IntValue . intValue Int64) strip_multiples
    sLoopNest nest_shape $ \local_strip_ids -> do
      tile_local_ids <- mapM (dPrimVE "tile_local_id")
                      $ zipWith (+) local_ids
                      $ zipWith (*) (map sExt32 local_strip_ids) group_sizes
      tile_local_id_flat <- dPrimVE "tile_local_id_flat" $ flattenIx shared_sizes tile_local_ids
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
        forM_ (zip tile_ixs variant_params_tuples) $ \(tile_ix, pars) ->
          forM_ (zip pars tiles) $ \(par, tile) ->
            copyDWIMFix (paramName par) [] (Var tile) [sExt64 tile_ix]

        -- compile lambda function and designate output style
        compileStms mempty (bodyStms lamBody) $
          zipWithM_ (compileThreadResult space) (patternElements pat) $
            map (Returns ResultMaySimplify) $ bodyResult lamBody

