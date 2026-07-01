module Futhark.Pass.Flatten.BasicOp (transformDistBasicOp) where

import Control.Monad
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Tuple.Solo
import Debug.Trace
import Futhark.IR.GPU
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Monad
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util.IntegralExp
import Prelude hiding (div, quot, rem)

-- Do 'map2 (++) A B' where 'A' and 'B' are irregular arrays and have the same
-- number of subarrays
concatIrreg ::
  SegLevel ->
  Segments ->
  DistEnv ->
  VName ->
  [IrregularRep] ->
  FlattenM IrregularRep
concatIrreg lvl _segments _env ns reparr = do
  -- Concatenation does not change the number of segments - it simply
  -- makes each of them larger.

  num_segments <- arraySize 0 <$> lookupType ns

  -- Constructs the full list size / shape that should hold the final results.
  ns_full <- letExp (baseName ns <> "_full") <=< segMap lvl (MkSolo num_segments) $
    \(MkSolo i) -> do
      old_segments <-
        forM reparr $ \rep ->
          letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      new_segment <-
        letSubExp "new_segment"
          =<< toExp (sum $ map pe64 old_segments)
      pure $ subExpsRes [new_segment]

  (ns_full_F, ns_full_O, _ns_II1) <- doRepIota lvl ns_full

  repIota <- mapM (doRepIota lvl . irregularS) reparr
  segIota <- mapM (doSegIota lvl . irregularS) reparr

  let (_, _, rep_II1) = unzip3 repIota
  let (_, _, rep_II2) = unzip3 segIota

  n_arr <- mapM (fmap (arraySize 0) . lookupType) rep_II1

  -- Calculate offsets for the scatter operations
  let shapes = map irregularS reparr
  scatter_offsets <-
    letTupExp "irregular_scatter_offsets" <=< segMap lvl (MkSolo num_segments) $
      \(MkSolo i) -> do
        segment_sizes <-
          forM shapes $ \shape ->
            letSubExp "segment_size" =<< eIndex shape [eSubExp i]
        let scanned = scanl (+) 0 $ map pe64 segment_sizes   
        sumprefix   <- mapM (letSubExp "segment_prefix" <=< toExp) (init scanned)
        pure $ subExpsRes sumprefix

  scatter_offsets_T <-
    letTupExp "irregular_scatter_offsets_T" <=< segMap lvl (MkSolo num_segments) $
      \(MkSolo i) -> do
        columns <-
          forM scatter_offsets $ \offsets ->
            letSubExp "segment_offset" =<< eIndex offsets [eSubExp i]
        pure $ subExpsRes columns

  m <- arraySize 0 <$> lookupType ns_full_F
  data_t <- lookupType (irregularD (head reparr))
  let pt = elemType data_t
  let result_type = Array pt (Shape [m]) NoUniqueness
  elems_blank <- letExp "blank_res" =<< eBlank result_type

  -- Scatter data into result array
  elems <-
    foldlM
      ( \elems (reparr1, scatter_offset, n, ii1, ii2) -> do
          letExp "irregular_scatter_elems" <=< genScatter lvl elems n $ \gid -> do
            -- Which segment we are in.
            segment_i <-
              letSubExp "segment_i" =<< eIndex ii1 [eSubExp gid]

            -- Get segment offset in final array
            segment_o <-
              letSubExp "segment_o" =<< eIndex ns_full_O [eSubExp segment_i]

            -- Get local segment offset
            segment_local_o <-
              letSubExp "segment_local_o"
                =<< eIndex scatter_offset [eSubExp segment_i]

            o' <- letSubExp "o" =<< eIndex ii2 [eSubExp gid]
            src_segment_o <-
              letSubExp "src_segment_o" =<< eIndex (irregularO reparr1) [eSubExp segment_i]
            src_i <-
              letSubExp "src_i" <=< toExp $ pe64 src_segment_o + pe64 o'
            v' <-
              letSubExp "v" =<< eIndex (irregularD reparr1) [eSubExp src_i]

            -- Index to write `v'` at
            i <-
              letExp "i" =<< toExp (pe64 o' + pe64 segment_local_o + pe64 segment_o)

            pure (i, v')
      )
      elems_blank
      $ L.zip5 reparr scatter_offsets_T n_arr rep_II1 rep_II2

  pure $
    IrregularRep
      { irregularS = ns_full,
        irregularF = ns_full_F,
        irregularO = ns_full_O,
        irregularD = elems,
        irregularK = Dense
      }

-- We also can do reearange -> concat -> rearrange but this should be more efficient
concatIrregAlongDim ::
  SegLevel ->
  Segments ->
  DistEnv ->
  VName ->
  [IrregularRep] ->
  [Type] ->
  DistInputs ->
  Int ->
  FlattenM IrregularRep
concatIrregAlongDim lvl segments env ns rep_arr type_arr inps d = do
  num_segments <- arraySize 0 <$> lookupType ns

  ns_full <- letExp (baseName ns <> "_full") <=< segMap lvl (MkSolo num_segments) $
    \(MkSolo i) -> do
      old_segments <-
        forM rep_arr $ \rep ->
          letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      new_segment <-
        letSubExp "new_segment"
          =<< toExp (sum $ map pe64 old_segments)
      pure $ subExpsRes [new_segment]

  (ns_full_F, ns_full_O, _ns_II1) <- doRepIota lvl ns_full

  repIota <- mapM (doRepIota lvl . irregularS) rep_arr
  segIota <- mapM (doSegIota lvl . irregularS) rep_arr

  let (_, _, rep_II1) = unzip3 repIota
  let (_, _, rep_II2) = unzip3 segIota

  n_arr <- mapM (fmap (arraySize 0) . lookupType) rep_II1

  scatter_info <-
    letTupExp "irregular_scatter_offsets" <=< segMap lvl (MkSolo num_segments) $
      \(MkSolo i) -> do
        seg_is <- segmentCoordsFromFlat segments i

        block_sizes <-
          forM type_arr $ \t -> do
            v_dims <- readTypeDims segments env seg_is inps t
            letSubExp "block_size" =<< toExp (product $ map pe64 $ drop d v_dims)

        let scanned = scanl (+) 0 $ map pe64 block_sizes   
        sum_prefix   <- mapM (letSubExp "segment_prefix" <=< toExp) (init scanned)
        total_block <- letSubExp "total_block" =<< toExp (last scanned)

        pure $ subExpsRes (block_sizes <> sum_prefix <> [total_block])

  let k = length type_arr
      (scatter_blocks, rest) = splitAt k scatter_info
      (scatter_offsets, [total_block_size]) = splitAt k rest

  m <- arraySize 0 <$> lookupType ns_full_F
  data_t <- lookupType (irregularD (head rep_arr))
  let pt = elemType data_t
  let result_type = Array pt (Shape [m]) NoUniqueness
  elems_blank <- letExp "blank_res" =<< eBlank result_type

  -- Scatter data into result array
  elems <-
    foldlM
      ( \elems (reparr1, scatter_block, scatter_offset, n, ii1, ii2) -> do
          letExp "irregular_scatter_elems" <=< genScatter lvl elems n $ \gid -> do
            -- Which segment we are in.
            segment_i <-
              letSubExp "segment_i" =<< eIndex ii1 [eSubExp gid]

            -- Get segment offset in final array
            segment_o <-
              letSubExp "segment_o" =<< eIndex ns_full_O [eSubExp segment_i]

            -- Get local segment offset
            segment_local_o <-
              letSubExp "segment_local_o"
                =<< eIndex scatter_offset [eSubExp segment_i]

            o' <- letSubExp "o" =<< eIndex ii2 [eSubExp gid]
            src_segment_o <-
              letSubExp "src_segment_o" =<< eIndex (irregularO reparr1) [eSubExp segment_i]
            src_i <-
              letSubExp "src_i" <=< toExp $ pe64 src_segment_o + pe64 o'
            v' <-
              letSubExp "v" =<< eIndex (irregularD reparr1) [eSubExp src_i]

            scatter_block_size <-
              letSubExp "scatter_block_size" =<< eIndex scatter_block [eSubExp segment_i]

            scatter_total_block_size <-
              letSubExp "scatter_total_block_size" =<< eIndex total_block_size [eSubExp segment_i]

            outer_i <-
              letSubExp "outer_i" =<< toExp (pe64 o' `div` pe64 scatter_block_size)

            i <-
              letExp "i"
                =<< toExp
                  ( pe64 o'
                      + pe64 outer_i * (pe64 scatter_total_block_size - pe64 scatter_block_size)
                      + pe64 segment_local_o
                      + pe64 segment_o
                  )
            pure (i, v')
      )
      elems_blank
      $ L.zip6 rep_arr scatter_blocks scatter_offsets n_arr rep_II1 rep_II2

  pure $
    IrregularRep
      { irregularS = ns_full,
        irregularF = ns_full_F,
        irregularO = ns_full_O,
        irregularD = elems,
        irregularK = Dense
      }

-- Do 'map2 replicate ns A', where 'A' is an irregular array (and so
-- is the result, obviously).
replicateIrreg ::
  SegLevel ->
  Segments ->
  DistEnv ->
  VName ->
  Name ->
  IrregularRep ->
  FlattenM IrregularRep
replicateIrreg lvl _segments _env ns desc rep = do
  -- Replication does not change the number of segments - it simply
  -- makes each of them larger.

  num_segments <- arraySize 0 <$> lookupType ns

  -- ns multipled with existing segment sizes.
  ns_full <- letExp (baseName ns <> "_full") <=< segMap lvl (MkSolo num_segments) $
    \(MkSolo i) -> do
      n <-
        letSubExp "n" =<< eIndex ns [eSubExp i]
      old_segment <-
        letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      full_segment <-
        letSubExp "new_segment" =<< toExp (pe64 n * pe64 old_segment)
      pure $ subExpsRes [full_segment]

  (ns_full_F, ns_full_O, ns_full_D) <- doRepIota lvl ns_full
  (_, _, flat_to_segs) <- doSegIota lvl ns_full

  w <- arraySize 0 <$> lookupType ns_full_D

  elems <- letExp (desc <> "_rep_D") <=< segMap lvl (MkSolo w) $ \(MkSolo i) -> do
    -- Which segment we are in.
    segment_i <-
      letSubExp "segment_i" =<< eIndex ns_full_D [eSubExp i]
    -- Size of original segment.
    old_segment <-
      letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp segment_i]
    -- Index of value inside *new* segment.
    j_new <-
      letSubExp "j_new" =<< eIndex flat_to_segs [eSubExp i]
    -- Index of value inside *old* segment.
    j_old <-
      letSubExp "j_old" =<< toExp (pe64 j_new `rem` pe64 old_segment)
    -- Offset of values in original segment.
    offset <-
      letSubExp "offset" =<< eIndex (irregularO rep) [eSubExp segment_i]
    v <-
      letSubExp "v"
        =<< eIndex (irregularD rep) [toExp $ pe64 offset + pe64 j_old]
    pure $ subExpsRes [v]

  pure $
    IrregularRep
      { irregularS = ns_full,
        irregularF = ns_full_F,
        irregularO = ns_full_O,
        irregularD = elems,
        irregularK = Dense
      }

rearrangeFlat :: (IntegralExp num) => [Int] -> [num] -> num -> num
rearrangeFlat perm dims i =
  flattenIndex dims $
    rearrangeShape (rearrangeInverse perm) $
      unflattenIndex (rearrangeShape perm dims) i

segmentCoordsFromFlat :: Segments -> SubExp -> FlattenM [SubExp]
segmentCoordsFromFlat segments seg_i =
  mapM (letSubExp "seg_coord" <=< toExp) $
    unflattenIndex (map pe64 $ shapeDims $ segmentsShape segments) (pe64 seg_i)

-- TODO: We do not need to actully make this Dense
rearrangeIrreg ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  TypeBase Shape u ->
  [Int] ->
  IrregularRep ->
  FlattenM IrregularRep
rearrangeIrreg lvl segments env inps v_t perm ir = do
  (IrregularRep shape _ offsets elems _) <- flattenIrregularRep lvl ir
  (new_F, new_O, ii1_vss) <- doRepIota lvl shape
  (_, _, ii2_vss) <- doSegIota lvl shape
  m <- arraySize 0 <$> lookupType ii1_vss
  elems' <- letExp "elems_rearrange" <=< renameExp <=< segMap lvl (MkSolo m) $
    \(MkSolo i) -> do
      seg_i <- letSubExp "seg_i" =<< eIndex ii1_vss [eSubExp i]
      offset <- letSubExp "offset" =<< eIndex offsets [eSubExp seg_i]
      in_seg_i <- letSubExp "in_seg_i" =<< eIndex ii2_vss [eSubExp i]
      seg_is <- segmentCoordsFromFlat segments seg_i
      v_dims <- readTypeDims segments env seg_is inps v_t
      let v_dims' = map pe64 v_dims
          in_seg_is_tr = rearrangeFlat perm v_dims' $ pe64 in_seg_i
      v' <-
        letSubExp "v"
          =<< eIndex elems [toExp $ pe64 offset + in_seg_is_tr]
      pure [subExpRes v']
  pure $
    IrregularRep
      { irregularS = shape,
        irregularF = new_F,
        irregularO = new_O,
        irregularD = elems',
        irregularK = Dense
      }

transformDistBasicOp ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  ( DistInputs,
    DistResult,
    PatElem Type,
    StmAux (),
    BasicOp
  ) ->
  FlattenM DistEnv
transformDistBasicOp ops segments env (inps, res, pe, aux, e) =
  case e of
    BinOp {} ->
      scalarCase
    CmpOp {} ->
      scalarCase
    ConvOp {} ->
      scalarCase
    UnOp {} ->
      scalarCase
    Assert {} ->
      scalarCase
    -- Potentially no need for this
    ArrayLit [] row_type
      | not $ any (isVariant inps) (arrayDims row_type) -> do
          let resultType =
                Array
                  (elemType row_type)
                  (segmentsShape segments <> Shape [intConst Int64 0] <> arrayShape row_type)
                  NoUniqueness
          v <- letExp "arraylit_empty_reg" =<< eBlank resultType
          pure $ insertRegulars [distResTag res] [v] env
      | otherwise -> do
          ns <- dataArr lvl segments env inps $ intConst Int64 0
          (flags, offsets, _elems) <- doRepIota lvl ns
          let resultType = Array (elemType row_type) (Shape [intConst Int64 0]) NoUniqueness
          elems <- letExp "arraylit_empty_elems" =<< eBlank resultType
          pure $ insertIrregular ns flags offsets (distResTag res) elems Dense env
    ArrayLit vs row_type
      | not $ any (isVariant inps) (arrayDims row_type) -> do
          res_v <-
            if any (isVariant inps) vs
              then do
                let seg_shape = segmentsShape segments
                    one = intConst Int64 1
                    arr_outer_dim = intConst Int64 $ toInteger $ length vs
                    expected = seg_shape <> arrayShape row_type
                    stacked = seg_shape <> Shape [one] <> arrayShape row_type
                    d = segmentsRank segments

                vs_reg <- mapM (liftSubExpRegular lvl segments inps env expected) vs

                vs_reg_1 <-
                  forM vs_reg $ \v -> do
                    v_t <- lookupType v
                    letExp (baseName v <> "_stack") $
                      BasicOp $
                        Reshape v $
                          reshapeAll (arrayShape v_t) stacked

                case vs_reg_1 of
                  [] -> undefined
                  [v] ->
                    pure v
                  v : vs' ->
                    letExp "arraylit_reg" $ BasicOp $ Concat d (v NE.:| vs') arr_outer_dim
              else do
                base_v <- letExp "arraylit_base" $ BasicOp $ ArrayLit vs row_type
                letExp "arraylit_reg" $
                  BasicOp $
                    Replicate (segmentsShape segments) (Var base_v)
          pure $ insertRegulars [distResTag res] [res_v] env
      | otherwise -> do
          let arr_outer_dim = intConst Int64 $ fromIntegral $ length vs
          vs_reparr <- mapM (dataArr lvl segments env inps) vs
          dim_arrs <- mapM (dataArr lvl segments env inps) (arrayDims row_type)
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          ~[row_size, full_size] <- letTupExp "arraylit_row_size" <=< segMap lvl (MkSolo num_segments) $ \(MkSolo i) -> do
            vals <- mapM (\dim_arr -> letSubExp "dim_i" =<< eIndex dim_arr [eSubExp i]) dim_arrs
            n <- letSubExp "n" <=< toExp $ product $ map pe64 vals
            fs <- letSubExp "fs" <=< toExp $ pe64 n * pe64 arr_outer_dim
            pure $ subExpsRes [n, fs]

          (_, _, row_II1) <- doRepIota lvl row_size
          (_, _, row_II2) <- doSegIota lvl row_size

          row_flat_size <- arraySize 0 <$> lookupType row_II1

          (full_flags, full_offset, full_II1) <- doRepIota lvl full_size

          m <- arraySize 0 <$> lookupType full_II1
          let pt = elemType row_type
          let resultType = Array pt (Shape [m]) NoUniqueness
          elems_blank <- letExp "blank_res" =<< eBlank resultType

          elems <-
            foldlM
              ( \elems (var_num, arr) -> do
                  letExp "irregular_scatter_elems" <=< genScatter lvl elems row_flat_size $ \gid -> do
                    -- Which segment we are in.
                    segment_i <-
                      letSubExp "segment_i" =<< eIndex row_II1 [eSubExp gid]

                    row_size_i <-
                      letSubExp "row_size_i" =<< eIndex row_size [eSubExp segment_i]

                    segment_global_o <-
                      letSubExp "segment_global_o"
                        =<< eIndex full_offset [eSubExp segment_i]

                    v' <-
                      letSubExp "v" =<< eIndex arr [eSubExp gid]

                    o' <- letSubExp "o" =<< eIndex row_II2 [eSubExp gid]

                    i <-
                      letExp "i"
                        =<< toExp
                          ( pe64 o'
                              + pe64 segment_global_o
                              + pe64 row_size_i * pe64 (intConst Int64 var_num)
                          )

                    pure (i, v')
              )
              elems_blank
              $ zip [0 ..] vs_reparr

          pure $ insertIrregular full_size full_flags full_offset (distResTag res) elems Dense env
    ArrayVal vs row_type -> do
      base_v <- letExp "arraylit_base" $ BasicOp $ ArrayVal vs row_type
      res_v <- letExp "arraylit_reg" $ BasicOp $ Replicate (segmentsShape segments) (Var base_v)
      pure $ insertRegulars [distResTag res] [res_v] env

    Opaque _op se
      | Var v <- se,
        Just (DistInput rt_in _) <- lookup v inps ->
          -- TODO: actually insert opaques
          pure $ insertRep (distResTag res) (resVar rt_in env) env
      | otherwise ->
          scalarCase
    Reshape arr reshape
      | isRegularDistResult res,
        not (any (isVariant inps) reshape) -> do
          let outer = segmentsShape segments
              inner_target = newShape reshape
              reshape' = reshapeCoerce outer <> newshapeInner outer reshape

          arr_t <- lookupInputType inps arr
          let arr_shape = arrayShape arr_t
          let unform_arr = not (any (isVariant inps) arr_shape)
          if unform_arr
            then do
              arr' <-
                liftSubExpRegular
                  lvl
                  segments
                  inps
                  env
                  (outer <> arr_shape)
                  (Var arr)
              v <- certifying (distCerts inps aux env) . letExp "reshape_reg" . BasicOp $ Reshape arr' reshape'
              pure $ insertRegulars [distResTag res] [v] env
            else do
              arr' <-
                liftSubExpRegular
                  lvl
                  segments
                  inps
                  env
                  (outer <> inner_target)
                  (Var arr)
              pure $ insertRegulars [distResTag res] [arr'] env
      | otherwise -> do
          irreg_v <- getIrregRep lvl segments env inps arr
          pure $ insertRep (distResTag res) (Irregular irreg_v) env
    Index arr slice
      | isRegularDistResult res,
        not (any (isVariant inps) slice) -> do
          arr_t <- lookupInputType inps arr
          arr' <-
            liftSubExpRegular
              lvl
              segments
              inps
              env
              (segmentsShape segments <> arrayShape arr_t)
              (Var arr)
          let segmentSlice = map sliceDim . shapeDims . segmentsShape
          v <-
            certifying (distCerts inps aux env) . letExp "index_reg" . BasicOp $
              Index arr' (Slice $ segmentSlice segments <> unSlice slice)
          pure $ insertRegulars [distResTag res] [v] env
      | otherwise -> do
          -- Maximally irregular case.
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          ns <- letExp "slice_sizes" <=< renameExp <=< segMap lvl (MkSolo num_segments) $ \(MkSolo segment) -> do
            segment_is <- segmentCoordsFromFlat segments segment
            slice_ns <- mapM (readInput segments env segment_is inps) $ sliceDims slice
            fmap varsRes . letTupExp "n" <=< toExp $ product $ map pe64 slice_ns
          (_n, offsets, m) <- exScanAndSum lvl ns
          (_, _, repiota_D) <- doRepIota lvl ns
          flags <- genFlags lvl m offsets
          elems <- letExp "elems" <=< renameExp <=< segMap lvl (NE.singleton m) $ \is -> do
            segment <- letSubExp "segment" =<< eIndex repiota_D (toList $ fmap eSubExp is)
            segment_start <- letSubExp "segment_start" =<< eIndex offsets [eSubExp segment]
            segment_is <- segmentCoordsFromFlat segments segment
            readInputs segments env segment_is inps
            let slice' =
                  fixSlice (fmap pe64 slice) $
                    unflattenIndex (map pe64 (sliceDims slice)) $
                      subtract (pe64 segment_start) . pe64 $
                        NE.head is
            auxing aux $
              fmap (subExpsRes . pure) . letSubExp "v"
                =<< eIndex arr (map toExp slice')
          pure $ insertIrregular ns flags offsets (distResTag res) elems Dense env
    FlatIndex arr flat_slice
      | isRegularDistResult res,
        not (any (isVariant inps) flat_slice) -> do
          arr_t <- lookupInputType inps arr
          -- arr should be 1D
          let [n] = arrayDims arr_t
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          arr_flat_size <- letSubExp "arr_flat_size" =<< toExp (pe64 num_segments * pe64 n)
          let arr_lift_shape = segmentsShape segments <> arrayShape arr_t
              arr_flat_shape = Shape [arr_flat_size]
          arr' <-
            liftSubExpRegular
              lvl
              segments
              inps
              env
              arr_lift_shape
              (Var arr)
          arr'_flat <-
            letExp (baseName arr <> "_reshaped") $ BasicOp $ Reshape arr' $ reshapeAll arr_lift_shape arr_flat_shape
          let FlatSlice off dims = flat_slice
              flat_slice' = FlatSlice off (FlatDimIndex num_segments n : dims)
          out_flat_updated <-
            certifying (distCerts inps aux env) . letExp "flat_index_reg" . BasicOp $
              FlatIndex arr'_flat flat_slice'
          out_updated <-
            letExp "flat_index_reg_reshaped" $
              BasicOp $
                Reshape out_flat_updated $
                  reshapeAll arr_flat_shape arr_lift_shape
          pure $ insertRegulars [distResTag res] [out_updated] env
      | otherwise -> do
          -- Maximally irregular case.
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          ns <- letExp "slice_sizes" <=< renameExp <=< segMap lvl (MkSolo num_segments) $ \(MkSolo segment) -> do
            segment_is <- segmentCoordsFromFlat segments segment
            slice_ns <- mapM (readInput segments env segment_is inps) $ flatSliceDims flat_slice
            fmap varsRes . letTupExp "n" <=< toExp $ product $ map pe64 slice_ns
          (_n, offsets, m) <- exScanAndSum lvl ns
          (_, _, repiota_D) <- doRepIota lvl ns
          flags <- genFlags lvl m offsets
          elems <- letExp "elems" <=< renameExp <=< segMap lvl (NE.singleton m) $ \is -> do
            segment <- letSubExp "segment" =<< eIndex repiota_D (toList $ fmap eSubExp is)
            segment_start <- letSubExp "segment_start" =<< eIndex offsets [eSubExp segment]
            segment_is <- segmentCoordsFromFlat segments segment
            readInputs segments env segment_is inps
            let flat_slice'@(FlatSlice flat_offset _) = fmap pe64 flat_slice
                local_flat = pe64 (NE.head is) - pe64 segment_start
                local_is = unflattenIndex (flatSliceDims flat_slice') local_flat
                flat_i = flat_offset + sum (zipWith (*) local_is (flatSliceStrides flat_slice'))
            auxing aux $
              fmap (subExpsRes . pure) . letSubExp "v"
                =<< eIndex arr [toExp flat_i]
          pure $ insertIrregular ns flags offsets (distResTag res) elems Dense env
    Iota n (Constant x) (Constant s) Int64
      | zeroIsh x,
        oneIsh s -> do
          ns <- dataArr lvl segments env inps n
          (flags, offsets, elems) <- certifying (distCerts inps aux env) $ doSegIota lvl ns
          pure $ insertIrregular ns flags offsets (distResTag res) elems Dense env
    Iota n x s it -> do
      ns <- dataArr lvl segments env inps n
      xs <- dataArr lvl segments env inps x
      ss <- dataArr lvl segments env inps s
      (res_F, res_O, res_D) <- certifying (distCerts inps aux env) $ doSegIota lvl ns
      (_, _, repiota_D) <- doRepIota lvl ns
      m <- arraySize 0 <$> lookupType res_D
      res_D' <- letExp "iota_D_fixed" <=< segMap lvl (MkSolo m) $ \(MkSolo i) -> do
        segment <- letSubExp "segment" =<< eIndex repiota_D [eSubExp i]
        v' <- letSubExp "v" =<< eIndex res_D [eSubExp i]
        x' <- letSubExp "x" =<< eIndex xs [eSubExp segment]
        s' <- letSubExp "s" =<< eIndex ss [eSubExp segment]
        fmap (subExpsRes . pure) . letSubExp "v" <=< toExp $
          primExpFromSubExp (IntType it) x'
            ~+~ sExt it (untyped (pe64 v'))
            ~*~ primExpFromSubExp (IntType it) s'
      pure $ insertIrregular ns res_F res_O (distResTag res) res_D' Dense env
    Concat d arr shp -> do
      arr_ts <- mapM (lookupInputType inps) (NE.toList arr)
      let inputShapeUniform t =
            not $ any (isVariant inps) (arrayDims t)
      if isRegularDistResult res
        && not (isVariant inps shp)
        && all inputShapeUniform arr_ts
        then do
          --  Unifrom Concat
          arrs_lifted <-
            forM (zip (NE.toList arr) arr_ts) $ \(v, t) -> do
              let expectedShape = segmentsShape segments <> arrayShape t
              liftSubExpRegular lvl segments inps env expectedShape (Var v)
          v' <-
            letExp "concat_reg" $
              BasicOp $
                Concat
                  (segmentsRank segments + d)
                  (NE.fromList arrs_lifted)
                  shp

          pure $ insertRegulars [distResTag res] [v'] env
        else do
          ns <- dataArr lvl segments env inps shp
          reparr <- mapM (getIrregRep lvl segments env inps) (NE.toList arr)
          rep' <- case d of
            0 -> concatIrreg lvl segments env ns reparr
            d' -> do
              concatIrregAlongDim lvl segments env ns reparr arr_ts inps d'
          pure $ insertRep (distResTag res) (Irregular rep') env

    --  Unifrom Replicate
    Replicate (Shape dims) se
      | isRegularDistResult res -> do
          t <- subExpInputType inps se
          let expectedShape = segmentsShape segments <> arrayShape t
          lifted <- liftSubExpRegular lvl segments inps env expectedShape se
          v_rep <- replicateForDims segments (Shape dims) lifted
          pure $ insertRegulars [distResTag res] [v_rep] env
    Replicate (Shape [n]) (Var v) -> do
      ns <- dataArr lvl segments env inps n
      rep <- getIrregRep lvl segments env inps v
      rep' <- replicateIrreg lvl segments env ns (baseName v) rep
      pure $ insertRep (distResTag res) (Irregular rep') env
    Replicate (Shape [n]) (Constant v) -> do
      ns <- dataArr lvl segments env inps n
      (res_F, res_O, res_D) <-
        certifying (distCerts inps aux env) $ doSegIota lvl ns
      w <- arraySize 0 <$> lookupType res_D
      res_D' <- letExp "rep_const" $ BasicOp $ Replicate (Shape [w]) (Constant v)
      pure $ insertIrregular ns res_F res_O (distResTag res) res_D' Dense env
    Replicate (Shape dims) (Constant v) -> do
      dim_arrs <- mapM (dataArr lvl segments env inps) dims
      seg_number <- arraySize 0 <$> lookupType (head dim_arrs)
      mul_dims <- letExp "mul_dims" <=< segMap lvl (MkSolo seg_number) $ \(MkSolo i) -> do
        vals <- mapM (\dim_arr -> letSubExp "dim_i" =<< eIndex dim_arr [eSubExp i]) dim_arrs
        n <- letSubExp "n" <=< toExp $ product $ map pe64 vals
        pure [subExpRes n]
      (res_F, res_O, res_D) <-
        certifying (distCerts inps aux env) $ doSegIota lvl mul_dims
      w <- arraySize 0 <$> lookupType res_D
      res_D' <- letExp "rep_const" $ BasicOp $ Replicate (Shape [w]) (Constant v)
      pure $ insertIrregular mul_dims res_F res_O (distResTag res) res_D' Dense env
    Replicate (Shape []) (Var v) ->
      case lookup v inps of
        Just (DistInputFree v' _) -> do
          v'' <-
            letExp (baseName v' <> "_copy") . BasicOp $
              Replicate mempty (Var v')
          pure $ insertRegulars [distResTag res] [v''] env
        Just (DistInput rt _) ->
          case resVar rt env of
            Irregular r -> do
              let name = baseName (irregularD r) <> "_copy"
              elems_copy <-
                letExp name . BasicOp $
                  Replicate mempty (Var $ irregularD r)
              let rep = Irregular $ r {irregularD = elems_copy}
              pure $ insertRep (distResTag res) rep env
            Regular v' -> do
              v'' <-
                letExp (baseName v' <> "_copy") . BasicOp $
                  Replicate mempty (Var v')
              pure $ insertRegulars [distResTag res] [v''] env
        Nothing -> do
          v' <-
            letExp (baseName v <> "_copy_free") . BasicOp $
              Replicate (segmentsShape segments) (Var v)
          pure $ insertRegulars [distResTag res] [v'] env
    Replicate (Shape dims) (Var v) -> do
      dim_arrs <- mapM (dataArr lvl segments env inps) dims
      seg_number <- arraySize 0 <$> lookupType (head dim_arrs)
      mul_dims <- letExp "mul_dims" <=< segMap lvl (MkSolo seg_number) $ \(MkSolo i) -> do
        vals <- mapM (\dim_arr -> letSubExp "dim_i" =<< eIndex dim_arr [eSubExp i]) dim_arrs
        n <- letSubExp "n" <=< toExp $ product $ map pe64 vals
        pure [subExpRes n]
      rep <- getIrregRep lvl segments env inps v
      rep' <- replicateIrreg lvl segments env mul_dims (baseName v) rep
      pure $ insertRep (distResTag res) (Irregular rep') env
    
    Manifest v perm
      | isRegularDistResult res -> do
          t <- lookupInputType inps v
          v_lifted <-
            liftSubExpRegular
              lvl
              segments
              inps
              env
              (segmentsShape segments <> arrayShape t)
              (Var v)
          let segment_rank = segmentsRank segments
          v_manifest <- letExp (baseName v <> "_manifest") . BasicOp $ Manifest v_lifted ([0 .. segment_rank - 1] ++ map (+ segment_rank) perm)
          pure $ insertRegulars [distResTag res] [v_manifest] env
      | otherwise -> do
          irreg <- getIrregRep lvl segments env inps v
          irreg_dense <- ensureDenseIrregular lvl (baseName v <> "_manifest") irreg
          elems_copy <-
            letExp (baseName (irregularD irreg_dense) <> "_manifest") . BasicOp $
              Replicate mempty (Var $ irregularD irreg_dense)
          pure $
            insertRep
              (distResTag res)
              (Irregular $ irreg_dense {irregularD = elems_copy})
              env
    Update safety as slice se
      -- Uniform Update
      | Just as_t <- distInputType <$> lookup as inps,
        isRegularDistResult res,
        not (any (isVariant inps) slice) -> do
          as' <-
            liftSubExpRegular
              lvl
              segments
              inps
              env
              (segmentsShape segments <> arrayShape as_t)
              (Var as)
          se' <-
            liftSubExpRegular
              lvl
              segments
              inps
              env
              (segmentsShape segments <> sliceShape slice)
              se
          let segmentSlice = map sliceDim . shapeDims . segmentsShape
          v <-
            certifying (distCerts inps aux env) . letExp "update_reg" . BasicOp $
              Update safety as' (Slice $ segmentSlice segments <> unSlice slice) (Var se')
          pure $ insertRegulars [distResTag res] [v] env
      | Just as_t <- distInputType <$> lookup as inps -> do
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          ns <- letExp "slice_sizes"
            <=< renameExp
            <=< segMap lvl (MkSolo num_segments)
            $ \(MkSolo seg_i) -> do
              seg_is <- segmentCoordsFromFlat segments seg_i
              readInputs segments env seg_is $
                filter ((`elem` sliceDims slice) . Var . fst) inps
              slice_dims <- mapM (readInput segments env seg_is inps) $ sliceDims slice
              n <- letSubExp "n" <=< toExp $ product $ map pe64 slice_dims
              pure [subExpRes n]
          -- Irregular representation of `as`
          as_rep <- getIrregRep lvl segments env inps as
          IrregularRep shape flags offsets elems _ <-
            ensureDenseIrregular lvl (baseName as <> "_update") as_rep
          -- Inner indices (1 and 2) of `ns`
          (_, _, ii1_vss) <- doRepIota lvl ns
          (_, _, ii2_vss) <- certifying (distCerts inps aux env) $ doSegIota lvl ns
          -- Number of updates to perform
          m <- arraySize 0 <$> lookupType ii2_vss
          elems' <- letExp "elems_scatter" <=< renameExp <=< genScatter lvl elems m $ \gid -> do
            seg_i <- letSubExp "seg_i" =<< eIndex ii1_vss [eSubExp gid]
            in_seg_i <- letSubExp "in_seg_i" =<< eIndex ii2_vss [eSubExp gid]
            seg_is <- segmentCoordsFromFlat segments seg_i
            readInputs segments env seg_is $ filter ((/= as) . fst) inps
            as_dims <- readTypeDims segments env seg_is inps as_t
            slice_dims <- mapM (readInput segments env seg_is inps) $ sliceDims slice
            case se of
              Var v -> do
                let in_seg_is =
                      unflattenIndex (map pe64 slice_dims) (pe64 in_seg_i)
                    slice' = fmap pe64 slice
                    flat_i =
                      flattenIndex
                        (map pe64 as_dims)
                        (fixSlice slice' in_seg_is)
                -- Value to write
                v' <- letSubExp "v" =<< eIndex v (map toExp in_seg_is)
                o' <- letSubExp "o" =<< eIndex offsets [eSubExp seg_i]
                -- Index to write `v'` at
                i <- letExp "i" =<< toExp (pe64 o' + flat_i)
                pure (i, v')
              Constant c -> do
                let slice' = fmap pe64 slice
                    flat_i = flattenIndex (map pe64 as_dims) (fixSlice slice' [])
                o' <- letSubExp "o" =<< eIndex offsets [eSubExp seg_i]
                i <- letExp "i" =<< toExp (pe64 o' + flat_i)
                pure (i, Constant c)
          pure $ insertIrregular shape flags offsets (distResTag res) elems' Dense env
      | otherwise ->
          error "Flattening update: destination is not input."
    FlatUpdate as flat_slice v
      -- Uniform Flat Update
      | Just as_t <- distInputType <$> lookup as inps,
        isRegularDistResult res,
        not (any (isVariant inps) flat_slice) -> do
          -- as should be 1D
          let [n] = arrayDims as_t
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          as_flat_size <- letSubExp "as_flat_size" =<< toExp (pe64 num_segments * pe64 n)

          let se_shape = Shape $ flatSliceDims flat_slice
              as_lift_shape = segmentsShape segments <> arrayShape as_t
              se_lift_shape = segmentsShape segments <> se_shape
              se_flat_shape = Shape [num_segments] <> se_shape
              as_flat_shape = Shape [as_flat_size]
          as' <-
            liftSubExpRegular
              lvl
              segments
              inps
              env
              as_lift_shape
              (Var as)
          v' <-
            liftSubExpRegular
              lvl
              segments
              inps
              env
              se_lift_shape
              (Var v)
          as_flat <-
            letExp (baseName as <> "_reshaped") $ BasicOp $ Reshape as' $ reshapeAll as_lift_shape as_flat_shape
          v_flat <-
            letExp (baseName v <> "_reshaped") $ BasicOp $ Reshape v' $ reshapeAll se_lift_shape se_flat_shape
          let FlatSlice off dims = flat_slice
              flat_slice' = FlatSlice off (FlatDimIndex num_segments n : dims)
          out_flat_updated <-
            certifying (distCerts inps aux env) . letExp "flat_update_reg" . BasicOp $
              FlatUpdate as_flat flat_slice' v_flat
          out_updated <-
            letExp "flat_update_reg_reshaped" $
              BasicOp $
                Reshape out_flat_updated $
                  reshapeAll as_flat_shape as_lift_shape
          pure $ insertRegulars [distResTag res] [out_updated] env
      | Just _ <- distInputType <$> lookup as inps -> do
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          ns <- letExp "slice_sizes"
            <=< renameExp
            <=< segMap lvl (MkSolo num_segments)
            $ \(MkSolo seg_i) -> do
              seg_is <- segmentCoordsFromFlat segments seg_i
              readInputs segments env seg_is $
                filter ((`elem` flatSliceDims flat_slice) . Var . fst) inps
              slice_dims <- mapM (readInput segments env seg_is inps) $ flatSliceDims flat_slice
              n <- letSubExp "n" <=< toExp $ product $ map pe64 slice_dims
              pure [subExpRes n]
          -- Irregular representation of `as`
          as_rep <- getIrregRep lvl segments env inps as
          IrregularRep shape flags offsets elems _ <-
            ensureDenseIrregular lvl (baseName as <> "_update") as_rep
          -- Inner indices (1 and 2) of `ns`
          (_, _, ii1_vss) <- doRepIota lvl ns
          (_, _, ii2_vss) <- certifying (distCerts inps aux env) $ doSegIota lvl ns
          -- Number of updates to perform
          m <- arraySize 0 <$> lookupType ii2_vss
          elems' <- letExp "elems_scatter" <=< renameExp <=< genScatter lvl elems m $ \gid -> do
            seg_i <- letSubExp "seg_i" =<< eIndex ii1_vss [eSubExp gid]
            in_seg_i <- letSubExp "in_seg_i" =<< eIndex ii2_vss [eSubExp gid]
            seg_is <- segmentCoordsFromFlat segments seg_i
            readInputs segments env seg_is $ filter ((/= as) . fst) inps
            let slice_dims = flatSliceDims flat_slice
                flat_stride = flatSliceStrides flat_slice
                (FlatSlice flat_offset _) = fmap pe64 flat_slice
                in_seg_is =
                  unflattenIndex (map pe64 slice_dims) (pe64 in_seg_i)
                flat_i = flat_offset + sum (zipWith (*) in_seg_is (map pe64 flat_stride))
            -- Value to write
            v' <- letSubExp "v" =<< eIndex v (map toExp in_seg_is)
            o' <- letSubExp "o" =<< eIndex offsets [eSubExp seg_i]
            -- Index to write `v'` at
            i <- letExp "i" =<< toExp (pe64 o' + flat_i)
            pure (i, v')
          pure $ insertIrregular shape flags offsets (distResTag res) elems' Dense env
      | otherwise ->
          error "Flattening update: destination is not input."
    Rearrange v perm
      | isRegularDistResult res -> do
          t <- lookupInputType inps v
          v_lifted <-
            liftSubExpRegular
              lvl
              segments
              inps
              env
              (segmentsShape segments <> arrayShape t)
              (Var v)
          let segment_rank = segmentsRank segments
          v_rearrange <- letExp (baseName v <> "_tr") . BasicOp $ Rearrange v_lifted ([0 .. segment_rank - 1] ++ map (+ segment_rank) perm)
          pure $ insertRegulars [distResTag res] [v_rearrange] env
      | otherwise -> do
          irreg <- getIrregRep lvl segments env inps v
          -- TODO: Maybe we can avoid this?
          t <- lookupInputType inps v
          rep' <-
            certifying (distCerts inps aux env) $
              rearrangeIrreg lvl segments env inps t perm irreg
          pure $ insertRep (distResTag res) (Irregular rep') env
    Scratch pt dims
      | not $ any (isVariant inps) dims -> do
          -- All dims are invariant result is regular across segments.
          v' <-
            letExp "scratch" . BasicOp $
              Scratch pt (shapeDims (segmentsShape segments) ++ dims)
          pure $ insertRegulars [distResTag res] [v'] env
      | [n] <- dims -> do
          ns <- dataArr lvl segments env inps n
          (_n, offsets, m) <- exScanAndSum lvl ns
          flags <- genFlags lvl m offsets
          res_D <- letExp "scratch_D" $ BasicOp $ Scratch pt [m]
          pure $ insertIrregular ns flags offsets (distResTag res) res_D Dense env
      | otherwise -> do
          dim_arrs <- mapM (dataArr lvl segments env inps) dims
          w <- arraySize 0 <$> lookupType (head dim_arrs)
          ns <- letExp "scratch_sizes" <=< segMap lvl (MkSolo w) $ \(MkSolo i) -> do
            vals <- mapM (\arr -> letSubExp "d" =<< eIndex arr [eSubExp i]) dim_arrs
            n <- letSubExp "n" <=< toExp $ product $ map pe64 vals
            pure [subExpRes n]
          (_n, offsets, m) <- exScanAndSum lvl ns
          flags <- genFlags lvl m offsets
          res_D <- letExp "scratch_D" $ BasicOp $ Scratch pt [m]
          pure $ insertIrregular ns flags offsets (distResTag res) res_D Dense env
    UpdateAcc {} ->
      -- TODO: handle irregular case, which is however rare, and also needs
      -- modifications to WithAcc. The only irregularity that is possible is in
      -- the values to be written.
      scalarCase
    _ -> error $ "Unhandled BasicOp:\n" ++ prettyString e
  where
    lvl = flattenSegLevel ops
    scalarCase =
      flattenScalarStm ops segments env inps [res] $
        Let (Pat [pe]) aux (BasicOp e)
