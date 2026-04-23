{-# LANGUAGE TypeFamilies #-}

-- The idea is to perform distribution on one level at a time, and
-- produce "irregular Maps" that can accept and produce irregular
-- arrays.  These irregular maps will then be transformed into flat
-- parallelism based on their contents.  This is a sensitive detail,
-- but if irregular maps contain only a single Stm, then it is fairly
-- straightforward, as we simply implement flattening rules for every
-- single kind of expression.  Of course that is also somewhat
-- inefficient, so we want to support multiple Stms for things like
-- scalar code.
module Futhark.Pass.Flatten (flattenSOACs) where

import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (isNothing, mapMaybe)
import Data.Set qualified as S
import Data.Tuple.Solo
import Debug.Trace
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Pass.ExtractKernels.ToGPU (soacsLambdaToGPU, soacsStmToGPU)
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Match
import Futhark.Pass.Flatten.Monad
import Futhark.Pass.Flatten.PreProcess
import Futhark.Pass.Flatten.WithAcc
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.IntegralExp
import Prelude hiding (div, quot, rem)

data InnerMapMode
  = MultiDim
  | SingleDim

flattenOps :: FlattenOps
flattenOps = FlattenOps {flattenDistStm = transformDistStm}

transformScalarStms ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Stms SOACS ->
  Builder GPU DistEnv
transformScalarStms segments env inps distres stms = do
  let bound_in_batch = namesFromList $ concatMap (patNames . stmPat) $ stmsToList stms
      allCerts = foldMap (\stm -> distCerts inps (stmAux stm) env) (stmsToList stms)
      certs = Certs $ filter (`notNameIn` bound_in_batch) $ unCerts allCerts
  vs <- certifying certs $ letTupExp "scalar_dist" <=< renameExp <=< segMap segments $ \is -> do
    readInputs segments env (toList is) inps
    addStms $ fmap soacsStmToGPU stms
    pure $ subExpsRes $ map (Var . distResName) distres
  pure $ insertReps (zip (map distResTag distres) $ map Regular vs) env

transformScalarStm ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Stm SOACS ->
  Builder GPU DistEnv
transformScalarStm segments env inps res stm =
  transformScalarStms segments env inps res (oneStm stm)

-- Do 'map2 (++) A B' where 'A' and 'B' are irregular arrays and have the same
-- number of subarrays
concatIrreg ::
  Segments ->
  DistEnv ->
  VName ->
  [IrregularRep] ->
  Builder GPU IrregularRep
concatIrreg _segments _env ns reparr = do
  -- Concatenation does not change the number of segments - it simply
  -- makes each of them larger.

  num_segments <- arraySize 0 <$> lookupType ns

  -- Constructs the full list size / shape that should hold the final results.
  let zero = Constant $ IntValue $ intValue Int64 (0 :: Int)
  ns_full <- letExp (baseName ns <> "_full") <=< segMap (MkSolo num_segments) $
    \(MkSolo i) -> do
      old_segments <-
        forM reparr $ \rep ->
          letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      new_segment <-
        letSubExp "new_segment"
          =<< toExp (foldl (+) (pe64 zero) $ map pe64 old_segments)
      pure $ subExpsRes [new_segment]

  (ns_full_F, ns_full_O, _ns_II1) <- doRepIota ns_full

  repIota <- mapM (doRepIota . irregularS) reparr
  segIota <- mapM (doSegIota . irregularS) reparr

  let (_, _, rep_II1) = unzip3 repIota
  let (_, _, rep_II2) = unzip3 segIota

  n_arr <- mapM (fmap (arraySize 0) . lookupType) rep_II1

  -- Calculate offsets for the scatter operations
  let shapes = map irregularS reparr
  scatter_offsets <-
    letTupExp "irregular_scatter_offsets" <=< segMap (MkSolo num_segments) $
      \(MkSolo i) -> do
        segment_sizes <-
          forM shapes $ \shape ->
            letSubExp "segment_size" =<< eIndex shape [eSubExp i]
        let prefixes = L.init $ L.inits segment_sizes
        sumprefix <-
          mapM
            ( letSubExp "segment_prefix"
                <=< foldBinOp (Add Int64 OverflowUndef) (intConst Int64 0)
            )
            prefixes
        pure $ subExpsRes sumprefix

  scatter_offsets_T <-
    letTupExp "irregular_scatter_offsets_T" <=< segMap (MkSolo num_segments) $
      \(MkSolo i) -> do
        columns <-
          forM scatter_offsets $ \offsets ->
            letSubExp "segment_offset" =<< eIndex offsets [eSubExp i]
        pure $ subExpsRes columns

  m <- arraySize 0 <$> lookupType ns_full_F
  data_t <- lookupType (irregularD (head reparr))
  let pt = elemType data_t
  let resultType = Array pt (Shape [m]) NoUniqueness
  elems_blank <- letExp "blank_res" =<< eBlank resultType

  -- Scatter data into result array
  elems <-
    foldlM
      ( \elems (reparr1, scatter_offset, n, ii1, ii2) -> do
          letExp "irregular_scatter_elems" <=< genScatter elems n $ \gid -> do
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
  Segments ->
  DistEnv ->
  VName ->
  [IrregularRep] ->
  [Type] ->
  DistInputs ->
  Int ->
  Builder GPU IrregularRep
concatIrregAlongDim segments env ns reparr typearr inps d = do
  num_segments <- arraySize 0 <$> lookupType ns

  let zero = Constant $ IntValue $ intValue Int64 (0 :: Int)
  ns_full <- letExp (baseName ns <> "_full") <=< segMap (MkSolo num_segments) $
    \(MkSolo i) -> do
      old_segments <-
        forM reparr $ \rep ->
          letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      new_segment <-
        letSubExp "new_segment"
          =<< toExp (foldl (+) (pe64 zero) $ map pe64 old_segments)
      pure $ subExpsRes [new_segment]

  (ns_full_F, ns_full_O, _ns_II1) <- doRepIota ns_full

  repIota <- mapM (doRepIota . irregularS) reparr
  segIota <- mapM (doSegIota . irregularS) reparr

  let (_, _, rep_II1) = unzip3 repIota
  let (_, _, rep_II2) = unzip3 segIota

  n_arr <- mapM (fmap (arraySize 0) . lookupType) rep_II1

  scatter_info <-
    letTupExp "irregular_scatter_offsets" <=< segMap (MkSolo num_segments) $
      \(MkSolo i) -> do
        seg_is <- segmentCoordsFromFlat segments i

        block_sizes <-
          forM typearr $ \t -> do
            v_dims <- readTypeDims segments env seg_is inps t
            letSubExp "block_size" =<< toExp (product $ map pe64 $ drop d v_dims)

        total_block <-
          letSubExp "total_block"
            <=< foldBinOp (Add Int64 OverflowUndef) (intConst Int64 0)
            $ block_sizes

        let prefixes = L.init $ L.inits block_sizes

        sumprefix <-
          mapM
            ( letSubExp "segment_prefix"
                <=< foldBinOp (Add Int64 OverflowUndef) (intConst Int64 0)
            )
            prefixes

        pure $ subExpsRes (block_sizes <> sumprefix <> [total_block])

  let k = length typearr
      (scatter_blocks, rest) = splitAt k scatter_info
      (scatter_offsets, [total_block_size]) = splitAt k rest

  m <- arraySize 0 <$> lookupType ns_full_F
  data_t <- lookupType (irregularD (head reparr))
  let pt = elemType data_t
  let resultType = Array pt (Shape [m]) NoUniqueness
  elems_blank <- letExp "blank_res" =<< eBlank resultType

  -- Scatter data into result array
  elems <-
    foldlM
      ( \elems (reparr1, scatter_block, scatter_offset, n, ii1, ii2) -> do
          letExp "irregular_scatter_elems" <=< genScatter elems n $ \gid -> do
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
      $ L.zip6 reparr scatter_blocks scatter_offsets n_arr rep_II1 rep_II2

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
  Segments ->
  DistEnv ->
  VName ->
  Name ->
  IrregularRep ->
  Builder GPU IrregularRep
replicateIrreg _segments _env ns desc rep = do
  -- Replication does not change the number of segments - it simply
  -- makes each of them larger.

  num_segments <- arraySize 0 <$> lookupType ns

  -- ns multipled with existing segment sizes.
  ns_full <- letExp (baseName ns <> "_full") <=< segMap (MkSolo num_segments) $
    \(MkSolo i) -> do
      n <-
        letSubExp "n" =<< eIndex ns [eSubExp i]
      old_segment <-
        letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      full_segment <-
        letSubExp "new_segment" =<< toExp (pe64 n * pe64 old_segment)
      pure $ subExpsRes [full_segment]

  (ns_full_F, ns_full_O, ns_full_D) <- doRepIota ns_full
  (_, _, flat_to_segs) <- doSegIota ns_full

  w <- arraySize 0 <$> lookupType ns_full_D

  elems <- letExp (desc <> "_rep_D") <=< segMap (MkSolo w) $ \(MkSolo i) -> do
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

-- | Flatten the arrays of an IrregularRep to be entirely one-dimensional.
flattenIrregularRep :: IrregularRep -> Builder GPU IrregularRep
flattenIrregularRep ir@(IrregularRep shape flags offsets elems kind) = do
  elems_t <- lookupType elems
  if arrayRank elems_t == 1
    then pure ir
    else do
      n <- arraySize 0 <$> lookupType shape
      m' <- letSubExp "flat_m" <=< toExp $ product $ map pe64 $ arrayDims elems_t
      elems' <-
        letExp (baseName elems <> "_flat") . BasicOp $
          Reshape elems (reshapeAll (arrayShape elems_t) (Shape [m']))

      shape' <- letExp (baseName shape <> "_flat") <=< renameExp <=< segMap (MkSolo n) $
        \(MkSolo i) -> do
          old_shape <- letSubExp "old_shape" =<< eIndex shape [toExp i]
          segment_shape <-
            letSubExp "segment_shape" <=< toExp $
              pe64 old_shape * product (map pe64 $ tail $ arrayDims elems_t)
          pure [subExpRes segment_shape]

      offsets' <- letExp (baseName offsets <> "_flat") <=< renameExp <=< segMap (MkSolo n) $
        \(MkSolo i) -> do
          old_offsets <- letSubExp "old_offsets" =<< eIndex offsets [toExp i]
          segment_offsets <-
            letSubExp "segment_offsets" <=< toExp $
              pe64 old_offsets * product (map pe64 $ tail $ arrayDims elems_t)
          pure [subExpRes segment_offsets]

      flags' <- letExp (baseName flags <> "_flat") <=< renameExp <=< segMap (MkSolo m') $
        \(MkSolo i) -> do
          let head_i = head $ unflattenIndex (map pe64 $ arrayDims elems_t) (pe64 i)
          flag <- letSubExp "flag" =<< eIndex flags [toExp head_i]
          pure [subExpRes flag]
      pure $ IrregularRep shape' flags' offsets' elems' kind

rearrangeFlat :: (IntegralExp num) => [Int] -> [num] -> num -> num
rearrangeFlat perm dims i =
  -- TODO?  Maybe we need to invert one of these permutations.
  flattenIndex dims $
    rearrangeShape perm $
      unflattenIndex (rearrangeShape perm dims) i

segmentCoordsFromFlat :: Segments -> SubExp -> Builder GPU [SubExp]
segmentCoordsFromFlat segments seg_i =
  mapM (letSubExp "seg_coord" <=< toExp) $
    unflattenIndex (map pe64 $ shapeDims $ segmentsShape segments) (pe64 seg_i)

segmentCount :: Segments -> TPrimExp Int64 VName
segmentCount = product . map pe64 . shapeDims . segmentsShape

-- TODO: We do not need to actully make this Dense
rearrangeIrreg ::
  Segments ->
  DistEnv ->
  DistInputs ->
  TypeBase Shape u ->
  [Int] ->
  IrregularRep ->
  Builder GPU IrregularRep
rearrangeIrreg segments env inps v_t perm ir = do
  (IrregularRep shape _ offsets elems _) <- flattenIrregularRep ir
  (new_F, new_O, ii1_vss) <- doRepIota shape
  (_, _, ii2_vss) <- doSegIota shape
  m <- arraySize 0 <$> lookupType ii1_vss
  elems' <- letExp "elems_rearrange" <=< renameExp <=< segMap (MkSolo m) $
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

sufficientParallelism ::
  Name ->
  [SubExp] ->
  KernelPath ->
  Maybe Int64 ->
  Builder GPU (SubExp, Name)
sufficientParallelism desc ws path def = do
  size_key <- nameFromText . prettyText <$> newVName desc

  amount <-
    letSubExp "comparatee"
      =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) ws

  cmp_res <-
    letSubExp desc $
      Op $
        SizeOp $
          CmpSizeLe size_key (SizeThreshold path def) amount

  pure (cmp_res, size_key)

kernelAlternatives ::
  Name ->
  [Type] ->
  Body GPU ->
  [(SubExp, Body GPU)] ->
  Builder GPU [VName]
kernelAlternatives desc _ default_body [] = do
  ses <- bodyBind default_body
  forM ses $ \(SubExpRes cs se) ->
    certifying cs $
      letExp desc $
        BasicOp $
          SubExp se
kernelAlternatives desc result_ts default_body ((cond, alt) : alts) = do
  fallback_body <- do
    (fallback_vs, fallback_stms) <-
      collectStms $
        kernelAlternatives desc result_ts default_body alts
    pure $ mkBody fallback_stms $ varsRes fallback_vs

  letTupExp desc $
    Match [cond] [Case [Just $ BoolValue True] alt] fallback_body $
      MatchDec (staticShapes result_ts) MatchEquiv

regularResultVars :: [DistResult] -> DistEnv -> [VName]
regularResultVars ress env =
  map onRes ress
  where
    onRes res =
      case resVar (distResTag res) env of
        Regular v -> v
        Irregular {} ->
          error "regularResultVars: expected regular result"

regularRepVars :: [ResRep] -> [VName]
regularRepVars =
  map onRep
  where
    onRep (Regular v) = v
    onRep Irregular {} =
      error "regularRepVars: expected regular result"

isVersionableRegularResult :: DistResult -> Bool
isVersionableRegularResult = isRegularDistResult

regularBranchBody ::
  Builder GPU [VName] ->
  Builder GPU (Body GPU)
regularBranchBody m = do
  (vs, stms) <- collectStms m
  renameBody $ mkBody stms $ varsRes vs

versionedRegularMap ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  Lambda SOACS ->
  Builder GPU DistEnv
versionedRegularMap segments env inps ress pat aux w arrs form map_lam = do
  (outer_suff, _) <-
    sufficientParallelism "suff_outer_map" (NE.toList segments) mempty Nothing

  let fullFlatten =
        regularRepVars <$> transformInnerMap segments env inps pat w arrs map_lam

      outerOnly = do
        env' <-
          transformScalarStm segments env inps ress $
            Let pat aux $
              Op $
                Screma w arrs form
        pure $ regularResultVars ress env'

  full_body <- regularBranchBody fullFlatten
  outer_body <- regularBranchBody outerOnly

  let result_ts =
        [ t `arrayOfShape` segmentsShape segments
        | DistResult _ (DistType _ _ t) _ <- ress
        ]

  match_res <-
    certifying (distCerts inps aux env) $
      kernelAlternatives "match_res" result_ts full_body [(outer_suff, outer_body)]

  pure $ insertRegulars (map distResTag ress) match_res env

transformDistBasicOp ::
  Segments ->
  DistEnv ->
  ( DistInputs,
    DistResult,
    PatElem Type,
    StmAux (),
    BasicOp
  ) ->
  Builder GPU DistEnv
transformDistBasicOp segments env (inps, res, pe, aux, e) =
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
      | not $ any (isVariant inps env) (arrayDims row_type) -> do
          let resultType =
                Array
                  (elemType row_type)
                  (segmentsShape segments <> Shape [intConst Int64 0] <> arrayShape row_type)
                  NoUniqueness
          v <- letExp "arraylit_empty_reg" =<< eBlank resultType
          pure $ insertRegulars [distResTag res] [v] env
      | otherwise -> do
          ns <- dataArr segments env inps $ intConst Int64 0
          (flags, offsets, _elems) <- doRepIota ns
          let resultType = Array (elemType row_type) (Shape [intConst Int64 0]) NoUniqueness
          elems <- letExp "arraylit_empty_elems" =<< eBlank resultType
          pure $ insertIrregular ns flags offsets (distResTag res) elems Dense env
    -- TODO: not sure about this
    ArrayVal vs row_type -> do
      base_v <- letExp "arraylit_base" $ BasicOp $ ArrayVal vs row_type
      res_v <- letExp "arraylit_reg" $ BasicOp $ Replicate (segmentsShape segments) (Var base_v)
      pure $ insertRegulars [distResTag res] [res_v] env
    ArrayLit vs row_type
      | not $ any (isVariant inps env) (arrayDims row_type) -> do
          res_v <-
            if any (isVariant inps env) vs
              then do
                let seg_shape = segmentsShape segments
                    one = intConst Int64 1
                    arr_outer_dim = intConst Int64 $ toInteger $ length vs
                    expected = seg_shape <> arrayShape row_type
                    stacked = seg_shape <> Shape [one] <> arrayShape row_type
                    d = segmentsRank segments

                vs_reg <- mapM (liftSubExpRegular segments inps env expected) vs

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
          vs_reparr <- mapM (dataArr segments env inps) vs
          dim_arrs <- mapM (dataArr segments env inps) (arrayDims row_type)
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          ~[row_size, full_size] <- letTupExp "arraylit_row_size" <=< segMap (MkSolo num_segments) $ \(MkSolo i) -> do
            vals <- mapM (\dim_arr -> letSubExp "dim_i" =<< eIndex dim_arr [eSubExp i]) dim_arrs
            n <- letSubExp "n" <=< toExp $ product $ map pe64 vals
            fs <- letSubExp "fs" <=< toExp $ pe64 n * pe64 arr_outer_dim
            pure $ subExpsRes [n, fs]

          (_, _, row_II1) <- doRepIota row_size
          (_, _, row_II2) <- doSegIota row_size

          row_flat_size <- arraySize 0 <$> lookupType row_II1

          (full_flags, full_offset, full_II1) <- doRepIota full_size

          m <- arraySize 0 <$> lookupType full_II1
          data_t <- lookupType (head vs_reparr)
          let pt = elemType data_t
          let resultType = Array pt (Shape [m]) NoUniqueness
          elems_blank <- letExp "blank_res" =<< eBlank resultType

          elems <-
            foldlM
              ( \elems (var_num, arr) -> do
                  letExp "irregular_scatter_elems" <=< genScatter elems row_flat_size $ \gid -> do
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
    Opaque _op se
      | Var v <- se,
        Just (DistInput rt_in _) <- lookup v inps ->
          -- TODO: actually insert opaques
          pure $ insertRep (distResTag res) (resVar rt_in env) env
      | otherwise ->
          scalarCase
    -- TODO: Probably have to change this.
    Reshape arr _ -> do
      irreg_v <- getIrregRep segments env inps arr
      pure $ insertRep (distResTag res) (Irregular irreg_v) env
    Index arr slice
      | null $ sliceDims slice ->
          scalarCase
      | otherwise -> do
          -- Maximally irregular case.
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          ns <- letExp "slice_sizes" <=< renameExp <=< segMap (MkSolo num_segments) $ \(MkSolo segment) -> do
            segment_is <- segmentCoordsFromFlat segments segment
            slice_ns <- mapM (readInput segments env segment_is inps) $ sliceDims slice
            fmap varsRes . letTupExp "n" <=< toExp $ product $ map pe64 slice_ns
          (_n, offsets, m) <- exScanAndSum ns
          (_, _, repiota_D) <- doRepIota ns
          flags <- genFlags m offsets
          elems <- letExp "elems" <=< renameExp <=< segMap (NE.singleton m) $ \is -> do
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
    Iota n (Constant x) (Constant s) Int64
      | zeroIsh x,
        oneIsh s -> do
          ns <- dataArr segments env inps n
          (flags, offsets, elems) <- certifying (distCerts inps aux env) $ doSegIota ns
          pure $ insertIrregular ns flags offsets (distResTag res) elems Dense env
    Iota n x s it -> do
      ns <- dataArr segments env inps n
      xs <- dataArr segments env inps x
      ss <- dataArr segments env inps s
      (res_F, res_O, res_D) <- certifying (distCerts inps aux env) $ doSegIota ns
      (_, _, repiota_D) <- doRepIota ns
      m <- arraySize 0 <$> lookupType res_D
      res_D' <- letExp "iota_D_fixed" <=< segMap (MkSolo m) $ \(MkSolo i) -> do
        segment <- letSubExp "segment" =<< eIndex repiota_D [eSubExp i]
        v' <- letSubExp "v" =<< eIndex res_D [eSubExp i]
        x' <- letSubExp "x" =<< eIndex xs [eSubExp segment]
        s' <- letSubExp "s" =<< eIndex ss [eSubExp segment]
        fmap (subExpsRes . pure) . letSubExp "v" <=< toExp $
          primExpFromSubExp (IntType it) x'
            ~+~ sExt it (untyped (pe64 v'))
            ~*~ primExpFromSubExp (IntType it) s'
      pure $ insertIrregular ns res_F res_O (distResTag res) res_D' Dense env
    Concat 0 arr shp -> do
      ns <- dataArr segments env inps shp
      reparr <- mapM (getIrregRep segments env inps) (NE.toList arr)
      rep' <- concatIrreg segments env ns reparr
      pure $ insertRep (distResTag res) (Irregular rep') env
    -- TODO: add invariant special handling
    Concat d arr shp -> do
      ns <- dataArr segments env inps shp
      reparr <- mapM (getIrregRep segments env inps) (NE.toList arr)
      -- typearr <- mapM lookupType arr
      typearr <-
        forM arr $ \v ->
          case lookup v inps of
            Just inp -> pure $ distInputType inp
            Nothing -> lookupType v
      rep' <- concatIrregAlongDim segments env ns reparr (NE.toList typearr) inps d
      pure $ insertRep (distResTag res) (Irregular rep') env

    --  TODO: add invaraint special handling
    Replicate (Shape [n]) (Var v) -> do
      ns <- dataArr segments env inps n
      rep <- getIrregRep segments env inps v
      rep' <- replicateIrreg segments env ns (baseName v) rep
      pure $ insertRep (distResTag res) (Irregular rep') env
    Replicate (Shape [n]) (Constant v) -> do
      ns <- dataArr segments env inps n
      (res_F, res_O, res_D) <-
        certifying (distCerts inps aux env) $ doSegIota ns
      w <- arraySize 0 <$> lookupType res_D
      res_D' <- letExp "rep_const" $ BasicOp $ Replicate (Shape [w]) (Constant v)
      pure $ insertIrregular ns res_F res_O (distResTag res) res_D' Dense env
    Replicate (Shape dims) (Constant v) -> do
      dim_arrs <- mapM (dataArr segments env inps) dims
      seg_number <- arraySize 0 <$> lookupType (head dim_arrs)
      mul_dims <- letExp "mul_dims" <=< segMap (MkSolo seg_number) $ \(MkSolo i) -> do
        vals <- mapM (\dim_arr -> letSubExp "dim_i" =<< eIndex dim_arr [eSubExp i]) dim_arrs
        n <- letSubExp "n" <=< toExp $ product $ map pe64 vals
        pure [subExpRes n]
      (res_F, res_O, res_D) <-
        certifying (distCerts inps aux env) $ doSegIota mul_dims
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
      dim_arrs <- mapM (dataArr segments env inps) dims
      seg_number <- arraySize 0 <$> lookupType (head dim_arrs)
      mul_dims <- letExp "mul_dims" <=< segMap (MkSolo seg_number) $ \(MkSolo i) -> do
        vals <- mapM (\dim_arr -> letSubExp "dim_i" =<< eIndex dim_arr [eSubExp i]) dim_arrs
        n <- letSubExp "n" <=< toExp $ product $ map pe64 vals
        pure [subExpRes n]
      rep <- getIrregRep segments env inps v
      rep' <- replicateIrreg segments env mul_dims (baseName v) rep
      pure $ insertRep (distResTag res) (Irregular rep') env
    Update _ as slice se
      | Just as_t <- distInputType <$> lookup as inps -> do
          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          ns <- letExp "slice_sizes"
            <=< renameExp
            <=< segMap (MkSolo num_segments)
            $ \(MkSolo seg_i) -> do
              seg_is <- segmentCoordsFromFlat segments seg_i
              readInputs segments env seg_is $
                filter ((`elem` sliceDims slice) . Var . fst) inps
              slice_dims <- mapM (readInput segments env seg_is inps) $ sliceDims slice
              n <- letSubExp "n" <=< toExp $ product $ map pe64 slice_dims
              pure [subExpRes n]
          -- Irregular representation of `as`
          as_rep <- getIrregRep segments env inps as
          IrregularRep shape flags offsets elems _ <-
            ensureDenseIrregular (baseName as <> "_update") as_rep
          -- Inner indices (1 and 2) of `ns`
          (_, _, ii1_vss) <- doRepIota ns
          (_, _, ii2_vss) <- certifying (distCerts inps aux env) $ doSegIota ns
          -- Number of updates to perform
          m <- arraySize 0 <$> lookupType ii2_vss
          elems' <- letExp "elems_scatter" <=< renameExp <=< genScatter elems m $ \gid -> do
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
    Rearrange v perm -> do
      case lookup v inps of
        Just (DistInputFree v' _) -> do
          v'' <-
            letExp (baseName v' <> "_tr") . BasicOp $
              Rearrange v' perm
          pure $ insertRegulars [distResTag res] [v''] env
        Just (DistInput rt v_t) -> do
          case resVar rt env of
            Irregular rep -> do
              rep' <-
                certifying (distCerts inps aux env) $
                  rearrangeIrreg segments env inps v_t perm rep
              pure $ insertRep (distResTag res) (Irregular rep') env
            Regular v' -> do
              let r = segmentsRank segments
              v'' <-
                letExp (baseName v' <> "_tr") . BasicOp $
                  Rearrange v' ([0 .. r - 1] ++ map (+ r) perm)
              pure $ insertRegulars [distResTag res] [v''] env
        Nothing -> do
          let r = segmentsRank segments
          v' <-
            letExp (baseName v <> "_tr") . BasicOp $
              Rearrange v ([0 .. r - 1] ++ map (+ r) perm)
          pure $ insertRegulars [distResTag res] [v'] env
    Scratch pt dims
      | not $ any (isVariant inps env) dims -> do
          -- All dims are invariant result is regular across segments.
          v' <-
            letExp "scratch" . BasicOp $
              Scratch pt (shapeDims (segmentsShape segments) ++ dims)
          pure $ insertRegulars [distResTag res] [v'] env
      | [n] <- dims -> do
          ns <- dataArr segments env inps n
          (_n, offsets, m) <- exScanAndSum ns
          flags <- genFlags m offsets
          res_D <- letExp "scratch_D" $ BasicOp $ Scratch pt [m]
          pure $ insertIrregular ns flags offsets (distResTag res) res_D Dense env
      | otherwise -> do
          dim_arrs <- mapM (dataArr segments env inps) dims
          w <- arraySize 0 <$> lookupType (head dim_arrs)
          ns <- letExp "scratch_sizes" <=< segMap (MkSolo w) $ \(MkSolo i) -> do
            vals <- mapM (\arr -> letSubExp "d" =<< eIndex arr [eSubExp i]) dim_arrs
            n <- letSubExp "n" <=< toExp $ product $ map pe64 vals
            pure [subExpRes n]
          (_n, offsets, m) <- exScanAndSum ns
          flags <- genFlags m offsets
          res_D <- letExp "scratch_D" $ BasicOp $ Scratch pt [m]
          pure $ insertIrregular ns flags offsets (distResTag res) res_D Dense env
    UpdateAcc {} ->
      -- TODO: handle irregular case, which is however rare, and also needs
      -- modifications to WithAcc. The only irregularity that is possible is in
      -- the values to be written.
      scalarCase
    _ -> error $ "Unhandled BasicOp:\n" ++ prettyString e
  where
    scalarCase =
      transformScalarStm segments env inps [res] $
        Let (Pat [pe]) aux (BasicOp e)

-- Replicates inner dimension for inputs.
onMapFreeVar ::
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  (VName, VName, VName) ->
  VName ->
  Maybe (Builder GPU (VName, MapArray IrregularRep))
onMapFreeVar segments env inps _ws (_ws_F, _ws_O, ws_data) v = do
  let segments_per_elem = ws_data
  v_inp <- lookup v inps
  pure $ do
    ws_prod <- arraySize 0 <$> lookupType ws_data
    fmap (v,) $ case v_inp of
      DistInputFree v' t -> do
        fmap (`MapArray` t)
          . letExp (baseName v <> "_rep_free_free_inp")
          <=< segMap (MkSolo ws_prod)
          $ \(MkSolo i) -> do
            segment <- letSubExp "segment" =<< eIndex segments_per_elem [eSubExp i]
            segment_is <- segmentCoordsFromFlat segments segment
            subExpsRes . pure <$> (letSubExp "v" =<< eIndex v' (map eSubExp segment_is))
      -- subExpsRes . pure <$> readInput segments env segment_is inps (Var v)
      DistInput rt t -> case resVar rt env of
        Irregular rep -> do
          ~[new_S, offsets] <- letTupExp (baseName v <> "_rep_free_irreg")
            <=< segMap (MkSolo ws_prod)
            $ \(MkSolo i) -> do
              segment <- letSubExp "segment" =<< eIndex ws_data [eSubExp i]
              s <- letSubExp "s" =<< eIndex (irregularS rep) [eSubExp segment]
              o <- letSubExp "o" =<< eIndex (irregularO rep) [eSubExp segment]
              pure $ subExpsRes [s, o]
          let rep' =
                IrregularRep
                  { irregularS = new_S,
                    irregularF = irregularF rep,
                    irregularO = offsets,
                    irregularD = irregularD rep,
                    irregularK = Replicated
                  }
          pure $ MapOther rep' t
        Regular vs ->
          fmap (`MapArray` t)
            . letExp (baseName v <> "_rep_free_reg_inp")
            <=< segMap (MkSolo ws_prod)
            $ \(MkSolo i) -> do
              segment <- letSubExp "segment" =<< eIndex segments_per_elem [eSubExp i]
              segment_is <- segmentCoordsFromFlat segments segment
              subExpsRes . pure <$> (letSubExp "v" =<< eIndex vs (map eSubExp segment_is))

-- subExpsRes . pure <$> readInput segments env segment_is inps (Var v)

onMapInputArr ::
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  VName ->
  VName ->
  Param Type ->
  VName ->
  Builder GPU (MapArray IrregularRep)
onMapInputArr segments env inps ws ws_O ws_data p arr = do
  ws_prod <- arraySize 0 <$> lookupType ws_data
  case lookup arr inps of
    Just v_inp ->
      case v_inp of
        DistInputFree vs t -> do
          let inner_shape = arrayShape $ paramType p
          vs_t <- lookupType vs
          v <-
            if isAcc vs_t
              then pure vs
              else
                letExp (baseName vs <> "_flat") . BasicOp . Reshape vs $
                  reshapeAll (arrayShape vs_t) (Shape [ws_prod] <> inner_shape)
          pure $ MapArray v t
        DistInput rt _ ->
          case resVar rt env of
            Irregular rep -> do
              onMapIrregularInputArr SingleDim segments ws ws_O ws_data p arr rep ws_prod
            Regular vs -> do
              let inner_shape = arrayShape $ paramType p
              vs_t <- lookupType vs
              v <-
                letExp (baseName arr <> "_reg_flat") . BasicOp . Reshape vs $
                  reshapeAll (arrayShape vs_t) (Shape [ws_prod] <> inner_shape)
              pure $ MapArray v (stripArray 1 vs_t)
    -- undefined
    Nothing -> do
      arr_row_t <- rowType <$> lookupType arr
      arr_rep <-
        letExp (baseName arr <> "_inp_rep") . BasicOp $
          Replicate (segmentsShape segments) (Var arr)
      arr_rep_t <- lookupType arr_rep
      v <-
        letExp (baseName arr <> "_inp_rep_flat") . BasicOp . Reshape arr_rep $
          reshapeAll (arrayShape arr_rep_t) (Shape [ws_prod] <> arrayShape arr_row_t)
      pure $ MapArray v arr_row_t

transformInnerMap ::
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Builder GPU [ResRep]
transformInnerMap segments env inps pat w arrs map_lam
  | not (isVariant inps env w) = do
      traceM "transformInnerMap: w is invariant, treating as multi-dim map"
      transformInnerMapMultiDim segments env inps pat w arrs map_lam
  | otherwise = do
      traceM "transformInnerMap: w is variant, treating as single-dim map"
      transformInnerMapSingleDim segments env inps pat w arrs map_lam

transformInnerMapSingleDim ::
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Builder GPU [ResRep]
transformInnerMapSingleDim segments env inps pat w arrs map_lam = do
  ws <- dataArr segments env inps w
  (ws_F, ws_O, ws_data) <- doRepIota ws
  new_segment <- arraySize 0 <$> lookupType ws_data
  arrs' <-
    zipWithM
      (onMapInputArr segments env inps ws ws_O ws_data)
      (lambdaParams map_lam)
      arrs
  distributeAndTransformInnerMap
    SingleDim
    (ws_F, ws_O, ws)
    (NE.singleton new_segment)
    inps
    pat
    arrs'
    (onMapFreeVar segments env inps ws (ws_F, ws_O, ws_data))
    map_lam

onMapIrregularInputArr ::
  InnerMapMode ->
  Segments ->
  VName ->
  VName ->
  VName ->
  Param Type ->
  VName ->
  IrregularRep ->
  SubExp ->
  Builder GPU (MapArray IrregularRep)
onMapIrregularInputArr mode new_segments ws ws_O ws_data p arr rep ws_prod = do
  -- new_segments has already has the the new w inside unlike other functions
  rep_t <- lookupType $ irregularD rep
  when (arrayRank rep_t > 1) $
    error $
      error "onMapIrregularInputArr: irregularD is not 1D"
  if null (arrayDims $ paramType p)
    then do
      -- assuimg the irregD is 1D size(irregularD rep) == ws_prod should hold and this should be fine
      let old_shape = arrayShape rep_t
          new_shape =
            case mode of
              SingleDim -> Shape [ws_prod]
              MultiDim -> segmentsShape new_segments
      case irregularK rep of
        Dense -> do
          v_reshaped <- letExp (baseName (paramName p) <> "_reshaped") $ BasicOp $ Reshape (irregularD rep) $ reshapeAll old_shape new_shape
          pure $ MapArray v_reshaped (stripArray 1 rep_t)
        Replicated -> do
          new_flat <-
            letExp (baseName arr <> "_flat_expand")
              <=< segMap (MkSolo ws_prod)
              $ \(MkSolo i) -> do
                j <- letSubExp "j" =<< eIndex ws_data [eSubExp i]
                data_off <- letSubExp "data_off" =<< eIndex (irregularO rep) [eSubExp j]
                seg_start <- letSubExp "seg_start" =<< eIndex ws_O [eSubExp j]
                local_pos <- letSubExp "local_pos" <=< toExp $ pe64 i - pe64 seg_start
                flat_idx <- letSubExp "flat_idx" <=< toExp $ pe64 data_off + pe64 local_pos
                fmap (subExpsRes . pure) $ letSubExp "elem" =<< eIndex (irregularD rep) [eSubExp flat_idx]
          v_reshaped <- letExp (baseName (paramName p) <> "_reshaped") $ BasicOp $ Reshape new_flat $ reshapeAll old_shape new_shape
          pure $ MapArray v_reshaped (stripArray 1 rep_t)
    else do
      -- We need to split multi-dimensional irregular segments
      -- into per-row segments. Compute per-row size by dividing
      -- each segment's total size by the number of inner iterations.
      -- Important TODO: I should ask troels about this.
      -- we should make this consistent.
      -- we can avoid getting per_row_size by division.
      num_segments <- arraySize 0 <$> lookupType ws
      -- per_row_size[s] = irregularS[s] / ws[s]
      per_row_size <-
        letExp (baseName (paramName p) <> "_per_row_size")
          <=< segMap (MkSolo num_segments)
          $ \(MkSolo s) -> do
            total_s <- letSubExp "total_s" =<< eIndex (irregularS rep) [eSubExp s]
            num_rows_s <- letSubExp "num_rows_s" =<< eIndex ws [eSubExp s]
            row_size <-
              letSubExp "row_size"
                =<< eIf
                  (toExp $ pe64 num_rows_s .==. 0)
                  (eBody [toExp $ intConst Int64 0])
                  (eBody [toExp $ pe64 total_s `div` pe64 num_rows_s])
            pure $ subExpsRes [row_size]
      new_S <-
        letExp (baseName (paramName p) <> "_new_S")
          <=< segMap (MkSolo ws_prod)
          $ \(MkSolo i) -> do
            seg_i <- letSubExp "seg_i" =<< eIndex ws_data [eSubExp i]
            sz <- letSubExp "sz" =<< eIndex per_row_size [eSubExp seg_i]
            pure $ subExpsRes [sz]
      rep' <- case irregularK rep of
        Dense -> do
          (new_F, new_O, _new_elems) <- doSegIota new_S
          pure $
            IrregularRep
              { irregularD = irregularD rep,
                irregularF = new_F,
                irregularS = new_S,
                irregularO = new_O,
                irregularK = Dense
              }
        Replicated -> do
          new_O <-
            letExp (baseName (paramName p) <> "_new_O")
              <=< segMap (MkSolo ws_prod)
              $ \(MkSolo i) -> do
                seg_i <- letSubExp "seg_i" =<< eIndex ws_data [eSubExp i]
                row_size <- letSubExp "row_size" =<< eIndex per_row_size [eSubExp seg_i]
                seg_row_start <- letSubExp "seg_row_start" =<< eIndex ws_O [eSubExp seg_i]
                row_in_seg <- letSubExp "row_in_seg" <=< toExp $ pe64 i - pe64 seg_row_start
                base_off <- letSubExp "base_off" =<< eIndex (irregularO rep) [eSubExp seg_i]
                off <- letSubExp "off" <=< toExp $ pe64 base_off + pe64 row_in_seg * pe64 row_size
                pure $ subExpsRes [off]
          m <- arraySize 0 <$> lookupType (irregularD rep)
          -- we will have mutliple write but it is the same value so it should be fine.
          new_F <- genFlags m new_O
          pure $
            IrregularRep
              { irregularD = irregularD rep,
                irregularF = new_F,
                irregularS = new_S,
                irregularO = new_O,
                irregularK = Replicated
              }
      pure $ MapOther rep' rep_t

onMapInputArrMultiDim ::
  Segments ->
  SubExp ->
  DistEnv ->
  DistInputs ->
  VName ->
  VName ->
  VName ->
  Param Type ->
  VName ->
  Builder GPU (MapArray IrregularRep)
onMapInputArrMultiDim old_segments w env inps ws ws_O ws_data p arr = do
  case lookup arr inps of
    Just v_inp ->
      case v_inp of
        DistInputFree vs t -> pure $ MapArray vs t
        DistInput rt t -> case resVar rt env of
          Irregular rep -> do
            ws_prod <- arraySize 0 <$> lookupType ws_data
            onMapIrregularInputArr MultiDim (old_segments <> pure w) ws ws_O ws_data p arr rep ws_prod
          Regular vs -> do
            vs_t <- lookupType vs
            -- let's be cautious and make sure it has the correct shape
            let expected_shape = segmentsShape old_segments <> arrayShape t
            if arrayShape vs_t == expected_shape
              then pure $ MapArray vs t
              else do
                v <-
                  letExp (baseName arr <> "_reg_reshape") . BasicOp . Reshape vs $
                    reshapeAll (arrayShape vs_t) expected_shape
                pure $ MapArray v t
    Nothing -> do
      arr_row_t <- rowType <$> lookupType arr
      arr_rep <-
        letExp (baseName arr <> "_inp_rep") . BasicOp $
          Replicate (segmentsShape old_segments) (Var arr)
      pure $ MapArray arr_rep arr_row_t

onMapFreeVarMultiDim ::
  Segments ->
  SubExp ->
  DistEnv ->
  DistInputs ->
  VName ->
  Maybe (Builder GPU (VName, MapArray IrregularRep))
onMapFreeVarMultiDim segments w env inps v = do
  v_inp <- lookup v inps
  pure $ fmap (v,) $ case v_inp of
    DistInputFree v' t -> do
      v_rep <- replicateForW segments w v'
      pure $ MapArray v_rep t
    DistInput rt t -> case resVar rt env of
      Regular v' -> do
        v_rep <- replicateForW segments w v'
        pure $ MapArray v_rep t
      Irregular rep -> do
        -- Can replicate as well
        old_nseg <- arraySize 0 <$> lookupType (irregularS rep)
        new_nseg <- letSubExp "new_nseg" <=< toExp $ pe64 old_nseg * pe64 w
        ~[new_S, offsets] <- letTupExp (baseName v <> "_rep_free_irreg")
          <=< segMap (MkSolo new_nseg)
          $ \(MkSolo i) -> do
            old_seg <- letSubExp "old_seg" <=< toExp $ pe64 i `quot` pe64 w
            s <- letSubExp "s" =<< eIndex (irregularS rep) [eSubExp old_seg]
            o <- letSubExp "o" =<< eIndex (irregularO rep) [eSubExp old_seg]
            pure $ subExpsRes [s, o]
        let rep' =
              IrregularRep
                { irregularS = new_S,
                  irregularF = irregularF rep,
                  irregularO = offsets,
                  irregularD = irregularD rep,
                  irregularK = Replicated
                }
        pure $ MapOther rep' t

-- old_nseg <- arraySize 0 <$> lookupType (irregularS rep)
-- new_nseg <- letSubExp "new_nseg" <=< toExp $ pe64 old_nseg * pe64 w

-- new_S <-
--   letExp (baseName v <> "_new_S")
--     <=< segMap (MkSolo new_nseg)
--     $ \(MkSolo i) -> do
--       old_seg <- letSubExp "old_seg" <=< toExp $ pe64 i `quot` pe64 w
--       s <- letSubExp "s" =<< eIndex (irregularS rep) [eSubExp old_seg]
--       pure [subExpRes s]

-- (new_F, new_O, new_II1) <- doRepIota new_S
-- m <- arraySize 0 <$> lookupType new_II1

-- new_D <-
--   letExp (baseName v <> "_new_D")
--     <=< segMap (MkSolo m)
--     $ \(MkSolo i) -> do
--       new_seg <- letSubExp "new_seg" =<< eIndex new_II1 [eSubExp i]
--       old_seg <- letSubExp "old_seg" <=< toExp $ pe64 new_seg `quot` pe64 w
--       new_off <- letSubExp "new_off" =<< eIndex new_O [eSubExp new_seg]
--       old_off <- letSubExp "old_off" =<< eIndex (irregularO rep) [eSubExp old_seg]
--       j <- letSubExp "j" <=< toExp $ pe64 i - pe64 new_off
--       x <- letSubExp "x" =<< eIndex (irregularD rep) [toExp $ pe64 old_off + pe64 j]
--       pure [subExpRes x]

-- pure $
--   MapOther
--     IrregularRep
--       { irregularS = new_S,
--         irregularF = new_F,
--         irregularO = new_O,
--         irregularD = new_D
--       }
--     t

-- | Replicate an array to insert a new inner dimension  after the
-- existing segment dimensions.
replicateForW :: Segments -> SubExp -> VName -> Builder GPU VName
replicateForW segments w v = do
  v_t <- lookupType v
  let seg_rank = length (NE.toList segments)
      v_rank = arrayRank v_t
      perm = [1 .. seg_rank] ++ [0] ++ [seg_rank + 1 .. v_rank]
  v_rep <-
    letExp (baseName v <> "_free_rep") . BasicOp $
      Replicate (Shape [w]) (Var v)
  letExp (baseName v <> "_free_rep_tr") . BasicOp $
    Rearrange v_rep perm

transformInnerMapMultiDim ::
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Builder GPU [ResRep]
transformInnerMapMultiDim segments env inps pat w arrs map_lam = do
  ws <- dataArr segments env inps w
  (ws_F, ws_O, ws_data) <- doRepIota ws
  arrs' <-
    zipWithM
      (onMapInputArrMultiDim segments w env inps ws ws_O ws_data)
      (lambdaParams map_lam)
      arrs
  distributeAndTransformInnerMap
    MultiDim
    (ws_F, ws_O, ws)
    (segments <> pure w)
    inps
    pat
    arrs'
    (onMapFreeVarMultiDim segments w env inps)
    map_lam

distributeAndTransformInnerMap ::
  InnerMapMode ->
  (VName, VName, VName) ->
  Segments ->
  DistInputs ->
  Pat Type ->
  [MapArray IrregularRep] ->
  (VName -> Maybe (Builder GPU (VName, MapArray IrregularRep))) ->
  Lambda SOACS ->
  Builder GPU [ResRep]
distributeAndTransformInnerMap mode ws_triple new_segment inps pat arrs' onFreeVar map_lam = do
  let free = freeIn map_lam
  outer_scope <- askScope
  let input_scope = scopeOfDistInputs inps `M.difference` outer_scope
  free_sizes <-
    localScope input_scope $
      foldMap freeIn <$> mapM lookupType (namesToList free)
  let free_and_sizes = namesToList $ free <> free_sizes
  traceM "distributing inner map with free variables\n"
  traceM $ unlines ["inputs: ", prettyString inps, "free variables:", prettyString free_and_sizes]
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe
        onFreeVar
        free_and_sizes
  free_ps <-
    zipWithM
      newParam
      (map ((<> "_free") . baseName) free_and_sizes) -- this should free_replicated?
      (map mapArrayRowType replicated)
  scope <- askScope
  let substs = M.fromList $ zip free_replicated $ map paramName free_ps
      map_lam' =
        substituteNames
          substs
          ( map_lam
              { lambdaParams = free_ps <> lambdaParams map_lam
              }
          )
      (distributed, arrmap) =
        distributeMap scope pat new_segment (replicated <> arrs') map_lam'
      m =
        transformDistributedInnerMap mode ws_triple arrmap new_segment distributed
  traceM $ unlines ["inner map distributed", prettyString distributed]
  (res, stms) <- runReaderT (runBuilder m) scope
  addStms stms
  -- order the result representations in the same order as the pattern
  pure $ resRepsInPatOrder pat res

-- Reduction or scan operators may not have any free variables that are variant
-- to the nest (that is, are inputs to the distributed operation). This is
-- because we would be unable to express them as SegScan/SegReds. Fixing this
-- would require modifications to the SegOp representation, but it is likely not
-- worth it, as such operators are extremely rare - and we can just fall back on
suitableOperator :: DistEnv -> DistInputs -> Lambda SOACS -> [SubExp] -> Bool
suitableOperator env inps lam nes =
  allNames notVariant (freeIn lam)
    -- && not (any (isVariant inps env) nes) -- TODO: maybe not needed
    && all primType (lambdaReturnType lam) -- TODO
  where
    notVariant v = isNothing $ M.lookup v $ inputReps inps env

suitableSegOpMap :: DistEnv -> DistInputs -> Lambda SOACS -> Bool
suitableSegOpMap env inps map_lam =
  not (any isParallelStm (bodyStms $ lambdaBody map_lam))
    -- TODO: do we want to add variants as inputs?
    && allNames (not . isVariant inps env . Var) (freeIn map_lam)

-- doSegScan :: [Scan SOACS] -> VName -> [VName] -> Builder GPU [VName]
doSegScan :: [Scan SOACS] -> VName -> [VName] -> Segments -> DistInputs -> DistEnv -> Builder GPU [VName]
doSegScan scans flags elems segments inps env = do
  let scan = singleScan scans
  -- TODO: FixME: this is temp hack
  let zeros = replicate (segmentsRank segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
  let nes = scanNeutral scan
  nes' <- mapM (readInput segments env zeros inps) nes
  genSegScan "scan" (soacsLambdaToGPU $ scanLambda scan) nes' flags elems

doSegScanomap ::
  [Scan SOACS] ->
  VName ->
  [VName] ->
  Lambda SOACS ->
  Segments ->
  DistInputs ->
  DistEnv ->
  Builder GPU [VName]
doSegScanomap scans flags elems map_lam segments inps env = do
  let scan = singleScan scans
  let zeros = replicate (segmentsRank segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
  let nes = scanNeutral scan
  nes' <- mapM (readInput segments env zeros inps) nes
  genSegScanomap
    "scanomap"
    (soacsLambdaToGPU $ scanLambda scan)
    nes'
    flags
    (soacsLambdaToGPU map_lam)
    elems

doSegMaposcanomap ::
  [Scan SOACS] ->
  VName ->
  [VName] ->
  Lambda SOACS ->
  Lambda SOACS ->
  Segments ->
  DistInputs ->
  DistEnv ->
  Builder GPU [VName]
doSegMaposcanomap scans flags elems post_lam map_lam segments inps env = do
  let scan = singleScan scans
  let zeros = replicate (segmentsRank segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
  let nes = scanNeutral scan
  nes' <- mapM (readInput segments env zeros inps) nes
  genSegScanomapWithPost
    "maposcanomap"
    (soacsLambdaToGPU $ scanLambda scan)
    nes'
    flags
    (soacsLambdaToGPU post_lam)
    (soacsLambdaToGPU map_lam)
    elems

postMapResultRep :: Segments -> DistEnv -> DistInputs -> SubExp -> VName -> VName -> VName -> VName -> IrregularKind -> Param Type -> Builder GPU ResRep
postMapResultRep segments env inps w ws_F ws_O ws_S elems elems_kind post_param
  | isVariant inps env w || any isTypeVariant (arrayDims p_t) =
      pure $
        Irregular $
          IrregularRep
            { irregularS = ws_S,
              irregularF = ws_F,
              irregularO = ws_O,
              irregularD = elems,
              irregularK = elems_kind
            }
  | otherwise = do
      elem_t <- lookupType elems
      elem_v' <-
        letExp (baseName elems <> "_reshaped") . BasicOp $
          Reshape elems $
            reshapeAll (arrayShape elem_t) expected_shape
      pure $ Regular elem_v'
  where
    p_t = paramType post_param
    expected_shape = segmentsShape segments <> Shape [w] <> arrayShape p_t

    isTypeVariant (Var v) = isVariant inps env (Var v)
    isTypeVariant Constant {} = False

transformPostMaposcanomap ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  [Scan SOACS] ->
  Lambda SOACS ->
  Builder GPU DistEnv
transformPostMaposcanomap segments env inps res pat w arrs post_lam scans map_lam = do
  reps <- mapM (segOpInputRep segments env inps) arrs
  (ws_F, ws_O, ws_S, elems, elems_kind) <-
    prepareSegOpInputs segments env inps w reps arrs
  elems' <- doSegScanomap scans ws_F elems map_lam segments inps env

  let post_params = lambdaParams post_lam

  post_reps <-
    zipWithM
      (\elem' post_param ->
        postMapResultRep
          segments env inps w ws_F ws_O ws_S
          elem' elems_kind post_param)
      elems'
      post_params

  transformMaposcanomapPostReps segments env inps res pat w post_reps post_lam

transformMaposcanomapPostReps ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  SubExp ->
  [ResRep] ->
  Lambda SOACS ->
  Builder GPU DistEnv
transformMaposcanomapPostReps segments env inps res pat w post_reps post_lam = do
  let (inps_local, env_local, next_tag) = localiseInputs env inps
      post_params = lambdaParams post_lam
      post_tags = map ResTag [next_tag ..]
      post_inputs =
        zipWith
          (\p tag -> (paramName p, DistInput tag (paramType p `arrayOfRow` w)))
          post_params
          post_tags
          <> inps_local
      post_env = insertReps (zip post_tags post_reps) env_local
      post_arrs = map paramName post_params
  traceM "transforming post maposcanomap results with inputs\n"
  post_res <- transformInnerMap segments post_env post_inputs pat w post_arrs post_lam
  pure $ insertReps (zip (map distResTag res) post_res) env

transformPreMaposcanomap ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  [Scan SOACS] ->
  Lambda SOACS ->
  Builder GPU DistEnv
transformPreMaposcanomap segments env inps res w arrs post_lam scans map_lam = do
  map_pat <- fmap Pat $ forM (lambdaReturnType map_lam) $ \t ->
    PatElem <$> newVName "map" <*> pure (t `arrayOfRow` w)
  map_res_all <- transformInnerMap segments env inps map_pat w arrs map_lam
  (ws_F, ws_O, ws_S, elems, elems_kind) <-
    prepareSegOpInputs segments env inps w map_res_all (patNames map_pat)
  id_lam <- mkIdentityLambda $ lambdaReturnType map_lam
  elems' <- doSegMaposcanomap scans ws_F elems post_lam id_lam segments inps env
  insertSegOpMapResults
    segments
    ws_S
    ws_F
    ws_O
    elems_kind
    (zip res elems')
    env

transformPrePostMaposcanomap ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  [Scan SOACS] ->
  Lambda SOACS ->
  Builder GPU DistEnv
transformPrePostMaposcanomap segments env inps res pat w arrs post_lam scans map_lam = do
  map_pat <- fmap Pat $ forM (lambdaReturnType map_lam) $ \t ->
    PatElem <$> newVName "map" <*> pure (t `arrayOfRow` w)

  map_res_all <- transformInnerMap segments env inps map_pat w arrs map_lam

  let num_scan_results = scanResults scans
      post_params = lambdaParams post_lam
      (scan_res_names, _) = splitAt num_scan_results $ patNames map_pat
      (scan_params, _) = splitAt num_scan_results post_params
      (scan_res, map_res) = splitAt num_scan_results map_res_all

  (ws_F, ws_O, ws_S, scan_elems, scan_elems_kind) <-
    prepareSegOpInputs segments env inps w scan_res scan_res_names

  scan_elems' <- doSegScan scans ws_F scan_elems segments inps env

  scan_res' <-
    zipWithM
      ( \elem' scan_param ->
          postMapResultRep
            segments
            env
            inps
            w
            ws_F
            ws_O
            ws_S
            elem'
            scan_elems_kind
            scan_param
      )
      scan_elems'
      scan_params

  transformMaposcanomapPostReps
    segments
    env
    inps
    res
    pat
    w
    (scan_res' <> map_res)
    post_lam

-- Hacky fix to get result representations in the same order as the pattern
resRepsInPatOrder :: Pat Type -> [(VName, ResRep)] -> [ResRep]
resRepsInPatOrder pat reps =
  let rep_map = M.fromList reps
      lookupRes v =
        case M.lookup v rep_map of
          Just rep -> rep
          Nothing ->
            error $
              "resRepsInPatOrder: missing result for "
                ++ prettyString v
   in map lookupRes (patNames pat)

segOpInputRep ::
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  Builder GPU ResRep
segOpInputRep segments env inps arr =
  case lookup arr inps of
    Just (DistInput rt _) ->
      pure $ resVar rt env
    Just (DistInputFree arr' _) ->
      pure $ Regular arr'
    Nothing ->
      Irregular <$> getIrregRep segments env inps arr

-- Basically we need to make our arrays ready for our segscan/segred.
-- Regular arrays are flattened only across the outer segment dimensions and
-- the SOAC width; any row shape expected by the consumer is preserved.
-- we need to check the dense/replicated status of the input.
-- if all of scan inputs are replicated we are fine.
-- otherwise, we need to make the replicated inputs dense.
-- for regulars we can just use the segment descriptor and this should be also the same descriptor for dense irregulars.
prepareSegOpInputs ::
  Segments ->
  DistEnv ->
  DistInputs ->
  SubExp ->
  [ResRep] ->
  [VName] ->
  Builder GPU (VName, VName, VName, [VName], IrregularKind)
prepareSegOpInputs segments env inps w reps names
  | all isRegular reps = do
      ws <- dataArr segments env inps w
      (ws_F, ws_O, ws_data) <- doRepIota ws
      m <- arraySize 0 <$> lookupType ws_data
      names' <- mapM (flattenRegularRep m) reps
      pure (ws_F, ws_O, ws, names', Dense)
  | all isReplicatedIrregular reps = do
      let Irregular rep0 = head reps
      pure (irregularF rep0, irregularO rep0, irregularS rep0, map getData reps, Replicated)
  | otherwise = do
      desc_rep <- findOrMakeDense reps
      m <- arraySize 0 <$> lookupType (irregularD desc_rep)
      names' <- zipWithM (normalise m) reps names
      pure (irregularF desc_rep, irregularO desc_rep, irregularS desc_rep, names', Dense)
  where
    isRegular (Regular _) = True
    isRegular _ = False

    isReplicatedIrregular (Irregular rep) = irregularK rep == Replicated
    isReplicatedIrregular _ = False

    flattenRegularRep m (Regular v) =
      flattenRegularToRows segments m v
    flattenRegularRep _ _ =
      error "prepareSegOpInputs: impossible irregular regular input"
    getData (Irregular rep) = irregularD rep
    getData _ = error "prepareSegOpInputs: impossible"

    findOrMakeDense rs =
      case [rep | Irregular rep <- rs, irregularK rep == Dense] of
        rep : _ -> pure rep
        [] ->
          case [rep | Irregular rep <- rs] of
            rep : _ -> ensureDenseIrregular "segop_desc" rep
            [] -> error "prepareSegOpInputs: impossible"

    normalise m rep v =
      case rep of
        Regular v' ->
          flattenRegularToRows segments m v'
        Irregular ir
          | irregularK ir == Dense ->
              pure $ irregularD ir
          | otherwise ->
              irregularD <$> ensureDenseIrregular (baseName v <> "_dense") ir

flattenRegularToRows :: Segments -> SubExp -> VName -> Builder GPU VName
flattenRegularToRows segments m v = do
  v_t <- lookupType v
  when (arrayRank v_t < segmentsRank segments + 1) $
    error "prepareSegOpInputs: regular input rank too small"
  let row_shape = arrayShape $ stripArray (segmentsRank segments + 1) v_t
  letExp (baseName v <> "_flat") . BasicOp $
    Reshape v $
      reshapeAll (arrayShape v_t) (Shape [m] <> row_shape)

insertSegOpMapResults ::
  Segments ->
  VName ->
  VName ->
  VName ->
  IrregularKind ->
  [(DistResult, VName)] ->
  DistEnv ->
  Builder GPU DistEnv
insertSegOpMapResults segments segs flags offsets kind bnds env0 =
  foldM insert env0 bnds
  where
    insert env (dist_res, v)
      | isRegularDistResult dist_res = do
          let DistType _ _ t = distResType dist_res
              expected_shape = segmentsShape segments <> arrayShape t
          v_t <- lookupType v
          v' <-
            letExp (baseName v <> "_reshaped") . BasicOp $
              Reshape v $
                reshapeAll (arrayShape v_t) expected_shape
          pure $ insertRegulars [distResTag dist_res] [v'] env
      | otherwise =
          pure $ insertIrregular segs flags offsets (distResTag dist_res) v kind env

transformDistStm :: Segments -> DistEnv -> DistStm -> Builder GPU DistEnv
transformDistStm segments env (DistStm inps res (ScalarStm stms)) =
  transformScalarStms segments env inps res (stmsFromList stms)
transformDistStm segments env (DistStm inps res (ParallelStm stm)) = do
  case stm of
    Let pat aux (BasicOp e) -> do
      let ~[res'] = res
          ~[pe] = patElems pat
      transformDistBasicOp segments env (inps, res', pe, aux, e)
    Let pat aux (Op (Screma w arrs form))
      | Just reds <- isReduceSOAC form,
        all (\red -> suitableOperator env inps (redLambda red) (redNeutral red)) reds -> do
          traceM "HELLO REDUCE"
          reps <- mapM (segOpInputRep segments env inps) arrs
          (flags, offsets, arr_segments, elems, _elems_kind) <-
            prepareSegOpInputs segments env inps w reps arrs
          -- TODO: FixME: this is temp hack
          let sing_red = singleReduce reds
          let zeros = replicate (length segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
          nes' <- mapM (readInput segments env zeros inps) (redNeutral sing_red)
          let sing_red' = sing_red {redNeutral = nes'}
          elems' <- genSegRed arr_segments flags offsets elems sing_red'
          elems'' <- forM elems' $ \v -> do
            v_t <- lookupType v
            letExp (baseName v <> "_reshaped") . BasicOp $
              Reshape v $
                reshapeAll (arrayShape v_t) (segmentsShape segments)
          pure $ insertRegulars (map distResTag res) elems'' env
      | Just (reds, map_lam) <- isRedomapSOAC form,
        suitableSegOpMap env inps map_lam,
        all (\red -> suitableOperator env inps (redLambda red) (redNeutral red)) reds -> do
          traceM "HELLO Fast REDOMAP"
          reps <- mapM (segOpInputRep segments env inps) arrs
          (ws_F, ws_O, ws_S, elems, elems_kind) <-
            prepareSegOpInputs segments env inps w reps arrs
          let sing_red = singleReduce reds
          let zeros = replicate (length segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
          nes' <- mapM (readInput segments env zeros inps) (redNeutral sing_red)
          let sing_red' = sing_red {redNeutral = nes'}
          (red_elems, mapout_elems) <-
            genSegRedomap ws_S ws_F ws_O elems sing_red' (soacsLambdaToGPU map_lam)
          red_elems' <- forM red_elems $ \v -> do
            v_t <- lookupType v
            letExp (baseName v <> "_reshaped") . BasicOp $
              Reshape v $
                reshapeAll (arrayShape v_t) (segmentsShape segments)
          let (red_res, map_res) = splitAt (redResults reds) res
          env' <-
            insertSegOpMapResults
              segments
              ws_S
              ws_F
              ws_O
              elems_kind
              (zip map_res mapout_elems)
              env
          pure $ insertRegulars (map distResTag red_res) red_elems' env'
      | Just (reds, map_lam) <- isRedomapSOAC form,
        all (\red -> suitableOperator env inps (redLambda red) (redNeutral red)) reds -> do
          traceM "HELLO REDOMAP"
          map_pat <- fmap Pat $ forM (lambdaReturnType map_lam) $ \t ->
            PatElem <$> newVName "map" <*> pure (t `arrayOfRow` w)
          map_res_all <-
            transformInnerMap segments env inps map_pat w arrs map_lam
          let (redout_names, _) = splitAt (redResults reds) (patNames map_pat)
              (redout_res, mapout_res) = splitAt (redResults reds) map_res_all
          (ws_F, ws_O, ws_S, redout_names', _redout_kind) <-
            prepareSegOpInputs segments env inps w redout_res redout_names
          -- For multi-dim (Regular) results, flatten to 1D before segmented reduce.
          -- TODO: FixME: this is temp hack
          let sing_red = singleReduce reds
          let zeros = replicate (length segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
          nes' <- mapM (readInput segments env zeros inps) (redNeutral sing_red)
          let sing_red' = sing_red {redNeutral = nes'}
          elems' <-
            genSegRed ws_S ws_F ws_O redout_names' sing_red'
          elems'' <- forM elems' $ \v -> do
            v_t <- lookupType v
            letExp (baseName v <> "_reshaped") . BasicOp $
              Reshape v $
                reshapeAll (arrayShape v_t) (segmentsShape segments)
          let (red_tags, map_tags) = splitAt (redResults reds) $ map distResTag res
          pure $
            insertRegulars red_tags elems'' $
              insertReps (zip map_tags mapout_res) env
      | Just scans <- isScanSOAC form,
        all (\scan -> suitableOperator env inps (scanLambda scan) (scanNeutral scan)) scans -> do
          reps <- mapM (segOpInputRep segments env inps) arrs
          (flags, offsets, arr_segments, elems, elems_kind) <-
            prepareSegOpInputs segments env inps w reps arrs
          elems' <- doSegScan scans flags elems segments inps env
          pure $
            insertIrregulars arr_segments flags offsets (zip (map distResTag res) elems') elems_kind env
      | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
        suitableSegOpMap env inps map_lam,
        suitableSegOpMap env inps post_lam,
        all (\scan -> suitableOperator env inps (scanLambda scan) (scanNeutral scan)) scans -> do
          traceM "Status: everything integetrated"
          reps <- mapM (segOpInputRep segments env inps) arrs
          (ws_F, ws_O, ws_S, elems, elems_kind) <-
            prepareSegOpInputs segments env inps w reps arrs
          elems' <- doSegMaposcanomap scans ws_F elems post_lam map_lam segments inps env
          insertSegOpMapResults
            segments
            ws_S
            ws_F
            ws_O
            elems_kind
            (zip res elems')
            env
      | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
        suitableSegOpMap env inps map_lam,
        not $ suitableSegOpMap env inps post_lam,
        all (\scan -> suitableOperator env inps (scanLambda scan) (scanNeutral scan)) scans -> do
          traceM "Status: pre map integetrated"
          transformPostMaposcanomap segments env inps res pat w arrs post_lam scans map_lam
      | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
        suitableSegOpMap env inps post_lam,
        not $ suitableSegOpMap env inps map_lam,
        all (\scan -> suitableOperator env inps (scanLambda scan) (scanNeutral scan)) scans -> do
          traceM "Status: post map integetrated"
          transformPreMaposcanomap segments env inps res w arrs post_lam scans map_lam
      | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
        all (\scan -> suitableOperator env inps (scanLambda scan) (scanNeutral scan)) scans -> do
          traceM "Status Nothing integerated"
          transformPrePostMaposcanomap segments env inps res pat w arrs post_lam scans map_lam
      | Just map_lam <- isMapSOAC form,
        all isVersionableRegularResult res ->
          versionedRegularMap segments env inps res pat aux w arrs form map_lam
      | Just map_lam <- isMapSOAC form -> do
          map_res <-
            transformInnerMap segments env inps pat w arrs map_lam
          pure $ insertReps (zip (map distResTag res) map_res) env
      | otherwise -> do
          -- XXX: here we silently sequentialise any SOAC that is not handled
          -- above. We need to make sure that we actually handle everything we
          -- care about!
          error "unhandled SOAC"
    -- transformScalarStm segments env inps res $
    --   Let { stmPat = pat, stmAux = aux, stmExp = Op (Screma w arrs form) }
    Let _ aux (Match scrutinees cases defaultCase rt) ->
      if any (isVariant inps env) scrutinees
        then
          transformMatch flattenOps segments env inps res scrutinees cases defaultCase
        -- else error $ unlines ["scrutinees: ", prettyString scrutinees, "cases:", prettyString cases, "defaultCase:", prettyString defaultCase]
        else do
          scope <- askScope
          new_cases <- forM cases $ \(Case c body) -> do
            let (case_body_inputs, case_dstms) = distributeBody scope segments inps body

            (case_body_res, case_body_stms) <-
              runReaderT
                ( runBuilder $
                    liftBodyWithDistResults segments case_body_inputs env case_dstms res (bodyResult body)
                )
                scope
            pure $ Case c $ Body () case_body_stms case_body_res
          new_default_body <- do
            let (new_default_body_inputs, new_default_dstms) = distributeBody scope segments inps defaultCase
            (new_default_body_res, new_default_body_stms) <-
              runReaderT
                ( runBuilder $
                    liftBodyWithDistResults segments new_default_body_inputs env new_default_dstms res (bodyResult defaultCase)
                )
                scope
            pure $ Body () new_default_body_stms new_default_body_res

          -- Maybe it is better to build MatchDec ourselves
          match_e <-
            eMatch'
              scrutinees
              [Case c (pure body) | Case c body <- new_cases]
              (pure new_default_body)
              (matchSort rt)

          match_res <-
            certifying (distCerts inps aux env) $
              letTupExp "match_res" match_e

          rets <- expExtType match_e
          -- get rid of the existential context
          traceM $ unlines ["match res type:", prettyString rets]
          let payload_res = drop (S.size (shapeContext rets)) match_res
          let reps = distResultsToResReps res payload_res
          pure $ insertReps (zip (map distResTag res) reps) env
    Let _ _ (Apply name args rettype s) -> do
      let name' = liftFunName name
      w <- letSubExp "num_segments" =<< toExp (segmentCount segments)
      args' <- ((w, Observe) :) . concat <$> mapM (liftArg segments w inps env) args
      args_ts <- mapM (subExpType . fst) args'
      let dietToUnique Consume = Unique
          dietToUnique Observe = Nonunique
          dietToUnique ObservePrim = Nonunique
          param_ts = zipWith toDecl args_ts $ map (dietToUnique . snd) args'
          rettype' = addRetAls param_ts $ liftRetType w $ map fst rettype
      result <- letTupExp (name' <> "_res") $ Apply name' args' rettype' s
      reps <-
        zipWithM (reshapeLiftedApplyResult segments) (map fst rettype) $
          resultToResReps (map fst rettype) result
      pure $ insertReps (zip (map distResTag res) reps) env
    Let _ aux (Loop merge (ForLoop i it n) body) -> do
      if isVariant inps env n
        then transformFortoWhile segments env inps res aux merge i it n body
        else do
          let old_loop_params = map fst merge
              old_loop_inits = map snd merge
              loopParamNames = S.fromList $ map paramName old_loop_params

          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          (lifted_loop_params, lifted_loop_reps, lifted_init) <-
            unzip3 <$> mapM (liftLoopParam segments num_segments inps env loopParamNames) (zip old_loop_params old_loop_inits)

          let lifted_loop_params' = concat lifted_loop_params
              lifted_init' = concat lifted_init

          traceM $ unlines ["lifted_loop_params:", prettyString lifted_loop_params', "lifted_init:", prettyString lifted_init']

          let (inps_local, env_local0, next0) = localiseInputs env inps
              loop_param_inputs_local =
                zipWith
                  (\p j -> (paramName p, DistInput (ResTag j) (paramType p)))
                  old_loop_params
                  [next0 ..]

              loop_param_reps_local =
                zipWith
                  (\j rep -> (ResTag j, rep))
                  [next0 ..]
                  lifted_loop_reps
              loop_new_inputs = inps_local <> loop_param_inputs_local
              loop_env_local = insertReps loop_param_reps_local env_local0

          let i_param = Param mempty i (Prim (IntType it))
          let build_scope = scopeOfFParams lifted_loop_params' <> scopeOfLParams [i_param]
          scope <- askScope
          let (loop_new_inputs', loop_dstms) =
                distributeBody scope segments loop_new_inputs body

          (loop_body_res, loop_body_stms) <-
            runReaderT
              ( runBuilder $
                  liftLoopBody segments num_segments loop_new_inputs' loop_env_local loop_dstms res (bodyResult body)
              )
              (scope <> build_scope)

          let loop_body_gpu = Body () loop_body_stms loop_body_res
              loop_exp_gpu =
                Loop
                  (zip lifted_loop_params' lifted_init')
                  (ForLoop i it n)
                  loop_body_gpu

          loop_out_vs <-
            certifying (distCerts inps aux env) $
              letTupExp "loop_res_out" loop_exp_gpu

          let out_reps = loopResultToResReps res loop_out_vs
          pure $ insertReps (zip (map distResTag res) out_reps) env
    Let _ aux (Loop merge (WhileLoop cond) body) -> do
      -- TODO:
      -- 4) Use reduction rather than scan for any_active
      -- 5) Consider updating the active segment so we don't go over w everytime

      -- inside the body we should compute the indices for which the condition is true and for which it is false, and then distribute the body based on that.
      --  We can then merge the results of the two branches by writing them back to a blank space like we do for the branches of a match.

      let old_loop_params = map fst merge
          old_loop_inits = map snd merge
          loopParamNames = S.fromList $ map paramName old_loop_params
      w <- letSubExp "num_segments" =<< toExp (segmentCount segments)
      (lifted_loop_params, lifted_loop_reps, lifted_init) <-
        unzip3 <$> mapM (liftLoopParam segments w inps env loopParamNames) (zip old_loop_params old_loop_inits)

      let lifted_loop_params' = concat lifted_loop_params
          lifted_init' = concat lifted_init

      -- find cond_lifted_param in old_lifted_loop_params to get the lifted_loop_reps
      let (inps_local, env_local0, next0) = localiseInputs env inps
          loop_param_inputs_local =
            zipWith
              (\p j -> (paramName p, DistInput (ResTag j) (paramType p)))
              old_loop_params
              [next0 ..]
          loop_param_reps_local =
            zipWith
              (\j rep -> (ResTag j, rep))
              [next0 ..]
              lifted_loop_reps
          loop_new_inputs = inps_local <> loop_param_inputs_local
          loop_env_local = insertReps loop_param_reps_local env_local0

      let maybe_cond = lookup cond (zip (map paramName old_loop_params) (zip lifted_loop_reps lifted_init))
      scope <- askScope
      case maybe_cond of
        -- infinite loop . later can be uniform case as well.
        -- !!! TODO: !!!! update this as well.
        Nothing -> do
          let build_scope = scopeOfFParams lifted_loop_params'
          let (loop_new_inputs', loop_dstms) = distributeBody scope segments loop_new_inputs body
          (loop_body_res, loop_body_stms) <-
            runReaderT
              (runBuilder $ liftBody w loop_new_inputs' loop_env_local loop_dstms (bodyResult body))
              (scope <> build_scope)
          let loop_body_gpu = Body () loop_body_stms loop_body_res
              loop_exp_gpu = Loop (zip lifted_loop_params' lifted_init') (WhileLoop cond) loop_body_gpu
          loop_out_vs <- certifying (distCerts inps aux env) $ letTupExp "loop_res_out" loop_exp_gpu
          let result_types = map ((\(DistType _ _ t) -> t) . distResType) res
              out_reps = resultToResReps result_types loop_out_vs
          pure $ insertReps (zip (map distResTag res) out_reps) env
        Just (cond_lifted_rep, cond_init) -> do
          let [cond_init_se] = cond_init

          -- Compute initial any_active
          cond_init_arr_v <- letExp "cond_init_arr" $ BasicOp $ SubExp cond_init_se
          let cond_lifted_param = case cond_lifted_rep of
                Regular v -> v
                Irregular {} -> error "WhileLoop condition cannot be irregular"

          -- latter chagne to reduction
          cond_init_arr_t <- lookupType cond_init_arr_v
          cond_init_flat <-
            letExp "cond_init_flat" . BasicOp $
              Reshape cond_init_arr_v $
                reshapeAll (arrayShape cond_init_arr_t) (Shape [w])

          or_lam <- binOpLambda LogOr Bool
          cond_scanned <- genScan "any_scan" (NE.singleton w) or_lam [constant False] [cond_init_flat]
          let [cond_scanned_v] = cond_scanned

          any_active_init <-
            letSubExp "any_active_init"
              =<< eIf
                (toExp $ pe64 w .==. 0)
                (eBody [eSubExp $ constant False])
                (eBody [eIndex cond_scanned_v [toExp $ pe64 w - 1]])

          any_active_param <- newParam "any_active" (Prim Bool)
          let build_scope = scopeOfFParams lifted_loop_params' <> scopeOfFParams [any_active_param]
          -- ‌build body
          (loop_body_res, loop_body_stms) <-
            runReaderT
              ( runBuilder $ do
                  -- (num_data, active_inds) <- genFilter cond_lifted_param
                  equiv_classes <- letExp "equiv_classes" <=< segMap (MkSolo w) $ \(MkSolo i) -> do
                    let seg_is = unflattenIndex (segmentDims segments) (pe64 i)
                    c <- letSubExp "c" =<< eIndex cond_lifted_param (map toExp seg_is)
                    cls <-
                      letSubExp "cls"
                        =<< eIf
                          (eSubExp c)
                          (eBody [toExp $ intConst Int64 1])
                          (eBody [toExp $ intConst Int64 0])
                    pure [subExpRes cls]
                  n_cases <- letExp "n_cases" <=< toExp $ intConst Int64 2
                  (partition_sizes, partition_offs, partition_inds) <- doPartition n_cases equiv_classes
                  inds_t <- lookupType partition_inds

                  let getInds nm k = do
                        sz <-
                          letSubExp (nm <> "_sz")
                            =<< eIndex partition_sizes [toExp $ intConst Int64 k]
                        off <-
                          letSubExp (nm <> "_off")
                            =<< eIndex partition_offs [toExp $ intConst Int64 k]
                        inds <-
                          letExp (nm <> "_inds") $
                            BasicOp $
                              Index partition_inds $
                                fullSlice inds_t [DimSlice off sz (intConst Int64 1)]
                        pure (sz, inds)

                  (_, inactive_inds) <- getInds "inactive" 0
                  (active_size, active_inds) <- getInds "active" 1

                  inactive_reps <- forM old_loop_params $ \p -> do
                    (_, _, rep) <- splitInput segments loop_new_inputs loop_env_local inactive_inds (paramName p)
                    pure rep

                  let free_in_body =
                        filter
                          (isVariant loop_new_inputs loop_env_local . Var)
                          (namesToList $ freeIn body)
                  (ts, vs, reps) <- unzip3 <$> mapM (splitInput segments loop_new_inputs loop_env_local active_inds) free_in_body
                  let subset_inputs = do
                        (v, t, i) <- zip3 vs ts [0 ..]
                        pure (v, DistInput (ResTag i) t)
                      env_subset = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
                  let subset_segments = NE.singleton active_size
                  let (subset_inputs', subset_dstms) = distributeBody scope subset_segments subset_inputs body
                  env_subset' <- foldM (transformDistStm subset_segments) env_subset subset_dstms
                  active_reps <-
                    zipWithM
                      (liftDistResultRep subset_segments subset_inputs' env_subset')
                      res
                      (bodyResult body)

                  let mergeOneLifted t rep0 rep1 =
                        case (rep0, rep1) of
                          (Regular x0, Regular x1) -> do
                            let initial_shape = Shape [w] <> arrayShape t
                            let final_shape = segmentsShape segments <> arrayShape t
                            let pt = elemType t
                            space <- letExp "blank" =<< eBlank (Array pt initial_shape NoUniqueness)

                            out <-
                              foldM
                                scatterRegular
                                space
                                [(inactive_inds, x0), (active_inds, x1)]

                            out_type <- arrayShape <$> lookupType out
                            out_reshaped <-
                              letExp "out_reshaped" . BasicOp $
                                Reshape out $
                                  reshapeAll out_type final_shape

                            pure [SubExpRes mempty (Var out_reshaped)]
                          (Irregular ir0, Irregular ir1) -> do
                            segsSpace <-
                              letExp "blank_segs"
                                =<< eBlank (Array int64 (Shape [w]) NoUniqueness)

                            segs <-
                              foldM
                                scatterRegular
                                segsSpace
                                [(inactive_inds, irregularS ir0), (active_inds, irregularS ir1)]

                            (_, offsets, num_data) <- exScanAndSum segs

                            let pt = elemType t
                            elemsSpace <-
                              letExp "blank_elems"
                                =<< eBlank (Array pt (Shape [num_data]) NoUniqueness)

                            elems <-
                              foldM
                                (scatterIrregular offsets)
                                elemsSpace
                                [(inactive_inds, ir0), (active_inds, ir1)]

                            flags <- genFlags num_data offsets

                            pure
                              [ SubExpRes mempty num_data,
                                SubExpRes mempty (Var segs),
                                SubExpRes mempty (Var flags),
                                SubExpRes mempty (Var offsets),
                                SubExpRes mempty (Var elems)
                              ]
                          _ -> error "mergeOneLifted: mismatched reps"

                  merged_results <-
                    concat
                      <$> zipWithM
                        (\p (r0, r1) -> mergeOneLifted (declTypeOf p) r0 r1)
                        old_loop_params
                        (zip inactive_reps active_reps)

                  -- we have one extra iteration but it is better than extra reduction in the loop body,
                  any_active <-
                    letSubExp "any_active"
                      =<< eIf
                        (toExp $ pe64 active_size .==. 0)
                        (eBody [eSubExp $ constant False])
                        (eBody [eSubExp $ constant True])

                  pure $ merged_results ++ [SubExpRes mempty any_active]
              )
              (scope <> build_scope)

          let loop_body_gpu = Body () loop_body_stms loop_body_res
              loop_exp_gpu =
                Loop
                  (zip (lifted_loop_params' ++ [any_active_param]) (lifted_init' ++ [any_active_init]))
                  (WhileLoop (paramName any_active_param))
                  loop_body_gpu

          loop_out_vs <-
            certifying (distCerts inps aux env) $
              letTupExp "loop_res_out" loop_exp_gpu
          let loop_out_vs' = L.init loop_out_vs
          let out_reps = loopResultToResReps res loop_out_vs'
          pure $ insertReps (zip (map distResTag res) out_reps) env
    Let pat aux (WithAcc inputs lam) ->
      transformWithAcc flattenOps segments env inps res pat aux inputs lam
    Let _ _ (Op (Stream {})) -> error "transformDistStm: Stream should have been removed"
    Let _ _ (Op (Hist {})) -> error "Unhandled Hist"
    Let _ _ (Op (JVP {})) -> error "Unhandled JVP"
    Let _ _ (Op (VJP {})) -> error "Unhandled VJP"

-- helper to not mess up the tags when generating new ones for the loop parameters
-- probably won't be used in future
localiseInputs :: DistEnv -> DistInputs -> (DistInputs, DistEnv, Int)
localiseInputs env_outer inps =
  let step (i, env_acc) (v, inp) =
        case inp of
          DistInputFree arr t ->
            ((i, env_acc), (v, DistInputFree arr t))
          DistInput oldrt t ->
            let newrt = ResTag i
                rep = resVar oldrt env_outer
                env_acc' = insertRep newrt rep env_acc
             in ((i + 1, env_acc'), (v, DistInput newrt t))

      ((next, env_local), inps_local) =
        L.mapAccumL step (0, mempty) inps
   in (inps_local, env_local, next)

distResCerts :: DistEnv -> [DistInput] -> Certs
distResCerts env = Certs . map f
  where
    f (DistInputFree v _) = v
    f (DistInput rt _) = case resVar rt env of
      Regular v -> v
      Irregular {} -> error "resCerts: irregular"

reshapeAndBind :: VName -> VName -> Shape -> Builder GPU ()
reshapeAndBind v src shape = do
  v_copy <- letExp (baseName v) . BasicOp $ Replicate mempty (Var src)
  v_copy_shape <- arrayShape <$> lookupType v_copy
  letBindNames [v] $ BasicOp $ Reshape v_copy $ reshapeAll v_copy_shape shape

mapResultRep :: InnerMapMode -> (VName, VName, VName) -> VName -> Builder GPU ResRep
mapResultRep MultiDim _ v = pure $ Regular v
mapResultRep SingleDim (ws, ws_F, ws_O) v =
  -- Forcing the irregular rep to be 1D because in some places that is my assumption
  -- and also this will make the metadata consistent.
  Irregular
    <$> flattenIrregularRep
      IrregularRep
        { irregularS = ws,
          irregularF = ws_F,
          irregularO = ws_O,
          irregularD = v,
          irregularK = Dense
        }

resultMapMode :: InnerMapMode -> DistInputs -> Type -> InnerMapMode
resultMapMode SingleDim _ _ = SingleDim
resultMapMode MultiDim new_inps v_t
  | any isTypeVariant (arrayDims v_t) = SingleDim
  | otherwise = MultiDim
  where
    new_inp_var = S.fromList $ map fst new_inps
    isTypeVariant se = case se of
      Var v -> v `S.member` new_inp_var
      _ -> False

irregularMapResult ::
  InnerMapMode ->
  (VName, VName, VName) ->
  Segments ->
  IrregularRep ->
  VName ->
  Type ->
  DistInputs ->
  Builder GPU ResRep
irregularMapResult mode (ws, ws_F, ws_O) segments irreg v v_t new_inps =
  do
    irreg_dense <- ensureDenseIrregular (baseName v <> "_map_result") irreg
    if any (isTypeVariant new_inp_var) (arrayShape v_t)
      then do
        old_segment <- arraySize 0 <$> lookupType ws
        new_shape <- letExp (baseName v <> "_outer_shape") <=< segMap (MkSolo old_segment) $ \(MkSolo is) -> do
          outer_ind <- letSubExp "outer_ind" =<< eIndex ws_O [eSubExp is]
          outer_ws_i <- letSubExp "outer_ws" =<< eIndex ws [eSubExp is]
          sz <-
            letSubExp "sz"
              =<< eIf
                (toExp $ pe64 outer_ws_i .==. 0)
                (eBody [toExp $ intConst Int64 0])
                ( do
                    last_row <- letSubExp "last_row" <=< toExp $ pe64 outer_ind + pe64 outer_ws_i - 1
                    start <- letSubExp "start" =<< eIndex (irregularO irreg_dense) [eSubExp outer_ind]
                    last_offset <- letSubExp "last_offset" =<< eIndex (irregularO irreg_dense) [eSubExp last_row]
                    last_size <- letSubExp "last_size" =<< eIndex (irregularS irreg_dense) [eSubExp last_row]
                    eBody [toExp $ pe64 last_offset - pe64 start + pe64 last_size]
                )
          pure [subExpRes sz]
        (new_ws_F, new_ws_O, _) <- doRepIota new_shape
        letBindNames [v] $ BasicOp $ Replicate mempty $ Var $ irregularD irreg_dense
        mapResultRep SingleDim (new_shape, new_ws_F, new_ws_O) v
      else case mode of
        MultiDim -> do
          reshapeAndBind v (irregularD irreg_dense) (segmentsShape segments <> arrayShape v_t)
          mapResultRep MultiDim (ws, ws_F, ws_O) v
        SingleDim -> do
          -- TODO: have to do this even it seems very annoying should think something better
          reshapeAndBind v (irregularD irreg_dense) (segmentsShape segments <> arrayShape v_t)
          mapResultRep SingleDim (ws, ws_F, ws_O) v
  where
    isTypeVariant vin se = case se of
      Var v' -> S.member v' vin
      _ -> False
    new_inp_var = S.fromList $ map fst new_inps

transformDistributedInnerMap ::
  InnerMapMode ->
  (VName, VName, VName) ->
  M.Map ResTag IrregularRep ->
  Segments ->
  Distributed ->
  Builder GPU [(VName, ResRep)]
transformDistributedInnerMap mode (ws_F, ws_O, ws) irregs segments dist = do
  let Distributed dstms (DistResults resmap reps) = dist
  let new_inps = concatMap distStmInputs dstms
  env <- foldM (transformDistStm segments) env_initial dstms
  resmap_res <- fmap concat $ forM (M.toList resmap) $ \(rt, binds) ->
    forM binds $ \(cs_inps, v, v_t) ->
      certifying (distResCerts env cs_inps) $
        -- FIXME: the copies are because we have too liberal aliases on
        -- lifted functions.
        case (resultMapMode mode new_inps v_t, resVar rt env) of
          (MultiDim, Regular v') -> do
            if isAcc v_t
              then do
                letBindNames [v] $ BasicOp $ Replicate mempty $ Var v'
                pure (v, Regular v)
              else do
                reshapeAndBind v v' (segmentsShape segments <> arrayShape v_t)
                pure (v, Regular v)
          (SingleDim, Regular v') -> do
            if isAcc v_t
              then do
                letBindNames [v] $ BasicOp $ Replicate mempty $ Var v'
                pure (v, Regular v)
              else do
                letBindNames [v] $ BasicOp $ Replicate mempty $ Var v'
                rep <- mapResultRep SingleDim (ws, ws_F, ws_O) v
                pure (v, rep)
          (result_mode, Irregular irreg) -> do
            rep <- irregularMapResult result_mode (ws, ws_F, ws_O) segments irreg v v_t new_inps
            pure (v, rep)
  reps_res <- forM reps $ \(v, r) -> do
    case r of
      Left se -> do
        letBindNames [v] $ BasicOp $ Replicate (segmentsShape segments) se
        -- the se is not part of input so this should be fine
        rep <- mapResultRep mode (ws, ws_F, ws_O) v
        pure (v, rep)
      Right (DistInputFree arr t) -> do
        letBindNames [v] $ BasicOp $ SubExp $ Var arr
        rep <- mapResultRep (resultMapMode mode new_inps t) (ws, ws_F, ws_O) v
        pure (v, rep)
      Right (DistInput rt t) ->
        let result_mode = resultMapMode mode new_inps t
         in case resVar rt env of
              Regular v' -> do
                letBindNames [v] $ BasicOp $ SubExp $ Var v'
                rep <- mapResultRep result_mode (ws, ws_F, ws_O) v
                pure (v, rep)
              Irregular irreg -> do
                rep <- irregularMapResult result_mode (ws, ws_F, ws_O) segments irreg v t new_inps
                pure (v, rep)
  pure $ resmap_res <> reps_res
  where
    env_initial = DistEnv {distResMap = M.map Irregular irregs}

transformDistributed ::
  M.Map ResTag IrregularRep ->
  Segments ->
  Distributed ->
  Builder GPU ()
transformDistributed irregs segments dist = do
  let Distributed dstms (DistResults resmap reps) = dist
  env <- foldM (transformDistStm segments) env_initial dstms
  forM_ (M.toList resmap) $ \(rt, binds) ->
    forM_ binds $ \(cs_inps, v, v_t) ->
      certifying (distResCerts env cs_inps) $
        -- FIXME: the copies are because we have too liberal aliases on
        -- lifted functions.
        case resVar rt env of
          Regular v' -> letBindNames [v] $ BasicOp $ Replicate mempty $ Var v'
          Irregular irreg ->
            -- It might have an irregular representation, but we know
            -- that it is actually regular because it is a result.
            do
              irreg' <- ensureDenseIrregular (baseName v <> "_dist_res") irreg
              reshapeAndBind v (irregularD irreg') (segmentsShape segments <> arrayShape v_t)
  forM_ reps $ \(v, r) ->
    case r of
      Left se ->
        letBindNames [v] $ BasicOp $ Replicate (segmentsShape segments) se
      Right (DistInputFree arr _) ->
        letBindNames [v] $ BasicOp $ SubExp $ Var arr
      -- This can happen. ask Troels
      Right (DistInput rt t) ->
        case resVar rt env of
          Regular v' -> letBindNames [v] $ BasicOp $ SubExp $ Var v'
          Irregular irreg ->
            do
              irreg' <- ensureDenseIrregular (baseName v <> "_dist_rep") irreg
              reshapeAndBind v (irregularD irreg') (segmentsShape segments <> arrayShape t)
  where
    env_initial = DistEnv {distResMap = M.map Irregular irregs}

-- Check whether a loop parameter array needs irregular representation.
-- we need the irregular representation when any of its dimensions are either:
-- a loop parameter name or variant in the outer map context

needsIrregular :: DistInputs -> DistEnv -> S.Set VName -> DeclType -> Bool
needsIrregular inps env loopParamNames t =
  case t of
    Array {} -> any dimIsVariant (arrayDims t)
    _ -> False
  where
    dimIsVariant (Constant _) = False
    dimIsVariant (Var v) = v `S.member` loopParamNames || isVariant inps env (Var v)

-- Lift a loop parameter and its initial value together.
-- If the parameter is an array whose dimensions are all invariant,
-- we lift it to a regular array. Otherwise we fall back to irregular.
liftLoopParam ::
  Segments ->
  SubExp ->
  DistInputs ->
  DistEnv ->
  S.Set VName ->
  (FParam SOACS, SubExp) ->
  Builder GPU ([FParam GPU], ResRep, [SubExp])
liftLoopParam segments num_segments inps env loopParamNames (fparam, initSE) = do
  let t = declTypeOf fparam
  case t of
    Prim pt -> do
      param <-
        newParam
          (baseName (paramName fparam) <> "_lifted")
          (arrayOf (Prim pt) (segmentsShape segments) Nonunique)
      initV <- liftSubExpRegular segments inps env (segmentsShape segments) initSE
      pure ([param], Regular $ paramName param, [Var initV])
    Array pt _ u
      | needsIrregular inps env loopParamNames t -> do
          (params, rep) <- liftParam num_segments fparam
          initVals <- liftLoopInit segments inps env initSE num_segments
          pure (params, rep, initVals)
      | otherwise -> do
          -- Regular case: all dims are invariant, just add w as outermost dim
          let pShape = segmentsShape segments <> arrayShape t
          p <-
            newParam
              (baseName (paramName fparam) <> "_lifted")
              (arrayOf (Prim pt) pShape u)
          initV <- liftSubExpRegular segments inps env pShape initSE
          pure ([p], Regular $ paramName p, [Var initV])
    Acc {} ->
      error "liftLoopParam: Acc"
    Mem {} ->
      error "liftLoopParam: Mem"

liftParam :: (MonadFreshNames m) => SubExp -> FParam SOACS -> m ([FParam GPU], ResRep)
liftParam w fparam =
  case declTypeOf fparam of
    Prim pt -> do
      p <-
        newParam
          (desc <> "_lifted")
          (arrayOf (Prim pt) (Shape [w]) Nonunique)
      pure ([p], Regular $ paramName p)
    Array pt _ u -> do
      num_data <-
        newParam (desc <> "_num_data") $ Prim int64
      segments <-
        newParam (desc <> "_segments") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      flags <-
        newParam (desc <> "_F") $
          arrayOf (Prim Bool) (Shape [Var (paramName num_data)]) Nonunique
      offsets <-
        newParam (desc <> "_O") $
          arrayOf (Prim int64) (Shape [w]) Nonunique
      elems <-
        newParam (desc <> "_data") $
          arrayOf (Prim pt) (Shape [Var (paramName num_data)]) u
      pure
        ( [num_data, segments, flags, offsets, elems],
          Irregular $
            IrregularRep
              { irregularS = paramName segments,
                irregularF = paramName flags,
                irregularO = paramName offsets,
                irregularD = paramName elems,
                irregularK = Dense
              }
        )
    Acc {} ->
      error "liftParam: Acc"
    Mem {} ->
      error "liftParam: Mem"
  where
    desc = baseName (paramName fparam)

liftArg :: Segments -> SubExp -> DistInputs -> DistEnv -> (SubExp, Diet) -> Builder GPU [(SubExp, Diet)]
liftArg segments w inps env (se, d) = do
  (_, rep) <- liftSubExp segments inps env se
  case rep of
    Regular v -> do
      v_t <- lookupType v
      v' <-
        if arrayShape v_t == Shape [w]
          then pure v
          else
            letExp "lifted_arg_flat" . BasicOp $
              Reshape v $
                reshapeAll (arrayShape v_t) (Shape [w])
      pure [(Var v', d)]
    Irregular irreg -> mkIrrep irreg
  where
    mkIrrep
      ( IrregularRep
          { irregularS = segs,
            irregularF = flags,
            irregularO = offsets,
            irregularD = elems
          }
        ) = do
        t <- lookupType elems
        t_o <- lookupType offsets
        flags_t <- lookupType flags
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        segs' <- letExp "segs" $ BasicOp $ Reshape segs $ reshapeAll (arrayShape t_o) (Shape [w])
        offsets' <- letExp "offsets" $ BasicOp $ Reshape offsets $ reshapeAll (arrayShape t_o) (Shape [w])

        -- Only apply the original diet to the 'elems' array
        let diets = replicate 4 Observe ++ [d]
        pure $ zipWith (curry (first Var)) [num_data, segs', flags', offsets', elems'] diets

reshapeLiftedApplyResult :: Segments -> RetType SOACS -> ResRep -> Builder GPU ResRep
reshapeLiftedApplyResult segments Prim {} (Regular v) = do
  v_t <- lookupType v
  let expectedShape = segmentsShape segments
  v' <-
    if arrayShape v_t == expectedShape
      then pure v
      else
        letExp "lifted_apply_res" . BasicOp $
          Reshape v $
            reshapeAll (arrayShape v_t) expectedShape
  pure $ Regular v'
reshapeLiftedApplyResult _ _ rep =
  pure rep

liftLoopInit :: Segments -> DistInputs -> DistEnv -> SubExp -> SubExp -> Builder GPU [SubExp]
liftLoopInit segments inps env se num_segments = do
  (_, rep) <- liftSubExp segments inps env se
  case rep of
    Regular v -> pure [Var v]
    Irregular irreg -> mkIrrep irreg
  where
    mkIrrep
      ( IrregularRep
          { irregularS = segs,
            irregularF = flags,
            irregularO = offsets,
            irregularD = elems
          }
        ) = do
        t <- lookupType elems
        t_o <- lookupType offsets
        flags_t <- lookupType flags
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        -- I'm not sure why I need this reshapes
        segs' <- letExp "segs" $ BasicOp $ Reshape segs $ reshapeAll (arrayShape t_o) (Shape [num_segments])
        offsets' <- letExp "offsets" $ BasicOp $ Reshape offsets $ reshapeAll (arrayShape t_o) (Shape [num_segments])
        pure $ map Var [num_data, segs', flags', offsets', elems']

-- Lifts a functions return type such that it matches the lifted functions return type.
liftRetType :: SubExp -> [RetType SOACS] -> [RetType GPU]
liftRetType w = concat . snd . L.mapAccumL liftType 0
  where
    liftType i rettype =
      let lifted = case rettype of
            Prim pt -> pure $ arrayOf (Prim pt) (Shape [Free w]) Nonunique
            Array pt _ u ->
              let num_data = Prim int64
                  segs = arrayOf (Prim int64) (Shape [Free w]) Nonunique
                  flags = arrayOf (Prim Bool) (Shape [Ext i]) Nonunique
                  offsets = arrayOf (Prim int64) (Shape [Free w]) Nonunique
                  elems = arrayOf (Prim pt) (Shape [Ext i]) u
               in [num_data, segs, flags, offsets, elems]
            Acc {} -> error "liftRetType: Acc"
            Mem {} -> error "liftRetType: Mem"
       in (i + length lifted, lifted)

loopResultToResReps :: [DistResult] -> [VName] -> [ResRep]
loopResultToResReps dist_res results =
  snd $
    L.mapAccumL
      ( \rs dist_res' ->
          if isRegularDistResult dist_res'
            then
              let (v : rs') = rs
               in (rs', Regular v)
            else
              let (_ : segs : flags : offsets : elems : rs') = rs
               in (rs', Irregular $ IrregularRep segs flags offsets elems Dense)
      )
      results
      dist_res

liftLoopResult :: Segments -> SubExp -> DistInputs -> DistEnv -> DistResult -> SubExpRes -> Builder GPU Result
liftLoopResult segments num_segments inps env dist_res res =
  if isRegularDistResult dist_res
    then do
      let (DistType _ _ t) = distResType dist_res
      let expectedShape = segmentsShape segments <> arrayShape t
      v <- liftSubExpRegular segments inps env expectedShape (resSubExp res)
      pure [SubExpRes mempty (Var v)]
    else case resSubExp res of
      Var v -> do
        irreg <- getIrregRep segments env inps v
        map (SubExpRes mempty . Var) <$> mkIrrep irreg
      _ -> undefined
  where
    mkIrrep
      ( IrregularRep
          { irregularS = segs,
            irregularF = flags,
            irregularO = offsets,
            irregularD = elems
          }
        ) = do
        flags_t <- lookupType flags
        t <- lookupType elems
        t_o <- lookupType offsets
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        segs' <- letExp "segs" $ BasicOp $ Reshape segs $ reshapeAll (arrayShape t_o) (Shape [num_segments])
        offsets' <- letExp "offsets" $ BasicOp $ Reshape offsets $ reshapeAll (arrayShape t_o) (Shape [num_segments])
        pure [num_data, segs', flags', offsets', elems']

liftLoopBody :: Segments -> SubExp -> DistInputs -> DistEnv -> [DistStm] -> [DistResult] -> Result -> Builder GPU Result
liftLoopBody segments num_segments inputs env dstms dist_res result = do
  env' <- foldM (transformDistStm segments) env dstms
  results <- zipWithM (liftLoopResult segments num_segments inputs env') dist_res result
  pure $ concat results

distResultsToResReps :: [DistResult] -> [VName] -> [ResRep]
distResultsToResReps dist_res results =
  snd $
    L.mapAccumL
      ( \rs dist_res' ->
          if isRegularDistResult dist_res'
            then
              let (v : rs') = rs
               in (rs', Regular v)
            else
              let (segs : flags : offsets : elems : rs') = rs
               in (rs', Irregular $ IrregularRep segs flags offsets elems Dense)
      )
      results
      dist_res

liftDistResult :: Segments -> DistInputs -> DistEnv -> DistResult -> SubExpRes -> Builder GPU Result
liftDistResult segments inps env dist_res res =
  if isRegularDistResult dist_res
    then do
      let (DistType _ _ t) = distResType dist_res
      let expectedShape = segmentsShape segments <> arrayShape t
      v <- liftSubExpRegular segments inps env expectedShape (resSubExp res)
      pure [SubExpRes mempty (Var v)]
    else case resSubExp res of
      Var v -> do
        irreg <- getIrregRep segments env inps v
        pure $ map (SubExpRes mempty . Var) [irregularS irreg, irregularF irreg, irregularO irreg, irregularD irreg]
      _ -> undefined

liftBodyWithDistResults :: Segments -> DistInputs -> DistEnv -> [DistStm] -> [DistResult] -> Result -> Builder GPU Result
liftBodyWithDistResults segments inputs env dstms dist_res result = do
  env' <- foldM (transformDistStm segments) env dstms
  result' <- zipWithM (liftDistResult segments inputs env') dist_res result
  pure $ concat result'

liftBody :: SubExp -> DistInputs -> DistEnv -> [DistStm] -> Result -> Builder GPU Result
liftBody w inputs env dstms result = do
  let segments = NE.singleton w
  env' <- foldM (transformDistStm segments) env dstms
  result' <- mapM (liftResult segments inputs env') result
  pure $ concat result'

liftFunName :: Name -> Name
liftFunName name = name <> "_lifted"

addRetAls :: [DeclType] -> [RetType GPU] -> [(RetType GPU, RetAls)]
addRetAls params rettype = zip rettype $ map possibleAliases rettype
  where
    aliasable (Array _ _ Nonunique) = True
    aliasable _ = False
    aliasable_params =
      map snd $ filter (aliasable . fst) $ zip params [0 ..]
    aliasable_rets =
      map snd $ filter (aliasable . declExtTypeOf . fst) $ zip rettype [0 ..]
    possibleAliases t
      | aliasable t = RetAls aliasable_params aliasable_rets
      | otherwise = mempty

liftFunDef :: Scope SOACS -> FunDef SOACS -> PassM (FunDef GPU)
liftFunDef const_scope fd = do
  let FunDef
        { funDefBody = body,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  wp <- newParam "w" $ Prim int64
  let w = Var $ paramName wp
  (fparams', reps) <- mapAndUnzipM (liftParam w) fparams
  let fparams'' = wp : concat fparams'
  let inputs = do
        (p, i) <- zip fparams [0 ..]
        pure (paramName p, DistInput (ResTag i) (paramType p))
  let rettype' =
        addRetAls (map paramDeclType fparams'') $
          liftRetType w (map fst rettype)
  let (inputs', dstms) =
        distributeBody const_scope (NE.singleton (Var (paramName wp))) inputs body
      env = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
  -- Lift the body of the function and get the results
  (result, stms) <-
    runReaderT
      (runBuilder $ liftBody w inputs' env dstms $ bodyResult body)
      (const_scope <> scopeOfFParams fparams'')
  let name = liftFunName $ funDefName fd
  pure $
    fd
      { funDefName = name,
        funDefBody = Body () stms result,
        funDefParams = fparams'',
        funDefRetType = rettype'
      }

transformLambda :: Scope SOACS -> Lambda SOACS -> PassM (Lambda GPU)
transformLambda scope (Lambda params ret body) = do
  body' <- transformBody (scopeOfLParams params <> scope) body
  pure $ Lambda params ret body'

transformStm :: Scope SOACS -> Stm SOACS -> PassM (Stms GPU)
transformStm scope (Let pat _ (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form = do
      let arrs' =
            zipWith MapArray arrs $
              map paramType (lambdaParams (scremaLambda form))
          (distributed, _) = distributeMap scope pat (NE.singleton w) arrs' lam
          m = transformDistributed mempty (NE.singleton w) distributed
      traceM $ prettyString distributed
      runReaderT (runBuilder_ m) scope
transformStm scope (Let pat aux (Loop params form body)) =
  oneStm . Let pat aux . Loop params form <$> transformBody scope' body
  where
    scope' = scopeOfLoopForm form <> scopeOfFParams (map fst params) <> scope
transformStm scope (Let pat aux (Match ses cases def_body ret)) =
  oneStm . Let pat aux
    <$> (Match ses <$> mapM onCase cases <*> transformBody scope def_body <*> pure ret)
  where
    onCase = traverse (transformBody scope)
transformStm scope (Let pat aux (WithAcc inputs withacc_lam)) =
  oneStm . Let pat aux
    <$> (WithAcc (map onInput inputs) <$> transformLambda scope withacc_lam)
  where
    onInput (shape, arrs, Nothing) =
      (shape, arrs, Nothing)
    onInput (shape, arrs, Just (lam, nes)) =
      (shape, arrs, Just (soacsLambdaToGPU lam, nes))
transformStm _ stm = pure $ oneStm $ soacsStmToGPU stm

transformStms :: Scope SOACS -> Stms SOACS -> PassM (Stms GPU)
transformStms scope stms =
  fold <$> traverse (transformStm (scope <> scopeOf stms)) stms

transformBody :: Scope SOACS -> Body SOACS -> PassM (Body GPU)
transformBody scope (Body () stms res) = do
  stms' <- transformStms scope stms
  pure $ Body () stms' res

transformFunDef :: Scope SOACS -> FunDef SOACS -> PassM (FunDef GPU)
transformFunDef consts_scope fd = do
  let FunDef
        { funDefBody = body,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  body' <- transformBody (scopeOfFParams fparams <> consts_scope) body
  pure $
    fd
      { funDefBody = body',
        funDefRetType = rettype,
        funDefParams = fparams
      }

transformProg :: Prog SOACS -> PassM (Prog GPU)
transformProg prog = do
  progAfterPreProcessing <- preprocessProg prog
  traceM $ "After preprocessProg:" <> prettyString progAfterPreProcessing
  consts' <- transformStms mempty $ progConsts progAfterPreProcessing
  funs' <- mapM (transformFunDef $ scopeOf (progConsts progAfterPreProcessing)) $ progFuns progAfterPreProcessing
  lifted_funs <-
    mapM (liftFunDef $ scopeOf (progConsts progAfterPreProcessing)) $
      filter (isNothing . funDefEntryPoint) $
        progFuns progAfterPreProcessing
  -- In extremely unlikely cases (mostly empty programs), we may end up having a
  -- name source that overlaps the names used in the builtin functions. Avoid
  -- that by bumping it by enough that we probably will not have a conflict.
  modifyNameSource $ \src -> ((), mappend (newNameSource 1000) src)
  pure $
    prog
      { progConsts = consts',
        progFuns = flatteningBuiltins <> lifted_funs <> funs'
      }

-- transform a for-loop with a variant iteration count into a while-loop
transformFortoWhile ::
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  StmAux () ->
  [(FParam SOACS, SubExp)] ->
  VName ->
  IntType ->
  SubExp ->
  Body SOACS ->
  Builder GPU DistEnv
transformFortoWhile segments env inps res aux merge i it n body = do
  let old_loop_params = map fst merge
  -- Fresh names used only in the synthetic rewritten body.
  cond_param_v <- newVName "for_cond"
  cond0_v <- newVName "for_cond0"
  cond_next_v <- newVName "for_cond_next"
  i_next_v <- newVName "for_i_next"
  loop_old_out_vs <- replicateM (length merge) $ newVName "for_out"
  i_out_v <- newVName "for_i_out"
  cond_out_v <- newVName "for_cond_out"

  let zero = intConst it 0
      one = intConst it 1
      aux_no_certs = aux {stmAuxCerts = mempty}

      cond0_stm =
        Let
          (Pat [PatElem cond0_v (Prim Bool)])
          aux_no_certs
          (BasicOp $ CmpOp (CmpSlt it) zero n)

      -- Extend the loop parameters with iteration variable and condition variable
      i_param = Param mempty i (Prim (IntType it))
      cond_param = Param mempty cond_param_v (Prim Bool)

      Body loop_body_dec loop_body_stms loop_body_res = body

      i_next_stm =
        Let
          (Pat [PatElem i_next_v (Prim (IntType it))])
          aux_no_certs
          -- OverflowWrap or OverflowUndef?
          (BasicOp $ BinOp (Add it OverflowUndef) (Var i) one)

      cond_next_stm =
        Let
          (Pat [PatElem cond_next_v (Prim Bool)])
          aux_no_certs
          (BasicOp $ CmpOp (CmpSlt it) (Var i_next_v) n)

      loop_new_body =
        Body
          loop_body_dec
          (loop_body_stms <> oneStm i_next_stm <> oneStm cond_next_stm)
          ( [ SubExpRes mempty (Var cond_next_v),
              SubExpRes mempty (Var i_next_v)
            ]
              <> loop_body_res
          )

      merge' =
        [ (cond_param, Var cond0_v),
          (i_param, zero)
        ]
          <> merge

      loop_out_tys = [Prim Bool, Prim (IntType it)] ++ map paramType old_loop_params

      loop_pat =
        Pat $
          zipWith
            PatElem
            ([cond_out_v, i_out_v] ++ loop_old_out_vs)
            loop_out_tys

      while_stm =
        Let
          loop_pat
          aux
          (Loop merge' (WhileLoop (paramName cond_param)) loop_new_body)

      synthetic_body =
        Body
          ()
          (oneStm cond0_stm <> oneStm while_stm)
          (map (SubExpRes mempty . Var) loop_old_out_vs)

  let (inps_local, env_local, _) = localiseInputs env inps

  scope <- askScope
  let (inps_dist, dstms) = distributeBody scope segments inps_local synthetic_body

  lifted_res <- liftBodyWithDistResults segments inps_dist env_local dstms res (bodyResult synthetic_body)
  lifted_vs <- mapM (letExp "for_variant_res" <=< toExp . resSubExp) lifted_res
  let reps = distResultsToResReps res lifted_vs
  pure $ insertReps (zip (map distResTag res) reps) env

splitInput ::
  Segments ->
  DistInputs ->
  DistEnv ->
  VName ->
  VName ->
  Builder GPU (Type, VName, ResRep)
splitInput segments inps env is v = do
  (t, rep) <- liftSubExpPreserveRep segments inps env (Var v)
  (t,v,) <$> case rep of
    Regular arr -> do
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      -- isnt' it better to do the segmap over all dims?
      arr' <- letExp "split_arr" <=< segMap (MkSolo n) $ \(MkSolo i) -> do
        idx <- letSubExp "idx" =<< eIndex is [eSubExp i]
        let arr_is = unflattenIndex (segmentDims segments) (pe64 idx)
        subExpsRes . pure <$> (letSubExp "arr" =<< eIndex arr (map toExp arr_is))
      pure $ Regular arr'
    Irregular (IrregularRep segs flags offsets elems _) -> do
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      segs' <- letExp "split_segs" <=< segMap (MkSolo n) $ \(MkSolo i) -> do
        idx <- letExp "idx" =<< eIndex is [eSubExp i]
        subExpsRes . pure <$> (letSubExp "segs" =<< eIndex segs [toExp idx])
      (_, offsets', num_data) <- exScanAndSum segs'
      (_, _, ii1) <- doRepIota segs'
      (_, _, ii2) <- doSegIota segs'
      ~[flags', elems'] <- letTupExp "split_F_data" <=< segMap (MkSolo num_data) $ \(MkSolo i) -> do
        offset <- letExp "offset" =<< eIndex offsets [eIndex is [eIndex ii1 [eSubExp i]]]
        idx <- letExp "idx" =<< eBinOp (Add Int64 OverflowUndef) (toExp offset) (eIndex ii2 [eSubExp i])
        flags_split <- letSubExp "flags" =<< eIndex flags [toExp idx]
        elems_split <- letSubExp "elems" =<< eIndex elems [toExp idx]
        pure $ subExpsRes [flags_split, elems_split]
      pure $
        Irregular $
          IrregularRep
            { irregularS = segs',
              irregularF = flags',
              irregularO = offsets',
              irregularD = elems',
              irregularK = Dense
            }

-- | Transform a SOACS program to a GPU program, using flattening.
flattenSOACs :: Pass SOACS GPU
flattenSOACs =
  Pass
    { passName = "flatten",
      passDescription = "Perform full flattening",
      passFunction = transformProg
    }
{-# NOINLINE flattenSOACs #-}
