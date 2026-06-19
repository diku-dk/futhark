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
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (isJust, isNothing)
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
import Futhark.Pass.Flatten.Intrablock qualified as Intrablock
import Futhark.Pass.Flatten.Match
import Futhark.Pass.Flatten.Monad
import Futhark.Pass.Flatten.PreProcess
import Futhark.Pass.Flatten.SOAC
import Futhark.Pass.Flatten.WithAcc
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Futhark.Util.IntegralExp
import Prelude hiding (div, quot, rem)

defaultSegLevel :: SegLevel
defaultSegLevel = SegThread SegVirt Nothing

inBlockSegLevel :: SegLevel
inBlockSegLevel = SegThreadInBlock SegNoVirt

flattenOpsFor :: FunHasParallelism -> SegLevel -> FlattenOps
flattenOpsFor funHasParallelism lvl =
  FlattenOps
    { flattenSegLevel = lvl,
      flattenFunHasParallelism = funHasParallelism,
      flattenDistStmAtLevel = transformDistStm funHasParallelism,
      flattenScalarStm = transformScalarStm lvl
    }

transformScalarStms ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Stms SOACS ->
  Builder GPU DistEnv
transformScalarStms lvl segments env inps distres stms = do
  let bound_in_batch = namesFromList $ concatMap (patNames . stmPat) $ stmsToList stms
      allCerts = foldMap (\stm -> distCerts inps (stmAux stm) env) (stmsToList stms)
      certs = Certs $ filter (`notNameIn` bound_in_batch) $ unCerts allCerts
  vs <- certifying certs $ letTupExp "scalar_dist" <=< renameExp <=< segMap lvl segments $ \is -> do
    readInputs segments env (toList is) inps
    addStms $ fmap soacsStmToGPU stms
    pure $ subExpsRes $ map (Var . distResName) distres
  pure $ insertReps (zip (map distResTag distres) $ map Regular vs) env

transformScalarStm ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Stm SOACS ->
  Builder GPU DistEnv
transformScalarStm lvl segments env inps res stm =
  transformScalarStms lvl segments env inps res (oneStm stm)

topLevelversionScanRed ::
  FunHasParallelism ->
  Name ->
  Scope SOACS ->
  Pat Type ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  StmAux dec ->
  Stms GPU ->
  PassM (Stms GPU)
topLevelversionScanRed funHasParallelism desc scope pat w arrs form aux outer_only_stms = do
  let outerOnlyBody0 = mkBody outer_only_stms $ varsRes $ patNames pat
  (maybeFullFlattenBody, _) <- runReaderT (runBuilder (factorScremaForParallelism funHasParallelism scope (stmAuxCerts aux) pat w arrs form)) scope
  case maybeFullFlattenBody of
    Nothing -> pure outer_only_stms
    Just fullFlattenBody0 -> do
      outerOnlyBody <- renameBody outerOnlyBody0
      fullFlattenBody <- transformBody funHasParallelism scope =<< renameBody fullFlattenBody0
      let result_ts = patTypes pat
          attrs = stmAuxAttrs aux
      runReaderT
        ( runBuilder_ $ do
            let fullAlternative = kernelAlternatives desc result_ts fullFlattenBody []
                outerAlternative = kernelAlternatives desc result_ts outerOnlyBody []
                fullWithOuterAlternative = do
                  (outer_suff, _) <-
                    sufficientParallelism
                      (desc <> "_suff_outer")
                      [w]
                      mempty
                      Nothing
                  kernelAlternatives desc result_ts fullFlattenBody [(outer_suff, outerOnlyBody)]
                alternatives
                  | isParallelFunInside funHasParallelism $ lambdaBody . scremaLambda $ form =
                      fullAlternative
                  | "sequential_inner" `inAttrs` attrs =
                      outerAlternative
                  | mayExploitOuter attrs =
                      fullWithOuterAlternative
                  | otherwise =
                      fullAlternative
            alt_vs <- alternatives
            forM_ (zip (patNames pat) alt_vs) $ \(v, v_alt) ->
              letBindNames [v] $ BasicOp $ SubExp (Var v_alt)
        )
        scope

factorScremaForParallelism ::
  FunHasParallelism ->
  Scope SOACS ->
  Certs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  Builder GPU (Maybe (Body SOACS))
factorScremaForParallelism funHasParallelism scope certs pat w arrs form
  | Just (reds, map_lam) <- isRedomapSOAC form,
    lambdaHasParallelism funHasParallelism map_lam = do
      (map_stm, red_stm) <-
        redomapToMapAndReduce
          pat
          (w, reds, map_lam, arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, red_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasParallelism funHasParallelism map_lam,
    lambdaHasParallelism funHasParallelism post_lam = do
      (map_stm, scan_stm, post_stm) <-
        maposcanomapToMapScanAndMap
          pat
          (w, post_lam, scans, map_lam, arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scan_stm, post_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasParallelism funHasParallelism map_lam = do
      (map_stm, scanomap_stm) <-
        maposcanomapToMaposcanAndMap
          pat
          (w, post_lam, scans, map_lam, arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scanomap_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasParallelism funHasParallelism post_lam = do
      (map_stm, scan_stm, post_stm) <-
        maposcanomapToMapScanAndMap
          pat
          (w, post_lam, scans, map_lam, arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scan_stm, post_stm])
  | otherwise =
      pure Nothing
  where
    mkFactoredBody stms = do
      stms' <- fmap (certify certs) <$> preprocessStms scope stms
      pure $ mkBody stms' $ varsRes $ patNames pat

-- Do 'map2 (++) A B' where 'A' and 'B' are irregular arrays and have the same
-- number of subarrays
concatIrreg ::
  SegLevel ->
  Segments ->
  DistEnv ->
  VName ->
  [IrregularRep] ->
  Builder GPU IrregularRep
concatIrreg lvl _segments _env ns reparr = do
  -- Concatenation does not change the number of segments - it simply
  -- makes each of them larger.

  num_segments <- arraySize 0 <$> lookupType ns

  -- Constructs the full list size / shape that should hold the final results.
  let zero = Constant $ IntValue $ intValue Int64 (0 :: Int)
  ns_full <- letExp (baseName ns <> "_full") <=< segMap lvl (MkSolo num_segments) $
    \(MkSolo i) -> do
      old_segments <-
        forM reparr $ \rep ->
          letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      new_segment <-
        letSubExp "new_segment"
          =<< toExp (foldl (+) (pe64 zero) $ map pe64 old_segments)
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
        let prefixes = L.init $ L.inits segment_sizes
        sumprefix <-
          mapM
            ( letSubExp "segment_prefix"
                <=< foldBinOp (Add Int64 OverflowUndef) (intConst Int64 0)
            )
            prefixes
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
  let resultType = Array pt (Shape [m]) NoUniqueness
  elems_blank <- letExp "blank_res" =<< eBlank resultType

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
  Builder GPU IrregularRep
concatIrregAlongDim lvl segments env ns reparr typearr inps d = do
  num_segments <- arraySize 0 <$> lookupType ns

  let zero = Constant $ IntValue $ intValue Int64 (0 :: Int)
  ns_full <- letExp (baseName ns <> "_full") <=< segMap lvl (MkSolo num_segments) $
    \(MkSolo i) -> do
      old_segments <-
        forM reparr $ \rep ->
          letSubExp "old_segment" =<< eIndex (irregularS rep) [eSubExp i]
      new_segment <-
        letSubExp "new_segment"
          =<< toExp (foldl (+) (pe64 zero) $ map pe64 old_segments)
      pure $ subExpsRes [new_segment]

  (ns_full_F, ns_full_O, _ns_II1) <- doRepIota lvl ns_full

  repIota <- mapM (doRepIota lvl . irregularS) reparr
  segIota <- mapM (doSegIota lvl . irregularS) reparr

  let (_, _, rep_II1) = unzip3 repIota
  let (_, _, rep_II2) = unzip3 segIota

  n_arr <- mapM (fmap (arraySize 0) . lookupType) rep_II1

  scatter_info <-
    letTupExp "irregular_scatter_offsets" <=< segMap lvl (MkSolo num_segments) $
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
  SegLevel ->
  Segments ->
  DistEnv ->
  VName ->
  Name ->
  IrregularRep ->
  Builder GPU IrregularRep
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

-- | Flatten the arrays of an IrregularRep to be entirely one-dimensional.
flattenIrregularRep :: SegLevel -> IrregularRep -> Builder GPU IrregularRep
flattenIrregularRep lvl ir@(IrregularRep shape flags offsets elems kind) = do
  elems_t <- lookupType elems
  if arrayRank elems_t == 1
    then pure ir
    else do
      n <- arraySize 0 <$> lookupType shape
      m' <- letSubExp "flat_m" <=< toExp $ product $ map pe64 $ arrayDims elems_t
      elems' <-
        letExp (baseName elems <> "_flat") . BasicOp $
          Reshape elems (reshapeAll (arrayShape elems_t) (Shape [m']))

      shape' <- letExp (baseName shape <> "_flat") <=< renameExp <=< segMap lvl (MkSolo n) $
        \(MkSolo i) -> do
          old_shape <- letSubExp "old_shape" =<< eIndex shape [toExp i]
          segment_shape <-
            letSubExp "segment_shape" <=< toExp $
              pe64 old_shape * product (map pe64 $ tail $ arrayDims elems_t)
          pure [subExpRes segment_shape]

      offsets' <- letExp (baseName offsets <> "_flat") <=< renameExp <=< segMap lvl (MkSolo n) $
        \(MkSolo i) -> do
          old_offsets <- letSubExp "old_offsets" =<< eIndex offsets [toExp i]
          segment_offsets <-
            letSubExp "segment_offsets" <=< toExp $
              pe64 old_offsets * product (map pe64 $ tail $ arrayDims elems_t)
          pure [subExpRes segment_offsets]

      flags' <- letExp (baseName flags <> "_flat") <=< renameExp <=< segMap lvl (MkSolo m') $
        \(MkSolo i) -> do
          let head_i = head $ unflattenIndex (map pe64 $ arrayDims elems_t) (pe64 i)
          flag <- letSubExp "flag" =<< eIndex flags [toExp head_i]
          pure [subExpRes flag]
      pure $ IrregularRep shape' flags' offsets' elems' kind

rearrangeFlat :: (IntegralExp num) => [Int] -> [num] -> num -> num
rearrangeFlat perm dims i =
  flattenIndex dims $
    rearrangeShape (rearrangeInverse perm) $
      unflattenIndex (rearrangeShape perm dims) i

segmentCoordsFromFlat :: Segments -> SubExp -> Builder GPU [SubExp]
segmentCoordsFromFlat segments seg_i =
  mapM (letSubExp "seg_coord" <=< toExp) $
    unflattenIndex (map pe64 $ shapeDims $ segmentsShape segments) (pe64 seg_i)

segmentCount :: Segments -> TPrimExp Int64 VName
segmentCount = product . map pe64 . shapeDims . segmentsShape

-- TODO: We do not need to actully make this Dense
rearrangeIrreg ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  TypeBase Shape u ->
  [Int] ->
  IrregularRep ->
  Builder GPU IrregularRep
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

-- | Intra-group parallelism is worthwhile if the lambda contains more
-- than one instance of non-map nested parallelism, or any nested
-- parallelism inside a loop.
worthIntrablock :: Lambda SOACS -> Bool
worthIntrablock lam = bodyInterest (lambdaBody lam) > 1
  where
    bodyInterest body =
      sum $ interest <$> bodyStms body
    interest stm
      | "sequential" `inAttrs` attrs =
          0 :: Int
      | Op (Screma w _ form) <- stmExp stm,
        Just lam' <- isMapSOAC form =
          mapLike w lam'
      | Loop _ _ body <- stmExp stm =
          bodyInterest body * 10
      | Match _ cases defbody _ <- stmExp stm =
          foldl
            max
            (bodyInterest defbody)
            (map (bodyInterest . caseBody) cases)
      | Op (Screma w _ (ScremaForm lam' _ _ _)) <- stmExp stm =
          zeroIfTooSmall w + bodyInterest (lambdaBody lam')
      | Op (Stream _ _ _ lam') <- stmExp stm =
          bodyInterest $ lambdaBody lam'
      | WithAcc _ lam' <- stmExp stm =
          bodyInterest $ lambdaBody lam'
      | otherwise =
          0
      where
        attrs = stmAuxAttrs $ stmAux stm
        sequential_inner = "sequential_inner" `inAttrs` attrs

        zeroIfTooSmall (Constant (IntValue x))
          | intToInt64 x < 32 = 0
        zeroIfTooSmall _ = 1

        mapLike w lam' =
          if sequential_inner
            then 0
            else max (zeroIfTooSmall w) (bodyInterest (lambdaBody lam'))

-- TODO: maybe update this or just always consider Sequentialising

-- | A lambda is worth sequentialising if it contains enough nested
-- parallelism of an interesting kind.
worthSequentialising :: FunHasParallelism -> Lambda SOACS -> Bool
worthSequentialising funHasParallelism lam = bodyInterest (0 :: Int) (lambdaBody lam) > 1
  where
    bodyInterest depth body =
      sum $ interest depth <$> bodyStms body
    interest depth stm
      | "sequential" `inAttrs` attrs =
          0 :: Int
      | Op (Screma _ _ form@(ScremaForm lam' _ _ _)) <- stmExp stm,
        isJust $ isMapSOAC form =
          if sequential_inner
            then 0
            else bodyInterest (depth + 1) (lambdaBody lam')
      | Loop _ _ body <- stmExp stm =
          bodyInterest (depth + 1) body * 10
      | Match _ cases defbody _ <- stmExp stm =
          foldl
            max
            (bodyInterest (depth + 1) defbody)
            (map (bodyInterest (depth + 1) . caseBody) cases)
      | WithAcc _ withacc_lam <- stmExp stm =
          bodyInterest (depth + 1) (lambdaBody withacc_lam)
      | Op (Screma _ _ form@(ScremaForm lam' _ _ _)) <- stmExp stm =
          1
            + bodyInterest (depth + 1) (lambdaBody lam')
            +
            -- Give this a bigger score if it's a redomap just inside
            -- the the outer lambda, as these are often tileable and
            -- thus benefit more from sequentialisation.
            case (isRedomapSOAC form, depth) of
              (Just _, 0) -> 1
              _ -> 0
      | Op (Stream _ _ _ lam') <- stmExp stm =
          bodyInterest (depth + 1) (lambdaBody lam')
      | otherwise =
          if isParallelStm funHasParallelism stm then 1 else 0
      where
        attrs = stmAuxAttrs $ stmAux stm
        sequential_inner = "sequential_inner" `inAttrs` attrs

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

runInnerSeqMap ::
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Pat Type ->
  [DistResult] ->
  Builder GPU [VName]
runInnerSeqMap w arrs map_lam _pat _ress = do
  map_lam' <- renameLambda $ soacsLambdaToGPU map_lam
  let new_segments = pure w
  letTupExp "outer_map" <=< renameExp <=< segMap defaultSegLevel new_segments $ \is -> do
    forM_ (zip (lambdaParams map_lam') arrs) $ \(p, arr) -> do
      let [gtid] = is
      letBindNames [paramName p]
        =<< case paramType p of
          Acc {} ->
            eSubExp $ Var arr
          _ ->
            eIndex arr [eSubExp gtid]
    addStms $ bodyStms $ lambdaBody map_lam'
    pure $ bodyResult $ lambdaBody map_lam'

-- Check if the in the body there is a call to a parallel function.
-- XXX: we use this function to even reject the intra version of
-- maps that call parallel function. We should do better there.
-- One other things to note is that maybe we should create a sequential
-- version of function and replace them in these cases.
isParallelFunInside :: FunHasParallelism -> Body SOACS -> Bool
isParallelFunInside funHasParallelism = inBody
  where
    inLambda = any (callParallelFunction . stmExp) . bodyStms . lambdaBody
    inBody = any (callParallelFunction . stmExp) . bodyStms
    callParallelFunction (Apply fname _ _ _) = funHasParallelism fname
    callParallelFunction (BasicOp _) = False
    callParallelFunction (Match _ cases def_case _) =
      inBody def_case
        || any (inBody . caseBody) cases
    callParallelFunction (Loop _ _ body) = inBody body
    callParallelFunction (WithAcc _ lam) = inLambda lam
    callParallelFunction (Op (Stream _ _ _ lam)) = inLambda lam
    callParallelFunction (Op (Screma _ _ (ScremaForm lam _ _ _))) = inLambda lam
    callParallelFunction (Op (Hist _ _ ops lam)) =
      inLambda lam || any (inLambda . histLambda) ops
      where
        histLambda (Futhark.IR.SOACS.HistOp _ _ _ _ op) = op
    callParallelFunction (Op JVP {}) = error "isParallelFunInside: unexpected JVP"
    callParallelFunction (Op VJP {}) = error "isParallelFunInside: unexpected VJP"
    callParallelFunction (Op WithVJP {}) = error "isParallelFunInside: unexpected WithVJP"

onlyExploitIntra :: Attrs -> Bool
onlyExploitIntra attrs =
  AttrComp "incremental_flattening" ["only_intra"] `inAttrs` attrs

mayExploitOuter :: Attrs -> Bool
mayExploitOuter attrs =
  not $
    AttrComp "incremental_flattening" ["no_outer"]
      `inAttrs` attrs
      || AttrComp "incremental_flattening" ["only_inner"]
        `inAttrs` attrs

mayExploitIntra :: Attrs -> Bool
mayExploitIntra attrs =
  not $
    AttrComp "incremental_flattening" ["no_intra"]
      `inAttrs` attrs
      || AttrComp "incremental_flattening" ["only_inner"]
        `inAttrs` attrs

intraBlockAlternative ::
  Intrablock.IntrablockResult ->
  Builder GPU (SubExp, Body GPU)
intraBlockAlternative intra = do
  addStms $ Intrablock.intraPreludeStms intra
  max_tblock_size <-
    letSubExp "max_tblock_size" $ Op $ SizeOp $ GetSizeMax SizeThreadBlock
  fits <-
    letSubExp "fits" $
      BasicOp $
        CmpOp
          (CmpSle Int64)
          (Intrablock.intraThreadBlockSize intra)
          max_tblock_size
  (intra_suff, _) <-
    sufficientParallelism
      "suff_intra_map"
      [Intrablock.intraAvailPar intra]
      mempty
      (Just Intrablock.intraMinInnerPar)
  intra_ok <-
    letSubExp "intra_suff_and_fits" $
      BasicOp $
        BinOp LogAnd fits intra_suff
  intra_body <-
    renameBody $
      mkBody
        (Intrablock.intraKernelStms intra)
        (varsRes $ Intrablock.intraResultNames intra)
  pure (intra_ok, intra_body)

transformMapForInBlock ::
  FunHasParallelism ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Builder GPU (Stms GPU)
transformMapForInBlock funHasParallelism pat w arrs map_lam = do
  scope <- castScope <$> askScope
  lam <- preprocessLambda scope map_lam
  collectStms_ $ do
    let arrs' =
          zipWith MapArray arrs $
            map paramType (lambdaParams lam)
        (distributed, _) =
          distributeMap funHasParallelism scope pat (NE.singleton w) arrs' lam
    transformDistributed funHasParallelism inBlockSegLevel mempty (NE.singleton w) distributed

transformDistBasicOp ::
  SegLevel ->
  Segments ->
  DistEnv ->
  ( DistInputs,
    DistResult,
    PatElem Type,
    StmAux (),
    BasicOp
  ) ->
  Builder GPU DistEnv
transformDistBasicOp lvl segments env (inps, res, pe, aux, e) =
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
          ns <- dataArr lvl segments env inps $ intConst Int64 0
          (flags, offsets, _elems) <- doRepIota lvl ns
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
          data_t <- lookupType (head vs_reparr)
          let pt = elemType data_t
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
    Opaque _op se
      | Var v <- se,
        Just (DistInput rt_in _) <- lookup v inps ->
          -- TODO: actually insert opaques
          pure $ insertRep (distResTag res) (resVar rt_in env) env
      | otherwise ->
          scalarCase
    Reshape arr reshape
      | isRegularDistResult res,
        not (any (isVariant inps env) reshape) -> do
          let outer = segmentsShape segments
              inner_target = newShape reshape
              reshape' = reshapeCoerce outer <> newshapeInner outer reshape

          arr_t <- lookupInputType inps arr
          let arr_shape = arrayShape arr_t
          let unform_arr = not (any (isVariant inps env) arr_shape)
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
        not (any (isVariant inps env) slice) -> do
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
        not (any (isVariant inps env) flat_slice) -> do
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
            not $ any (isVariant inps env) (arrayDims t)
      if isRegularDistResult res
        && not (isVariant inps env shp)
        && all inputShapeUniform arr_ts
        then do
          --  Unifrom Concat
          traceM "unform concat"
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
          traceM "!!!NON unform concat"
          ns <- dataArr lvl segments env inps shp
          reparr <- mapM (getIrregRep lvl segments env inps) (NE.toList arr)
          traceM "lets enter"
          rep' <- case d of
            0 -> concatIrreg lvl segments env ns reparr
            d' -> do
              concatIrregAlongDim lvl segments env ns reparr arr_ts inps d'
          traceM "Job Done"
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
    Update safety as slice se
      -- Uniform Update
      | Just as_t <- distInputType <$> lookup as inps,
        isRegularDistResult res,
        not (any (isVariant inps env) slice) -> do
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
        not (any (isVariant inps env) flat_slice) -> do
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
          irreg_dense <- ensureDenseIrregular lvl (baseName v <> "_rearrange_dense") irreg
          t <- lookupInputType inps v
          rep' <-
            certifying (distCerts inps aux env) $
              rearrangeIrreg lvl segments env inps t perm irreg_dense
          pure $ insertRep (distResTag res) (Irregular rep') env
    Scratch pt dims
      | not $ any (isVariant inps env) dims -> do
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
    scalarCase =
      transformScalarStm lvl segments env inps [res] $
        Let (Pat [pe]) aux (BasicOp e)

replicateForDims :: Segments -> Shape -> VName -> Builder GPU VName
replicateForDims segments dims v = do
  v_t <- lookupType v
  let seg_rank = length (NE.toList segments)
      v_rank = arrayRank v_t
      dims_rank = shapeRank dims
      perm = [dims_rank .. dims_rank + seg_rank - 1] ++ [0 .. dims_rank - 1] ++ [seg_rank + dims_rank .. dims_rank + v_rank - 1]
  v_rep <-
    letExp (baseName v <> "_reg_rep") . BasicOp $
      Replicate dims (Var v)
  letExp (baseName v <> "_reg_rep_tr") . BasicOp $
    Rearrange v_rep perm

lambdaHasParallelism :: FunHasParallelism -> Lambda SOACS -> Bool
lambdaHasParallelism funHasParallelism =
  any (isParallelStm funHasParallelism) . bodyStms . lambdaBody

transformDistStm :: FunHasParallelism -> SegLevel -> Segments -> DistEnv -> DistStm -> Builder GPU DistEnv
transformDistStm _ lvl segments env (DistStm inps res (ScalarStm stms)) =
  transformScalarStms lvl segments env inps res (stmsFromList stms)
transformDistStm funHasParallelism lvl segments env (DistStm inps res (ParallelStm stm)) = do
  case stm of
    Let pat aux (BasicOp e) -> do
      let ~[res'] = res
          ~[pe] = patElems pat
      transformDistBasicOp lvl segments env (inps, res', pe, aux, e)
    Let pat aux (Op (Screma w arrs form)) ->
      transformScrema (flattenOpsFor funHasParallelism lvl) segments env inps res (pat, aux) (w, arrs, form)
    Let _ aux (Match scrutinees cases defaultCase rt) -> do
      if any (isVariant inps env) scrutinees
        then
          transformMatch (flattenOpsFor funHasParallelism lvl) segments env inps res scrutinees cases defaultCase
        -- else error $ unlines ["scrutinees: ", prettyString scrutinees, "cases:", prettyString cases, "defaultCase:", prettyString defaultCase]
        else do
          scope <- askScope
          new_cases <- forM cases $ \(Case c body) -> do
            let (case_body_inputs, case_dstms) = distributeBody funHasParallelism scope segments inps body

            (case_body_res, case_body_stms) <-
              runReaderT
                ( runBuilder $
                    liftBodyWithDistResults funHasParallelism lvl segments case_body_inputs env case_dstms res (bodyResult body)
                )
                scope
            pure $ Case c $ Body () case_body_stms case_body_res
          new_default_body <- do
            let (new_default_body_inputs, new_default_dstms) = distributeBody funHasParallelism scope segments inps defaultCase
            (new_default_body_res, new_default_body_stms) <-
              runReaderT
                ( runBuilder $
                    liftBodyWithDistResults funHasParallelism lvl segments new_default_body_inputs env new_default_dstms res (bodyResult defaultCase)
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
          let payload_res = drop (S.size (shapeContext rets)) match_res
          let reps = distResultsToResReps res payload_res
          pure $ insertReps (zip (map distResTag res) reps) env
    Let pat aux (Apply name args rettype s) ->
      case lvl of
        SegThread {} -> do
          let name' = liftFunName name
          w <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          args' <- ((w, Observe) :) . concat <$> mapM (liftArg lvl segments w inps env) args
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
        -- TODO: Do something about intra functions
        _ ->
          if all isRegularDistResult res
            then transformScalarStm lvl segments env inps res $ Let pat aux (Apply name args rettype s)
            else error "Unhandled Apply in non SegThread Seglevel"
    Let _ aux (Loop merge (ForLoop i it n) body) -> do
      if isVariant inps env n
        then transformFortoWhile funHasParallelism lvl segments env inps res aux merge i it n body
        else do
          let old_loop_params = map fst merge
              old_loop_inits = map snd merge
              loopParamNames = S.fromList $ map paramName old_loop_params

          num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          (lifted_loop_params, lifted_loop_reps, lifted_init) <-
            unzip3 <$> mapM (liftLoopParam lvl segments num_segments inps env loopParamNames) (zip old_loop_params old_loop_inits)

          let lifted_loop_params' = concat lifted_loop_params
              lifted_init' = concat lifted_init

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
          let (loop_new_inputs', loop_dstms) = distributeBody funHasParallelism scope segments loop_new_inputs body

          (loop_body_res, loop_body_stms) <-
            runReaderT
              ( runBuilder $
                  liftLoopBody funHasParallelism lvl segments num_segments loop_new_inputs' loop_env_local loop_dstms res (bodyResult body)
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
        unzip3 <$> mapM (liftLoopParam lvl segments w inps env loopParamNames) (zip old_loop_params old_loop_inits)

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
        -- infinite loop
        Nothing -> do
          let build_scope = scopeOfFParams lifted_loop_params'
          let (loop_new_inputs', loop_dstms) = distributeBody funHasParallelism scope segments loop_new_inputs body
          (loop_body_res, loop_body_stms) <-
            runReaderT
              (runBuilder $ liftLoopBody funHasParallelism lvl segments w loop_new_inputs' loop_env_local loop_dstms res (bodyResult body))
              (scope <> build_scope)
          let loop_body_gpu = Body () loop_body_stms loop_body_res
              loop_exp_gpu = Loop (zip lifted_loop_params' lifted_init') (WhileLoop cond) loop_body_gpu
          loop_out_vs <- certifying (distCerts inps aux env) $ letTupExp "loop_res_out" loop_exp_gpu
          let out_reps = loopResultToResReps res loop_out_vs
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
          cond_scanned <- genScan lvl "any_scan" (NE.singleton w) or_lam [constant False] [cond_init_flat]
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
                  equiv_classes <- letExp "equiv_classes" <=< segMap lvl (MkSolo w) $ \(MkSolo i) -> do
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
                  (partition_sizes, partition_offs, partition_inds) <- doPartition lvl n_cases equiv_classes
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
                    (_, _, rep) <- splitInput lvl segments loop_new_inputs loop_env_local inactive_inds (paramName p)
                    pure rep

                  let free_in_body =
                        filter
                          (isVariant loop_new_inputs loop_env_local . Var)
                          (namesToList $ freeIn body)
                  free_sizes <-
                    foldMap freeIn <$> mapM (lookupInputType loop_new_inputs) free_in_body
                  let free_variant_sizes = filter (isVariant loop_new_inputs loop_env_local . Var) (namesToList free_sizes)
                      free_size_vars = nubOrd (free_variant_sizes <> free_in_body)
                  (ts, vs, reps) <- unzip3 <$> mapM (splitInput lvl segments loop_new_inputs loop_env_local active_inds) free_size_vars
                  let subset_inputs = do
                        (v, t, i) <- zip3 vs ts [0 ..]
                        pure (v, DistInput (ResTag i) t)
                      env_subset = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
                  let subset_segments = NE.singleton active_size
                  let (subset_inputs', subset_dstms) = distributeBody funHasParallelism scope subset_segments subset_inputs body
                  env_subset' <- foldM (transformDistStm funHasParallelism lvl subset_segments) env_subset subset_dstms
                  active_reps <-
                    zipWithM
                      (liftDistResultRep lvl subset_segments subset_inputs' env_subset')
                      res
                      (bodyResult body)

                  let mergeOneLifted t rep0 rep1
                        | isAcc t = do
                            let (Regular acc_res) = rep1
                            pure [SubExpRes mempty (Var acc_res)]
                        | otherwise =
                            case (rep0, rep1) of
                              (Regular x0, Regular x1) -> do
                                let initial_shape = Shape [w] <> arrayShape t
                                let final_shape = segmentsShape segments <> arrayShape t
                                let pt = elemType t
                                space <- letExp "blank" =<< eBlank (Array pt initial_shape NoUniqueness)

                                out <-
                                  foldM
                                    (scatterRegular lvl)
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
                                    (scatterRegular lvl)
                                    segsSpace
                                    [(inactive_inds, irregularS ir0), (active_inds, irregularS ir1)]

                                (_, offsets, num_data) <- exScanAndSum lvl segs

                                let pt = elemType t
                                elemsSpace <-
                                  letExp "blank_elems"
                                    =<< eBlank (Array pt (Shape [num_data]) NoUniqueness)

                                elems <-
                                  foldM
                                    (scatterIrregular lvl offsets)
                                    elemsSpace
                                    [(inactive_inds, ir0), (active_inds, ir1)]

                                flags <- genFlags lvl num_data offsets

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
      transformWithAcc ops segments env inps res pat aux inputs lam
    (Let pat aux (Op (Hist w hist_inputs hist_ops bucket_fun))) ->
      transformHist ops segments env inps res (pat, aux) (w, hist_inputs, hist_ops, bucket_fun)
    Let _ _ (Op (Stream {})) -> error "transformDistStm: Stream should have been removed"
    Let _ _ (Op (JVP {})) -> error "Unhandled JVP"
    Let _ _ (Op (VJP {})) -> error "Unhandled VJP"
    Let _ _ (Op (WithVJP {})) -> error "Unhandled WithVJP"
  where
    ops = flattenOpsFor funHasParallelism lvl

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
      Irregular r -> irregularD r

reshapeAndBind :: VName -> VName -> Shape -> Builder GPU ()
reshapeAndBind v src shape = do
  v_copy <- letExp (baseName v) . BasicOp $ Replicate mempty (Var src)
  v_copy_shape <- arrayShape <$> lookupType v_copy
  letBindNames [v] $ BasicOp $ Reshape v_copy $ reshapeAll v_copy_shape shape

transformDistributed ::
  FunHasParallelism ->
  SegLevel ->
  M.Map ResTag IrregularRep ->
  Segments ->
  Distributed ->
  Builder GPU ()
transformDistributed funHasParallelism lvl irregs segments dist = do
  let Distributed dstms (DistResults resmap reps) = dist
  env <- foldM (transformDistStm funHasParallelism lvl segments) env_initial dstms
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
              irreg' <- ensureDenseIrregular lvl (baseName v <> "_dist_res") irreg
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
              irreg' <- ensureDenseIrregular lvl (baseName v <> "_dist_rep") irreg
              reshapeAndBind v (irregularD irreg') (segmentsShape segments <> arrayShape t)
  where
    env_initial = DistEnv {distResMap = M.map Irregular irregs}

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
  SegLevel ->
  Segments ->
  SubExp ->
  DistInputs ->
  DistEnv ->
  S.Set VName ->
  (FParam SOACS, SubExp) ->
  Builder GPU ([FParam GPU], ResRep, [SubExp])
liftLoopParam lvl segments num_segments inps env loopParamNames (fparam, initSE) = do
  let t = declTypeOf fparam
  case t of
    Prim pt -> do
      param <-
        newParam
          (baseName (paramName fparam) <> "_lifted")
          (arrayOf (Prim pt) (segmentsShape segments) Nonunique)
      initV <- liftSubExpRegular lvl segments inps env (segmentsShape segments) initSE
      pure ([param], Regular $ paramName param, [Var initV])
    Array pt _ u
      | needsIrregular inps env loopParamNames t -> do
          (params, rep) <- liftParam num_segments fparam
          initVals <- liftLoopInit lvl segments inps env initSE num_segments
          pure (params, rep, initVals)
      | otherwise -> do
          -- Regular case: all dims are invariant, just add w as outermost dim
          let pShape = segmentsShape segments <> arrayShape t
          p <-
            newParam
              (baseName (paramName fparam) <> "_lifted")
              (arrayOf (Prim pt) pShape u)
          initV <- liftSubExpRegular lvl segments inps env pShape initSE
          pure ([p], Regular $ paramName p, [Var initV])
    Acc {} -> do
      initV <- liftSubExpRegular lvl segments inps env mempty initSE
      let Param attrs v acc_t = fparam
      param <- Param attrs <$> newName v <*> pure acc_t
      pure ([param], Regular $ paramName param, [Var initV])
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

liftArg :: SegLevel -> Segments -> SubExp -> DistInputs -> DistEnv -> (SubExp, Diet) -> Builder GPU [(SubExp, Diet)]
liftArg lvl segments w inps env (se, d) = do
  (_, rep) <- liftSubExp lvl segments inps env se
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

liftLoopInit :: SegLevel -> Segments -> DistInputs -> DistEnv -> SubExp -> SubExp -> Builder GPU [SubExp]
liftLoopInit lvl segments inps env se num_segments = do
  (_, rep) <- liftSubExp lvl segments inps env se
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

liftLoopResult :: SegLevel -> Segments -> SubExp -> DistInputs -> DistEnv -> DistResult -> SubExpRes -> Builder GPU Result
liftLoopResult lvl segments num_segments inps env dist_res res =
  if isRegularDistResult dist_res
    then do
      let (DistType _ _ t) = distResType dist_res
      let expectedShape = segmentsShape segments <> arrayShape t
      v <- liftSubExpRegular lvl segments inps env expectedShape (resSubExp res)
      pure [SubExpRes mempty (Var v)]
    else case resSubExp res of
      Var v -> do
        irreg <- getIrregRep lvl segments env inps v
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

liftLoopBody :: FunHasParallelism -> SegLevel -> Segments -> SubExp -> DistInputs -> DistEnv -> [DistStm] -> [DistResult] -> Result -> Builder GPU Result
liftLoopBody funHasParallelism lvl segments num_segments inputs env dstms dist_res result = do
  env' <- foldM (transformDistStm funHasParallelism lvl segments) env dstms
  results <- zipWithM (liftLoopResult lvl segments num_segments inputs env') dist_res result
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

liftDistResult :: SegLevel -> Segments -> DistInputs -> DistEnv -> DistResult -> SubExpRes -> Builder GPU Result
liftDistResult lvl segments inps env dist_res res =
  if isRegularDistResult dist_res
    then do
      let (DistType _ _ t) = distResType dist_res
      let expectedShape = segmentsShape segments <> arrayShape t
      v <- liftSubExpRegular lvl segments inps env expectedShape (resSubExp res)
      pure [SubExpRes mempty (Var v)]
    else case resSubExp res of
      Var v -> do
        irreg <- getIrregRep lvl segments env inps v
        pure $ map (SubExpRes mempty . Var) [irregularS irreg, irregularF irreg, irregularO irreg, irregularD irreg]
      _ -> undefined

liftBodyWithDistResults :: FunHasParallelism -> SegLevel -> Segments -> DistInputs -> DistEnv -> [DistStm] -> [DistResult] -> Result -> Builder GPU Result
liftBodyWithDistResults funHasParallelism lvl segments inputs env dstms dist_res result = do
  env' <- foldM (transformDistStm funHasParallelism lvl segments) env dstms
  result' <- zipWithM (liftDistResult lvl segments inputs env') dist_res result
  pure $ concat result'

liftBody :: FunHasParallelism -> SegLevel -> SubExp -> DistInputs -> DistEnv -> [DistStm] -> Result -> Builder GPU Result
liftBody funHasParallelism lvl w inputs env dstms result = do
  let segments = NE.singleton w
  env' <- foldM (transformDistStm funHasParallelism lvl segments) env dstms
  result' <- mapM (liftResult lvl segments inputs env') result
  pure $ concat result'

liftFunName :: Name -> Name
liftFunName name = name <> "_lifted"

analyseFunParallelism :: [FunDef SOACS] -> M.Map Name Bool
analyseFunParallelism funs =
  M.fromList [(funDefName fun, hasParallelFun mempty (funDefName fun)) | fun <- funs]
  where
    funsByName =
      M.fromList [(funDefName fun, fun) | fun <- funs]
    hasParallelFun seen fname
      | isBuiltInFunction fname =
          False
      -- avoid cycles even thought it is impossible now
      | fname `S.member` seen =
          False
      | Just fun <- M.lookup fname funsByName =
          any (isParallelStm (hasParallelFun (S.insert fname seen))) $
            bodyStms $
              funDefBody fun
      | otherwise =
          error $ "analyseFunParallelism: unknown function " ++ prettyString fname

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

liftFunDef :: FunHasParallelism -> Scope SOACS -> FunDef SOACS -> PassM (FunDef GPU)
liftFunDef funHasParallelism const_scope fd = do
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
        distributeBody funHasParallelism const_scope (NE.singleton (Var (paramName wp))) inputs body
      env = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
  -- Lift the body of the function and get the results
  (result, stms) <-
    runReaderT
      (runBuilder $ liftBody funHasParallelism defaultSegLevel w inputs' env dstms $ bodyResult body)
      (const_scope <> scopeOfFParams fparams'')
  let name = liftFunName $ funDefName fd
  pure $
    fd
      { funDefName = name,
        funDefBody = Body () stms result,
        funDefParams = fparams'',
        funDefRetType = rettype'
      }

transformLambda :: FunHasParallelism -> Scope SOACS -> Lambda SOACS -> PassM (Lambda GPU)
transformLambda funHasParallelism scope (Lambda params ret body) = do
  body' <- transformBody funHasParallelism (scopeOfLParams params <> scope) body
  pure $ Lambda params ret body'

transformStm :: FunHasParallelism -> Scope SOACS -> Stm SOACS -> PassM (Stms GPU)
transformStm funHasParallelism scope (Let pat aux (Op soac))
  | "sequential_outer" `inAttrs` stmAuxAttrs aux = do
      stms <- runBuilderT_ (FOT.transformSOAC pat soac) scope
      transformStms funHasParallelism scope $ fmap (certify (stmAuxCerts aux)) stms
transformStm _ _ stm
  | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) = pure $ oneStm $ soacsStmToGPU stm
transformStm _ scope (Let pat aux (Op (Hist w arrs ops bucket_fun))) = do
  runReaderT
    ( runBuilder_ $
        certifying (stmAuxCerts aux) $ do
          res <-
            genUniformSegHist
              defaultSegLevel
              "topLevelSegHist"
              [w]
              ops
              (soacsLambdaToGPU bucket_fun)
              arrs
              (const $ pure ())
          forM_ (zip (patNames pat) res) $ \(v, v') ->
            letBindNames [v] $ BasicOp $ SubExp $ Var v'
    )
    scope
transformStm funHasParallelism scope (Let pat aux (Op (Screma w arrs form)))
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    Scan scan_lam nes <- singleScan scans = do
      outer_only_stms <-
        runReaderT
          ( runBuilder_ $
              certifying (stmAuxCerts aux) $ do
                (scan_lam', nes', shape) <- determineReduceOp scan_lam nes
                res <-
                  genUniformSegScanomapWithPost
                    defaultSegLevel
                    [w]
                    "topLevelSegScan"
                    (soacsLambdaToGPU scan_lam')
                    shape
                    nes'
                    (soacsLambdaToGPU post_lam)
                    (soacsLambdaToGPU map_lam)
                    arrs
                    (const $ pure ())
                forM_ (zip (patNames pat) res) $ \(v, v') ->
                  letBindNames [v] $ BasicOp $ SubExp $ Var v'
          )
          scope
      topLevelversionScanRed funHasParallelism "top_level_scan_alt" scope pat w arrs form aux outer_only_stms
transformStm funHasParallelism scope (Let pat aux (Op (Screma w arrs form)))
  | Just (reds, map_lam) <- isRedomapSOAC form = do
      outer_only_stms <-
        runReaderT
          ( runBuilder_ $
              certifying (stmAuxCerts aux) $ do
                let sing_red = singleReduce reds
                (red_lam, nes', shape) <- determineReduceOp (redLambda sing_red) (redNeutral sing_red)
                let comm
                      | commutativeLambda red_lam = Commutative
                      | otherwise = redComm sing_red
                let sing_red_gpu = Reduce comm (soacsLambdaToGPU red_lam) nes'
                res <- genNonSegRed defaultSegLevel "topLevelSegRed" [w] sing_red_gpu shape (soacsLambdaToGPU map_lam) arrs
                forM_ (zip (patNames pat) res) $ \(v, v') ->
                  letBindNames [v] $ BasicOp $ SubExp $ Var v'
          )
          scope
      topLevelversionScanRed funHasParallelism "top_level_red_alt" scope pat w arrs form aux outer_only_stms
transformStm funHasParallelism scope (Let pat aux (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form = do
      let certs = stmAuxCerts aux
      (outer_only_res, outer_only_stms) <- runReaderT (runBuilder $ certifying certs $ runInnerSeqMap w arrs lam pat []) scope
      lamFullFlatten <- renameLambda =<< preprocessLambda scope lam
      let arrs' =
            zipWith MapArray arrs $
              map paramType (lambdaParams (scremaLambda form))
          (distributed, _) = distributeMap funHasParallelism scope pat (NE.singleton w) arrs' lamFullFlatten
          m = transformDistributed funHasParallelism defaultSegLevel mempty (NE.singleton w) distributed
      traceM $ prettyString distributed
      stms <- runReaderT (runBuilder_ $ certifying certs m) scope
      let fullFlattenBody0 = mkBody stms $ varsRes $ patNames pat
          outerOnlyBody0 = mkBody outer_only_stms $ varsRes outer_only_res
      fullFlattenBody <- renameBody fullFlattenBody0
      outerOnlyBody <- renameBody outerOnlyBody0
      runReaderT
        ( runBuilder_ $ do
            let only_intra = onlyExploitIntra (stmAuxAttrs aux)
                may_intra = worthIntrablock lam && mayExploitIntra (stmAuxAttrs aux)
                result_ts = patTypes pat
            intra' <-
              if only_intra || may_intra
                then
                  Intrablock.intrablockParalleliseTopLevelMap
                    (transformMapForInBlock funHasParallelism)
                    pat
                    aux
                    w
                    arrs
                    lam
                else
                  pure Nothing
            alt_vs <- case intra' of
              _
                -- We have non-inlined parallel function call we have to fully flatten the body
                | isParallelFunInside funHasParallelism (lambdaBody lam) ->
                    kernelAlternatives "top_level_map_alt" result_ts fullFlattenBody []
                | "sequential_inner" `inAttrs` stmAuxAttrs aux ->
                    kernelAlternatives "top_level_map_alt" result_ts outerOnlyBody []
              Nothing
                | not only_intra,
                  worthSequentialising funHasParallelism lam,
                  mayExploitOuter (stmAuxAttrs aux) -> do
                    (outer_suff, _) <- sufficientParallelism "suff_outer_map" [w] mempty Nothing
                    kernelAlternatives
                      "top_level_map_alt"
                      result_ts
                      fullFlattenBody
                      [(outer_suff, outerOnlyBody)]
                | otherwise ->
                    kernelAlternatives "top_level_map_alt" result_ts fullFlattenBody []
              Just intra_res
                | only_intra -> do
                    (_, intra_body) <- intraBlockAlternative intra_res
                    kernelAlternatives "top_level_map_alt" result_ts intra_body []
                | worthSequentialising funHasParallelism lam,
                  mayExploitOuter (stmAuxAttrs aux) -> do
                    intra_alt <- intraBlockAlternative intra_res
                    (outer_suff, _) <- sufficientParallelism "suff_outer_map" [w] mempty Nothing
                    kernelAlternatives
                      "top_level_map_alt"
                      result_ts
                      fullFlattenBody
                      [(outer_suff, outerOnlyBody), intra_alt]
                | otherwise -> do
                    intra_alt <- intraBlockAlternative intra_res
                    kernelAlternatives
                      "top_level_map_alt"
                      result_ts
                      fullFlattenBody
                      [intra_alt]
            forM_ (zip (patNames pat) alt_vs) $ \(v, v_alt) ->
              letBindNames [v] $ BasicOp $ SubExp (Var v_alt)
        )
        scope
transformStm funHasParallelism scope (Let pat aux (Loop params form body)) =
  oneStm . Let pat aux . Loop params form <$> transformBody funHasParallelism scope' body
  where
    scope' = scopeOfLoopForm form <> scopeOfFParams (map fst params) <> scope
transformStm funHasParallelism scope (Let pat aux (Match ses cases def_body ret)) =
  oneStm . Let pat aux
    <$> (Match ses <$> mapM onCase cases <*> transformBody funHasParallelism scope def_body <*> pure ret)
  where
    onCase = traverse (transformBody funHasParallelism scope)
transformStm funHasParallelism scope (Let pat aux (WithAcc inputs withacc_lam)) =
  oneStm . Let pat aux
    <$> (WithAcc (map onInput inputs) <$> transformLambda funHasParallelism scope withacc_lam)
  where
    onInput (shape, arrs, Nothing) =
      (shape, arrs, Nothing)
    onInput (shape, arrs, Just (lam, nes)) =
      (shape, arrs, Just (soacsLambdaToGPU lam, nes))
transformStm _ _ stm = pure $ oneStm $ soacsStmToGPU stm

transformStms :: FunHasParallelism -> Scope SOACS -> Stms SOACS -> PassM (Stms GPU)
transformStms funHasParallelism scope stms =
  fold <$> traverse (transformStm funHasParallelism (scope <> scopeOf stms)) stms

transformBody :: FunHasParallelism -> Scope SOACS -> Body SOACS -> PassM (Body GPU)
transformBody funHasParallelism scope (Body () stms res) = do
  stms' <- transformStms funHasParallelism scope stms
  pure $ Body () stms' res

transformFunDef :: FunHasParallelism -> Scope SOACS -> FunDef SOACS -> PassM (FunDef GPU)
transformFunDef funHasParallelism consts_scope fd = do
  let FunDef
        { funDefBody = body,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  body' <- transformBody funHasParallelism (scopeOfFParams fparams <> consts_scope) body
  pure $
    fd
      { funDefBody = body',
        funDefRetType = rettype,
        funDefParams = fparams
      }

transformProg :: Prog SOACS -> PassM (Prog GPU)
transformProg prog = do
  progAfterPreProcessing <- preprocessProg prog
  let consts = progConsts progAfterPreProcessing
      consts_scope = scopeOf consts
      funs = progFuns progAfterPreProcessing
      funParallelism = analyseFunParallelism funs
      funHasParallelism fname =
        M.findWithDefault (not $ isBuiltInFunction fname) fname funParallelism

  consts' <- transformStms funHasParallelism mempty consts
  funs' <- mapM (transformFunDef funHasParallelism consts_scope) funs
  lifted_funs <-
    mapM (liftFunDef funHasParallelism consts_scope) $
      filter (isNothing . funDefEntryPoint) funs
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
  FunHasParallelism ->
  SegLevel ->
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
transformFortoWhile funHasParallelism lvl segments env inps res aux merge i it n body = do
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
  let (inps_dist, dstms) = distributeBody funHasParallelism scope segments inps_local synthetic_body

  lifted_res <- liftBodyWithDistResults funHasParallelism lvl segments inps_dist env_local dstms res (bodyResult synthetic_body)
  lifted_vs <- mapM (letExp "for_variant_res" <=< toExp . resSubExp) lifted_res
  let reps = distResultsToResReps res lifted_vs
  pure $ insertReps (zip (map distResTag res) reps) env

splitInput ::
  SegLevel ->
  Segments ->
  DistInputs ->
  DistEnv ->
  VName ->
  VName ->
  Builder GPU (Type, VName, ResRep)
splitInput lvl segments inps env is v = do
  (t, rep) <- liftSubExpPreserveRep segments inps env (Var v)
  (t,v,) <$> case rep of
    Regular arr ->
      if isAcc t
        then
          pure $ Regular arr
        else do
          n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
          -- isnt' it better to do the segmap over all dims?
          arr' <- letExp "split_arr" <=< segMap lvl (MkSolo n) $ \(MkSolo i) -> do
            idx <- letSubExp "idx" =<< eIndex is [eSubExp i]
            let arr_is = unflattenIndex (segmentDims segments) (pe64 idx)
            subExpsRes . pure <$> (letSubExp "arr" =<< eIndex arr (map toExp arr_is))
          pure $ Regular arr'
    Irregular (IrregularRep segs flags offsets elems _) -> do
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      segs' <- letExp "split_segs" <=< segMap lvl (MkSolo n) $ \(MkSolo i) -> do
        idx <- letExp "idx" =<< eIndex is [eSubExp i]
        subExpsRes . pure <$> (letSubExp "segs" =<< eIndex segs [toExp idx])
      (_, offsets', num_data) <- exScanAndSum lvl segs'
      (_, _, ii1) <- doRepIota lvl segs'
      (_, _, ii2) <- doSegIota lvl segs'
      ~[flags', elems'] <- letTupExp "split_F_data" <=< segMap lvl (MkSolo num_data) $ \(MkSolo i) -> do
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
