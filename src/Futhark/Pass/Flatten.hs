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
import Futhark.Pass.Flatten.WithAcc
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.IntegralExp
import Prelude hiding (div, rem)

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

            -- Value to write
            v' <-
              letSubExp "v" =<< eIndex (irregularD reparr1) [eSubExp gid]
            o' <- letSubExp "o" =<< eIndex ii2 [eSubExp gid]

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
        irregularD = elems
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
        irregularD = elems
      }

-- | Flatten the arrays of an IrregularRep to be entirely one-dimensional.
flattenIrregularRep :: IrregularRep -> Builder GPU IrregularRep
flattenIrregularRep ir@(IrregularRep shape flags offsets elems) = do
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
      pure $ IrregularRep shape' flags' offsets' elems'

rearrangeFlat :: (IntegralExp num) => [Int] -> [num] -> num -> num
rearrangeFlat perm dims i =
  -- TODO?  Maybe we need to invert one of these permutations.
  flattenIndex dims $
    rearrangeShape perm $
      unflattenIndex (rearrangeShape perm dims) i

rearrangeIrreg ::
  Segments ->
  DistEnv ->
  TypeBase Shape u ->
  [Int] ->
  IrregularRep ->
  Builder GPU IrregularRep
rearrangeIrreg _segments _env v_t perm ir = do
  (IrregularRep shape flags offsets elems) <- flattenIrregularRep ir
  m <- arraySize 0 <$> lookupType elems
  (_, _, ii1_vss) <- doRepIota shape
  (_, _, ii2_vss) <- doSegIota shape
  elems' <- letExp "elems_rearrange" <=< renameExp <=< segMap (MkSolo m) $
    \(MkSolo i) -> do
      seg_i <- letSubExp "seg_i" =<< eIndex ii1_vss [eSubExp i]
      offset <- letSubExp "offset" =<< eIndex offsets [eSubExp seg_i]
      in_seg_i <- letSubExp "in_seg_i" =<< eIndex ii2_vss [eSubExp i]
      let v_dims = map pe64 $ arrayDims v_t
          in_seg_is_tr = rearrangeFlat perm v_dims $ pe64 in_seg_i
      v' <-
        letSubExp "v"
          =<< eIndex elems [toExp $ pe64 offset + in_seg_is_tr]
      pure [subExpRes v']
  pure $
    IrregularRep
      { irregularS = shape,
        irregularF = flags,
        irregularO = offsets,
        irregularD = elems'
      }

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
    ArrayLit {} -> scalarCase
    Opaque _op se
      | Var v <- se,
        Just (DistInput rt_in _) <- lookup v inps ->
          -- TODO: actually insert opaques
          pure $ insertRep (distResTag res) (resVar rt_in env) env
      | otherwise ->
          scalarCase
    Reshape arr _
      | Just (DistInput rt_in _) <- lookup arr inps ->
          pure $ insertRep (distResTag res) (resVar rt_in env) env
    Index arr slice
      | null $ sliceDims slice ->
          scalarCase
      | otherwise -> do
          -- Maximally irregular case.
          ns <- letExp "slice_sizes" <=< segMap segments $ \is -> do
            slice_ns <- mapM (readInput segments env (toList is) inps) $ sliceDims slice
            fmap varsRes . letTupExp "n" <=< toExp $ product $ map pe64 slice_ns
          (_n, offsets, m) <- exScanAndSum ns
          (_, _, repiota_D) <- doRepIota ns
          flags <- genFlags m offsets
          elems <- letExp "elems" <=< renameExp <=< segMap (NE.singleton m) $ \is -> do
            segment <- letSubExp "segment" =<< eIndex repiota_D (toList $ fmap eSubExp is)
            segment_start <- letSubExp "segment_start" =<< eIndex offsets [eSubExp segment]
            readInputs segments env [segment] inps
            -- TODO: multidimensional segments
            let slice' =
                  fixSlice (fmap pe64 slice) $
                    unflattenIndex (map pe64 (sliceDims slice)) $
                      subtract (pe64 segment_start) . pe64 $
                        NE.head is
            auxing aux $
              fmap (subExpsRes . pure) . letSubExp "v"
                =<< eIndex arr (map toExp slice')
          pure $ insertIrregular ns flags offsets (distResTag res) elems env
    Iota n (Constant x) (Constant s) Int64
      | zeroIsh x,
        oneIsh s -> do
          ns <- dataArr segments env inps n
          (flags, offsets, elems) <- certifying (distCerts inps aux env) $ doSegIota ns
          pure $ insertIrregular ns flags offsets (distResTag res) elems env
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
      pure $ insertIrregular ns res_F res_O (distResTag res) res_D' env
    Concat 0 arr shp -> do
      ns <- dataArr segments env inps shp
      reparr <- mapM (getIrregRep segments env inps) (NE.toList arr)
      rep' <- concatIrreg segments env ns reparr
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
      pure $ insertIrregular ns res_F res_O (distResTag res) res_D' env
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
      pure $ insertIrregular mul_dims res_F res_O (distResTag res) res_D' env
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
          ns <- letExp "slice_sizes"
            <=< renameExp
            <=< segMap (shapeDims (segmentsShape segments))
            $ \is -> do
              readInputs segments env is $
                filter ((`elem` sliceDims slice) . Var . fst) inps
              n <- letSubExp "n" <=< toExp $ product $ map pe64 $ sliceDims slice
              pure [subExpRes n]
          -- Irregular representation of `as`
          IrregularRep shape flags offsets elems <- getIrregRep segments env inps as
          -- Inner indices (1 and 2) of `ns`
          (_, _, ii1_vss) <- doRepIota ns
          (_, _, ii2_vss) <- certifying (distCerts inps aux env) $ doSegIota ns
          -- Number of updates to perform
          m <- arraySize 0 <$> lookupType ii2_vss
          elems' <- letExp "elems_scatter" <=< renameExp <=< genScatter elems m $ \gid -> do
            seg_i <- letSubExp "seg_i" =<< eIndex ii1_vss [eSubExp gid]
            in_seg_i <- letSubExp "in_seg_i" =<< eIndex ii2_vss [eSubExp gid]
            readInputs segments env [seg_i] $ filter ((/= as) . fst) inps
            case se of
              Var v -> do
                v_t <- lookupType v
                let in_seg_is =
                      unflattenIndex (map pe64 (arrayDims v_t)) (pe64 in_seg_i)
                    slice' = fmap pe64 slice
                    flat_i =
                      flattenIndex
                        (map pe64 $ arrayDims as_t)
                        (fixSlice slice' in_seg_is)
                -- Value to write
                v' <- letSubExp "v" =<< eIndex v (map toExp in_seg_is)
                o' <- letSubExp "o" =<< eIndex offsets [eSubExp seg_i]
                -- Index to write `v'` at
                i <- letExp "i" =<< toExp (pe64 o' + flat_i)
                pure (i, v')
              Constant c -> do
                let slice' = fmap pe64 slice
                    flat_i = flattenIndex (map pe64 $ arrayDims as_t) (fixSlice slice' [])
                o' <- letSubExp "o" =<< eIndex offsets [eSubExp seg_i]
                i <- letExp "i" =<< toExp (pe64 o' + flat_i)
                pure (i, Constant c)
          pure $ insertIrregular shape flags offsets (distResTag res) elems' env
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
                  rearrangeIrreg segments env v_t perm rep
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
          pure $ insertIrregular ns flags offsets (distResTag res) res_D env
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
          pure $ insertIrregular ns flags offsets (distResTag res) res_D env
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
onMapFreeVar _segments env inps _ws (_ws_F, _ws_O, ws_data) v = do
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
            subExpsRes . pure <$> (letSubExp "v" =<< eIndex v' [eSubExp segment])
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
                    irregularD = irregularD rep
                  }
          pure $ MapOther rep' t
        Regular vs ->
          fmap (`MapArray` t)
            . letExp (baseName v <> "_rep_free_reg_inp")
            <=< segMap (MkSolo ws_prod)
            $ \(MkSolo i) -> do
              segment <- letSubExp "segment" =<< eIndex segments_per_elem [eSubExp i]
              subExpsRes . pure <$> (letSubExp "v" =<< eIndex vs [eSubExp segment])

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
              elems_t <- lookupType $ irregularD rep
              if stripArray (segmentsRank segments) elems_t == paramType p
                then do
                  data_size <- arraySize 0 <$> lookupType (irregularD rep)
                  if data_size == ws_prod
                    then
                      -- Data already has the right layout and we can map it directly.
                      pure $ MapArray (irregularD rep) (stripArray 1 elems_t)
                    else do
                      -- We need to materialize the data.
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
                      pure $ MapArray new_flat (stripArray 1 elems_t)
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
                          letSubExp "row_size" <=< toExp $
                            pe64 total_s `div` pe64 num_rows_s
                        pure $ subExpsRes [row_size]
                  new_S <-
                    letExp (baseName (paramName p) <> "_new_S")
                      <=< segMap (MkSolo ws_prod)
                      $ \(MkSolo i) -> do
                        seg_i <- letSubExp "seg_i" =<< eIndex ws_data [eSubExp i]
                        sz <- letSubExp "sz" =<< eIndex per_row_size [eSubExp seg_i]
                        pure $ subExpsRes [sz]
                  (new_F, new_O, _new_elems) <- doSegIota new_S
                  let rep' =
                        IrregularRep
                          { irregularD = irregularD rep,
                            irregularF = new_F,
                            irregularS = new_S,
                            irregularO = new_O
                          }
                  pure $ MapOther rep' elems_t
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
  Builder GPU (VName, VName, VName)
transformInnerMap segments env inps pat w arrs map_lam = do
  ws <- dataArr segments env inps w
  (ws_F, ws_O, ws_data) <- doRepIota ws
  new_segment <- arraySize 0 <$> lookupType ws_data
  arrs' <-
    zipWithM
      (onMapInputArr segments env inps ws ws_O ws_data)
      (lambdaParams map_lam)
      arrs
  let free = freeIn map_lam
  free_sizes <-
    localScope (scopeOfDistInputs inps) $
      foldMap freeIn <$> mapM lookupType (namesToList free)
  let free_and_sizes = namesToList $ free <> free_sizes
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe
        (onMapFreeVar segments env inps ws (ws_F, ws_O, ws_data))
        free_and_sizes
  free_ps <-
    zipWithM
      newParam
      (map ((<> "_free") . baseName) free_and_sizes)
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
        transformDistributed arrmap (NE.singleton new_segment) distributed
  traceM $ unlines ["inner map distributed", prettyString distributed]
  addStms =<< runReaderT (runBuilder_ m) scope
  pure (ws_F, ws_O, ws)

-- Reduction or scan operators may not have any free variables that are variant
-- to the nest (that is, are inputs to the distributed operation). This is
-- because we would be unable to express them as SegScan/SegReds. Fixing this
-- would require modifications to the SegOp representation, but it is likely not
-- worth it, as such operators are extremely rare - and we can just fall back on
-- sequentialisation.
suitableOperator :: DistEnv -> DistInputs -> Lambda SOACS -> Bool
suitableOperator env inps =
  allNames notVariant . freeIn
  where
    notVariant v = isNothing $ M.lookup v $ inputReps inps env

doSegScan :: [Scan SOACS] -> VName -> [VName] -> Builder GPU [VName]
doSegScan scans flags elems =
  let scan = singleScan scans
   in genSegScan "scan" (soacsLambdaToGPU $ scanLambda scan) (scanNeutral scan) flags elems

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
        all (suitableOperator env inps . redLambda) reds,
        Just arrs' <- mapM (`lookup` inps) arrs,
        (Just (arr_segments, flags, offsets), elems) <- segsAndElems env arrs' -> do
          elems' <- genSegRed arr_segments flags offsets elems $ singleReduce reds
          pure $ insertReps (zip (map distResTag res) (map Regular elems')) env
      | Just (reds, map_lam) <- isRedomapSOAC form,
        all (suitableOperator env inps . redLambda) reds -> do
          map_pat <- fmap Pat $ forM (lambdaReturnType map_lam) $ \t ->
            PatElem <$> newVName "map" <*> pure (t `arrayOfRow` w)
          (ws_F, ws_O, ws) <-
            transformInnerMap segments env inps map_pat w arrs map_lam
          let (redout_names, mapout_names) =
                splitAt (redResults reds) (patNames map_pat)
          elems' <-
            genSegRed ws ws_F ws_O redout_names $
              singleReduce reds
          let (red_tags, map_tags) = splitAt (redResults reds) $ map distResTag res
          pure $
            insertRegulars red_tags elems' $
              insertIrregulars ws ws_F ws_O (zip map_tags mapout_names) env
      | Just scans <- isScanSOAC form,
        all (suitableOperator env inps . scanLambda) scans,
        Just arrs' <- mapM (`lookup` inps) arrs,
        (Just (arr_segments, flags, offsets), elems) <- segsAndElems env arrs' -> do
          elems' <- doSegScan scans flags elems
          pure $ insertIrregulars arr_segments flags offsets (zip (map distResTag res) elems') env
      | Just (scans, map_lam) <- isScanomapSOAC form,
        all (suitableOperator env inps . scanLambda) scans -> do
          map_pat <- fmap Pat $ forM (lambdaReturnType map_lam) $ \t ->
            PatElem <$> newVName "map" <*> pure (t `arrayOfRow` w)
          (ws_F, ws_O, ws) <-
            transformInnerMap segments env inps map_pat w arrs map_lam
          let (scanout_names, mapout_names) =
                splitAt (scanResults scans) (patNames map_pat)
          elems' <- doSegScan scans ws_F scanout_names
          let (scan_tags, map_tags) = splitAt (scanResults scans) $ map distResTag res
          pure $
            insertIrregulars ws ws_F ws_O (zip scan_tags elems') $
              insertIrregulars ws ws_F ws_O (zip map_tags mapout_names) env
      | Just map_lam <- isMapSOAC form -> do
          (ws_F, ws_O, ws) <- transformInnerMap segments env inps pat w arrs map_lam
          pure $ insertIrregulars ws ws_F ws_O (zip (map distResTag res) $ patNames pat) env
      | otherwise -> do
          -- XXX: here we silently sequentialise any SOAC that is not handled
          -- above. We need to make sure that we actually handle everything we
          -- care about!
          error $ "unhandled SOAC:\n" ++ prettyString stm
    -- transformScalarStm segments env inps res $
    --   Let { stmPat = pat, stmAux = aux, stmExp = (Op (Screma w arrs form)) }
    Let _ _ (Match scrutinees cases defaultCase _) ->
      transformMatch flattenOps segments env inps res scrutinees cases defaultCase
    Let _ _ (Apply name args rettype s) -> do
      let [w] = NE.toList segments
          name' = liftFunName name
      args' <- ((w, Observe) :) . concat <$> mapM (liftArg segments inps env) args
      args_ts <- mapM (subExpType . fst) args'
      let dietToUnique Consume = Unique
          dietToUnique Observe = Nonunique
          dietToUnique ObservePrim = Nonunique
          param_ts = zipWith toDecl args_ts $ map (dietToUnique . snd) args'
          rettype' = addRetAls param_ts $ liftRetType w $ map fst rettype

      result <- letTupExp (name' <> "_res") $ Apply name' args' rettype' s
      let reps = resultToResReps (map fst rettype) result
      pure $ insertReps (zip (map distResTag res) reps) env
    Let _ aux (Loop merge (ForLoop i it n) body) -> do
      if isVariant inps env n
        then transformFortoWhile segments env inps res aux merge i it n body
        else do
          let [w] = NE.toList segments
              old_loop_params = map fst merge
              old_loop_inits = map snd merge
              loopParamNames = S.fromList $ map paramName old_loop_params

          (lifted_loop_params, lifted_loop_reps, lifted_init) <-
            unzip3 <$> mapM (liftLoopParam segments inps env loopParamNames w) (zip old_loop_params old_loop_inits)

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
          let (loop_new_inputs', loop_dstms) =
                distributeBody scope w loop_new_inputs body

          let lifted_loop_rep_types = zip lifted_loop_reps (map declTypeOf old_loop_params)

          (loop_body_res, loop_body_stms) <-
            runReaderT
              ( runBuilder $
                  liftLoopBody w loop_new_inputs' loop_env_local loop_dstms lifted_loop_rep_types (bodyResult body)
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

          let out_reps = loopResultToResReps lifted_loop_reps loop_out_vs
          pure $ insertReps (zip (map distResTag res) out_reps) env
    Let _ aux (Loop merge (WhileLoop cond) body) -> do
      -- TODO:
      -- 4) Use reduction rather than scan for any_active
      -- 5) Consider updating the active segment so we don't go over w everytime

      -- inside the body we should compute the indices for which the condition is true and for which it is false, and then distribute the body based on that.
      --  We can then merge the results of the two branches by writing them back to a blank space like we do for the branches of a match.

      let [w] = NE.toList segments
          old_loop_params = map fst merge
          old_loop_inits = map snd merge

      (lifted_loop_params, lifted_loop_reps) <- mapAndUnzipM (liftParam w) old_loop_params
      lifted_init <- mapM (liftLoopInit segments inps env) old_loop_inits
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
        Nothing -> do
          let build_scope = scopeOfFParams lifted_loop_params'
          let (loop_new_inputs', loop_dstms) = distributeBody scope w loop_new_inputs body
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
          or_lam <- binOpLambda LogOr Bool
          cond_scanned <- genScan "any_scan" [w] or_lam [constant False] [cond_init_arr_v]
          let [cond_scanned_v] = cond_scanned
          any_active_init <-
            letSubExp "any_active_init"
              =<< eIndex cond_scanned_v [toExp $ pe64 w - 1]

          any_active_param <- newParam "any_active" (Prim Bool)
          let build_scope = scopeOfFParams lifted_loop_params' <> scopeOfFParams [any_active_param]

          -- ‌build body
          (loop_body_res, loop_body_stms) <-
            runReaderT
              ( runBuilder $ do
                  -- (num_data, active_inds) <- genFilter cond_lifted_param
                  equiv_classes <- letExp "equiv_classes" <=< segMap (MkSolo w) $ \(MkSolo i) -> do
                    c <- letSubExp "c" =<< eIndex cond_lifted_param [eSubExp i]
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
                        letExp (nm <> "_inds") $
                          BasicOp $
                            Index partition_inds $
                              fullSlice inds_t [DimSlice off sz (intConst Int64 1)]

                  inactive_inds <- getInds "inactive" 0
                  active_inds <- getInds "active" 1
                  num_data <- letSubExp "num_data" =<< eIndex partition_sizes [toExp $ intConst Int64 1]

                  inactive_reps <- forM old_loop_params $ \p -> do
                    (_, _, rep) <- splitInput segments loop_new_inputs loop_env_local inactive_inds (paramName p)
                    pure rep

                  let free_in_body = namesToList $ freeIn body
                  -- free_sizes <- localScope (scopeOfDistInputs loop_new_inputs) $ foldMap freeIn <$> mapM lookupType (namesToList $ freeIn body)
                  (ts, vs, reps) <- unzip3 <$> mapM (splitInput segments loop_new_inputs loop_env_local active_inds) free_in_body
                  let subset_inputs = do
                        (v, t, i) <- zip3 vs ts [0 ..]
                        pure (v, DistInput (ResTag i) t)
                      env_subset = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
                  let (subset_inputs', subset_dstms) = distributeBody scope num_data subset_inputs body
                  subset_body_res <- liftBody num_data subset_inputs' env_subset subset_dstms (bodyResult body)
                  subset_result_vs <- mapM (letExp "subset_result" <=< toExp . resSubExp) subset_body_res
                  let active_reps = resultToResReps (map declTypeOf old_loop_params) subset_result_vs

                  let mergeOneLifted t rep0 rep1 =
                        case t of
                          Prim pt -> do
                            let Regular xs0 = rep0
                                Regular xs1 = rep1
                            space <- letExp "blank" =<< eBlank (Array pt (Shape [w]) NoUniqueness)
                            out <-
                              foldM
                                scatterRegular
                                space
                                [(inactive_inds, xs0), (active_inds, xs1)]
                            pure [SubExpRes mempty (Var out)]
                          Array pt _ _ -> do
                            let Irregular ir0 = rep0
                                Irregular ir1 = rep1
                            segsSpace <-
                              letExp "blank_segs"
                                =<< eBlank (Array int64 (Shape [w]) NoUniqueness)

                            segs <-
                              foldM
                                scatterRegular
                                segsSpace
                                [(inactive_inds, irregularS ir0), (active_inds, irregularS ir1)]

                            (_, offsets, num_data) <- exScanAndSum segs

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
                          Acc {} -> error "WhileLoop: Acc merge param not supported"
                          Mem {} -> error "WhileLoop: Mem merge param not supported"

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
                        (toExp $ pe64 num_data .==. 0)
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
              result_types = map ((\(DistType _ _ t) -> t) . distResType) res
              out_reps = resultToResReps result_types loop_out_vs'
          pure $ insertReps (zip (map distResTag res) out_reps) env
    Let pat aux (WithAcc inputs lam) ->
      transformWithAcc flattenOps segments env inps res pat aux inputs lam

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
          Irregular irreg -> do
            -- It might have an irregular representation, but we know
            -- that it is actually regular because it is a result.
            let shape = segmentsShape segments <> arrayShape v_t
            v_copy <-
              letExp (baseName v) . BasicOp $
                Replicate mempty (Var $ irregularD irreg)
            v_copy_shape <- arrayShape <$> lookupType v_copy
            letBindNames [v] $ BasicOp $ Reshape v_copy $ reshapeAll v_copy_shape shape
  forM_ reps $ \(v, r) ->
    case r of
      Left se ->
        letBindNames [v] $ BasicOp $ Replicate (segmentsShape segments) se
      Right (DistInputFree arr _) ->
        letBindNames [v] $ BasicOp $ SubExp $ Var arr
      -- This can happen. ask Troels
      Right (DistInput rt t) -> do
        let shape = segmentsShape segments <> arrayShape t
        case resVar rt env of
          Regular v' -> letBindNames [v] $ BasicOp $ SubExp $ Var v'
          Irregular irreg -> do
            v_copy <-
              letExp (baseName v <> "_copy_right_dist") . BasicOp $
                Replicate mempty (Var $ irregularD irreg)
            v_copy_shape <- arrayShape <$> lookupType v_copy
            letBindNames [v] $ BasicOp $ Reshape v_copy $ reshapeAll v_copy_shape shape
  where
    env_initial = DistEnv {distResMap = M.map Irregular irregs}

transformStm :: Scope SOACS -> Stm SOACS -> PassM (Stms GPU)
transformStm scope (Let pat _ (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form = do
      traceM $ "&&&&transformStm: " ++ prettyString (Let pat mempty (Op (Screma w arrs form)))
      traceM $ "W :" ++ prettyString w
      let arrs' =
            zipWith MapArray arrs $
              map paramType (lambdaParams (scremaLambda form))
          (distributed, _) = distributeMap scope pat w arrs' lam
          m = transformDistributed mempty (NE.singleton w) distributed
      traceM $ prettyString distributed
      runReaderT (runBuilder_ m) scope
transformStm _ stm = pure $ oneStm $ soacsStmToGPU stm

transformStms :: Scope SOACS -> Stms SOACS -> PassM (Stms GPU)
transformStms scope stms =
  fold <$> traverse (transformStm (scope <> scopeOf stms)) stms

-- Like 'liftSubExp' but always returns a Regular result with the
-- given expected shape. Reshapes the underlying data if necessary.
liftSubExpRegular :: Segments -> DistInputs -> DistEnv -> Shape -> SubExp -> Builder GPU VName
liftSubExpRegular segments inps env expectedShape se = do
  v <- case se of
    c@(Constant _) ->
      letExp "lifted_const" (BasicOp $ Replicate (segmentsShape segments) c)
    Var x -> case M.lookup x $ inputReps inps env of
      Just (_, Regular v') -> pure v'
      Just (_, Irregular irreg) -> pure $ irregularD irreg
      Nothing ->
        letExp "free_replicated" $ BasicOp $ Replicate (segmentsShape segments) (Var x)
  v_t <- lookupType v
  if arrayShape v_t == expectedShape
    then pure v
    else
      letExp "reg_lifted" . BasicOp $
        Reshape v (reshapeAll (arrayShape v_t) expectedShape)

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
  DistInputs ->
  DistEnv ->
  S.Set VName ->
  SubExp ->
  (FParam SOACS, SubExp) ->
  Builder GPU ([FParam GPU], ResRep, [SubExp])
liftLoopParam segments inps env loopParamNames w (fparam, initSE) = do
  let t = declTypeOf fparam
  case t of
    Prim _ -> do
      (params, rep) <- liftParam w fparam
      initV <- liftSubExpRegular segments inps env (Shape [w]) initSE
      pure (params, rep, [Var initV])
    Array pt _ u
      | needsIrregular inps env loopParamNames t -> do
          (params, rep) <- liftParam w fparam
          initVals <- liftLoopInit segments inps env initSE
          pure (params, rep, initVals)
      | otherwise -> do
          -- Regular case: all dims are invariant, just add w as outermost dim
          let pShape = Shape [w] <> arrayShape t
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
                irregularD = paramName elems
              }
        )
    Acc {} ->
      error "liftParam: Acc"
    Mem {} ->
      error "liftParam: Mem"
  where
    desc = baseName (paramName fparam)

liftArg :: Segments -> DistInputs -> DistEnv -> (SubExp, Diet) -> Builder GPU [(SubExp, Diet)]
liftArg segments inps env (se, d) = do
  (_, rep) <- liftSubExp segments inps env se
  case rep of
    Regular v -> pure [(Var v, d)]
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
        flags_t <- lookupType flags
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        -- Only apply the original diet to the 'elems' array
        let diets = replicate 4 Observe ++ [d]
        pure $ zipWith (curry (first Var)) [num_data, segs, flags', offsets, elems'] diets

-- Mostly same as liftArg
liftLoopInit :: Segments -> DistInputs -> DistEnv -> SubExp -> Builder GPU [SubExp]
liftLoopInit segments inps env se = do
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
        flags_t <- lookupType flags
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        pure $ map Var [num_data, segs, flags', offsets, elems']

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

loopResultToResReps :: [ResRep] -> [VName] -> [ResRep]
loopResultToResReps paramReps results =
  snd $
    L.mapAccumL
      ( \rs rep -> case rep of
          Regular _ ->
            let (v : rs') = rs
             in (rs', Regular v)
          Irregular _ ->
            let (_ : segs : flags : offsets : elems : rs') = rs
             in (rs', Irregular $ IrregularRep segs flags offsets elems)
      )
      results
      paramReps

liftLoopResult :: Segments -> DistInputs -> DistEnv -> (ResRep, DeclType) -> SubExpRes -> Builder GPU Result
liftLoopResult segments inps env (paramRep, origType) res =
  case paramRep of
    Regular _ -> do
      let [w] = NE.toList segments
          expectedShape = Shape [w] <> arrayShape origType
      v <- liftSubExpRegular segments inps env expectedShape (resSubExp res)
      pure [SubExpRes mempty (Var v)]
    Irregular _ ->
      liftResult segments inps env res

liftLoopBody :: SubExp -> DistInputs -> DistEnv -> [DistStm] -> [(ResRep, DeclType)] -> Result -> Builder GPU Result
liftLoopBody w inputs env dstms paramRepTypes result = do
  let segments = NE.singleton w
  env' <- foldM (transformDistStm segments) env dstms
  results <- zipWithM (liftLoopResult segments inputs env') paramRepTypes result
  pure $ concat results

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
        distributeBody const_scope (Var (paramName wp)) inputs body
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

transformFunDef :: Scope SOACS -> FunDef SOACS -> PassM (FunDef GPU)
transformFunDef consts_scope fd = do
  let FunDef
        { funDefBody = Body () stms res,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  stms' <- transformStms (consts_scope <> scopeOfFParams fparams) stms
  pure $
    fd
      { funDefBody = Body () stms' res,
        funDefRetType = rettype,
        funDefParams = fparams
      }

transformProg :: Prog SOACS -> PassM (Prog GPU)
transformProg prog = do
  consts' <- transformStms mempty $ progConsts prog
  funs' <- mapM (transformFunDef $ scopeOf (progConsts prog)) $ progFuns prog
  lifted_funs <-
    mapM (liftFunDef $ scopeOf (progConsts prog)) $
      filter (isNothing . funDefEntryPoint) $
        progFuns prog
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
  let [w] = NE.toList segments
      old_loop_params = map fst merge

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
  let (inps_dist, dstms) = distributeBody scope w inps_local synthetic_body

  lifted_res <- liftBody w inps_dist env_local dstms (bodyResult synthetic_body)
  lifted_vs <- mapM (letExp "for_variant_res" <=< toExp . resSubExp) lifted_res

  let result_types = map ((\(DistType _ _ t) -> t) . distResType) res
      out_reps = resultToResReps result_types lifted_vs

  pure $ insertReps (zip (map distResTag res) out_reps) env

splitInput ::
  Segments ->
  DistInputs ->
  DistEnv ->
  VName ->
  VName ->
  Builder GPU (Type, VName, ResRep)
splitInput segments inps env is v = do
  (t, rep) <- liftSubExp segments inps env (Var v)
  (t,v,) <$> case rep of
    Regular arr -> do
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      arr' <- letExp "split_arr" <=< segMap (MkSolo n) $ \(MkSolo i) -> do
        idx <- letExp "idx" =<< eIndex is [eSubExp i]
        subExpsRes . pure <$> (letSubExp "arr" =<< eIndex arr [toExp idx])
      pure $ Regular arr'
    Irregular (IrregularRep segs flags offsets elems) -> do
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
              irregularD = elems'
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
