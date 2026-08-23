{-# LANGUAGE LambdaCase #-}

-- | Flattening rules for SOACs.
module Futhark.Pass.Flatten.SOAC
  ( flattenScrema,
    flattenHist,
    flattenFlatMap,
    flattenFlatMapNested,
  )
where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.Functor.Identity (runIdentity)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.General
import Futhark.Pass.Flatten.Incremental
import Futhark.Pass.Flatten.Intrablock qualified as Intrablock
import Futhark.Pass.Flatten.PreProcess
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Transform.ToGPU (soacsLambdaToGPU)
import Futhark.Util (mapAccumLM)
import Futhark.Util.IntegralExp
import Prelude hiding (div, quot, rem)

-- | How the results of a nested map are represented, determined by whether the
-- map width is uniform (invariant to the enclosing nest) or nonuniform; see
-- 'transformInnerMap'.
data InnerMapMode
  = -- | Uniform width: the results are regular arrays that keep the full
    -- multi-dimensional shape (the enclosing segments followed by the map
    -- width).
    MultiDim
  | -- | Nonuniform width: the results are irregular, flattened into a single
    -- segment dimension.
    SingleDim

freeWithTypeDeps :: DistInputs -> Names -> FlattenM [VName]
freeWithTypeDeps inps free = do
  let free_names = namesToList free
  free_sizes <- foldMap freeIn <$> mapM (lookupInputType inps) free_names
  pure $ nubOrd $ namesToList free_sizes <> free_names

-- Reduction or scan operators may not have any free variables that are variant
-- to the nest (that is, are inputs to the distributed operation), and must
-- operate on primitive types. This is because we would be unable to express
-- them as SegScan/SegReds. Fixing this would require modifications to the
-- SegOp representation, but it is likely not worth it, as such operators are
-- extremely rare - and we can just fall back on sequentialising the SOAC and
-- flattening the resulting loop.
suitableOperator :: DistEnv -> DistInputs -> Lambda SOACS -> [SubExp] -> Bool
suitableOperator _env inps lam _nes =
  allNames notVariant (freeIn lam)
    && all primType (lambdaReturnType lam)
  where
    notVariant = not . isVariant inps . Var

suitableUniformOperator :: DistInputs -> Lambda SOACS -> Bool
suitableUniformOperator inps lam =
  allNames (not . isVariant inps . Var) (freeIn lam)

regularToReplicatedIrregularRep ::
  SegLevel ->
  Segments ->
  VName ->
  VName ->
  FlattenM IrregularRep
regularToReplicatedIrregularRep lvl segments ws_data v' = do
  ws_prod <- arraySize 0 <$> lookupType ws_data
  arr_t <- lookupType v'
  segment_size <-
    letSubExp "reg_seg_size" <=< toExp . product . map pe64 $
      drop (segmentsRank segments) (arrayDims arr_t)
  num_elems <-
    letSubExp "reg_num_elems" <=< toExp $ product $ map pe64 $ arrayDims arr_t
  arr_D <-
    letExp "reg_D" . BasicOp $
      Reshape v' (reshapeAll (arrayShape arr_t) (Shape [num_elems]))
  arr_F <- letExp "reg_F" <=< segMap lvl (MkSolo num_elems) $ \(MkSolo i) -> do
    flag <- letSubExp "flag" <=< toExp $ (pe64 i `rem` pe64 segment_size) .==. 0
    pure [subExpRes flag]

  arr_S <-
    letExp "reg_segments" . BasicOp $
      Replicate (Shape [ws_prod]) segment_size
  arr_O <- letExp "reg_O" <=< segMap lvl (MkSolo ws_prod) $ \(MkSolo i) -> do
    segment <- letSubExp "segment" =<< eIndex ws_data [eSubExp i]
    offset <- letSubExp "offset" <=< toExp $ pe64 segment * pe64 segment_size
    pure [subExpRes offset]
  let rep' =
        IrregularRep
          { irregularS = arr_S,
            irregularF = arr_F,
            irregularO = arr_O,
            irregularD = arr_D,
            irregularK = Replicated
          }
  pure rep'

-- Replicates inner dimension for inputs.
onMapFreeVar ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  (VName, VName, VName) ->
  VName ->
  Maybe (FlattenM (VName, MapArray IrregularRep))
onMapFreeVar lvl segments env inps _ws (_ws_F, _ws_O, ws_data) v = do
  v_inp <- lookup v inps
  pure $ do
    ws_prod <- arraySize 0 <$> lookupType ws_data
    fmap (v,) $ case v_inp of
      DistInputFree v' t ->
        --  I'm not totally sure if this will be better than previous approach
        (`MapOther` t) <$> regularToReplicatedIrregularRep lvl segments ws_data v'
      DistInput rt t -> case resVar rt env of
        Irregular rep -> do
          ~[new_S, offsets] <- letTupExp (baseName v <> "_rep_free_irreg")
            <=< segMap lvl (MkSolo ws_prod)
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
          (`MapOther` t) <$> regularToReplicatedIrregularRep lvl segments ws_data vs

onMapFreeVarMultiDim ::
  SegLevel ->
  Segments ->
  SubExp ->
  DistEnv ->
  DistInputs ->
  VName ->
  Maybe (FlattenM (VName, MapArray IrregularRep))
onMapFreeVarMultiDim lvl segments w env inps v = do
  v_inp <- lookup v inps
  pure $ fmap (v,) $ case v_inp of
    DistInputFree v' t -> do
      v_rep <- replicateForDims segments (Shape [w]) v'
      pure $ MapArray v_rep t
    DistInput rt t -> case resVar rt env of
      Regular v' -> do
        v_rep <- replicateForDims segments (Shape [w]) v'
        pure $ MapArray v_rep t
      Irregular rep -> do
        -- Can replicate as well
        old_nseg <- arraySize 0 <$> lookupType (irregularS rep)
        new_nseg <- letSubExp "new_nseg" <=< toExp $ pe64 old_nseg * pe64 w
        ~[new_S, offsets] <- letTupExp (baseName v <> "_rep_free_irreg")
          <=< segMap lvl (MkSolo new_nseg)
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

onMapInputArr ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  VName ->
  VName ->
  Param Type ->
  VName ->
  FlattenM (MapArray IrregularRep)
onMapInputArr lvl segments env inps ws ws_O ws_data p arr = do
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
          pure $ MapArray v (rowType t)
        DistInput rt t ->
          case resVar rt env of
            Irregular rep -> do
              onMapIrregularInputArr lvl SingleDim segments ws ws_O ws_data p arr rep ws_prod
            Regular vs -> do
              let inner_shape = arrayShape $ paramType p
              vs_t <- lookupType vs
              if isAcc vs_t
                then pure $ MapArray vs t
                else do
                  v <-
                    letExp (baseName arr <> "_reg_flat") . BasicOp . Reshape vs $
                      reshapeAll (arrayShape vs_t) (Shape [ws_prod] <> inner_shape)
                  pure $ MapArray v (stripArray 1 vs_t)
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

mapArraysToInputs ::
  [VName] ->
  [MapArray IrregularRep] ->
  FlattenM (DistEnv, DistInputs)
mapArraysToInputs param_names arrs = do
  ((_, env), inputs) <-
    mapAccumLM onInput (0, mempty) $ zip param_names arrs
  pure (env, inputs)
  where
    onInput (tag, env) (p, MapArray arr t) =
      pure ((tag, env), (p, DistInputFree arr t))
    onInput (tag, env) (p, MapOther rep t) = do
      let rt = ResTag tag
      env' <- insertRepM rt (Irregular rep) env
      pure
        ( (tag + 1, env'),
          (p, DistInput rt t)
        )

transformUniformRedomap ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  SubExp ->
  [VName] ->
  [Reduce SOACS] ->
  Lambda SOACS ->
  FlattenM [VName]
transformUniformRedomap lvl [] _env _inps w arrs reds map_lam = do
  -- Top-level (no enclosing segments): the arrays and any free variables are
  -- ordinary top-level values in scope, so this is an ordinary non-segmented
  -- reduce over the map width. We emit it as such ('genNonSegRed'); a segmented
  -- reduce over a single implicit segment would be equivalent, but downstream
  -- passes (e.g. migration, coalescing) handle the non-segmented form better.
  let sing_red = singleReduce reds
  (red_lam, nes', shape) <- determineReduceOp (redLambda sing_red) (redNeutral sing_red)
  let comm
        | commutativeLambda red_lam = Commutative
        | otherwise = redComm sing_red
      sing_red_gpu = Reduce comm (soacsLambdaToGPU red_lam) nes'
  genNonSegRed lvl "topLevelSegRed" [w] sing_red_gpu shape (soacsLambdaToGPU map_lam) arrs
transformUniformRedomap lvl segments env inps w arrs reds map_lam = do
  let free = freeIn map_lam
      new_segment = segments <> pure w
      shape = mempty
  reds_gpu <- forM reds $ \red -> do
    nes <- mapM (readNeutral segments env inps) (redNeutral red)
    let red_lam = redLambda red
        comm
          | commutativeLambda red_lam = Commutative
          | otherwise = redComm red
    pure $ Reduce comm (soacsLambdaToGPU red_lam) nes
  free_and_sizes <- freeWithTypeDeps inps free
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe
        (onMapFreeVarMultiDim lvl segments w env inps)
        free_and_sizes
  arrs' <-
    zipWithM
      ( \p arr ->
          liftSubExpRegular
            lvl
            segments
            inps
            env
            (segmentsShape new_segment <> arrayShape (paramType p))
            (Var arr)
      )
      (lambdaParams map_lam)
      arrs

  (free_env, free_inputs) <- mapArraysToInputs free_replicated replicated
  let readFree is = readInputs new_segment free_env is free_inputs
  genUniformSegRed lvl "uniformSegRed" new_segment reds_gpu shape (soacsLambdaToGPU map_lam) arrs' readFree

doUniformSegMaposcanomap ::
  SegLevel ->
  [Scan SOACS] ->
  [VName] ->
  Lambda SOACS ->
  Lambda SOACS ->
  Segments ->
  Segments ->
  DistInputs ->
  DistEnv ->
  ([SubExp] -> FlattenM ()) ->
  FlattenM [VName]
doUniformSegMaposcanomap lvl scans arrs post_lam map_lam old_segments new_segment inps env readFree = do
  let scan = singleScan scans
  nes <- mapM (readNeutral old_segments env inps) (scanNeutral scan)
  (scan_lam, nes', shape) <- determineReduceOp (scanLambda scan) nes
  genUniformSegScanomapWithPost
    lvl
    new_segment
    "uniformmaposcanomap"
    (soacsLambdaToGPU scan_lam)
    shape
    nes'
    (soacsLambdaToGPU post_lam)
    (soacsLambdaToGPU map_lam)
    arrs
    readFree

transformUniformMaposcanomap ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  SubExp ->
  [VName] ->
  [Scan SOACS] ->
  Lambda SOACS ->
  Lambda SOACS ->
  FlattenM [VName]
transformUniformMaposcanomap lvl segments env inps w arrs scans post_lam map_lam = do
  let free = freeIn map_lam <> freeIn post_lam
      new_segment = segments <> pure w
  free_and_sizes <- freeWithTypeDeps inps free
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe
        (onMapFreeVarMultiDim lvl segments w env inps)
        free_and_sizes
  arrs' <-
    zipWithM
      ( \p arr ->
          liftSubExpRegular
            lvl
            segments
            inps
            env
            (segmentsShape new_segment <> arrayShape (paramType p))
            (Var arr)
      )
      (lambdaParams map_lam)
      arrs
  (free_env, free_inputs) <- mapArraysToInputs free_replicated replicated
  let readFree is = readInputs new_segment free_env is free_inputs
  doUniformSegMaposcanomap lvl scans arrs' post_lam map_lam segments new_segment inps env readFree

doSegMaposcanomap ::
  SegLevel ->
  [Scan SOACS] ->
  VName ->
  [VName] ->
  Lambda SOACS ->
  Lambda SOACS ->
  Segments ->
  DistInputs ->
  DistEnv ->
  ([SubExp] -> FlattenM ()) ->
  FlattenM [VName]
doSegMaposcanomap lvl scans flags elems post_lam map_lam segments inps env readFree = do
  let scan = singleScan scans
  let nes = scanNeutral scan
  nes' <- mapM (readNeutral segments env inps) nes
  genSegScanomapWithPost
    lvl
    "maposcanomap"
    (soacsLambdaToGPU $ scanLambda scan)
    nes'
    flags
    (soacsLambdaToGPU post_lam)
    (soacsLambdaToGPU map_lam)
    elems
    readFree

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
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  FlattenM ResRep
segOpInputRep lvl segments env inps arr =
  case lookup arr inps of
    Just (DistInput rt _) ->
      pure $ resVar rt env
    Just (DistInputFree arr' _) ->
      pure $ Regular arr'
    Nothing ->
      Irregular <$> getIrregRep lvl segments env inps arr

-- Basically we need to make our arrays ready for our segscan/segred.
-- Regular arrays are flattened only across the outer segment dimensions and
-- the SOAC width; any row shape expected by the consumer is preserved.
-- we need to check the dense/replicated status of the input.
-- if all of scan inputs are replicated we are fine.
-- otherwise, we need to make the replicated inputs dense.
-- for regulars we can just use the segment descriptor and this should be also the same descriptor for dense irregulars.
prepareSegOpInputs ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  SubExp ->
  [ResRep] ->
  [VName] ->
  Bool ->
  FlattenM (VName, VName, VName, [VName], IrregularKind)
prepareSegOpInputs lvl segments env inps w reps names hasNoFreeVariant
  | all isRegular reps = do
      ws <- dataArr lvl segments env inps w
      (ws_F, ws_O, ws_data) <- doRepIota lvl ws
      m <- arraySize 0 <$> lookupType ws_data
      names' <- mapM (flattenRegularRep m) reps
      pure (ws_F, ws_O, ws, names', Dense)
  | all isReplicatedIrregular reps && hasNoFreeVariant = do
      -- We use the descriptor of the first rep for all inputs, which assumes
      -- that all the replicated inputs have the same offsets into their
      -- respective data arrays. This holds because same-width views produced
      -- by onMapFreeVar inherit the offsets of their underlying arrays, and
      -- those are compact per-segment arrays of the SOAC width (slices are
      -- materialised before they can become inputs here).
      let Irregular rep0 = head reps
      pure (irregularF rep0, irregularO rep0, irregularS rep0, map getData reps, Replicated)
  | otherwise = do
      -- The segment descriptor must count SOAC *elements* per segment, but an
      -- 'IrregularRep' stores primitive data whose structure arrays count
      -- _scalars_ - @c@ per element for a non-scalar element type @b=[c]t@. We
      -- take a dense input rep as the descriptor and convert its scalar-unit
      -- structure to element units: sizes and offsets divided by that input's
      -- @c@, flags subsampled with stride @c@ (all no-ops when @c=1@). Each
      -- input's flat data is reshaped into @[m]b@ rows.
      row_types <- mapM (fmap rowType . lookupInputType inps) names
      (desc, desc_c) <- descriptor $ zip reps row_types
      ws_S <- scaleSizesDown lvl desc_c (irregularS desc)
      ws_O <- scaleSizesDown lvl desc_c (irregularO desc)
      ws_F <- subsampleFlags lvl desc_c (irregularF desc)
      m <- arraySize 0 <$> lookupType ws_F
      names' <- sequence $ zipWith3 (toRows m) reps names row_types
      pure (ws_F, ws_O, ws_S, names', Dense)
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

    -- A dense input rep to use as the descriptor, and its element type's inner
    -- size @c@. Any dense irregular input serves, since all inputs share the
    -- SOAC's segmentation.
    descriptor rs =
      case [(rep, t) | (Irregular rep, t) <- rs, irregularK rep == Dense] of
        (rep, t) : _ -> pure (rep, innerSize t)
        [] ->
          case [(rep, t) | (Irregular rep, t) <- rs] of
            (rep, t) : _ -> do
              rep' <- ensureDenseIrregular lvl "segop_desc" rep
              pure (rep', innerSize t)
            [] -> error "prepareSegOpInputs: impossible"
    innerSize t = product $ map pe64 $ arrayDims t

    -- Reshape an input's data into @m@ rows of the (possibly non-scalar) element
    -- type. For an irregular input the flat scalar data is grouped into rows;
    -- for a regular input the enclosing dimensions are collapsed.
    toRows m (Regular v') _ _ =
      flattenRegularToRows segments m v'
    toRows m (Irregular ir) v row_t = do
      d <- irregularD <$> ensureDenseIrregular lvl (baseName v <> "_dense") ir
      d_t <- lookupType d
      letExp (baseName v <> "_rows") . BasicOp $
        Reshape d $
          reshapeAll (arrayShape d_t) (Shape [m] <> arrayShape row_t)

flattenRegularToRows :: Segments -> SubExp -> VName -> FlattenM VName
flattenRegularToRows segments m v = do
  v_t <- lookupType v
  if isAcc v_t
    then pure v
    else do
      when (arrayRank v_t < segmentsRank segments + 1) $
        error "prepareSegOpInputs: regular input rank too small"
      let row_shape = arrayShape $ stripArray (segmentsRank segments + 1) v_t
      letExp (baseName v <> "_flat") . BasicOp $
        Reshape v $
          reshapeAll (arrayShape v_t) (Shape [m] <> row_shape)

-- | Construct a body and immediately rename it.
renamedBody :: FlattenM [VName] -> FlattenM (Body GPU)
renamedBody = renameBody <=< buildBody_ . fmap varsRes

regularRepVars :: [ResRep] -> [VName]
regularRepVars =
  map onRep
  where
    onRep (Regular v) = v
    onRep Irregular {} = error "regularRepVars: expected regular result"

versionScanRed ::
  FlattenOps ->
  Name ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  StmAux () ->
  SubExp ->
  Body SOACS ->
  FlattenM [VName] ->
  FlattenM DistEnv
versionScanRed ops desc segments env inps res aux w factored_body outer_only = do
  let result_ts =
        [ t `arrayOfShape` segmentsShape segments
        | DistResult _ (DistType _ _ t) _ <- res
        ]
  let fullFlatten = case segments of
        -- Top-level (no enclosing segments): flatten the factored body's
        -- statements as ordinary top-level statements. Unlike distributing them
        -- over segments, this copes with array-valued operators and nested
        -- SOACs whose temporaries would otherwise escape the segmented
        -- machinery's scope.
        [] ->
          renameBody <=< buildBody_ $ do
            mapM_ (flattenTopLevelStm ops) $ bodyStms factored_body
            pure $ bodyResult factored_body
        _ ->
          renamedBody $ regularRepVars <$> distributeAndFlattenBody ops segments "versionScanRed_full_body" env inps res factored_body

  match_res <-
    certifying (distCerts inps aux env) $
      scanRedAlternatives
        desc
        result_ts
        (stmAuxAttrs aux)
        (isParallelFunInside (flattenFunHasParallelism ops) factored_body)
        (allowVersioning (flattenSegLevel ops))
        (segments <> pure w)
        fullFlatten
        (renamedBody outer_only)
  pure $ insertRegulars (map distResTag res) match_res env

insertSegOpMapResults ::
  Segments ->
  VName ->
  VName ->
  VName ->
  IrregularKind ->
  [(DistResult, VName)] ->
  DistEnv ->
  FlattenM DistEnv
insertSegOpMapResults segments segs flags offsets kind bnds env0 =
  foldM insert env0 bnds
  where
    insert env (dist_res, v)
      | isRegularDistResult dist_res = do
          let DistType _ _ t = distResType dist_res
          if isAcc t
            then pure $ insertRegulars [distResTag dist_res] [v] env
            else do
              let expected_shape = segmentsShape segments <> arrayShape t
              v_t <- lookupType v
              v' <-
                letExp (baseName v <> "_reshaped") . BasicOp $
                  Reshape v $
                    reshapeAll (arrayShape v_t) expected_shape
              pure $ insertRegulars [distResTag dist_res] [v'] env
      | otherwise =
          insertIrregularM segs flags offsets (distResTag dist_res) v kind env

distResCerts :: DistEnv -> [DistInput] -> Certs
distResCerts env = Certs . map f
  where
    f (DistInputFree v _) = v
    f (DistInput rt _) = case resVar rt env of
      Regular v -> v
      Irregular r -> irregularD r

reshapeAndBind :: VName -> VName -> Shape -> FlattenM ()
reshapeAndBind v src shape = do
  v_copy <- letExp (baseName v) . BasicOp $ SubExp $ Var src
  v_copy_shape <- arrayShape <$> lookupType v_copy
  letBindNames [v] $ BasicOp $ Reshape v_copy $ reshapeAll v_copy_shape shape

mapResultRep :: SegLevel -> InnerMapMode -> (VName, VName, VName) -> VName -> FlattenM ResRep
mapResultRep _ MultiDim _ v = pure $ Regular v
mapResultRep lvl SingleDim (ws, ws_F, ws_O) v =
  -- Forcing the irregular rep to be 1D because in some places that is my assumption
  -- and also this will make the metadata consistent.
  Irregular
    <$> flattenIrregularRep
      lvl
      IrregularRep
        { irregularS = ws,
          irregularF = ws_F,
          irregularO = ws_O,
          irregularD = v,
          irregularK = Dense
        }

transformDistributed ::
  FlattenOps ->
  M.Map ResTag IrregularRep ->
  Segments ->
  Distributed ->
  FlattenM ()
transformDistributed ops irregs segments dist = do
  let Distributed dstms (DistResults resmap reps) = dist
  env <- foldM (flattenDistStm ops segments) env_initial dstms
  forM_ (M.toList resmap) $ \(rt, binds) ->
    forM_ binds $ \(cs_inps, v, v_t) ->
      certifying (distResCerts env cs_inps) $
        case resVar rt env of
          Regular v' -> letBindNames [v] $ BasicOp $ SubExp $ Var v'
          Irregular irreg -> do
            -- It might have an irregular representation, but we know
            -- that it is actually regular because it is a result.
            irreg' <- ensureDenseIrregular (flattenSegLevel ops) (baseName v <> "_dist_res") irreg
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
          Irregular irreg -> do
            irreg' <- ensureDenseIrregular (flattenSegLevel ops) (baseName v <> "_dist_rep") irreg
            reshapeAndBind v (irregularD irreg') (segmentsShape segments <> arrayShape t)
  where
    env_initial = DistEnv {distResMap = M.map Irregular irregs}

onMapIrregularInputArr ::
  SegLevel ->
  InnerMapMode ->
  Segments ->
  VName ->
  VName ->
  VName ->
  Param Type ->
  VName ->
  IrregularRep ->
  SubExp ->
  FlattenM (MapArray IrregularRep)
onMapIrregularInputArr lvl mode new_segments ws ws_O ws_data p arr rep ws_prod = do
  -- new_segments already has the new w inside, unlike other functions
  rep_t <- lookupType $ irregularD rep
  let p_t = paramType p
  when (arrayRank rep_t > 1) $
    error "onMapIrregularInputArr: irregularD is not 1D"
  if null (arrayDims p_t)
    then do
      -- Assuming irregularD is 1D, size(irregularD rep) == ws_prod should hold and this should be fine.
      let old_shape = arrayShape rep_t
          new_shape =
            case mode of
              SingleDim -> Shape [ws_prod]
              MultiDim -> segmentsShape new_segments
      case irregularK rep of
        Dense -> do
          v_reshaped <- letExp (baseName (paramName p) <> "_reshaped") $ BasicOp $ Reshape (irregularD rep) $ reshapeAll old_shape new_shape
          pure $ MapArray v_reshaped p_t
        Replicated -> do
          new_flat <-
            letExp (baseName arr <> "_flat_expand")
              <=< segMap lvl (MkSolo ws_prod)
              $ \(MkSolo i) -> do
                j <- letSubExp "j" =<< eIndex ws_data [eSubExp i]
                data_off <- letSubExp "data_off" =<< eIndex (irregularO rep) [eSubExp j]
                seg_start <- letSubExp "seg_start" =<< eIndex ws_O [eSubExp j]
                local_pos <- letSubExp "local_pos" <=< toExp $ pe64 i - pe64 seg_start
                flat_idx <- letSubExp "flat_idx" <=< toExp $ pe64 data_off + pe64 local_pos
                fmap (subExpsRes . pure) $ letSubExp "elem" =<< eIndex (irregularD rep) [eSubExp flat_idx]
          v_reshaped <- letExp (baseName (paramName p) <> "_reshaped") $ BasicOp $ Reshape new_flat $ reshapeAll old_shape new_shape
          pure $ MapArray v_reshaped p_t
    else do
      -- We need to split multi-dimensional irregular segments into per-row
      -- segments. We compute the per-row size by dividing each segment's total
      -- size by its number of rows. The division is exact: within a single
      -- segment the array is an ordinary rectangular value, so all rows have
      -- the same size - irregularity exists only across segments. The
      -- alternative would be to read the row size from the sizes in the
      -- parameter type, but those are per-segment distributed inputs, and we do
      -- not have the environment at hand here to look them up.
      num_segments <- arraySize 0 <$> lookupType ws
      -- per_row_size[s] = irregularS[s] / ws[s]
      per_row_size <-
        letExp (baseName (paramName p) <> "_per_row_size")
          <=< segMap lvl (MkSolo num_segments)
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
          <=< segMap lvl (MkSolo ws_prod)
          $ \(MkSolo i) -> do
            seg_i <- letSubExp "seg_i" =<< eIndex ws_data [eSubExp i]
            sz <- letSubExp "sz" =<< eIndex per_row_size [eSubExp seg_i]
            pure $ subExpsRes [sz]
      rep' <- case irregularK rep of
        Dense -> do
          (_, new_O, m) <- exScanAndSum lvl new_S
          new_F <- genFlags lvl m new_O
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
              <=< segMap lvl (MkSolo ws_prod)
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
          new_F <- genFlags lvl m new_O
          pure $
            IrregularRep
              { irregularD = irregularD rep,
                irregularF = new_F,
                irregularS = new_S,
                irregularO = new_O,
                irregularK = Replicated
              }
      pure $ MapOther rep' p_t

onMapInputArrMultiDim ::
  SegLevel ->
  Segments ->
  SubExp ->
  DistEnv ->
  DistInputs ->
  VName ->
  VName ->
  VName ->
  Param Type ->
  VName ->
  FlattenM (MapArray IrregularRep)
onMapInputArrMultiDim lvl old_segments w env inps ws ws_O ws_data p arr = do
  case lookup arr inps of
    Just v_inp ->
      case v_inp of
        DistInputFree vs t -> pure $ MapArray vs (rowType t)
        DistInput rt t -> case resVar rt env of
          Irregular rep -> do
            ws_prod <- arraySize 0 <$> lookupType ws_data
            onMapIrregularInputArr lvl MultiDim (old_segments <> pure w) ws ws_O ws_data p arr rep ws_prod
          Regular vs -> do
            vs_t <- lookupType vs
            if isAcc vs_t
              then pure $ MapArray vs (rowType t)
              else do
                -- let's be cautious and make sure it has the correct shape
                let expected_shape = segmentsShape old_segments <> arrayShape t
                if arrayShape vs_t == expected_shape
                  then pure $ MapArray vs t
                  else do
                    v <-
                      letExp (baseName arr <> "_reg_reshape") . BasicOp . Reshape vs $
                        reshapeAll (arrayShape vs_t) expected_shape
                    pure $ MapArray v (rowType t)
    Nothing -> do
      arr_row_t <- rowType <$> lookupType arr
      arr_rep <-
        letExp (baseName arr <> "_inp_rep") . BasicOp $
          Replicate (segmentsShape old_segments) (Var arr)
      pure $ MapArray arr_rep arr_row_t

flattenMapForInBlock ::
  FlattenOps ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM ()
flattenMapForInBlock ops pat w arrs map_lam = do
  scope <- askScope
  lam <- preprocessLambda (castScope scope) map_lam
  let arrs' = zipWith MapArray arrs $ map paramType (lambdaParams lam)
      (distributed, _) =
        distributeMapWith ops' scope pat [w] arrs' lam
  transformDistributed ops' mempty [w] distributed
  where
    ops' = atSegLevel inBlockSegLevel ops

resultMapMode :: InnerMapMode -> DistInputs -> Type -> InnerMapMode
resultMapMode SingleDim _ _ = SingleDim
resultMapMode MultiDim new_inps v_t
  | any (isVariant new_inps) (arrayDims v_t) = SingleDim
  | otherwise = MultiDim

irregularMapResult ::
  SegLevel ->
  InnerMapMode ->
  (VName, VName, VName) ->
  Segments ->
  IrregularRep ->
  VName ->
  Type ->
  DistInputs ->
  FlattenM ResRep
irregularMapResult lvl mode (ws, ws_F, ws_O) segments irreg v v_t new_inps =
  do
    irreg_dense <- ensureDenseIrregular lvl (baseName v <> "_map_result") irreg
    if any (isVariant new_inps) (arrayShape v_t)
      then do
        old_segment <- arraySize 0 <$> lookupType ws
        -- The size of each flattened outer segment is the sum of its rows'
        -- sizes. Because irreg_dense is dense (compact offsets), we get this in
        -- O(1) per segment as last_offset + last_size - start, avoiding a
        -- segmented reduction over the row sizes. The guard handles empty outer
        -- segments, which have no last row to read (and would index out of
        -- bounds).
        new_shape <- letExp (baseName v <> "_outer_shape") <=< segMap lvl (MkSolo old_segment) $ \(MkSolo is) -> do
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
        (new_ws_F, new_ws_O, _) <- doRepIota lvl new_shape
        letBindNames [v] $ BasicOp $ Replicate mempty $ Var $ irregularD irreg_dense
        mapResultRep lvl SingleDim (new_shape, new_ws_F, new_ws_O) v
      else do
        reshapeAndBind v (irregularD irreg_dense) (segmentsShape segments <> arrayShape v_t)
        mapResultRep lvl mode (ws, ws_F, ws_O) v

transformDistributedInnerMap ::
  FlattenOps ->
  InnerMapMode ->
  (VName, VName, VName) ->
  M.Map ResTag IrregularRep ->
  Segments ->
  Distributed ->
  FlattenM [(VName, ResRep)]
transformDistributedInnerMap ops mode (ws_F, ws_O, ws) irregs segments dist = do
  let Distributed dstms (DistResults resmap reps) = dist
  -- A name bound inside the distributed body is variant whether or not another
  -- statement uses it, so a result sized by such a name is irregular. Only
  -- counting the names that are used would leave the size existentially bound
  -- by a 'FlatMap' lambda (returned, but used by nothing) looking invariant,
  -- and the result would be given a type mentioning a name that is not in
  -- scope.
  let asInput (DistResult tag (DistType _ _ t) v) = (v, DistInput tag t)
      new_inps =
        concatMap distStmInputs dstms
          <> map asInput (concatMap distStmResult dstms)
  env <- foldM (flattenDistStm ops segments) env_initial dstms
  resmap_res <- fmap concat $ forM (M.toList resmap) $ \(rt, binds) ->
    forM binds $ \(cs_inps, v, v_t) ->
      certifying (distResCerts env cs_inps) $
        case (resultMapMode mode new_inps v_t, resVar rt env) of
          (MultiDim, Regular v') ->
            if isAcc v_t
              then do
                letBindNames [v] $ BasicOp $ SubExp $ Var v'
                pure (v, Regular v)
              else do
                reshapeAndBind v v' (segmentsShape segments <> arrayShape v_t)
                pure (v, Regular v)
          (SingleDim, Regular v') ->
            if isAcc v_t
              then do
                letBindNames [v] $ BasicOp $ SubExp $ Var v'
                pure (v, Regular v)
              else do
                letBindNames [v] $ BasicOp $ SubExp $ Var v'
                rep <- mapResultRep lvl SingleDim (ws, ws_F, ws_O) v
                pure (v, rep)
          (result_mode, Irregular irreg) -> do
            rep <- irregularMapResult lvl result_mode (ws, ws_F, ws_O) segments irreg v v_t new_inps
            pure (v, rep)
  reps_res <- forM reps $ \(v, r) -> do
    case r of
      Left se -> do
        letBindNames [v] $ BasicOp $ Replicate (segmentsShape segments) se
        -- the se is not part of input so this should be fine
        rep <- mapResultRep lvl mode (ws, ws_F, ws_O) v
        pure (v, rep)
      Right (DistInputFree arr t) -> do
        letBindNames [v] $ BasicOp $ SubExp $ Var arr
        if isAcc t
          then pure (v, Regular v)
          else do
            rep <- mapResultRep lvl (resultMapMode mode new_inps t) (ws, ws_F, ws_O) v
            pure (v, rep)
      Right (DistInput rt t) ->
        let result_mode = resultMapMode mode new_inps t
         in case resVar rt env of
              Regular v' -> do
                letBindNames [v] $ BasicOp $ SubExp $ Var v'
                if isAcc t
                  then pure (v, Regular v)
                  else do
                    rep <- mapResultRep lvl result_mode (ws, ws_F, ws_O) v
                    pure (v, rep)
              Irregular irreg -> do
                rep <- irregularMapResult lvl result_mode (ws, ws_F, ws_O) segments irreg v t new_inps
                pure (v, rep)
  pure $ resmap_res <> reps_res
  where
    env_initial = DistEnv {distResMap = M.map Irregular irregs}
    lvl = flattenSegLevel ops

distributeAndTransformInnerMap ::
  FlattenOps ->
  InnerMapMode ->
  (VName, VName, VName) ->
  Segments ->
  DistInputs ->
  Pat Type ->
  [MapArray IrregularRep] ->
  (VName -> Maybe (FlattenM (VName, MapArray IrregularRep))) ->
  Lambda SOACS ->
  FlattenM [ResRep]
distributeAndTransformInnerMap ops mode ws_triple new_segment inps pat arrs' onFreeVar map_lam = do
  -- Skip the return type: the results are described by 'pat', so variables
  -- occurring only there never need replicating into the body - and they may
  -- not even be in scope here. See Note [Ill-formed inner-map lambda].
  let free = freeIn $ map_lam {lambdaReturnType = [] :: [Type]}
  free_and_sizes <- freeWithTypeDeps inps free
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe
        onFreeVar
        free_and_sizes
  free_ps <-
    zipWithM
      newParam
      (map ((<> "_free") . baseName) free_replicated) -- this should free_replicated?
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
        distributeMapWith ops scope pat new_segment (replicated <> arrs') map_lam'
  -- order the result representations in the same order as the pattern
  resRepsInPatOrder pat
    <$> transformDistributedInnerMap ops mode ws_triple arrmap new_segment distributed

-- | Flatten a map nested in a map-nest (nonempty enclosing 'Segments'). The map
-- width is either uniform (invariant to the nest) or nonuniform, giving the
-- 'InnerMapMode': a uniform width produces a regular, multi-dimensional result
-- ('MultiDim'), while a nonuniform width is irregular and flattened into a
-- single segment dimension ('SingleDim'). The mode selects how inputs and free
-- variables are read and how the results are represented, and is threaded
-- through the rest of the flattening.
transformInnerMap ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM [ResRep]
transformInnerMap ops segments env inps pat w arrs map_lam = do
  outer_scope <- askScope
  let mode
        | isVariant inps w = SingleDim
        | otherwise = MultiDim
      -- In the uniform 'MultiDim' case - regular inputs and all result
      -- dimensions invariant - the flags/offsets/elements bookkeeping produced
      -- by 'doRepIota' is never consulted, so we do not emit it. This is not
      -- just an efficiency concern: when generating in-block code, the
      -- bookkeeping contains SegOps whose dimensions are bound inside the kernel
      -- body, which would make 'noNonuniformPar' reject the enclosing intrablock
      -- version.
      invariantDim Constant {} = True
      invariantDim (Var v) = v `M.member` outer_scope
      regularInput arr = case lookup arr inps of
        Just (DistInput rt _)
          | Irregular {} <- resVar rt env -> False
        _ -> True
      uniform =
        all (all invariantDim . arrayDims) (patTypes pat)
          && all regularInput arrs
  (ws, ws_F, ws_O, ws_data) <-
    case mode of
      MultiDim
        | uniform ->
            -- XXX: this depends on laziness to explode only on usage. It might
            -- be better to handle this path more explicitly.
            pure (bad "ws", bad "ws_F", bad "ws_O", bad "ws_data")
      _ -> do
        ws <- dataArr lvl segments env inps w
        (ws_F, ws_O, ws_data) <- doRepIota lvl ws
        pure (ws, ws_F, ws_O, ws_data)
  (arrs', new_segment, onFreeVar) <-
    case mode of
      MultiDim -> do
        arrs' <-
          zipWithM
            (onMapInputArrMultiDim lvl segments w env inps ws ws_O ws_data)
            (lambdaParams map_lam)
            arrs
        pure (arrs', segments <> pure w, onMapFreeVarMultiDim lvl segments w env inps)
      SingleDim -> do
        arrs' <-
          zipWithM
            (onMapInputArr lvl segments env inps ws ws_O ws_data)
            (lambdaParams map_lam)
            arrs
        new_segment <- arraySize 0 <$> lookupType ws_data
        pure (arrs', [new_segment], onMapFreeVar lvl segments env inps ws (ws_F, ws_O, ws_data))
  distributeAndTransformInnerMap ops mode (ws_F, ws_O, ws) new_segment inps pat arrs' onFreeVar map_lam
  where
    lvl = flattenSegLevel ops
    bad what =
      error $ "transformInnerMap: " <> what <> " demanded in uniform case"

-- | Flatten a map over the given enclosing 'Segments'. With no enclosing
-- segments this is a top-level map, whose inputs are ordinary regular values;
-- otherwise it is a map nested in a map-nest, whose inputs are the
-- per-enclosing-segment values.
transformMap ::
  FlattenOps ->
  -- | Incremental-flattening attributes of the map itself. These are
  -- propagated onto the (preprocessed) body, and so eventually reach maps at
  -- any depth of the nest.
  Attrs ->
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM [ResRep]
transformMap ops attrs [] _env _inps pat w arrs map_lam = do
  -- Top-level map (no enclosing segments). Preprocess the body and then
  -- propagate the attributes onto it, so they influence how the body is
  -- versioned (e.g. only_inner reaching a Screma produced by interchanging a
  -- 'sequential_outer' loop). Order matters: preprocessing may rewrite a body
  -- statement, so propagating first would lose the attributes on the rewritten
  -- form. XXX: this is arguably a bug in preprocessing.
  scope <- castScope <$> askScope :: FlattenM (Scope SOACS)
  lam <-
    fmap (propagateVersioningAttrs attrs) . renameLambda
      =<< preprocessLambda scope map_lam
  transformTopLevelMap ops pat w arrs lam
transformMap ops attrs segments env inps pat w arrs map_lam = do
  -- Nested map. As in the top-level case, propagate the attributes onto the
  -- preprocessed body; this is what carries them to maps deeper in the nest.
  gpu_scope <- askScope
  let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
  lam <- propagateVersioningAttrs attrs <$> preprocessLambda pp_scope map_lam
  transformInnerMap ops segments env inps pat w arrs lam

-- | Fully flatten a map that has no enclosing segments (a top-level map). This
-- is the empty-'Segments' special case of 'transformMap': the mapped arrays are
-- ordinary regular top-level values, so we distribute the map directly over its
-- own width and flatten the resulting body, rather than reconstructing
-- per-enclosing-segment inputs. The results are necessarily regular.
transformTopLevelMap ::
  FlattenOps ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM [ResRep]
transformTopLevelMap ops pat w arrs lam = do
  scope <- castScope <$> askScope :: FlattenM (Scope SOACS)
  let arrs' = zipWith MapArray arrs $ map paramType (lambdaParams lam)
      (distributed, _) =
        distributeMapWith ops scope pat [w] arrs' lam
  transformDistributed ops mempty [w] distributed
  pure $ map Regular $ patNames pat

runMapLambdaBody ::
  Segments ->
  DistEnv ->
  DistInputs ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Pat Type ->
  [DistResult] ->
  FlattenM [VName]
runMapLambdaBody [] _env _inps w arrs map_lam _pat _ress = do
  -- Top level (no enclosing segments): the mapped arrays are indexed directly
  -- and free variables are already in scope, so there is no per-segment input
  -- reconstruction to do - just run the (sequentialised) body under a segmap
  -- over the map width.
  map_lam' <- renameLambda $ soacsLambdaToGPU map_lam
  vs <- letTupExp "outer_map" <=< renameExp <=< segMap defaultSegLevel [w] $ \is -> do
    let gtid = case toList is of
          [i] -> i
          _ -> error "runMapLambdaBody: expected single index"
    forM_ (zip (lambdaParams map_lam') arrs) $ \(p, arr) ->
      letBindNames [paramName p]
        =<< case paramType p of
          Acc {} -> eSubExp $ Var arr
          _ -> eIndex arr [eSubExp gtid]
    bodyBind $ lambdaBody map_lam'
  forM vs $ \v ->
    letExp (baseName v <> "_copy") $ BasicOp $ Replicate mempty (Var v)
runMapLambdaBody segments env inps w arrs map_lam _pat _ress = do
  map_lam' <- renameLambda $ soacsLambdaToGPU map_lam
  ws <- dataArr defaultSegLevel segments env inps w
  (_ws_F, ws_O, ws_data) <- doRepIota defaultSegLevel ws
  arrs' <-
    zipWithM
      (onMapInputArrMultiDim defaultSegLevel segments w env inps ws ws_O ws_data)
      (lambdaParams map_lam')
      arrs

  free_and_sizes <- freeWithTypeDeps inps (freeIn map_lam')
  (param_env, param_inputs) <-
    mapArraysToInputs (map paramName (lambdaParams map_lam')) arrs'
  let new_segments = segments <> pure w
      free_inputs =
        [ (v, inp)
        | v <- free_and_sizes,
          Just inp <- [lookup v inps]
        ]

  vs <- letTupExp "outer_map" <=< renameExp <=< segMap defaultSegLevel new_segments $ \is -> do
    let full_is = toList is
        outer_is = take (segmentsRank segments) full_is

    readInputs segments env outer_is free_inputs
    readInputs new_segments param_env full_is param_inputs

    bodyBind $ lambdaBody map_lam'
  forM vs $ \v -> do
    letExp (baseName v <> "_copy") $
      BasicOp $
        Replicate mempty (Var v)

versionedUniformMap ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM DistEnv
versionedUniformMap ops segments env inps ress pat aux w arrs map_lam = do
  let only_intra = onlyExploitIntra (stmAuxAttrs aux)
      may_intra = worthIntrablock map_lam && mayExploitIntra (stmAuxAttrs aux)

  intra' <-
    if only_intra || may_intra
      then Intrablock.intrablockParallelise (flattenMapForInBlock ops) segments env inps ress pat aux w arrs map_lam
      else pure Nothing

  let fullFlatten =
        regularRepVars <$> transformMap (atSegLevel defaultSegLevel ops) (stmAuxAttrs aux) segments env inps pat w arrs map_lam

      outerOnly =
        runMapLambdaBody segments env inps w arrs map_lam pat ress

      result_ts =
        [ t `arrayOfShape` segmentsShape segments
        | DistResult _ (DistType _ _ t) _ <- ress
        ]

  match_res <-
    certifying (distCerts inps aux env) $
      mapAlternatives
        "match_res"
        result_ts
        (stmAuxAttrs aux)
        -- 'versionedUniformMap' is only reached via 'isVersionableMap', which
        -- guarantees the body calls no parallel function.
        False
        (worthSequentialising map_lam)
        (segments <> pure w)
        (renamedBody fullFlatten)
        (renamedBody outerOnly)
        intra'

  pure $ insertRegulars (map distResTag ress) match_res env

flattenUniformRedomap ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  [Reduce SOACS] ->
  Lambda SOACS ->
  FlattenM DistEnv
flattenUniformRedomap ops segments env inps res pat aux w arrs form reds map_lam = do
  let outer_only = transformUniformRedomap (flattenSegLevel ops) segments env inps w arrs reds map_lam
  gpu_scope <- askScope
  let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
  factored <- factorScremaForParallelism funHasParallelism pp_scope aux pat w arrs form
  case factored of
    Just body ->
      versionScanRed ops "uniform_redomap_alt" segments env inps res aux w body outer_only
    Nothing -> do
      elems' <- outer_only
      pure $ insertRegulars (map distResTag res) elems' env
  where
    funHasParallelism = flattenFunHasParallelism ops

flattenSegRedomap ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  SubExp ->
  [VName] ->
  [Reduce SOACS] ->
  Lambda SOACS ->
  FlattenM DistEnv
flattenSegRedomap ops segments env inps res w arrs reds map_lam = do
  reps <- mapM (segOpInputRep lvl segments env inps) arrs
  let sing_red = singleReduce reds
      hasNoFreeVariant = allNames (not . isVariant inps . Var) (freeIn sing_red <> freeIn map_lam)
  (ws_F, ws_O, ws_S, elems, elems_kind) <-
    prepareSegOpInputs lvl segments env inps w reps arrs hasNoFreeVariant
  nes' <- mapM (readNeutral segments env inps) (redNeutral sing_red)
  let sing_red' = sing_red {redNeutral = nes'}
  let free = freeIn map_lam
  free_and_sizes <- freeWithTypeDeps inps free
  ws <- dataArr lvl segments env inps w
  (_, _, ws_data) <- doRepIota lvl ws_S
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe
        (onMapFreeVar lvl segments env inps ws (ws_F, ws_O, ws_data))
        free_and_sizes
  (free_env, free_inputs) <- mapArraysToInputs free_replicated replicated

  new_segment <- arraySize 0 <$> lookupType ws_F
  let readFree is = readInputs [new_segment] free_env is free_inputs
  (red_elems, mapout_elems) <-
    genSegRedomap lvl ws_S ws_F ws_O elems sing_red' (soacsLambdaToGPU map_lam) readFree
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
  where
    lvl = flattenSegLevel ops

flattenUniformMaposcanomap ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  [Scan SOACS] ->
  Lambda SOACS ->
  Lambda SOACS ->
  FlattenM DistEnv
flattenUniformMaposcanomap ops segments env inps res pat aux w arrs form scans post_lam map_lam = do
  let outer_only =
        transformUniformMaposcanomap lvl segments env inps w arrs scans post_lam map_lam
  gpu_scope <- askScope
  let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
  factored <- factorScremaForParallelism funHasParallelism pp_scope aux pat w arrs form
  case factored of
    Just body ->
      versionScanRed ops "uniform_maposcanomap_alt" segments env inps res aux w body outer_only
    Nothing -> do
      elems' <- outer_only
      pure $ insertRegulars (map distResTag res) elems' env
  where
    funHasParallelism = flattenFunHasParallelism ops
    lvl = flattenSegLevel ops

flattenSegMaposcanomap ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  SubExp ->
  [VName] ->
  [Scan SOACS] ->
  Lambda SOACS ->
  Lambda SOACS ->
  FlattenM DistEnv
flattenSegMaposcanomap ops segments env inps res w arrs scans post_lam map_lam = do
  reps <- mapM (segOpInputRep lvl segments env inps) arrs
  let hasNoFreeVariant = allNames (not . isVariant inps . Var) (freeIn post_lam <> freeIn map_lam <> foldMap freeIn scans)
  (ws_F, ws_O, ws_S, elems, elems_kind) <-
    prepareSegOpInputs lvl segments env inps w reps arrs hasNoFreeVariant
  let free = freeIn map_lam <> freeIn post_lam
  free_and_sizes <- freeWithTypeDeps inps free
  ws <- dataArr lvl segments env inps w
  (_, _, ws_data) <- doRepIota lvl ws_S
  (free_replicated, replicated) <-
    fmap unzip . sequence $
      mapMaybe
        (onMapFreeVar lvl segments env inps ws (ws_F, ws_O, ws_data))
        free_and_sizes
  (free_env, free_inputs) <- mapArraysToInputs free_replicated replicated
  new_segment <- arraySize 0 <$> lookupType ws_F
  let readFree is = readInputs [new_segment] free_env is free_inputs
  elems' <- doSegMaposcanomap lvl scans ws_F elems post_lam map_lam segments inps env readFree
  insertSegOpMapResults
    segments
    ws_S
    ws_F
    ws_O
    elems_kind
    (zip res elems')
    env
  where
    lvl = flattenSegLevel ops

flattenPlainMap ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM DistEnv
flattenPlainMap ops segments env inps res pat aux w arrs map_lam = do
  map_res <-
    transformMap ops (stmAuxAttrs aux) segments env inps pat w arrs map_lam
  insertRepsM (zip (map distResTag res) map_res) env

flattenOtherScrema ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  FlattenM DistEnv
flattenOtherScrema ops segments env inps res pat aux w arrs form = do
  gpu_scope <- askScope
  let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
  factored <- factorScremaForParallelism funHasParallelism pp_scope aux pat w arrs form
  case factored of
    Just body -> do
      reps <- distributeAndFlattenBody ops segments "factorScremaForParallelism_body" env inps res body
      insertRepsM (zip (map distResTag res) reps) env
    Nothing
      -- XXX: here we silently sequentialise any SOAC that is not handled
      -- above if it is possible to do so. We need to make sure that we
      -- actually handle everything we care about!
      | shouldDissectForm form ->
          error "flattenScrema: complex Screma survived preprocessing"
      | all isRegularDistResult res ->
          flattenScalarStm ops segments env inps res $ Let pat aux (Op (Screma w arrs form))
      | otherwise -> do
          -- XXX: The results are nonuniform, so we cannot run the SOAC
          -- unchanged inside a kernel. Sequentialise it to a loop and
          -- flatten that instead. This does lose us potential parallelism.
          -- A solution would be to preprocess such cases to express them in
          -- terms of loops and maps instead, which we can indeed handle.
          stms <-
            preprocessStms pp_scope
              =<< runSimplifiedBuilder
                pp_scope
                (auxing aux $ FOT.transformSOAC pat $ Screma w arrs form)
          let body = mkBody stms $ varsRes $ patNames pat
          reps <- distributeAndFlattenBody ops segments "sequentialised_soac" env inps res body
          insertRepsM (zip (map distResTag res) reps) env
  where
    funHasParallelism = flattenFunHasParallelism ops

flattenScrema ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  (Pat Type, StmAux ()) ->
  (SubExp, [VName], ScremaForm SOACS) ->
  FlattenM DistEnv
flattenScrema ops segments env inps res (pat, aux) (w, arrs, form)
  | Just (reds, map_lam) <- isRedomapSOAC form,
    not $ isVariant inps w,
    all isRegularDistResult res,
    all (isRegularInputArr env inps) arrs,
    all (suitableUniformOperator inps . redLambda) reds =
      flattenUniformRedomap ops segments env inps res pat aux w arrs form reds map_lam
  | Just (reds, map_lam) <- isRedomapSOAC form,
    not $ lambdaHasParallelism funHasParallelism map_lam,
    all (\red -> suitableOperator env inps (redLambda red) (redNeutral red)) reds =
      flattenSegRedomap ops segments env inps res w arrs reds map_lam
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    not $ isVariant inps w,
    all isRegularDistResult res,
    all (isRegularInputArr env inps) arrs,
    all (suitableUniformOperator inps . scanLambda) scans =
      flattenUniformMaposcanomap ops segments env inps res pat aux w arrs form scans post_lam map_lam
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    not $ lambdaHasParallelism funHasParallelism map_lam,
    not $ lambdaHasParallelism funHasParallelism post_lam,
    all (\scan -> suitableOperator env inps (scanLambda scan) (scanNeutral scan)) scans =
      flattenSegMaposcanomap ops segments env inps res w arrs scans post_lam map_lam
  | Just map_lam <- isMapSOAC form,
    isVersionableMap funHasParallelism lvl inps env w res map_lam =
      versionedUniformMap ops segments env inps res pat aux w arrs map_lam
  | Just map_lam <- isMapSOAC form =
      flattenPlainMap ops segments env inps res pat aux w arrs map_lam
  | otherwise =
      flattenOtherScrema ops segments env inps res pat aux w arrs form
  where
    funHasParallelism = flattenFunHasParallelism ops
    lvl = flattenSegLevel ops

-- Note [FlatMap element counting]
--
-- 'FlatMap' counts in units of the lambda's element type @b@: the data array
-- for irregular results is @[m]b@, the shape and offset arrays hold per-segment
-- counts of @b@-elements, and the flag array has one entry per @b@-element.
--
-- The flattening pass, however, represents irregular data with an
-- 'IrregularRep' whose 'irregularD' is primitive (a flat array of scalars), so
-- its segment sizes and flags count *scalars*. When @b@ is itself a @c@-element
-- array there are @c@ scalars per @b@-element ('flatMapElemsPer'), so the two
-- notions of "size" differ by a factor of @c@.
--
-- The flattening rules therefore convert between the two: the metadata that
-- 'FlatMap' surfaces directly (the total @m@, and the shape, flag, and offset
-- arrays) is put in @b@-units by dividing scalar-unit sizes by @c@. The data
-- arrays themselves, however, keep their natural primitive 'IrregularRep' with
-- scalar-unit structure - the "'irregularD' is always primitive" invariant is
-- preserved. A consumer of such a data array reconstructs the @b@-element rows
-- from the primitive rep: 'onMapIrregularInputArr' does so for a plain map, and
-- 'prepareSegOpInputs' for a segmented redomap/scan.

-- Note [Ill-formed inner-map lambda]
--
-- 'flattenFlatMapNested' hands the inner-map machinery ('transformMap') an
-- ordinary 'Lambda' obtained from the 'FlatMap' 'ExtLambda' by instantiating
-- the existential size with the lambda's own leading result. That lambda is
-- ill-formed: the size is bound by a statement in the body, and so is
-- meaningless in the return type, which describes the lambda from outside.
--
-- We do it because 'transformMap' requires a 'Lambda', whose types cannot
-- express a result whose outer size varies per iteration, which is exactly what
-- an irregular 'FlatMap' result is.
--
-- Nothing downstream derives meaning from the return type - the results are
-- described by 'map_pat' throughout - but it is still traversed in two places,
-- which is what makes this contained rather than harmless:
--
-- - 'distributeAndTransformInnerMap' computes the free variables that must be
--   replicated into the body, and must skip the return type; otherwise the size
--   is looked up in the enclosing scope, where it does not exist.
--
-- - 'preprocessLambda' simplifies the return type outside the scope of the
--   parameters, where a size that resolves to nothing is left alone.
--
-- FIXME: Teaching the inner-map machinery to accept an 'ExtLambda' would remove
-- the need for all of this.

-- | The number of scalars per element of a 'FlatMap' lambda's (first) value
-- result; see Note [FlatMap element counting].
flatMapElemsPer :: ExtLambda SOACS -> TPrimExp Int64 VName
flatMapElemsPer lam =
  product $ map pe64 $ arrayDims $ head $ flatMapRowTypes lam

-- | Divide each entry of a per-segment size (or offset) array by the given
-- factor, converting scalar-unit counts to element-unit counts. A no-op when the
-- factor is statically 1 (a scalar element type). See Note [FlatMap element
-- counting].
scaleSizesDown :: SegLevel -> TPrimExp Int64 VName -> VName -> FlattenM VName
scaleSizesDown _ elems_per arr | elems_per == 1 = pure arr
scaleSizesDown lvl elems_per arr = do
  arr_t <- lookupType arr
  letExp "scaled_sizes" <=< segMap lvl (arrayDims arr_t) $ \gtids -> do
    x <- letSubExp "x" =<< eIndex arr (map eSubExp gtids)
    x_b <- letSubExp "x_b" =<< toExp (pe64 x `div` elems_per)
    pure [subExpRes x_b]

-- | Subsample a scalar-unit flag array to element units by taking every
-- @elems_per@-th entry, giving one flag per element. A no-op when the factor is
-- statically 1. This reuses the (already-built) flag array rather than
-- recomputing segment starts with a segmented scan. See Note [FlatMap element
-- counting].
subsampleFlags :: SegLevel -> TPrimExp Int64 VName -> VName -> FlattenM VName
subsampleFlags _ elems_per arr | elems_per == 1 = pure arr
subsampleFlags lvl elems_per arr = do
  big_m <- arraySize 0 <$> lookupType arr
  m <- letSubExp "flags_m" =<< toExp (pe64 big_m `div` elems_per)
  letExp "elem_flags" <=< segMap lvl (MkSolo m) $ \(MkSolo i) -> do
    flag <- letSubExp "flag" =<< eIndex arr [toExp $ pe64 i * elems_per]
    pure [subExpRes flag]

-- | Flattening rule for a top-level 'FlatMap', which is a very thin wrapper
-- over just flattening the lambda. The result produced by 'FlatMap' corresponds
-- exactly to the internal irregular representation, so we simply distribute the
-- lambda over the @w@ segments and obtain an 'IrregularRep' for each result
-- (they share the same segment structure, as required by the type of
-- 'FlatMap').
flattenFlatMap ::
  FlattenOps ->
  Pat Type ->
  SubExp ->
  [VName] ->
  ExtLambda SOACS ->
  FlattenM ()
flattenFlatMap ops pat w arrs lam = do
  let segments = [w]
      inps =
        zipWith
          (\p arr -> (paramName p, DistInputFree arr (paramType p)))
          (lambdaParams lam)
          arrs
      elem_ts = flatMapRowTypes lam
      (m_name, s_name, f_name, o_name, value_names) =
        case patNames pat of
          (a : b : c : d : hs) -> (a, b, c, d, hs)
          _ -> error "flattenFlatMap: pattern too short"
      (d_names, r_names) = flatMapSplitValues lam value_names
      -- A nonuniform result is distributed as a variably sized array, a uniform
      -- one as an ordinary array with one element per segment.
      distTypeOf t
        | flatMapNonuniform t = DistType segments (Rank 1) $ static $ rowType t
        | otherwise = DistType segments (Rank 0) $ static t
      res =
        zipWith3
          (\i v t -> DistResult (ResTag i) (distTypeOf t) v)
          [0 ..]
          value_names
          value_ts
      -- The lambda's leading size result is of no use here, as the metadata is
      -- derived from the segment structure of the flattened irregular results.
      body = lambdaBody lam
      body' = body {bodyResult = drop 1 (bodyResult body)}
  reps <- distributeAndFlattenBody ops segments "flatmap" mempty inps res body'
  let (irreg_reps, reg_reps) = flatMapSplitValues lam reps
  irregs <- forM irreg_reps $ \case
    Irregular ir -> ensureDenseIrregular lvl "flatmap_res" ir
    Regular _ -> error "flattenFlatMap: irregular result is not irregular"
  -- The regular results (outer size @w@) are already in the form we want.
  forM_ (zip r_names reg_reps) $ \(v, rep) -> case rep of
    Regular v' -> letBindNames [v] $ BasicOp $ SubExp $ Var v'
    Irregular _ -> error "flattenFlatMap: regular result is not regular"
  case (irregs, elem_ts) of
    (ir0 : _, _ : _) -> do
      -- The flattening structure arrays are in units of scalars, but the source
      -- 'flatmap' wants them in units of the lambda's element type @b@, of
      -- which there are 'elems_per' scalars. All results share the same segment
      -- structure, so we derive the source metadata once, from the first
      -- result:
      --
      --   * The shape and offset arrays (per-segment, size @w@) are the
      --     scalar-unit 'irregularS' and 'irregularO' scaled down by
      --     'elems_per'.
      --
      --   * the flag array (per-element, size @m@) is 'irregularF' subsampled
      --     with stride 'elems_per'.
      let elems_per = flatMapElemsPer lam
      -- The total size is the number of data elements.
      big_m <- arraySize 0 <$> lookupType (irregularD ir0)
      m <- letSubExp "flatmap_m" =<< toExp (pe64 big_m `div` elems_per)
      letBindNames [m_name] $ BasicOp $ SubExp m
      s <- scaleSizesDown lvl elems_per (irregularS ir0)
      o <- scaleSizesDown lvl elems_per (irregularO ir0)
      flags <- subsampleFlags lvl elems_per (irregularF ir0)
      -- The per-segment metadata (outer size @w@): shape and offset arrays.
      bindReshape s_name s (Shape [w])
      bindReshape o_name o (Shape [w])
      -- The per-element flag array (outer size @m@).
      bindReshape f_name flags (Shape [Var m_name])
      -- The concatenated data arrays, reshaped from the flat scalar data to
      -- @[m]b@ (the outer dimension being the freshly bound total size).
      forM_ (zip3 d_names irregs elem_ts) $ \(v, ir, elem_t) ->
        bindReshape v (irregularD ir) (Shape [Var m_name] <> Shape (arrayDims elem_t))
    _ -> error "flattenFlatMap: FlatMap with no irregular results"
  where
    lvl = flattenSegLevel ops
    value_ts = drop 1 (lambdaReturnType lam)
    static =
      fromMaybe (error "flattenFlatMap: existential size.") . hasStaticShape
    bindReshape name arr newshape = do
      arr_t <- lookupType arr
      letBindNames [name] . BasicOp $
        Reshape arr (reshapeAll (arrayShape arr_t) newshape)

-- | Flattening rule for a 'FlatMap' nested inside an enclosing map-nest. A
-- 'FlatMap' is just a nonuniform map with implicit concatenation, so we run it
-- through the ordinary inner-map machinery, which already produces - for a
-- variably-sized result - an 'IrregularRep' whose segment sizes are the
-- per-enclosing-segment concatenated lengths and whose data is the
-- concatenation. That directly gives the data arrays (results 4..) and, as
-- their per-enclosing segment sizes, the total sizes @m@ (result 0).
--
-- The metadata is derived from the shape array (result 1) - the per-iteration
-- output sizes - which the inner-map does not itself surface. The lambda
-- returns exactly that size as its leading result, so mapping it over the nest
-- yields the irregular array of per-iteration sizes, segmented by the 'FlatMap'
-- width. From it we then compute the offset array (result 3), as the
-- per-enclosing exclusive prefix sum, and the flag array (result 2).
--
-- See also Note [FlatMap element counting].
flattenFlatMapNested ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  StmAux () ->
  SubExp ->
  [VName] ->
  ExtLambda SOACS ->
  FlattenM DistEnv
flattenFlatMapNested ops segments env inps res aux w arrs lam = do
  size_name <- newVName "flatmap_sizes"
  data_names <- mapM (const $ newVName "flatmap_data") val_ts
  let size_pe = PatElem size_name $ arrayOfRow (Prim int64) w
      data_pes = zipWith (\v t -> PatElem v $ arrayOfRow t w) data_names val_ts
      map_pat = Pat $ size_pe : data_pes
  let lvl = flattenSegLevel ops
  certifying (distCerts inps aux env) $ do
    reps <- transformMap ops (stmAuxAttrs aux) segments env inps map_pat w arrs map_lam
    case reps of
      shape_rep : data_reps
        | Just first_data <- firstIrregular data_reps -> do
            -- An 'IrregularRep' always stores primitive data, so for a
            -- non-scalar element type @b@ the data result's segment sizes are
            -- in units of scalars, of which there are 'elems_per' per
            -- b-element. The source 'flatmap' counts b-elements, so we scale
            -- the affected metadata down.
            let elems_per = flatMapElemsPer lam

            -- The number of b-elements per enclosing segment: the per-segment
            -- totals (result 0), and the segment sizes of the flag array. These
            -- are the data result's segment sizes, scaled to b-element units.
            -- When the data is regular (the fully uniform case), it is the
            -- constant inner size, replicated across the segments.
            s_flag <- case first_data of
              Irregular ir -> scaleSizesDown lvl elems_per (irregularS ir)
              Regular d -> do
                d_t <- lookupType d
                let inner = product $ map pe64 $ drop (segmentsRank segments) $ arrayDims d_t
                n_elem <- letSubExp "flatmap_n_elem" =<< toExp (inner `div` elems_per)
                letExp "flatmap_n" (BasicOp $ Replicate (segmentsShape segments) n_elem)
            let n_rep = Regular s_flag

            -- The offset array (result 3) has the same structure as the shape
            -- array; its values are the exclusive prefix sum of the shape
            -- within each enclosing segment. We also need the shape as a single
            -- flat array of all per-iteration sizes (for the flags).
            (offset_rep, shape_flat) <- case shape_rep of
              Regular sd -> do
                off <- genExPrefixSum lvl "flatmap_offset" sd
                sd_t <- lookupType sd
                n_shape <- letSubExp "flatmap_shape_n" =<< toExp (product $ map pe64 $ arrayDims sd_t)
                flat <-
                  letExp "flatmap_shape_flat" . BasicOp $
                    Reshape sd (reshapeAll (arrayShape sd_t) (Shape [n_shape]))
                pure (Regular off, flat)
              Irregular s_ir -> do
                inc <- genSegPrefixSum lvl "flatmap_offset_inc" (irregularF s_ir) (irregularD s_ir)
                n_off <- arraySize 0 <$> lookupType inc
                off_D <- letExp "flatmap_offset_D" <=< segMap lvl (MkSolo n_off) $ \(MkSolo i) -> do
                  a <- letSubExp "a" =<< eIndex inc [eSubExp i]
                  b <- letSubExp "b" =<< eIndex (irregularD s_ir) [eSubExp i]
                  off <- letSubExp "off" =<< toExp (pe64 a - pe64 b)
                  pure [subExpRes off]
                pure (Irregular s_ir {irregularD = off_D}, irregularD s_ir)

            -- The flag array (result 2): the segment-start flags over all
            -- b-elements. Every enclosing-segment boundary is also a segment start,
            -- so a single 'doRepIota' over all per-iteration sizes yields the flags
            -- for all enclosing segments at once. Its per-enclosing segment sizes
            -- are @s_flag@ (in b-element units); we build the enclosing flag/offset
            -- structure from those, respecting the invariant that in an
            -- 'IrregularRep' the flag and data arrays share their shape (rather than
            -- borrowing the scalar-unit structure of the data rep).
            (flag_F, flag_O, _) <- doRepIota lvl s_flag
            (flag_D0, _, _) <- doRepIota lvl shape_flat
            -- 'flag_D0' and 'flag_F' both have the total number of b-elements as
            -- their size, but computed by different means; coerce so the rep's flag
            -- and data arrays share a size (the 'IrregularRep' invariant).
            m_flag <- arraySize 0 <$> lookupType flag_F
            flag_D0_t <- lookupType flag_D0
            flag_D <-
              letExp "flatmap_flag_D" . BasicOp $
                Reshape flag_D0 (reshapeAll (arrayShape flag_D0_t) (Shape [m_flag]))
            let flag_rep = Irregular $ IrregularRep s_flag flag_F flag_O flag_D Dense

            -- The data arrays (results 4..). The inner-map machinery already
            -- produced, for each, an 'IrregularRep' with primitive
            -- (scalar-unit) data and matching scalar-unit structure arrays.
            -- These are exactly the reps for the source @[m]b@ data arrays, so
            -- we pass them through unchanged; any consumer reconstructs the
            -- @b@-element rows from the primitive rep via the ordinary
            -- irregular-input machinery. See Note [FlatMap element counting].
            let all_reps = n_rep : shape_rep : flag_rep : offset_rep : data_reps
            insertRepsM (zip (map distResTag res) all_reps) env
      _ -> error "flattenFlatMapNested: FlatMap with no irregular results"
  where
    -- The metadata is derived from a nonuniform result; they all have the same
    -- segment structure, so any of them will do.
    firstIrregular =
      fmap snd
        . find (flatMapNonuniform . fst)
        . zip (drop 1 (lambdaReturnType lam))
    -- This lambda is deliberately ill-formed; see Note [Ill-formed inner-map
    -- lambda].
    map_lam =
      Lambda
        { lambdaParams = lambdaParams lam,
          lambdaReturnType = Prim int64 : val_ts,
          lambdaBody = lambdaBody lam
        }
    -- Only works because we allow just a single Ext in the return type.
    val_ts =
      runIdentity . instantiateShapes (const (pure k)) . drop 1 $
        lambdaReturnType lam
      where
        -- The size of the lambda's irregular results, which it returns before
        -- them, and which is what 'Ext 0' stands for.
        k = case bodyResult (lambdaBody lam) of
          r : _ -> resSubExp r
          [] -> error "flattenFlatMapNested: FlatMap with no results"

-- | Remove certificates that refer to variables free in the lambda (recursing
-- into nested bodies). Such certificates arise on pure operator lambdas (e.g.
-- the combining function of a 'hist' or reduce) through conservative
-- certificate propagation. They are redundant, and cannot be preserved when the
-- operator is lifted into a segmented operation, as the certificate's binding
-- does not survive into the generated kernel.
stripFreeCerts :: Lambda SOACS -> Lambda SOACS
stripFreeCerts lam = lam {lambdaBody = onBody (lambdaBody lam)}
  where
    frees = freeIn lam
    onBody body = body {bodyStms = onStm <$> bodyStms body}
    onStm (Let pat dec e) = Let pat (onDec dec) (onExp e)
    onDec dec =
      dec {stmAuxCerts = Certs $ filter (`notNameIn` frees) $ unCerts $ stmAuxCerts dec}
    onExp = runIdentity . mapExpM mapper
    mapper = identityMapper {mapOnBody = const (pure . onBody)}

flattenHist ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  (Pat Type, StmAux ()) ->
  (SubExp, [VName], [Futhark.IR.SOACS.HistOp SOACS], Lambda SOACS) ->
  FlattenM DistEnv
flattenHist ops segments env inps res (_pat, aux) (w, hist_inputs, hist_ops0, bucket_fun) = do
  -- The operator is a pure combining function; any certificates on its
  -- statements (from conservative propagation) are redundant and cannot be
  -- preserved when it is lifted into a segmented operation, so drop the ones
  -- referring to variables free in the operator.
  let hist_ops = do
        op <- hist_ops0
        pure $
          op
            { Futhark.IR.SOACS.histOp = stripFreeCerts $ Futhark.IR.SOACS.histOp op
            }
  -- TODO: check for suitableUniformOperator.
  let nonuniform =
        not (all (isRegularInputArr env inps) hist_inputs)
          || isVariant inps w
          || not (all isRegularDistResult res)
  if nonuniform
    then do
      gpu_scope <- askScope
      let scope = castScope $ scopeOfDistInputs inps <> gpu_scope
      (hist_res, stms) <-
        runBuilderT
          ( auxing aux $
              doHist "nonuniform_hist" hist_ops hist_inputs $ \params ->
                map resSubExp <$> eLambda bucket_fun (map eParam params)
          )
          scope
      let body = mkBody stms $ varsRes $ concat hist_res
      reps <- distributeAndFlattenBody ops segments "non_uniform_hist_body" env inps res body
      insertRepsM (zip (map distResTag res) reps) env
    else do
      let new_segment = segments <> pure w
      lifted_inps <- forM hist_inputs $ \hist_inp -> do
        t <- lookupInputType inps hist_inp
        let expectedShape = segmentsShape segments <> arrayShape t
        liftSubExpRegular lvl segments inps env expectedShape (Var hist_inp)
      hist_ops' <- forM hist_ops $ \(Futhark.IR.SOACS.HistOp num_bins rf dests nes op) -> do
        nes' <- mapM (readNeutral segments env inps) nes
        let rr (DistType _ _ t) = t
        let ts = map (rr . distResType) res
        let expectedShapes = map (\t -> segmentsShape segments <> arrayShape t) ts
        dests' <- mapM (\(shape, var) -> liftSubExpRegular lvl segments inps env shape (Var var)) (zip expectedShapes dests)
        pure $ Futhark.IR.SOACS.HistOp num_bins rf dests' nes' op
      let free = freeIn bucket_fun
      let isDest = flip elem $ concatMap Futhark.IR.SOACS.histDest hist_ops'
          free_notDest = filter (not . isDest) (namesToList free)
      free_and_sizes <- freeWithTypeDeps inps (namesFromList free_notDest)
      (free_replicated, replicated) <-
        fmap unzip . sequence $
          mapMaybe
            (onMapFreeVarMultiDim lvl segments w env inps)
            free_and_sizes
      (free_env, free_inputs) <- mapArraysToInputs free_replicated replicated
      let readFree is = readInputs new_segment free_env is free_inputs
      hist_res <-
        certifying (distCerts inps aux env) $
          genUniformSegHist lvl "Uniform_segHist" new_segment hist_ops' (soacsLambdaToGPU bucket_fun) lifted_inps readFree
      pure $ insertRegulars (map distResTag res) hist_res env
  where
    lvl = flattenSegLevel ops
