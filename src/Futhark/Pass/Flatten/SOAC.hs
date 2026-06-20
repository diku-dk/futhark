-- | Flattening rules for SOACs.
module Futhark.Pass.Flatten.SOAC
  ( transformScrema,
    transformHist,
    transformDistributed,
    transformMapForInBlock,

    -- * Building blocks
    distResultsToResReps,
    flattenIrregularRep,
  )
where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (isNothing, mapMaybe)
import Data.Set qualified as S
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.ToGPU (soacsLambdaToGPU)
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Incremental
import Futhark.Pass.Flatten.Intrablock qualified as Intrablock
import Futhark.Pass.Flatten.Monad
import Futhark.Pass.Flatten.PreProcess
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.IntegralExp
import Prelude hiding (div, quot, rem)

data InnerMapMode
  = MultiDim
  | SingleDim

-- Check whether a loop parameter array needs irregular representation.
-- we need the irregular representation when any of its dimensions are either:
-- a loop parameter name or variant in the outer map context
freeWithTypeDeps :: DistInputs -> Names -> FlattenM [VName]
freeWithTypeDeps inps free = do
  let free_names = namesToList free
  free_sizes <- foldMap freeIn <$> mapM (lookupInputType inps) free_names
  pure $ nubOrd $ namesToList free_sizes <> free_names

-- Reduction or scan operators may not have any free variables that are variant
-- to the nest (that is, are inputs to the distributed operation). This is
-- because we would be unable to express them as SegScan/SegReds. Fixing this
-- would require modifications to the SegOp representation, but it is likely not
-- worth it, as such operators are extremely rare - and we can just fall back on
suitableOperator :: DistEnv -> DistInputs -> Lambda SOACS -> [SubExp] -> Bool
suitableOperator env inps lam _nes =
  allNames notVariant (freeIn lam)
    && all primType (lambdaReturnType lam) -- TODO
  where
    notVariant v = isNothing $ M.lookup v $ inputReps inps env

suitableUniformOperator :: DistEnv -> DistInputs -> Lambda SOACS -> [SubExp] -> Bool
suitableUniformOperator env inps lam _nes =
  allNames notVariant (freeIn lam)
  where
    notVariant v = isNothing $ M.lookup v $ inputReps inps env

-- | Replicate an array to insert a new inner dimension  after the
-- existing segment dimensions.
replicateForW :: Segments -> SubExp -> VName -> FlattenM VName
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
          pure $ MapArray v t
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

mapArraysToInputs ::
  [Param Type] ->
  [MapArray IrregularRep] ->
  (DistEnv, DistInputs)
mapArraysToInputs params arrs =
  let ((_, env), inputs) =
        L.mapAccumL onInput (0, mempty) $ zip params arrs
   in (env, inputs)
  where
    onInput (tag, env) (p, MapArray arr _) =
      ((tag, env), (paramName p, DistInputFree arr (paramType p)))
    onInput (tag, env) (p, MapOther rep _) =
      let rt = ResTag tag
       in ( (tag + 1, insertRep rt (Irregular rep) env),
            (paramName p, DistInput rt (paramType p))
          )

mapArraysToInputs2 ::
  [VName] ->
  [MapArray IrregularRep] ->
  (DistEnv, DistInputs)
mapArraysToInputs2 param_names arrs =
  let ((_, env), inputs) =
        L.mapAccumL onInput (0, mempty) $ zip param_names arrs
   in (env, inputs)
  where
    onInput (tag, env) (p, MapArray arr t) =
      ((tag, env), (p, DistInputFree arr t))
    onInput (tag, env) (p, MapOther rep t) =
      let rt = ResTag tag
       in ( (tag + 1, insertRep rt (Irregular rep) env),
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
transformUniformRedomap lvl segments env inps w arrs reds map_lam = do
  let sing_red = singleReduce reds
      zeros = replicate (length segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
      free = freeIn map_lam
      new_segment = segments <> pure w
  nes <- mapM (readInput segments env zeros inps) (redNeutral sing_red)
  -- FIXME? I think a backend problem with vectorized reduce
  -- (red_lam, nes', shape) <- determineReduceOp (redLambda sing_red) nes
  let red_lam = redLambda sing_red
      nes' = nes
      shape = mempty
  let comm
        | commutativeLambda red_lam = Commutative
        | otherwise = redComm sing_red
      sing_red_gpu = Reduce comm (soacsLambdaToGPU red_lam) nes'
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

  let (free_env, free_inputs) = mapArraysToInputs2 free_replicated replicated
      readFree is = readInputs new_segment free_env is free_inputs
  genUniformSegRed lvl "uniformSegRed" (NE.toList new_segment) sing_red_gpu shape (soacsLambdaToGPU map_lam) arrs' readFree

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
  -- TODO: different segemnts fix
  let scan = singleScan scans
  let zeros = replicate (segmentsRank old_segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
  nes <- mapM (readInput old_segments env zeros inps) (scanNeutral scan)
  (scan_lam, nes', shape) <- determineReduceOp (scanLambda scan) nes
  genUniformSegScanomapWithPost
    lvl
    (NE.toList new_segment)
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
  let (free_env, free_inputs) = mapArraysToInputs2 free_replicated replicated
      readFree is = readInputs new_segment free_env is free_inputs
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
  let zeros = replicate (segmentsRank segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
  let nes = scanNeutral scan
  nes' <- mapM (readInput segments env zeros inps) nes
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
            rep : _ -> ensureDenseIrregular lvl "segop_desc" rep
            [] -> error "prepareSegOpInputs: impossible"

    normalise m rep v =
      case rep of
        Regular v' ->
          flattenRegularToRows segments m v'
        Irregular ir
          | irregularK ir == Dense ->
              pure $ irregularD ir
          | otherwise ->
              irregularD <$> ensureDenseIrregular lvl (baseName v <> "_dense") ir

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

regularBranchBody ::
  FlattenM [VName] ->
  FlattenM (Body GPU)
regularBranchBody m = do
  (vs, stms) <- collectStms m
  renameBody $ mkBody stms $ varsRes vs

regularRepVars :: [ResRep] -> [VName]
regularRepVars =
  map onRep
  where
    onRep (Regular v) = v
    onRep Irregular {} = error "regularRepVars: expected regular result"

transformFactoredDistBody ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Body SOACS ->
  FlattenM [ResRep]
transformFactoredDistBody ops segments env inps res body = do
  scope <- askScope
  let (inps_local, env_local, _) = localiseInputs env inps
      (inps_dist, dstms) = distributeBody (flattenFunHasParallelism ops) scope segments inps_local body
  lifted_res <- liftBodyWithDistResults ops segments inps_dist env_local dstms res (bodyResult body)
  lifted_vs <- mapM (letExp "factored_res" <=< toExp . resSubExp) lifted_res
  let reps = distResultsToResReps res lifted_vs
  pure reps

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
  outer_body <- regularBranchBody outer_only
  full_body <- regularBranchBody $ regularRepVars <$> transformFactoredDistBody ops segments env inps res factored_body

  let attrs = stmAuxAttrs aux
      fullAlternative = kernelAlternatives desc result_ts full_body []
      outerAlternative = kernelAlternatives desc result_ts outer_body []
      fullWithOuterAlternative = do
        (outer_suff, _) <-
          sufficientParallelism
            (desc <> "_suff_outer")
            (NE.toList $ segments <> pure w)
            mempty
            Nothing
        kernelAlternatives desc result_ts full_body [(outer_suff, outer_body)]
      alternatives
        | isParallelFunInside (flattenFunHasParallelism ops) factored_body =
            fullAlternative
        | "sequential_inner" `inAttrs` attrs =
            outerAlternative
        | mayExploitOuter attrs && allowVersioning (flattenSegLevel ops) =
            fullWithOuterAlternative
        | otherwise =
            fullAlternative
  match_res <-
    certifying (distCerts inps aux env) alternatives
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
          pure $ insertIrregular segs flags offsets (distResTag dist_res) v kind env

distResCerts :: DistEnv -> [DistInput] -> Certs
distResCerts env = Certs . map f
  where
    f (DistInputFree v _) = v
    f (DistInput rt _) = case resVar rt env of
      Regular v -> v
      Irregular r -> irregularD r

reshapeAndBind :: VName -> VName -> Shape -> FlattenM ()
reshapeAndBind v src shape = do
  v_copy <- letExp (baseName v) . BasicOp $ Replicate mempty (Var src)
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
        -- FIXME: the copies are because we have too liberal aliases on
        -- lifted functions.
        case resVar rt env of
          Regular v' -> letBindNames [v] $ BasicOp $ Replicate mempty $ Var v'
          Irregular irreg ->
            -- It might have an irregular representation, but we know
            -- that it is actually regular because it is a result.
            do
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
          Irregular irreg ->
            do
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
        -- TODO: What if we don't do this here? we can still just read from our replicated view
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
          (new_F, new_O, _new_elems) <- doSegIota lvl new_S
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
      pure $ MapOther rep' rep_t

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
        DistInputFree vs t -> pure $ MapArray vs t
        DistInput rt t -> case resVar rt env of
          Irregular rep -> do
            ws_prod <- arraySize 0 <$> lookupType ws_data
            onMapIrregularInputArr lvl MultiDim (old_segments <> pure w) ws ws_O ws_data p arr rep ws_prod
          Regular vs -> do
            vs_t <- lookupType vs
            if isAcc vs_t
              then pure $ MapArray vs t
              else do
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

transformMapForInBlock ::
  FlattenOps ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Builder GPU (Stms GPU)
transformMapForInBlock ops pat w arrs map_lam = do
  scope <- askScope
  lam <- preprocessLambda (castScope scope) map_lam
  runFlattenM scope $ collectStms_ $ do
    let arrs' =
          zipWith MapArray arrs $
            map paramType (lambdaParams lam)
        (distributed, _) =
          distributeMap (flattenFunHasParallelism ops) scope pat (NE.singleton w) arrs' lam
    transformDistributed ops' mempty (NE.singleton w) distributed
  where
    ops' = ops {flattenSegLevel = inBlockSegLevel}

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
    if any (isTypeVariant new_inp_var) (arrayShape v_t)
      then do
        irreg_dense <- ensureDenseIrregular lvl (baseName v <> "_map_result") irreg
        old_segment <- arraySize 0 <$> lookupType ws
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
      else case mode of
        MultiDim -> do
          reshapeAndBind v (irregularD irreg) (segmentsShape segments <> arrayShape v_t)
          mapResultRep lvl MultiDim (ws, ws_F, ws_O) v
        SingleDim -> do
          -- TODO: have to do this even it seems very annoying should think something better
          reshapeAndBind v (irregularD irreg) (segmentsShape segments <> arrayShape v_t)
          mapResultRep lvl SingleDim (ws, ws_F, ws_O) v
  where
    isTypeVariant vin se = case se of
      Var v' -> S.member v' vin
      _ -> False
    new_inp_var = S.fromList $ map fst new_inps

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
  let new_inps = concatMap distStmInputs dstms
  env <- foldM (flattenDistStm ops segments) env_initial dstms
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
  let free = freeIn map_lam
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
        distributeMap
          (flattenFunHasParallelism ops)
          scope
          pat
          new_segment
          (replicated <> arrs')
          map_lam'
  -- order the result representations in the same order as the pattern
  resRepsInPatOrder pat
    <$> transformDistributedInnerMap ops mode ws_triple arrmap new_segment distributed

transformInnerMapMultiDim ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM [ResRep]
transformInnerMapMultiDim ops segments env inps pat w arrs map_lam = do
  ws <- dataArr lvl segments env inps w
  (ws_F, ws_O, ws_data) <- doRepIota lvl ws
  arrs' <-
    zipWithM
      (onMapInputArrMultiDim lvl segments w env inps ws ws_O ws_data)
      (lambdaParams map_lam)
      arrs
  distributeAndTransformInnerMap
    ops
    MultiDim
    (ws_F, ws_O, ws)
    (segments <> pure w)
    inps
    pat
    arrs'
    (onMapFreeVarMultiDim lvl segments w env inps)
    map_lam
  where
    lvl = flattenSegLevel ops

transformInnerMapSingleDim ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM [ResRep]
transformInnerMapSingleDim ops segments env inps pat w arrs map_lam = do
  ws <- dataArr lvl segments env inps w
  (ws_F, ws_O, ws_data) <- doRepIota lvl ws
  new_segment <- arraySize 0 <$> lookupType ws_data
  arrs' <-
    zipWithM
      (onMapInputArr lvl segments env inps ws ws_O ws_data)
      (lambdaParams map_lam)
      arrs
  distributeAndTransformInnerMap
    ops
    SingleDim
    (ws_F, ws_O, ws)
    (NE.singleton new_segment)
    inps
    pat
    arrs'
    (onMapFreeVar lvl segments env inps ws (ws_F, ws_O, ws_data))
    map_lam
  where
    lvl = flattenSegLevel ops

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
  gpu_scope <- askScope
  let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
  lam <- preprocessLambda pp_scope map_lam
  if not (isVariant inps env w)
    then transformInnerMapMultiDim ops segments env inps pat w arrs lam
    else transformInnerMapSingleDim ops segments env inps pat w arrs lam

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
  let new_segments = segments <> pure w
      (param_env, param_inputs) =
        mapArraysToInputs (lambdaParams map_lam') arrs'
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

    addStms $ bodyStms $ lambdaBody map_lam'
    pure $ bodyResult $ lambdaBody map_lam'
  forM vs $ \v -> do
    letExp (baseName v <> "_copy") $
      BasicOp $
        Replicate mempty (Var v)

versionedRegularMap ::
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
versionedRegularMap ops segments env inps ress pat aux w arrs map_lam = do
  let only_intra = onlyExploitIntra (stmAuxAttrs aux)
      may_intra = worthIntrablock map_lam && mayExploitIntra (stmAuxAttrs aux)

  intra' <-
    if only_intra || may_intra
      then Intrablock.intrablockParallelise (transformMapForInBlock ops) segments env inps ress pat aux w arrs map_lam
      else pure Nothing

  let fullFlatten =
        regularRepVars <$> transformInnerMap (ops {flattenSegLevel = defaultSegLevel}) segments env inps pat w arrs map_lam

      outerOnly =
        runMapLambdaBody segments env inps w arrs map_lam pat ress

  full_body <- regularBranchBody fullFlatten
  outer_body <- regularBranchBody outerOnly

  let result_ts =
        [ t `arrayOfShape` segmentsShape segments
        | DistResult _ (DistType _ _ t) _ <- ress
        ]

  let alternatives = case intra' of
        _
          | "sequential_inner" `inAttrs` stmAuxAttrs aux ->
              kernelAlternatives "match_res" result_ts outer_body []
        Nothing
          | not only_intra,
            worthSequentialising (flattenFunHasParallelism ops) map_lam,
            mayExploitOuter $ stmAuxAttrs aux -> do
              (outer_suff, _) <- sufficientParallelism "suff_outer_par" (NE.toList $ segments <> pure w) mempty Nothing
              kernelAlternatives
                "match_res"
                result_ts
                full_body
                [(outer_suff, outer_body)]
          | otherwise ->
              kernelAlternatives "match_res" result_ts full_body []
        Just intra_res
          | only_intra -> do
              (_, intra_body) <- intraBlockAlternative intra_res
              kernelAlternatives "match_res" result_ts intra_body []
          | worthSequentialising (flattenFunHasParallelism ops) map_lam,
            mayExploitOuter $ stmAuxAttrs aux -> do
              (outer_suff, _) <- sufficientParallelism "suff_outer_par" (NE.toList $ segments <> pure w) mempty Nothing
              intra_alts <- intraBlockAlternative intra_res
              kernelAlternatives
                "match_res"
                result_ts
                full_body
                ((outer_suff, outer_body) : [intra_alts])
          | otherwise -> do
              intra_alts <- intraBlockAlternative intra_res
              kernelAlternatives "match_res" result_ts full_body [intra_alts]

  match_res <-
    certifying (distCerts inps aux env) alternatives

  pure $ insertRegulars (map distResTag ress) match_res env

transformScrema ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  (Pat Type, StmAux ()) ->
  (SubExp, [VName], ScremaForm SOACS) ->
  FlattenM DistEnv
transformScrema ops segments env inps res (pat, aux) (w, arrs, form)
  | Just (reds, map_lam) <- isRedomapSOAC form,
    not $ isVariant inps env w,
    all isRegularDistResult res,
    all (\red -> suitableUniformOperator env inps (redLambda red) (redNeutral red)) reds = do
      let outer_only = transformUniformRedomap (flattenSegLevel ops) segments env inps w arrs reds map_lam
      gpu_scope <- askScope
      let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
      factored <- factorScremaForParallelism funHasParallelism pp_scope (stmAuxCerts aux) pat w arrs form
      case factored of
        Just body ->
          versionScanRed ops "uniform_redomap_alt" segments env inps res aux w body outer_only
        Nothing -> do
          elems' <- outer_only
          pure $ insertRegulars (map distResTag res) elems' env
  | Just (reds, map_lam) <- isRedomapSOAC form,
    not $ lambdaHasParallelism funHasParallelism map_lam,
    all (\red -> suitableOperator env inps (redLambda red) (redNeutral red)) reds = do
      reps <- mapM (segOpInputRep lvl segments env inps) arrs
      let sing_red = singleReduce reds
          zeros = replicate (length segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
          hasNoFreeVariant = allNames (not . isVariant inps env . Var) (freeIn sing_red <> freeIn map_lam)
      (ws_F, ws_O, ws_S, elems, elems_kind) <-
        prepareSegOpInputs lvl segments env inps w reps arrs hasNoFreeVariant
      nes' <- mapM (readInput segments env zeros inps) (redNeutral sing_red)
      let sing_red' = sing_red {redNeutral = nes'}
      let free = freeIn map_lam
      free_and_sizes <- freeWithTypeDeps inps free
      ws <- dataArr lvl segments env inps w
      (_, _, ws_data) <- doRepIota lvl ws_S
      -- TODO: this will break in certain cases where the free variable is an irregular that needs to be replicated
      (free_replicated, replicated) <-
        fmap unzip . sequence $
          mapMaybe
            (onMapFreeVar lvl segments env inps ws (ws_F, ws_O, ws_data))
            free_and_sizes
      let (free_env, free_inputs) = mapArraysToInputs2 free_replicated replicated

      new_segment <- arraySize 0 <$> lookupType ws_F
      let readFree is = readInputs (NE.fromList [new_segment]) free_env is free_inputs
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
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    not $ isVariant inps env w,
    all isRegularDistResult res,
    all (\scan -> suitableUniformOperator env inps (scanLambda scan) (scanNeutral scan)) scans = do
      let outer_only =
            transformUniformMaposcanomap lvl segments env inps w arrs scans post_lam map_lam
      gpu_scope <- askScope
      let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
      factored <- factorScremaForParallelism funHasParallelism pp_scope (stmAuxCerts aux) pat w arrs form
      case factored of
        Just body ->
          versionScanRed ops "uniform_maposcanomap_alt" segments env inps res aux w body outer_only
        Nothing -> do
          elems' <- outer_only
          pure $ insertRegulars (map distResTag res) elems' env
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    not $ lambdaHasParallelism funHasParallelism map_lam,
    not $ lambdaHasParallelism funHasParallelism post_lam,
    all (\scan -> suitableOperator env inps (scanLambda scan) (scanNeutral scan)) scans = do
      reps <- mapM (segOpInputRep lvl segments env inps) arrs
      let hasNoFreeVariant = allNames (not . isVariant inps env . Var) (freeIn post_lam <> freeIn map_lam <> foldMap freeIn scans)
      (ws_F, ws_O, ws_S, elems, elems_kind) <-
        prepareSegOpInputs lvl segments env inps w reps arrs hasNoFreeVariant
      let free = freeIn map_lam <> freeIn post_lam
      free_and_sizes <- freeWithTypeDeps inps free
      ws <- dataArr lvl segments env inps w
      (_, _, ws_data) <- doRepIota lvl ws_S
      -- TODO: this will break in certain cases where the free variable is an irregular that needs to be replicated
      (free_replicated, replicated) <-
        fmap unzip . sequence $
          mapMaybe
            (onMapFreeVar lvl segments env inps ws (ws_F, ws_O, ws_data))
            free_and_sizes
      let (free_env, free_inputs) = mapArraysToInputs2 free_replicated replicated
      new_segment <- arraySize 0 <$> lookupType ws_F
      let readFree is = readInputs (NE.fromList [new_segment]) free_env is free_inputs
      elems' <- doSegMaposcanomap lvl scans ws_F elems post_lam map_lam segments inps env readFree
      insertSegOpMapResults
        segments
        ws_S
        ws_F
        ws_O
        elems_kind
        (zip res elems')
        env
  | Just map_lam <- isMapSOAC form,
    allowVersioning lvl,
    isVersionableMap funHasParallelism inps env w res map_lam =
      versionedRegularMap ops segments env inps res pat aux w arrs map_lam
  | Just map_lam <- isMapSOAC form = do
      map_res <-
        transformInnerMap ops segments env inps pat w arrs map_lam
      pure $ insertReps (zip (map distResTag res) map_res) env
  | otherwise = do
      gpu_scope <- askScope
      let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
      factored <- factorScremaForParallelism funHasParallelism pp_scope (stmAuxCerts aux) pat w arrs form
      case factored of
        Just body -> do
          reps <- transformFactoredDistBody ops segments env inps res body
          pure $ insertReps (zip (map distResTag res) reps) env
        Nothing ->
          -- XXX: here we silently sequentialise any SOAC that is not handled
          -- above if it is possible to do so. We need to make sure that we actually handle everything we
          -- care about!
          if all isRegularDistResult res
            then flattenScalarStm ops segments env inps res $ Let pat aux (Op (Screma w arrs form))
            else error "Unhandled SOAC"
  where
    funHasParallelism = flattenFunHasParallelism ops
    lvl = flattenSegLevel ops

transformHist ::
  FlattenOps ->
  NE.NonEmpty SubExp ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  (Pat Type, StmAux ()) ->
  (SubExp, [VName], [Futhark.IR.SOACS.HistOp SOACS], Lambda SOACS) ->
  FlattenM DistEnv
transformHist ops segments env inps res (_pat, aux) (w, hist_inputs, hist_ops, bucket_fun) = do
  -- todo: add this suitableUniformOperator
  nonuniform_inps <-
    any (any (isVariant inps env) . arrayDims)
      <$> mapM (lookupInputType inps) hist_inputs
  let nonuniform =
        nonuniform_inps
          || isVariant inps env w
          || not (all isRegularDistResult res)
  if nonuniform
    then error "TODO : transformDistStm: Unhandled nonuniform hist"
    else do
      let new_segment = segments <> pure w
      lifted_inps <- forM hist_inputs $ \hist_inp -> do
        t <- lookupInputType inps hist_inp
        let expectedShape = segmentsShape segments <> arrayShape t
        liftSubExpRegular lvl segments inps env expectedShape (Var hist_inp)
      let zeros = replicate (length segments) (Constant $ IntValue $ intValue Int64 (0 :: Int))
      hist_ops' <- forM hist_ops $ \(Futhark.IR.SOACS.HistOp num_bins rf dests nes op) -> do
        nes' <- mapM (readInput segments env zeros inps) nes
        let
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
      let (free_env, free_inputs) = mapArraysToInputs2 free_replicated replicated
          readFree is = readInputs new_segment free_env is free_inputs
      hist_res <-
        certifying (distCerts inps aux env) $
          genUniformSegHist lvl "Uniform_segHist" (NE.toList new_segment) hist_ops' (soacsLambdaToGPU bucket_fun) lifted_inps readFree
      pure $ insertRegulars (map distResTag res) hist_res env
  where
    lvl = flattenSegLevel ops
