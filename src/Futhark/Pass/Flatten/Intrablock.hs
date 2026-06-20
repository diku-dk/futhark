{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.Flatten.Intrablock
  ( IntrablockResult (..),
    intrablockParallelise,
    intrablockParalleliseTopLevelMap,
    intraMinInnerPar,
  )
where

import Control.Monad
import Control.Monad.RWS
import Control.Monad.State.Strict (runState)
import Control.Monad.Trans.Maybe
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.GPU hiding (HistOp)
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.ToGPU
import Futhark.Pass.Flatten.Builtins (genUniformSegHist, genUniformSegRed, genUniformSegScanomapWithPost, mkSegSpace)
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Monad
import Futhark.Pass.Flatten.PreProcess (preprocessLambda)
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Futhark.Util.Log
import Prelude hiding (log)

-- | The minimum amount of inner parallelism we require (by default)
-- in intra-group versions.
intraMinInnerPar :: Int64
intraMinInnerPar = 32

data IntrablockResult = IntrablockResult
  { intraMinPar :: SubExp,
    intraAvailPar :: SubExp,
    intraThreadBlockSize :: SubExp,
    intraLog :: Log,
    intraPreludeStms :: Stms GPU,
    intraKernelStms :: Stms GPU,
    intraResultNames :: [VName]
  }

type InBlockMapTransformer =
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Builder GPU (Stms GPU)

intrablockParallelise ::
  InBlockMapTransformer ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM (Maybe IntrablockResult)
intrablockParallelise map_in_block segments env inps dist_res _pat aux w arrs lam0 = runMaybeT $ do
  -- TODO : This should not be necessary.
  unless (all (regularMapInput env inps) arrs) mzero
  gpu_scope <- lift askScope
  let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
  lam <- renameLambda =<< preprocessLambda pp_scope lam0

  let result_ts =
        [ t `arrayOfShape` segmentsShape segments
        | DistResult _ (DistType _ _ t) _ <- dist_res
        ]

  ((arrs', nested_pat), input_prelude_stms) <- lift . collectStms $ do
    arrs' <-
      zipWithM
        (prepareRegularMapInput segments env inps)
        (lambdaParams lam)
        arrs
    nested_pat <-
      Pat
        <$> zipWithM
          (\res t -> PatElem <$> newName (distResName res) <*> pure t)
          dist_res
          result_ts
    pure (arrs', nested_pat)

  let new_segments = segments <> pure w
      body = lambdaBody lam
      (param_env, param_inputs) =
        mapArraysToInputs (lambdaParams lam) arrs'

  free_inputs <- lift $ freeInputsFor inps lam
  (wss_min, wss_avail, log, kbody) <-
    MaybeT . localScope (scopeOfDistInputs free_inputs <> scopeOfLParams (lambdaParams lam)) $
      intrablockParalleliseBody map_in_block body

  outside_scope <- lift askScope
  -- TODO: Double check this
  unless (allNames (`M.member` outside_scope) $ freeIn (wss_min ++ wss_avail)) mzero

  ((intra_avail_par, tblock_size, kspace, num_tblocks), prelude_stms) <-
    lift . collectStms $ do
      let foldBinOp' _ [] = eSubExp $ intConst Int64 1
          foldBinOp' bop (x : xs) = foldBinOp bop x xs

      num_tblocks <-
        letSubExp "intra_num_tblocks"
          =<< foldBinOp' (Mul Int64 OverflowUndef) (NE.toList new_segments)

      ws_min <-
        mapM (letSubExp "one_intra_par_min" <=< foldBinOp' (Mul Int64 OverflowUndef)) $
          filter (not . null) wss_min
      ws_avail <-
        mapM (letSubExp "one_intra_par_avail" <=< foldBinOp' (Mul Int64 OverflowUndef)) $
          filter (not . null) wss_avail

      -- The amount of parallelism available *in the worst case* is
      -- equal to the smallest parallel loop, or *at least* 1.
      intra_avail_par <-
        letSubExp "intra_avail_par" =<< foldBinOp' (SMin Int64) ws_avail

      tblock_size <- newVName "computed_tblock_size"
      -- The group size is either the maximum of the minimum parallelism
      -- exploited, or the desired parallelism (bounded by the max group
      -- size) in case there is no minimum.
      letBindNames [tblock_size]
        =<< if null ws_min
          then
            eBinOp
              (SMin Int64)
              (eSubExp =<< letSubExp "max_tblock_size" (Op $ SizeOp $ GetSizeMax SizeThreadBlock))
              (eSubExp intra_avail_par)
          else foldBinOp' (SMax Int64) ws_min

      gtids <- mapM (const $ newVName "gtid") $ NE.toList new_segments
      kspace <- mkSegSpace $ zip gtids $ NE.toList new_segments

      pure (intra_avail_par, Var tblock_size, kspace, num_tblocks)

  read_input_stms <-
    lift . collectStms_ . localScope (scopeOfSegSpace kspace <> scopeOf input_prelude_stms <> scopeOf prelude_stms) $ do
      let SegSpace _ gtids_and_dims = kspace
          full_is = map (Var . fst) gtids_and_dims
          outer_is = take (segmentsRank segments) full_is
      readInBlockInputs segments env outer_is free_inputs
      readInBlockInputs new_segments param_env full_is param_inputs

  let kbody' = kbody {bodyStms = read_input_stms <> bodyStms kbody}
      rts = map (length (NE.toList new_segments) `stripArray`) result_ts
      grid = KernelGrid (Count num_tblocks) (Count tblock_size)
      lvl = SegBlock SegNoVirt (Just grid)
      kstm = Let nested_pat aux $ Op $ SegOp $ SegMap lvl kspace rts kbody'

  pure $
    IntrablockResult
      { intraMinPar = intra_avail_par,
        intraAvailPar = intra_avail_par,
        intraThreadBlockSize = tblock_size,
        intraLog = log,
        intraPreludeStms = input_prelude_stms <> prelude_stms,
        intraKernelStms = oneStm kstm,
        intraResultNames = patNames nested_pat
      }

intrablockParalleliseTopLevelMap ::
  InBlockMapTransformer ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM (Maybe IntrablockResult)
intrablockParalleliseTopLevelMap map_in_block pat aux w arrs lam0 = runMaybeT $ do
  scope <- lift $ castScope <$> askScope
  lam <- renameLambda =<< preprocessLambda scope lam0
  let result_ts = patTypes pat
      body = lambdaBody lam

  nested_pat <-
    lift $
      Pat
        <$> mapM
          (\pe -> PatElem <$> newName (patElemName pe) <*> pure (patElemType pe))
          (patElems pat)

  (wss_min, wss_avail, log, kbody) <-
    MaybeT . localScope (scopeOfLParams $ lambdaParams lam) $
      intrablockParalleliseBody map_in_block body

  outside_scope <- lift askScope
  unless (allNames (`M.member` outside_scope) $ freeIn (wss_min ++ wss_avail)) mzero

  ((intra_avail_par, tblock_size, kspace, num_tblocks), prelude_stms) <-
    lift . collectStms $ do
      let foldBinOp' _ [] = eSubExp $ intConst Int64 1
          foldBinOp' bop (x : xs) = foldBinOp bop x xs

      ws_min <-
        mapM (letSubExp "one_intra_par_min" <=< foldBinOp' (Mul Int64 OverflowUndef)) $
          filter (not . null) wss_min
      ws_avail <-
        mapM (letSubExp "one_intra_par_avail" <=< foldBinOp' (Mul Int64 OverflowUndef)) $
          filter (not . null) wss_avail

      intra_avail_par <-
        letSubExp "intra_avail_par" =<< foldBinOp' (SMin Int64) ws_avail

      tblock_size <- newVName "computed_tblock_size"
      letBindNames [tblock_size]
        =<< if null ws_min
          then
            eBinOp
              (SMin Int64)
              (eSubExp =<< letSubExp "max_tblock_size" (Op $ SizeOp $ GetSizeMax SizeThreadBlock))
              (eSubExp intra_avail_par)
          else foldBinOp' (SMax Int64) ws_min

      gtid <- newVName "gtid"
      kspace <- mkSegSpace [(gtid, w)]
      pure (intra_avail_par, Var tblock_size, kspace, w)

  read_input_stms <- lift . collectStms_ . localScope (scopeOfSegSpace kspace) $ do
    let SegSpace _ gtids_and_dims = kspace
        [gtid] = map (Var . fst) gtids_and_dims
    forM_ (zip (lambdaParams lam) arrs) $ \(p, arr) ->
      case paramType p of
        Acc {} ->
          letBindNames [paramName p] =<< eSubExp (Var arr)
        t | arrayRank t > 0 -> do
          v <- letExp (baseName (paramName p) <> "_global") =<< eIndex arr [eSubExp gtid]
          letBindNames [paramName p] $
            BasicOp $
              Replicate mempty $
                Var v
        _ ->
          letBindNames [paramName p] =<< eIndex arr [eSubExp gtid]

  let kbody' = kbody {bodyStms = read_input_stms <> bodyStms kbody}
      rts = map (1 `stripArray`) result_ts
      grid = KernelGrid (Count num_tblocks) (Count tblock_size)
      lvl = SegBlock SegNoVirt (Just grid)
      kstm = Let nested_pat aux $ Op $ SegOp $ SegMap lvl kspace rts kbody'

  pure $
    IntrablockResult
      { intraMinPar = intra_avail_par,
        intraAvailPar = intra_avail_par,
        intraThreadBlockSize = tblock_size,
        intraLog = log,
        intraPreludeStms = prelude_stms,
        intraKernelStms = oneStm kstm,
        intraResultNames = patNames nested_pat
      }

readInBlockInputs :: Segments -> DistEnv -> [SubExp] -> DistInputs -> FlattenM ()
readInBlockInputs segments env is inputs =
  mapM_ onInput inputs
  where
    onInput (v, inp) = do
      v' <- readInputVar segments env is inputs v
      let t = distInputType inp
      if isAcc t
        then
          letBindNames [v] $ BasicOp $ SubExp $ Var v'
        else
          if arrayRank t > 0
            then
              letBindNames [v] $ BasicOp $ Replicate mempty $ Var v'
            else
              letBindNames [v] $ BasicOp $ SubExp $ Var v'

regularMapInput :: DistEnv -> DistInputs -> VName -> Bool
regularMapInput env inps arr =
  case lookup arr inps of
    Just DistInputFree {} ->
      True
    Just (DistInput rt _) ->
      case resVar rt env of
        Regular {} -> True
        Irregular {} -> False
    Nothing ->
      True

prepareRegularMapInput ::
  Segments ->
  DistEnv ->
  DistInputs ->
  Param Type ->
  VName ->
  FlattenM (MapArray ())
prepareRegularMapInput segments env inps p arr =
  case lookup arr inps of
    Just (DistInputFree vs t) ->
      pure $ MapArray vs t
    Just (DistInput rt t) ->
      case resVar rt env of
        Regular vs -> do
          vs_t <- lookupType vs
          if isAcc vs_t
            then pure $ MapArray vs t
            else do
              let expected_shape = segmentsShape segments <> arrayShape t
              v <-
                if arrayShape vs_t == expected_shape
                  then pure vs
                  else
                    letExp (baseName arr <> "_intra_reg_reshape") . BasicOp $
                      Reshape vs $
                        reshapeAll (arrayShape vs_t) expected_shape
              pure $ MapArray v t
        Irregular {} ->
          error "prepareRegularMapInput: unexpected irregular input"
    Nothing -> do
      arr_rep <-
        letExp (baseName arr <> "_intra_rep") . BasicOp $
          Replicate (segmentsShape segments) (Var arr)
      pure $ MapArray arr_rep $ paramType p

mapArraysToInputs ::
  [Param Type] ->
  [MapArray ()] ->
  (DistEnv, DistInputs)
mapArraysToInputs params arrs =
  (mempty, zipWith onInput params arrs)
  where
    onInput p (MapArray arr _) =
      (paramName p, DistInputFree arr (paramType p))
    onInput _ MapOther {} =
      error "mapArraysToInputs: unexpected irregular input"

freeInputsFor :: DistInputs -> Lambda SOACS -> FlattenM DistInputs
freeInputsFor inps lam =
  do
    let free = freeIn lam
    free_sizes <-
      foldMap freeIn <$> mapM (lookupInputType inps) (namesToList free)
    pure
      [ (v, inp)
      | v <- namesToList $ free <> free_sizes,
        Just inp <- [lookup v inps]
      ]

liftBuilderStms :: Builder GPU (Stms GPU) -> IntrablockM (Stms GPU)
liftBuilderStms m = do
  scope <- askScope
  src <- getNameSource
  let ((x, stms), src') = runState (runBuilderT m scope) src
  putNameSource src'
  pure $ stms <> x

recordInBlockParallelism :: Stms GPU -> IntrablockM ()
recordInBlockParallelism =
  mapM_ recordStm . stmsToList
  where
    recordStm (Let _ _ (Op (SegOp op))) =
      parallelMin $ segSpaceDims $ segSpace op
    recordStm _ =
      pure ()

data IntraAcc = IntraAcc
  { accMinPar :: S.Set [SubExp],
    accAvailPar :: S.Set [SubExp],
    accLog :: Log
  }

instance Semigroup IntraAcc where
  IntraAcc min_x avail_x log_x <> IntraAcc min_y avail_y log_y =
    IntraAcc (min_x <> min_y) (avail_x <> avail_y) (log_x <> log_y)

instance Monoid IntraAcc where
  mempty = IntraAcc mempty mempty mempty

type IntrablockM =
  BuilderT GPU (MaybeT (RWS () IntraAcc VNameSource))

instance MonadLogger IntrablockM where
  addLog log = lift $ lift $ tell mempty {accLog = log}

runIntrablockM ::
  (MonadFreshNames m, HasScope GPU m) =>
  IntrablockM () ->
  m (Maybe (IntraAcc, Stms GPU))
runIntrablockM m = do
  scope <- castScope <$> askScope
  modifyNameSource $ \src ->
    let (res, src', acc) = runRWS (runMaybeT (runBuilderT m scope)) () src
     in (fmap (\((), kstms) -> (acc, kstms)) res, src')

parallelMin :: [SubExp] -> IntrablockM ()
parallelMin ws =
  tell
    mempty
      { accMinPar = S.singleton ws,
        accAvailPar = S.singleton ws
      }

intrablockBody :: InBlockMapTransformer -> Body SOACS -> IntrablockM (Body GPU)
intrablockBody map_in_block body = do
  stms <- collectStms_ $ intrablockStms map_in_block $ bodyStms body
  pure $ mkBody stms $ bodyResult body

intrablockLambda :: InBlockMapTransformer -> Lambda SOACS -> IntrablockM (Lambda GPU)
intrablockLambda map_in_block lam =
  mkLambda (lambdaParams lam) $
    bodyBind =<< intrablockBody map_in_block (lambdaBody lam)

intrablockWithAccInput :: InBlockMapTransformer -> WithAccInput SOACS -> IntrablockM (WithAccInput GPU)
intrablockWithAccInput _ (shape, arrs, Nothing) =
  pure (shape, arrs, Nothing)
intrablockWithAccInput map_in_block (shape, arrs, Just (lam, nes)) = do
  lam' <- intrablockLambda map_in_block lam
  pure (shape, arrs, Just (lam', nes))

intrablockStm :: InBlockMapTransformer -> Stm SOACS -> IntrablockM ()
intrablockStm map_in_block stm@(Let pat aux e) = do
  scope <- askScope
  let lvl = SegThreadInBlock SegNoVirt

  case e of
    Loop merge form loopbody ->
      localScope (scopeOfLoopForm form <> scopeOfFParams (map fst merge)) $ do
        loopbody' <- intrablockBody map_in_block loopbody
        certifying (stmAuxCerts aux) . letBind pat $
          Loop merge form loopbody'
    Match cond cases defbody ifdec -> do
      cases' <- mapM (traverse $ intrablockBody map_in_block) cases
      defbody' <- intrablockBody map_in_block defbody
      certifying (stmAuxCerts aux) . letBind pat $
        Match cond cases' defbody' ifdec
    WithAcc inputs lam -> do
      inputs' <- mapM (intrablockWithAccInput map_in_block) inputs
      lam' <- intrablockLambda map_in_block lam
      certifying (stmAuxCerts aux) . letBind pat $ WithAcc inputs' lam'
    Op soac
      | "sequential_outer" `inAttrs` stmAuxAttrs aux ->
          intrablockStms map_in_block . fmap (certify (stmAuxCerts aux))
            =<< runBuilder_ (FOT.transformSOAC pat soac)
    Op (Screma w arrs form)
      | Just lam <- isMapSOAC form -> do
          stms <- liftBuilderStms (map_in_block pat w arrs lam)
          recordInBlockParallelism stms
          addStms stms
    Op (Screma w arrs form)
      | Just (post_lam, scans, mapfun) <- isMaposcanomapSOAC form,
        -- FIXME: Futhark.CodeGen.ImpGen.GPU.Block.compileGroupOp
        -- cannot handle multiple scan operators yet.
        Scan scanfun nes <- singleScan scans -> do
          let scanfun' = soacsLambdaToGPU scanfun
              mapfun' = soacsLambdaToGPU mapfun
              post_op = soacsLambdaToGPU post_lam
          (scan_res, stms) <- runBuilder (genUniformSegScanomapWithPost lvl (pure w) "intra_maposcanomap" scanfun' mempty nes post_op mapfun' arrs (const $ pure ()))
          certifying (stmAuxCerts aux) $ do
            addStms stms
            zipWithM_
              ( \pe v ->
                  letBindNames [patElemName pe] $
                    BasicOp $
                      SubExp $
                        Var v
              )
              (patElems pat)
              scan_res
          parallelMin [w]
    Op (Screma w arrs form)
      | Just (reds, map_lam) <- isRedomapSOAC form -> do
          let sing_red = singleReduce reds
              red_lam = redLambda sing_red
              nes = redNeutral sing_red
              comm
                | commutativeLambda red_lam = Commutative
                | otherwise = redComm sing_red
              sing_red_gpu = Reduce comm (soacsLambdaToGPU red_lam) nes
              map_lam' = soacsLambdaToGPU map_lam
          (red_res, stms) <- runBuilder (genUniformSegRed lvl "intra_redomap" (pure w) sing_red_gpu mempty map_lam' arrs (const $ pure ()))
          certifying (stmAuxCerts aux) $ do
            addStms stms
            zipWithM_
              ( \pe v ->
                  letBindNames [patElemName pe] $
                    BasicOp $
                      SubExp $
                        Var v
              )
              (patElems pat)
              red_res
          parallelMin [w]
    Op (Screma w arrs form) ->
      -- This screma is too complicated for us to immediately do
      -- anything, so split it up and try again.
      mapM_ (intrablockStm map_in_block) . fmap (certify (stmAuxCerts aux)) . snd
        =<< runBuilderT (dissectScrema pat w form arrs) (scopeForSOACs scope)
    Op (Hist w arrs ops bucket_fun) -> do
      let bucket_fun' = soacsLambdaToGPU bucket_fun

      (hist_res, stms) <- runBuilder (genUniformSegHist lvl "Uniform_segHist" (pure w) ops bucket_fun' arrs (const $ pure ()))
      certifying (stmAuxCerts aux) $ do
        addStms stms
        zipWithM_
          ( \pe v ->
              letBindNames [patElemName pe] $
                BasicOp $
                  SubExp $
                    Var v
          )
          (patElems pat)
          hist_res
      parallelMin [w]
    Op (Stream w arrs accs lam)
      | chunk_size_param : _ <- lambdaParams lam -> do
          types <- asksScope castScope
          ((), stream_stms) <-
            runBuilderT (sequentialStreamWholeArray pat w accs lam arrs) types
          let replace (Var v) | v == paramName chunk_size_param = w
              replace se = se
              replaceSets (IntraAcc x y log) =
                IntraAcc (S.map (map replace) x) (S.map (map replace) y) log
          censor replaceSets $ intrablockStms map_in_block stream_stms
    _ ->
      addStm $ soacsStmToGPU stm

intrablockStms :: InBlockMapTransformer -> Stms SOACS -> IntrablockM ()
intrablockStms map_in_block = mapM_ $ intrablockStm map_in_block

intrablockParalleliseBody ::
  (MonadFreshNames m, HasScope GPU m) =>
  InBlockMapTransformer ->
  Body SOACS ->
  m (Maybe ([[SubExp]], [[SubExp]], Log, KernelBody GPU))
intrablockParalleliseBody map_in_block body = do
  res <-
    runIntrablockM $ intrablockStms map_in_block $ bodyStms body
  pure $ do
    (IntraAcc min_ws avail_ws log, kstms) <- res
    pure
      ( S.toList min_ws,
        S.toList avail_ws,
        log,
        Body () kstms $ map ret $ bodyResult body
      )
  where
    ret (SubExpRes cs se) = Returns ResultMaySimplify cs se
