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
import Control.Monad.Writer
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.GPU hiding (HistOp)
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.General
import Futhark.Pass.Flatten.PreProcess (preprocessLambda)
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Futhark.Transform.ToGPU
import Prelude hiding (log)

-- | The minimum amount of inner parallelism we require (by default)
-- in intra-group versions.
intraMinInnerPar :: Int64
intraMinInnerPar = 32

data IntrablockResult = IntrablockResult
  { intraMinPar :: SubExp,
    intraAvailPar :: SubExp,
    intraThreadBlockSize :: SubExp,
    intraPreludeStms :: Stms GPU,
    intraKernelStms :: Stms GPU,
    intraResultNames :: [VName]
  }

type InBlockMapTransformer =
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM ()

foldBinOp' :: (MonadBuilder m) => BinOp -> [SubExp] -> m (Exp (Rep m))
foldBinOp' _ [] = eSubExp $ intConst Int64 1
foldBinOp' bop (x : xs) = foldBinOp bop x xs

-- | Extract the intra-block parallelism used by the body.
findParallelism :: KernelBody GPU -> [[SubExp]]
findParallelism = S.toList . execWriter . onKernelBody
  where
    onKernelBody = mapM_ onStm . bodyStms
    onBody = mapM_ onStm . bodyStms
    onStm stm = walkExpM walker $ stmExp stm
    walker =
      (identityWalker @GPU)
        { walkOnBody = const onBody,
          walkOnOp = onOp
        }
    onOp (SegOp op) = do
      tell $ S.singleton $ segSpaceDims $ segSpace op
      onKernelBody $ segBody op
    onOp _ = pure ()

computeThreadBlockSize :: [[SubExp]] -> [[SubExp]] -> FlattenM (SubExp, SubExp)
computeThreadBlockSize wss_min wss_avail = do
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
  pure (intra_avail_par, Var tblock_size)

-- | Check whether this result is actually something that is acceptable to use.
noIrregularPar :: Names -> FlattenM IntrablockResult -> FlattenM (Maybe IntrablockResult)
noIrregularPar pars m = do
  outside_scope <- askScope
  -- Reject Irregular parallelism
  if allNames (`M.member` outside_scope) pars
    then Just <$> m
    else pure Nothing

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
intrablockParallelise map_in_block segments env inps dist_res _pat aux w arrs lam0 = do
  gpu_scope <- askScope
  let pp_scope = castScope $ scopeOfDistInputs inps <> gpu_scope
  lam <- renameLambda =<< preprocessLambda pp_scope lam0

  let result_ts =
        [ t `arrayOfShape` segmentsShape segments
        | DistResult _ (DistType _ _ t) _ <- dist_res
        ]

  ((param_inputs, nested_pat), input_prelude_stms) <- collectStms $ do
    param_inputs <-
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
    pure (param_inputs, nested_pat)

  let new_segments = segments <> pure w
      body = lambdaBody lam

  free_inputs <- freeInputsFor inps lam
  kbody <-
    localScope (scopeOfDistInputs free_inputs <> scopeOfLParams (lambdaParams lam)) $
      intrablockParalleliseBody map_in_block body

  let wss = findParallelism kbody

  noIrregularPar (freeIn wss) $ do
    let new_segments_list = NE.toList new_segments
    ((intra_avail_par, tblock_size, kspace, num_tblocks), prelude_stms) <-
      collectStms $ do
        num_tblocks <-
          letSubExp "intra_num_tblocks"
            =<< foldBinOp' (Mul Int64 OverflowUndef) new_segments_list
        (intra_avail_par, tblock_size) <- computeThreadBlockSize wss wss
        gtids <- mapM (const $ newVName "gtid") new_segments_list
        kspace <- mkSegSpace $ zip gtids new_segments_list
        pure (intra_avail_par, tblock_size, kspace, num_tblocks)

    read_input_stms <-
      collectStms_ . localScope (scopeOfSegSpace kspace <> scopeOf input_prelude_stms <> scopeOf prelude_stms) $ do
        let SegSpace _ gtids_and_dims = kspace
            full_is = map (Var . fst) gtids_and_dims
            outer_is = take (segmentsRank segments) full_is
        readInBlockInputs segments env outer_is free_inputs
        readInBlockInputs new_segments mempty full_is param_inputs

    let kbody' = kbody {bodyStms = read_input_stms <> bodyStms kbody}
        rts = map (length new_segments_list `stripArray`) result_ts
        grid = KernelGrid (Count num_tblocks) (Count tblock_size)
        lvl = SegBlock SegNoVirt (Just grid)
        kstm = Let nested_pat aux $ Op $ SegOp $ SegMap lvl kspace rts kbody'

    pure $
      IntrablockResult
        { intraMinPar = intra_avail_par,
          intraAvailPar = intra_avail_par,
          intraThreadBlockSize = tblock_size,
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
intrablockParalleliseTopLevelMap map_in_block pat aux w arrs lam0 = do
  scope <- castScope <$> askScope
  lam <- renameLambda =<< preprocessLambda scope lam0
  let result_ts = patTypes pat
      body = lambdaBody lam

  nested_pat <-
    Pat
      <$> mapM
        (\pe -> PatElem <$> newName (patElemName pe) <*> pure (patElemType pe))
        (patElems pat)

  kbody <-
    localScope (scopeOfLParams $ lambdaParams lam) $
      intrablockParalleliseBody map_in_block body

  let wss = findParallelism kbody

  noIrregularPar (freeIn wss) $ do
    ((intra_avail_par, tblock_size, kspace, num_tblocks), prelude_stms) <-
      collectStms $ do
        (intra_avail_par, tblock_size) <- computeThreadBlockSize wss wss
        gtid <- newVName "gtid"
        kspace <- mkSegSpace [(gtid, w)]
        pure (intra_avail_par, tblock_size, kspace, w)

    read_input_stms <- collectStms_ . localScope (scopeOfSegSpace kspace) $ do
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

prepareRegularMapInput ::
  Segments ->
  DistEnv ->
  DistInputs ->
  Param Type ->
  VName ->
  FlattenM (VName, DistInput)
prepareRegularMapInput segments env inps p arr = do
  t <- lookupInputType inps arr
  let expectedShape = segmentsShape segments <> arrayShape t
      lvl = SegThread SegVirt Nothing
  arr_rep <- liftVarRegular lvl segments inps env expectedShape arr
  pure (paramName p, DistInputFree arr_rep (paramType p))

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

intrablockBody :: InBlockMapTransformer -> Body SOACS -> FlattenM (Body GPU)
intrablockBody map_in_block body = do
  stms <- collectStms_ $ intrablockStms map_in_block $ bodyStms body
  pure $ mkBody stms $ bodyResult body

intrablockLambda :: InBlockMapTransformer -> Lambda SOACS -> FlattenM (Lambda GPU)
intrablockLambda map_in_block lam =
  mkLambda (lambdaParams lam) $
    bodyBind =<< intrablockBody map_in_block (lambdaBody lam)

intrablockWithAccInput :: InBlockMapTransformer -> WithAccInput SOACS -> FlattenM (WithAccInput GPU)
intrablockWithAccInput _ (shape, arrs, Nothing) =
  pure (shape, arrs, Nothing)
intrablockWithAccInput map_in_block (shape, arrs, Just (lam, nes)) = do
  lam' <- intrablockLambda map_in_block lam
  pure (shape, arrs, Just (lam', nes))

intrablockStm :: InBlockMapTransformer -> Stm SOACS -> FlattenM ()
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
      | Just lam <- isMapSOAC form ->
          map_in_block pat w arrs lam
    Op (Screma w arrs form)
      | Just (post_lam, scans, mapfun) <- isMaposcanomapSOAC form,
        -- FIXME: Futhark.CodeGen.ImpGen.GPU.Block.compileGroupOp
        -- cannot handle multiple scan operators yet.
        Scan scanfun nes <- singleScan scans -> do
          let scanfun' = soacsLambdaToGPU scanfun
              mapfun' = soacsLambdaToGPU mapfun
              post_op = soacsLambdaToGPU post_lam
          scan_res <- certifying (stmAuxCerts aux) $ genUniformSegScanomapWithPost lvl (pure w) "intra_maposcanomap" scanfun' mempty nes post_op mapfun' arrs (const $ pure ())
          zipWithM_
            ( \pe v ->
                letBindNames [patElemName pe] $ BasicOp $ SubExp $ Var v
            )
            (patElems pat)
            scan_res
    Op (Screma _ _ form)
      -- Leave nested redomaps in place as (implicitly sequential) SOACs rather
      -- than flattening them into an in-block SegRed whose size may be bound in
      -- the body (which would inhibit the enclosing intrablock kernel).
      | Just _ <- isRedomapSOAC form ->
          addStm $ soacsStmToGPU stm
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
    Op (Stream w arrs accs lam) -> do
      types <- asksScope castScope
      ((), stream_stms) <-
        runBuilderT (sequentialStreamWholeArray pat w accs lam arrs) types
      intrablockStms map_in_block stream_stms
    _ ->
      addStm $ soacsStmToGPU stm

intrablockStms :: InBlockMapTransformer -> Stms SOACS -> FlattenM ()
intrablockStms map_in_block = mapM_ $ intrablockStm map_in_block

intrablockParalleliseBody ::
  InBlockMapTransformer ->
  Body SOACS ->
  FlattenM (KernelBody GPU)
intrablockParalleliseBody map_in_block body = do
  kstms <- collectStms_ $ intrablockStms map_in_block $ bodyStms body
  pure $ Body () kstms $ map ret $ bodyResult body
  where
    ret (SubExpRes cs se) = Returns ResultMaySimplify cs se
