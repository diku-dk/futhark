{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.Flatten.Intrablock
  ( IntrablockResult (..),
    intrablockParallelise,
    intraMinInnerPar,
    InBlockMapTransformer,
  )
where

import Control.Monad
import Data.Bitraversable
import Data.Map qualified as M
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.GPU hiding (HistOp)
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.General
import Futhark.Pass.Flatten.PreProcess (preprocessLambda)
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.ToGPU
import Prelude hiding (log)

-- | The minimum amount of inner parallelism we require (by default)
-- in intra-group versions.
intraMinInnerPar :: Int64
intraMinInnerPar = 32

data IntrablockResult = IntrablockResult
  { intraAvailPar :: SubExp,
    intraPreludeStms :: Stms GPU,
    intraKernelStms :: Stms GPU,
    intraResultNames :: [VName]
  }

type InBlockMapTransformer =
  SegLevel ->
  Pat Type ->
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  FlattenM ()

foldBinOp' :: (MonadBuilder m) => BinOp -> [SubExp] -> m (Exp (Rep m))
foldBinOp' _ [] = eSubExp $ intConst Int64 1
foldBinOp' bop (x : xs) = foldBinOp bop x xs

computeAvailablePar :: [([SubExp], SegVirt)] -> FlattenM SubExp
computeAvailablePar wss = do
  let prod = letSubExp "one_intra_par" <=< foldBinOp' (Mul Int64 OverflowUndef)
  ws <- mapM (bitraverse prod pure) $ filter (not . null . fst) wss

  -- The amount of parallelism available *in the worst case* is equal to the
  -- smallest parallel loop.
  letSubExp "intra_avail_par" =<< foldBinOp' (SMin Int64) (map fst ws)

-- | Check whether this result is actually something that is acceptable to use:
-- the parallel dimensions must all be bound outside the kernel, as nonuniform
-- parallelism cannot be exploited inside a thread block.
noNonuniformPar :: Names -> FlattenM IntrablockResult -> FlattenM (Maybe IntrablockResult)
noNonuniformPar pars m = do
  outside_scope <- askScope
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

  -- Reconstruct the per-enclosing-segment inputs, so they can be read at the
  -- appropriate indices inside the kernel.
  (param_inputs, input_prelude_stms) <-
    collectStms $
      zipWithM (prepareRegularMapInput segments env inps) (lambdaParams lam) arrs

  -- A free variable of the map body is a per-enclosing-segment value: it is
  -- brought into the body's scope as a distributed input and reconstructed at
  -- the outer indices inside the kernel (via 'readInBlockInputs' below). With no
  -- enclosing segments, however, it is an ordinary top-level value already in
  -- the kernel's outer scope, so there is nothing to reconstruct - hence the
  -- empty 'free_inputs', which makes both the scope extension and the read below
  -- vanish.
  free_inputs <- if null segments then pure [] else freeInputsFor inps lam
  kbody <-
    localScope (scopeOfDistInputs free_inputs <> scopeOfLParams (lambdaParams lam)) $
      intrablockParalleliseBody map_in_block (lambdaBody lam)

  nested_pat <-
    fmap Pat $
      zipWith PatElem
        <$> mapM (newName . distResName) dist_res
        <*> pure result_ts

  let new_segments = segments <> pure w
      wss = segOpParallelism kbody

  noNonuniformPar (freeIn $ map fst wss) $ do
    ((intra_avail_par, kspace), prelude_stms) <-
      collectStms $ do
        intra_avail_par <- computeAvailablePar wss
        gtids <- mapM (const $ newVName "gtid") new_segments
        kspace <- mkSegSpace $ zip gtids new_segments
        pure (intra_avail_par, kspace)

    let scope =
          scopeOfSegSpace kspace
            <> scopeOf input_prelude_stms
            <> scopeOf prelude_stms
    read_input_stms <-
      collectStms_ . localScope scope $ do
        let SegSpace _ gtids_and_dims = kspace
            full_is = map (Var . fst) gtids_and_dims
            outer_is = take (segmentsRank segments) full_is
        readInBlockInputs segments env outer_is free_inputs
        readInBlockInputs new_segments mempty full_is param_inputs

    let kbody' = kbody {bodyStms = read_input_stms <> bodyStms kbody}
        rts = map (length new_segments `stripArray`) result_ts
        lvl = SegBlock SegVirt Nothing
        kstm = Let nested_pat aux $ Op $ SegOp $ SegMap lvl kspace rts kbody'

    pure $
      IntrablockResult
        { intraAvailPar = intra_avail_par,
          intraPreludeStms = input_prelude_stms <> prelude_stms,
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

bindPatToRes :: Pat Type -> [VName] -> FlattenM ()
bindPatToRes pat res = forM_ (zip (patNames pat) res) $ \(pe_v, v) ->
  letBindNames [pe_v] $ BasicOp $ SubExp $ Var v

intrablockStm :: InBlockMapTransformer -> Stm SOACS -> FlattenM ()
intrablockStm map_in_block stm@(Let pat aux e) = do
  scope <- askScope
  let lvl = SegThreadInBlock SegVirt

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
    Op (Screma w arrs form)
      | Just lam <- isMapSOAC form ->
          map_in_block lvl pat w arrs lam
    Op (Screma w arrs form)
      | Just (post_lam, scans, mapfun) <- isMaposcanomapSOAC form,
        -- FIXME: Futhark.CodeGen.ImpGen.GPU.Block.compileBlockScan
        -- cannot handle multiple scan operators yet.
        Scan scanfun nes <- singleScan scans -> do
          let scanfun' = soacsLambdaToGPU scanfun
              mapfun' = soacsLambdaToGPU mapfun
              post_op = soacsLambdaToGPU post_lam
          bindPatToRes pat <=< certifying (stmAuxCerts aux) $
            genUniformSegScanomapWithPost lvl (pure w) "intra_maposcanomap" scanfun' mempty nes post_op mapfun' arrs (const $ pure ())
    Op (Screma w arrs form)
      | Just (reds, map_lam) <- isRedomapSOAC form -> do
          let onRed red =
                let red_lam = redLambda red
                    comm
                      | commutativeLambda red_lam = Commutative
                      | otherwise = redComm red
                 in Reduce comm (soacsLambdaToGPU red_lam) (redNeutral red)
              reds_gpu = map onRed reds
              map_lam' = soacsLambdaToGPU map_lam
          bindPatToRes pat <=< certifying (stmAuxCerts aux) $
            genUniformSegRed lvl "intra_redomap" (pure w) reds_gpu mempty map_lam' arrs (const $ pure ())
    Op (Screma w arrs form) ->
      -- This screma is too complicated for us to immediately do
      -- anything, so split it up and try again.
      mapM_ (intrablockStm map_in_block) . fmap (certify (stmAuxCerts aux)) . snd
        =<< runBuilderT (dissectScrema pat w form arrs) (scopeForSOACs scope)
    Op (Hist w arrs ops bucket_fun) -> do
      let bucket_fun' = soacsLambdaToGPU bucket_fun
      bindPatToRes pat <=< certifying (stmAuxCerts aux) $
        genUniformSegHist lvl "Uniform_segHist" (pure w) ops bucket_fun' arrs (const $ pure ())
    Op (Stream w arrs accs lam) -> do
      types <- asksScope castScope
      intrablockStms map_in_block
        =<< runBuilderT_ (sequentialStreamWholeArray pat w accs lam arrs) types
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
