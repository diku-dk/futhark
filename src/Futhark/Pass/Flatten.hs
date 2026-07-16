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
import Data.Bifunctor (second)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Futhark.Analysis.Alias (analyseBody)
import Futhark.IR.Aliases (Aliases, bodyAliases)
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Pass.Flatten.BasicOp
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.General
import Futhark.Pass.Flatten.Incremental
import Futhark.Pass.Flatten.Intrablock qualified as Intrablock
import Futhark.Pass.Flatten.Loop
import Futhark.Pass.Flatten.Match
import Futhark.Pass.Flatten.PreProcess
import Futhark.Pass.Flatten.SOAC
import Futhark.Pass.Flatten.WithAcc
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Futhark.Transform.ToGPU (soacsLambdaToGPU, soacsStmToGPU)
import Futhark.Util (debugTraceM)
import Prelude hiding (div, quot, rem)

flattenOpsFor :: FunHasParallelism -> SegLevel -> FlattenOps
flattenOpsFor funHasParallelism lvl =
  FlattenOps
    { flattenSegLevel = lvl,
      flattenFunHasParallelism = funHasParallelism,
      flattenDistStmAtLevel = transformDistStm funHasParallelism,
      flattenScalarStm = transformScalarStm lvl,
      flattenTopLevelStm = transformStm funHasParallelism
    }

transformScalarStms ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Stms SOACS ->
  FlattenM DistEnv
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
  FlattenM DistEnv
transformScalarStm lvl segments env inps res stm =
  transformScalarStms lvl segments env inps res (oneStm stm)

topLevelversionScanRed ::
  FunHasParallelism ->
  Name ->
  Pat Type ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  StmAux dec ->
  Stms GPU ->
  FlattenM ()
topLevelversionScanRed funHasParallelism desc pat w arrs form aux outer_only_stms = do
  scope <- castScope <$> askScope
  let body_outerpar = mkBody outer_only_stms $ varsRes $ patNames pat
  maybe_body_flattened <-
    factorScremaForParallelism funHasParallelism scope (stmAuxCerts aux) pat w arrs form
  case maybe_body_flattened of
    Nothing -> addStms outer_only_stms
    Just body_flattened0 -> do
      outerOnlyBody <- renameBody body_outerpar
      body_flattened <- transformBody funHasParallelism =<< renameBody body_flattened0
      let result_ts = patTypes pat
          attrs = stmAuxAttrs aux
          fullAlternative = kernelAlternatives desc result_ts body_flattened []
          outerAlternative = kernelAlternatives desc result_ts outerOnlyBody []
          fullWithOuterAlternative = do
            (outer_suff, _) <-
              sufficientParallelism
                (desc <> "_suff_outer")
                [w]
                mempty
                Nothing
            kernelAlternatives desc result_ts body_flattened [(outer_suff, outerOnlyBody)]
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

transformDistStm :: FunHasParallelism -> SegLevel -> Segments -> DistEnv -> DistStm -> FlattenM DistEnv
transformDistStm _ lvl segments env (DistStm inps res (ScalarStm stms)) =
  transformScalarStms lvl segments env inps res stms
transformDistStm funHasParallelism lvl segments env (DistStm inps res (ParallelStm stm)) = do
  case stm of
    Let pat aux (BasicOp e) -> do
      let ~[res'] = res
          ~[pe] = patElems pat
      transformDistBasicOp ops segments env (inps, res', pe, aux, e)
    Let pat aux (Op (Screma w arrs form)) ->
      transformScrema (flattenOpsFor funHasParallelism lvl) segments env inps res (pat, aux) (w, arrs, form)
    Let _ aux (Match scrutinees cases defaultCase rt) ->
      if any (isVariant inps) scrutinees
        && any (bodyHasParallelism funHasParallelism) (defaultCase : map caseBody cases)
        then transformVariantMatch ops segments env inps res aux scrutinees cases defaultCase rt
        else transformUniformMatch ops segments env inps res aux scrutinees cases defaultCase rt
    Let pat aux (Apply name args rettype s) ->
      case lvl of
        SegThread {} -> do
          demandLifted name
          let name' = liftFunName name
          w <- letSubExp "num_segments" =<< toExp (segmentCount segments)
          args' <- ((w, Observe) :) . concat <$> mapM (liftArg lvl segments w inps env) args
          args_ts <- mapM (subExpType . fst) args'
          let dietToUnique Consume = Unique
              dietToUnique Observe = Nonunique
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
    Let pat aux (Loop merge (ForLoop i it n) body) ->
      transformLoop ops segments env inps res (pat, aux) (merge, ForLoop i it n, body)
    Let pat aux (Loop merge (WhileLoop cond) body) -> do
      transformLoop ops segments env inps res (pat, aux) (merge, WhileLoop cond, body)
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

liftArg :: SegLevel -> Segments -> SubExp -> DistInputs -> DistEnv -> (SubExp, Diet) -> FlattenM [(SubExp, Diet)]
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
    Irregular irreg -> do
      vs <- irregularRepToFlatArrs w irreg
      -- Only apply the original diet to the 'elems' array.
      pure $ zip (map Var vs) $ replicate 4 Observe ++ [d]

reshapeLiftedApplyResult :: Segments -> RetType SOACS -> ResRep -> FlattenM ResRep
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

-- Lifts a functions return type such that it matches the lifted functions
-- return type.
--
-- A lifted function corresponds to 'map f', which always produces fresh arrays.
-- We therefore mark all array components of the return type as 'Unique', such
-- that the results are known to not alias anything (in particular not the
-- arguments). Maintaining this invariant may require inserting copies in the
-- function body; see 'freshenResult'.
liftRetType :: SubExp -> [RetType SOACS] -> [RetType GPU]
liftRetType w = concat . snd . L.mapAccumL liftType 0
  where
    liftType i rettype =
      let lifted = case rettype of
            Prim pt -> pure $ arrayOf (Prim pt) (Shape [Free w]) Unique
            Array pt _ _ ->
              let num_data = Prim int64
                  segs = arrayOf (Prim int64) (Shape [Free w]) Unique
                  flags = arrayOf (Prim Bool) (Shape [Ext i]) Unique
                  offsets = arrayOf (Prim int64) (Shape [Free w]) Unique
                  elems = arrayOf (Prim pt) (Shape [Ext i]) Unique
               in [num_data, segs, flags, offsets, elems]
            Acc {} -> error "liftRetType: Acc"
            Mem {} -> error "liftRetType: Mem"
       in (i + length lifted, lifted)

runInnerSeqMap ::
  SubExp ->
  [VName] ->
  Lambda SOACS ->
  Pat Type ->
  [DistResult] ->
  FlattenM [VName]
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

liftBody :: FunHasParallelism -> SegLevel -> SubExp -> DistInputs -> DistEnv -> DistStms -> Result -> FlattenM Result
liftBody funHasParallelism lvl w inputs env dstms result = do
  let segments = NE.singleton w
  env' <- foldM (transformDistStm funHasParallelism lvl segments) env dstms
  result' <- mapM (liftResult lvl segments inputs env') result
  pure $ concat result'

liftFunName :: Name -> Name
liftFunName name = name <> "_lifted"

-- | A lifted function must return fresh, non-aliasing arrays (as it
-- corresponds to 'map f'; see 'liftRetType').  This is not
-- automatically the case: a result may alias a parameter (when a value
-- is passed straight through), or the same array may be returned in
-- multiple result positions (which happens for functions that return
-- the same value more than once).  For every such result we insert a
-- copy to re-establish the invariant.  Results that are already fresh
-- are left untouched, so no superfluous copies are inserted.
freshenResult :: [FParam GPU] -> Stms GPU -> Result -> FlattenM Result
freshenResult params stms result = do
  addStms stms
  let param_names = namesFromList $ map paramName params
      -- Transitive aliases of each result, including aliases with
      -- parameters and other results.
      als = bodyAliases (analyseBody mempty (Body () stms result) :: Body (Aliases GPU))
  reverse . snd <$> foldM freshen (param_names, []) (zip result als)
  where
    freshen (taken, acc) (SubExpRes cs (Var v), v_als) = do
      v_t <- lookupType v
      case v_t of
        Array {}
          | taken `namesIntersect` v_als -> do
              v' <- letExp "fresh_result" $ BasicOp $ Replicate mempty $ Var v
              pure (taken, SubExpRes cs (Var v') : acc)
        _ ->
          pure (taken <> v_als, SubExpRes cs (Var v) : acc)
    freshen (taken, acc) (res', _) =
      pure (taken, res' : acc)

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

liftFunDef ::
  FunHasParallelism ->
  Scope SOACS ->
  FunDef SOACS ->
  PassM (FunDef GPU, S.Set DemandFn)
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
        distributeBody DistributeIrregular funHasParallelism const_scope (NE.singleton (Var (paramName wp))) inputs body
      env = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
  -- Lift the body of the function and get the results, inserting copies as
  -- necessary to ensure the results are fresh and unique (see 'freshenResult').
  ((result, stms), needs) <-
    runFlattenM (castScope const_scope <> scopeOfFParams fparams'') $ do
      (result0, body_stms) <-
        collectStms . liftBody funHasParallelism defaultSegLevel w inputs' env dstms $
          bodyResult body
      collectStms $ freshenResult fparams'' body_stms result0
  let name = liftFunName $ funDefName fd
  pure
    ( fd
        { funDefName = name,
          funDefBody = Body () stms result,
          funDefParams = fparams'',
          funDefRetType = rettype'
        },
      needs
    )

transformLambda :: FunHasParallelism -> Lambda SOACS -> FlattenM (Lambda GPU)
transformLambda funHasParallelism (Lambda params ret body) = do
  body' <- localScope (scopeOfLParams params) $ transformBody funHasParallelism body
  pure $ Lambda params ret body'

transformStm :: FunHasParallelism -> Stm SOACS -> FlattenM ()
transformStm funHasParallelism (Let pat aux (Op soac))
  | "sequential_outer" `inAttrs` stmAuxAttrs aux = do
      scope <- askScope
      stms <- runBuilderT_ (FOT.transformSOAC pat soac) (castScope scope)
      transformStms funHasParallelism $ fmap (certify (stmAuxCerts aux)) stms
transformStm _ stm
  | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) = addStm $ soacsStmToGPU stm
transformStm _ (Let pat aux (Op (Hist w arrs ops bucket_fun))) =
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
transformStm funHasParallelism (Let pat aux (Op (Screma w arrs form)))
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    Scan scan_lam nes <- singleScan scans = do
      outer_only_stms <-
        collectStms_ . certifying (stmAuxCerts aux) $ do
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
      topLevelversionScanRed funHasParallelism "top_level_scan_alt" pat w arrs form aux outer_only_stms
transformStm funHasParallelism (Let pat aux (Op (Screma w arrs form)))
  | shouldDissectForm form =
      error "transformStm: complex Screma survived preprocessing"
  | Just (reds, map_lam) <- isRedomapSOAC form = do
      outer_only_stms <-
        collectStms_ . certifying (stmAuxCerts aux) $ do
          let sing_red = singleReduce reds
          (red_lam, nes', shape) <- determineReduceOp (redLambda sing_red) (redNeutral sing_red)
          let comm
                | commutativeLambda red_lam = Commutative
                | otherwise = redComm sing_red
          let sing_red_gpu = Reduce comm (soacsLambdaToGPU red_lam) nes'
          res <- genNonSegRed defaultSegLevel "topLevelSegRed" [w] sing_red_gpu shape (soacsLambdaToGPU map_lam) arrs
          forM_ (zip (patNames pat) res) $ \(v, v') ->
            letBindNames [v] $ BasicOp $ SubExp $ Var v'
      topLevelversionScanRed funHasParallelism "top_level_red_alt" pat w arrs form aux outer_only_stms
transformStm funHasParallelism (Let pat aux (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form = do
      let certs = stmAuxCerts aux
      scope <- castScope <$> askScope
      -- Propagate incremental flattening attributes to the
      -- statements of the map body, such that they can influence how
      -- those statements are versioned during distribution (e.g. the
      -- Screma resulting from map-loop interchange). This corresponds
      -- to the attributes applying to the entire nest.

      lam_toflatten <-
        fmap (propagateVersioningAttrs $ stmAuxAttrs aux) . renameLambda
          =<< preprocessLambda scope lam
      let arrs' =
            zipWith MapArray arrs $
              map paramType (lambdaParams (scremaLambda form))
          (distributed, _) = distributeMap DistributeIrregular funHasParallelism scope pat (NE.singleton w) arrs' lam_toflatten
          ops = flattenOpsFor funHasParallelism defaultSegLevel
      debugTraceM 1 $ prettyString distributed

      body_flattened <- renameBody <=< buildBody_ $ do
        certifying certs $ transformDistributed ops mempty (NE.singleton w) distributed
        pure $ varsRes $ patNames pat

      body_outerpar <-
        renameBody <=< buildBody_ . fmap varsRes . certifying certs $
          runInnerSeqMap w arrs lam pat []

      let only_intra = onlyExploitIntra (stmAuxAttrs aux)
          may_intra = worthIntrablock lam && mayExploitIntra (stmAuxAttrs aux)
          result_ts = patTypes pat
      intra' <-
        if only_intra || may_intra
          then
            Intrablock.intrablockParalleliseTopLevelMap
              (transformMapForInBlock ops)
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
              kernelAlternatives "top_level_map_alt" result_ts body_flattened []
          | "sequential_inner" `inAttrs` stmAuxAttrs aux ->
              kernelAlternatives "top_level_map_alt" result_ts body_outerpar []
        Nothing
          | not only_intra,
            worthSequentialising lam,
            mayExploitOuter (stmAuxAttrs aux) -> do
              (outer_suff, _) <- sufficientParallelism "suff_outer_map" [w] mempty Nothing
              kernelAlternatives
                "top_level_map_alt"
                result_ts
                body_flattened
                [(outer_suff, body_outerpar)]
          | otherwise ->
              kernelAlternatives "top_level_map_alt" result_ts body_flattened []
        Just intra_res
          | only_intra -> do
              (_, intra_body) <- intraBlockAlternative intra_res
              kernelAlternatives "top_level_map_alt" result_ts intra_body []
          | worthSequentialising lam,
            mayExploitOuter (stmAuxAttrs aux) -> do
              intra_alt <- intraBlockAlternative intra_res
              (outer_suff, _) <- sufficientParallelism "suff_outer_map" [w] mempty Nothing
              kernelAlternatives
                "top_level_map_alt"
                result_ts
                body_flattened
                [(outer_suff, body_outerpar), intra_alt]
          | otherwise -> do
              intra_alt <- intraBlockAlternative intra_res
              kernelAlternatives
                "top_level_map_alt"
                result_ts
                body_flattened
                [intra_alt]
      forM_ (zip (patNames pat) alt_vs) $ \(v, v_alt) ->
        letBindNames [v] $ BasicOp $ SubExp $ Var v_alt
transformStm funHasParallelism (Let pat aux (Loop params form body)) =
  localScope (scopeOfLoopForm form <> scopeOfFParams (map fst params)) $
    addStm . Let pat aux . Loop params form =<< transformBody funHasParallelism body
transformStm funHasParallelism (Let pat aux (Match ses cases def_body ret)) =
  addStm . Let pat aux
    =<< (Match ses <$> mapM onCase cases <*> transformBody funHasParallelism def_body <*> pure ret)
  where
    onCase = traverse (transformBody funHasParallelism)
transformStm funHasParallelism (Let pat aux (WithAcc inputs withacc_lam)) = do
  addStm . Let pat aux . WithAcc (map onInput inputs)
    =<< transformLambda funHasParallelism withacc_lam
  where
    onInput (shape, arrs, Nothing) =
      (shape, arrs, Nothing)
    onInput (shape, arrs, Just (lam, nes)) =
      (shape, arrs, Just (soacsLambdaToGPU lam, nes))
transformStm _ stm = addStm $ soacsStmToGPU stm

transformStms :: FunHasParallelism -> Stms SOACS -> FlattenM ()
transformStms funHasParallelism stms =
  localScope (castScope $ scopeOf stms) $
    fold <$> traverse (transformStm funHasParallelism) stms

transformBody :: FunHasParallelism -> Body SOACS -> FlattenM (Body GPU)
transformBody funHasParallelism (Body () stms res) = buildBody_ $ do
  transformStms funHasParallelism stms
  pure res

transformFunDef ::
  FunHasParallelism ->
  Scope SOACS ->
  FunDef SOACS ->
  PassM (FunDef GPU, S.Set DemandFn)
transformFunDef funHasParallelism consts_scope fd = do
  let FunDef
        { funDefBody = body,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  (body', needs) <-
    runFlattenM (scopeOfFParams fparams <> castScope consts_scope) $
      transformBody funHasParallelism body
  pure
    ( fd
        { funDefBody = body',
          funDefRetType = rettype,
          funDefParams = fparams
        },
      needs
    )

liftUntilFixedPoint ::
  Prog SOACS ->
  FunHasParallelism ->
  Scope SOACS ->
  S.Set DemandFn ->
  S.Set DemandFn ->
  PassM [FunDef GPU]
liftUntilFixedPoint prog funHasParallelism consts_scope made needed = do
  let made' = made <> needed
  (lifted_funs, new_needed) <-
    fmap (second ((`S.difference` made') . mconcat)) $
      mapAndUnzipM mkDemanded $
        S.toList needed
  if new_needed == mempty
    then pure lifted_funs
    else
      (lifted_funs ++)
        <$> liftUntilFixedPoint prog funHasParallelism consts_scope made' new_needed
  where
    mkDemanded (DemandLifted fname) =
      case find ((== fname) . funDefName) $ progFuns prog of
        Just fundef -> liftFunDef funHasParallelism consts_scope fundef
        Nothing -> error $ "mkDemanded: " <> show fname
    mkDemanded (DemandBuiltin b) = pure (builtinFunDef b, mempty)

transformProg :: Prog SOACS -> PassM (Prog GPU)
transformProg prog = do
  progAfterPreProcessing <- preprocessProg prog
  let consts = progConsts progAfterPreProcessing
      consts_scope = scopeOf consts
      funs = progFuns progAfterPreProcessing
      funParallelism = analyseFunParallelism funs
      funHasParallelism fname =
        M.findWithDefault (not $ isBuiltInFunction fname) fname funParallelism

  (consts', consts_needs) <-
    runFlattenM mempty $ collectStms_ $ transformStms funHasParallelism consts
  (funs', funs_needs) <-
    second mconcat
      <$> mapAndUnzipM (transformFunDef funHasParallelism consts_scope) funs

  -- Now do fixpoint iteration until all needed functions have been provided.
  lifted_funs <-
    liftUntilFixedPoint
      prog
      funHasParallelism
      consts_scope
      mempty
      (consts_needs <> funs_needs)

  -- In extremely unlikely cases (mostly empty programs), we may end up having a
  -- name source that overlaps the names used in the builtin functions. Avoid
  -- that by bumping it by enough that we probably will not have a conflict.
  modifyNameSource $ \src -> ((), mappend (newNameSource 1000) src)
  pure $
    prog
      { progConsts = consts',
        progFuns = lifted_funs <> funs'
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
