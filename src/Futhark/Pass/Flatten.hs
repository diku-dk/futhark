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
import Data.Maybe (isNothing)
import Data.Set qualified as S
import Debug.Trace
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Pass.ExtractKernels.ToGPU (soacsLambdaToGPU, soacsStmToGPU)
import Futhark.Pass.Flatten.BasicOp
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Incremental
import Futhark.Pass.Flatten.Intrablock qualified as Intrablock
import Futhark.Pass.Flatten.Loop
import Futhark.Pass.Flatten.Match
import Futhark.Pass.Flatten.Monad
import Futhark.Pass.Flatten.PreProcess
import Futhark.Pass.Flatten.SOAC
import Futhark.Pass.Flatten.WithAcc
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Prelude hiding (div, quot, rem)

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

transformDistStm :: FunHasParallelism -> SegLevel -> Segments -> DistEnv -> DistStm -> Builder GPU DistEnv
transformDistStm _ lvl segments env (DistStm inps res (ScalarStm stms)) =
  transformScalarStms lvl segments env inps res (stmsFromList stms)
transformDistStm funHasParallelism lvl segments env (DistStm inps res (ParallelStm stm)) = do
  case stm of
    Let pat aux (BasicOp e) -> do
      let ~[res'] = res
          ~[pe] = patElems pat
      transformDistBasicOp ops segments env (inps, res', pe, aux, e)
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
                    liftBodyWithDistResults ops segments case_body_inputs env case_dstms res (bodyResult body)
                )
                scope
            pure $ Case c $ Body () case_body_stms case_body_res
          new_default_body <- do
            let (new_default_body_inputs, new_default_dstms) = distributeBody funHasParallelism scope segments inps defaultCase
            (new_default_body_res, new_default_body_stms) <-
              runReaderT
                ( runBuilder $
                    liftBodyWithDistResults ops segments new_default_body_inputs env new_default_dstms res (bodyResult defaultCase)
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
          ops = flattenOpsFor funHasParallelism defaultSegLevel
          m = transformDistributed ops mempty (NE.singleton w) distributed
      traceM $ prettyString distributed
      stms <- runReaderT (runBuilder_ $ certifying certs m) scope
      let fullFlattenBody0 = mkBody stms $ varsRes $ patNames pat
          outerOnlyBody0 = mkBody outer_only_stms $ varsRes outer_only_res
      fullFlattenBody <- renameBody fullFlattenBody0
      outerOnlyBody <- renameBody outerOnlyBody0
      flip runReaderT scope . runBuilder_ $ do
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

-- | Transform a SOACS program to a GPU program, using flattening.
flattenSOACs :: Pass SOACS GPU
flattenSOACs =
  Pass
    { passName = "flatten",
      passDescription = "Perform full flattening",
      passFunction = transformProg
    }
{-# NOINLINE flattenSOACs #-}
