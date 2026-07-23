{-# LANGUAGE TypeFamilies #-}

-- | This pass transforms parallelism expressed with arbitrarily nested SOACs to
-- instead be expressed with limited-nesting SegOps. This is the so-called
-- "flattening transformation" (sometimes called "vectorization", although we do
-- not use that term much in the Futhark compiler).
--
-- This is a sophisticated pass that does various clever things:
--
-- - Detects uniform nesting and flattens it more efficiently than the
-- - nonuniform case.
--
-- - DPH-style vectorization avoidance.
--
-- - Incremental flattening ("Futhark.Pass.Flatten.Incremental").
--
-- - Intrablock flattening ("Futhark.Pass.Flatten.Intrablock").
--
-- The goal is that *any* Futhark program must be compilable parallel GPU code,
-- although in some cases the resulting code is not particularly efficient.
--
-- The idea is to perform distribution on one level at a time, and produce
-- "irregular Maps" that can accept and produce irregular arrays. These
-- irregular maps will then be transformed into flat parallelism based on their
-- contents. This is a sensitive detail, but if irregular maps contain only a
-- single Stm, then it is fairly straightforward, as we simply implement
-- flattening rules for every single kind of expression. Of course that is also
-- somewhat inefficient, so we want to support multiple Stms for things like
-- scalar code.
--
-- Nomenclature:
--
-- A map-nest is the collection of parallel operations enclosing some code. For
-- simplicity, we say "map-nest" even when the top level parallel operation is
-- actually a redomap or other screma.
--
-- An irregular array is a multidimensional array like '[[1,2],[3]]', where rows
-- have different shapes. These are not directly supported in Futhark or in the
-- Futhark IR, but are encoded in various ways.
--
-- We say that an operation or type in a map-nest is "uniform" when its size and
-- control flow is invariant to the map-nest. Converse, it is "nonuniform" when
-- it is variant. When we distribute a uniform statement, the intermediate
-- results are regular, and otherwise irregular. Take care not to confuse the
-- terms "regular" and "uniform" - we say "regular" only about arrays! "Uniform"
-- is the general concept.
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

type FunSizeParams = Name -> S.Set Int

flattenOpsFor :: FunHasParallelism -> FunSizeParams -> SegLevel -> FlattenOps
flattenOpsFor funHasParallelism funSizeParams lvl =
  FlattenOps
    { flattenSegLevel = lvl,
      flattenFunHasParallelism = funHasParallelism,
      flattenDistStmAtLevel = transformDistStm funHasParallelism funSizeParams,
      flattenScalarStm = transformScalarStm lvl,
      flattenTopLevelStm = transformStm funHasParallelism funSizeParams
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
  FunSizeParams ->
  Name ->
  Pat Type ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  StmAux dec ->
  Stms GPU ->
  FlattenM ()
topLevelversionScanRed funHasParallelism funSizeParams desc pat w arrs form aux outer_only_stms = do
  scope <- castScope <$> askScope
  let body_outerpar = mkBody outer_only_stms $ varsRes $ patNames pat
  maybe_body_flattened <-
    factorScremaForParallelism funHasParallelism scope (stmAuxCerts aux) pat w arrs form
  case maybe_body_flattened of
    Nothing -> addStms outer_only_stms
    Just body_flattened0 -> do
      outerOnlyBody <- renameBody body_outerpar
      body_flattened <- transformBody funHasParallelism funSizeParams =<< renameBody body_flattened0
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

transformDistStm :: FunHasParallelism -> FunSizeParams -> SegLevel -> Segments -> DistEnv -> DistStm -> FlattenM DistEnv
transformDistStm _ _ lvl segments env (DistStm inps res (ScalarStm stms)) =
  transformScalarStms lvl segments env inps res stms
transformDistStm funHasParallelism funSizeParams lvl segments env (DistStm inps res (ParallelStm stm)) = do
  case stm of
    Let pat aux (BasicOp e) -> do
      let ~[res'] = res
          ~[pe] = patElems pat
      transformDistBasicOp ops segments env (inps, res', pe, aux, e)
    Let pat aux (Op (Screma w arrs form)) ->
      transformScrema (flattenOpsFor funHasParallelism funSizeParams lvl) segments env inps res (pat, aux) (w, arrs, form)
    Let _ aux (Match scrutinees cases defaultCase rt) ->
      -- 'transformUniformMatch' keeps the scrutinees in a plain GPU 'Match',
      -- which is only well-scoped when they are invariant to the nest. Whenever
      -- a scrutinee is variant we must partition the segments by branch, even if
      -- no branch contains parallelism (this happens e.g. for a variant
      -- conditional with an irregular result, which cannot be sequentialised
      -- into a scalar group).
      if any (isVariant inps) scrutinees
        then transformVariantMatch ops segments env inps res aux scrutinees cases defaultCase rt
        else transformUniformMatch ops segments env inps res aux scrutinees cases defaultCase rt
    Let pat aux (Apply name args rettype s) ->
      case lvl of
        SegThread {} -> do
          demandLifted name
          arg_types <- mapM (subExpInputType inps . fst) args
          let size_positions = funSizeParams name
              indexed_args = zip [0 ..] args
              isSizeArg = (`S.member` size_positions) . fst
              (size_args, value_args) = L.partition isSizeArg indexed_args
              nonuniform_inps = any (any (isVariant inps) . arrayDims) arg_types
              nonuniform =
                nonuniform_inps
                  || not (all isRegularDistResult res)
              name' = if nonuniform then liftFunName name else liftRegFunName name
          w <- letSubExp "num_segments" =<< toExp (segmentCount segments)

          args' <- if
            nonuniform
            then
              ((w, Observe) :) . concat <$> mapM (liftArg lvl segments w inps env) args
            else do
              value_args' <- mapM (liftRegArg lvl segments w inps env . snd) value_args
              pure $ (w, Observe) : map snd size_args <> value_args'
          args_ts <- mapM (subExpType . fst) args'
          let dietToUnique Consume = Unique
              dietToUnique Observe = Nonunique
              param_ts = zipWith toDecl args_ts $ map (dietToUnique . snd) args'
              rettype' = if nonuniform 
                          then addRetAls param_ts $ liftRetType w $ map fst rettype
                          else addRetAls param_ts $ liftRegularRetType w $ map fst rettype
          result <- letTupExp (name' <> "_res") $ Apply name' args' rettype' s
          let reps =  if nonuniform 
                        then resultToResReps (map fst rettype) result
                        else distResultsToResReps res result
          reps' <- zipWithM (reshapeLiftedApplyResult segments) (map fst rettype) reps
          pure $ insertReps (zip (map distResTag res) reps') env
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
    Let _ aux (Op (FlatMap w arrs lam)) ->
      transformFlatMapNested ops segments env inps res aux w arrs lam
    Let _ _ (Op (Stream {})) -> error "transformDistStm: Stream should have been removed"
    Let _ _ (Op (JVP {})) -> error "Unhandled JVP"
    Let _ _ (Op (VJP {})) -> error "Unhandled VJP"
    Let _ _ (Op (WithVJP {})) -> error "Unhandled WithVJP"
  where
    ops = flattenOpsFor funHasParallelism funSizeParams lvl

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

liftRegArg :: SegLevel -> Segments -> SubExp -> DistInputs -> DistEnv -> (SubExp, Diet) -> FlattenM (SubExp, Diet)
liftRegArg lvl _segments w inps env (se, d) = do
  se_t <- subExpInputType inps se
  let se_shape = arrayShape se_t
      expected_shape = Shape [w] <> se_shape
  v <- liftSubExpRegular lvl (NE.singleton w) inps env expected_shape se
  pure (Var v, d)

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

liftRegularRetType :: SubExp -> [RetType SOACS] -> [RetType GPU]
liftRegularRetType w = concat . snd . L.mapAccumL liftType 0
  where
    liftType i rettype =
      let lifted = case rettype of
            Prim pt -> pure $ arrayOf (Prim pt) (Shape [Free w]) Unique
            Array pt shape _ ->
              pure $ arrayOf (Prim pt) (Shape [Free w] <> shape) Unique
            Acc {} -> error "liftRegularRetType: Acc"
            Mem {} -> error "liftRegularRetType: Mem"
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
    bodyBind $ lambdaBody map_lam'

liftBody :: FunHasParallelism -> FunSizeParams -> SegLevel -> SubExp -> DistInputs -> DistEnv -> DistStms -> Result -> FlattenM Result
liftBody funHasParallelism funSizeParams lvl w inputs env dstms result = do
  let segments = NE.singleton w
  env' <- foldM (transformDistStm funHasParallelism funSizeParams lvl segments) env dstms
  result' <- mapM (liftResult lvl segments inputs env') result
  pure $ concat result'

liftRegFunBody :: FunHasParallelism -> FunSizeParams -> SegLevel -> SubExp -> DistInputs -> DistEnv -> DistStms -> Result -> FlattenM Result
liftRegFunBody funHasParallelism funSizeParams lvl w inputs env dstms result = do
  let segments = NE.singleton w
  env' <- foldM (transformDistStm funHasParallelism funSizeParams lvl segments) env dstms
  mapM (liftRegResult lvl segments inputs env') result

liftFunName :: Name -> Name
liftFunName name = name <> "_lifted"

liftRegFunName :: Name -> Name
liftRegFunName name = name <> "_regular_lifted"

-- | A lifted function must return fresh, non-aliasing arrays (as it
-- corresponds to 'map f'; see 'liftRetType').  This is not
-- automatically the case: a result may alias a parameter (when a value
-- is passed straight through), or the same array may be returned in
-- multiple result positions (which happens for functions that return
-- the same value more than once).  For every such result we insert a
-- copy to re-establish the invariant.  Results that are already fresh
-- are left untouched, so no superfluous copies are inserted.
freshenResult :: [FParam GPU] -> FlattenM Result -> FlattenM Result
freshenResult params m = do
  (result, stms) <- collectStms m
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

analyseFunSizeParams :: [FunDef SOACS] -> M.Map Name (S.Set Int)
analyseFunSizeParams = M.fromList . map analyse
  where
    analyse fd =
      let fparams = funDefParams fd
          rettype = funDefRetType fd
          size_names = freeIn (map paramType fparams, map fst rettype)
          isSizeParam p = paramName p `nameIn` size_names
          indexed_params = zip [0 ..] fparams
          size_params = filter (isSizeParam . snd) indexed_params
       in (funDefName fd, S.fromList $ map fst size_params)

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
  FunSizeParams ->
  Scope SOACS ->
  FunDef SOACS ->
  PassM (FunDef GPU, S.Set DemandFn)
liftFunDef funHasParallelism funSizeParams const_scope fd = do
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
  (body', needs) <-
    runFlattenM (castScope const_scope <> scopeOfFParams fparams'') $
      buildBody_ . freshenResult fparams'' $
        liftBody funHasParallelism funSizeParams defaultSegLevel w inputs' env dstms $
          bodyResult body
  let name = liftFunName $ funDefName fd
  pure
    ( fd
        { funDefName = name,
          funDefBody = body',
          funDefParams = fparams'',
          funDefRetType = rettype'
        },
      needs
    )

liftRegFunDef ::
  FunHasParallelism ->
  FunSizeParams ->
  Scope SOACS ->
  FunDef SOACS ->
  PassM (FunDef GPU, S.Set DemandFn)
liftRegFunDef funHasParallelism funSizeParams const_scope fd = do
  let FunDef
        { funDefBody = body,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  wp <- newParam "w" $ Prim int64
  let w = Var $ paramName wp
  let size_positions = funSizeParams $ funDefName fd
      isSizeParam = (`S.member` size_positions) . fst
      (indexed_sizes, indexed_values) =
        L.partition isSizeParam $ zip [0 ..] fparams
      fparam_sizes = map snd indexed_sizes
      fparams_explicit = map snd indexed_values

  (fparams_explicit', value_reps) <- mapAndUnzipM (liftRegularParam w) fparams_explicit
  let fparams'' = wp : fparam_sizes <> fparams_explicit'
  let inputs = do
        (p, i) <- zip fparams_explicit [0 ..]
        pure (paramName p, DistInput (ResTag i) (paramType p))
  let rettype' =
        addRetAls (map paramDeclType fparams'') $
          liftRegularRetType w (map fst rettype)
  let (inputs', dstms) =
        distributeBody DistributeIrregular funHasParallelism (const_scope <> scopeOfFParams fparam_sizes) (NE.singleton (Var (paramName wp))) inputs body
      env = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) value_reps
  -- Lift the body of the function and get the results, inserting copies as
  -- necessary to ensure the results are fresh and unique (see 'freshenResult').
  (body', needs) <-
    runFlattenM (castScope const_scope <> scopeOfFParams fparams'') $
      buildBody_ . freshenResult fparams'' $
        -- If there is an existential return size then the result will not be regular
        -- therefore, we are not handling this simillar to the way we handle parameters.
        liftRegFunBody funHasParallelism funSizeParams  defaultSegLevel w inputs' env dstms $
          bodyResult body
  let name = liftRegFunName $ funDefName fd
  pure
    ( fd
        { funDefName = name,
          funDefBody = body',
          funDefParams = fparams'',
          funDefRetType = rettype'
        },
      needs
    )

transformLambda :: FunHasParallelism -> FunSizeParams -> Lambda SOACS -> FlattenM (Lambda GPU)
transformLambda funHasParallelism funSizeParams (Lambda params ret body) = do
  body' <- localScope (scopeOfLParams params) $ transformBody funHasParallelism funSizeParams body
  pure $ Lambda params ret body'

transformStm :: FunHasParallelism -> FunSizeParams -> Stm SOACS -> FlattenM ()
transformStm funHasParallelism funSizeParams (Let pat aux (Op soac))
  | "sequential_outer" `inAttrs` stmAuxAttrs aux = do
      scope <- askScope
      stms <- runBuilderT_ (FOT.transformSOAC pat soac) (castScope scope)
      transformStms funHasParallelism funSizeParams $ fmap (certify (stmAuxCerts aux)) stms
transformStm _ _ stm
  | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) = addStm $ soacsStmToGPU stm
transformStm _ _ (Let pat aux (Op (Hist w arrs ops bucket_fun))) =
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
transformStm funHasParallelism funSizeParams (Let pat aux (Op (Screma w arrs form)))
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
      topLevelversionScanRed funHasParallelism funSizeParams "top_level_scan_alt" pat w arrs form aux outer_only_stms
transformStm funHasParallelism funSizeParams (Let pat aux (Op (Screma w arrs form)))
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
      topLevelversionScanRed funHasParallelism funSizeParams "top_level_red_alt" pat w arrs form aux outer_only_stms
transformStm funHasParallelism funSizeParams (Let pat aux (Op (Screma w arrs form)))
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
          ops = flattenOpsFor funHasParallelism funSizeParams defaultSegLevel
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
transformStm funHasParallelism funSizeParams (Let pat aux (Op (FlatMap w arrs lam))) =
  certifying (stmAuxCerts aux) $
    transformFlatMap (flattenOpsFor funHasParallelism funSizeParams defaultSegLevel) pat w arrs lam
transformStm funHasParallelism funSizeParams (Let pat aux (Loop params form body)) =
  localScope (scopeOfLoopForm form <> scopeOfFParams (map fst params)) $
    addStm . Let pat aux . Loop params form =<< transformBody funHasParallelism funSizeParams body
transformStm funHasParallelism funSizeParams (Let pat aux (Match ses cases def_body ret)) =
  addStm . Let pat aux
    =<< (Match ses <$> mapM onCase cases <*> transformBody funHasParallelism funSizeParams def_body <*> pure ret)
  where
    onCase = traverse (transformBody funHasParallelism funSizeParams)
transformStm funHasParallelism funSizeParams (Let pat aux (WithAcc inputs withacc_lam)) = do
  addStm . Let pat aux . WithAcc (map onInput inputs)
    =<< transformLambda funHasParallelism funSizeParams withacc_lam
  where
    onInput (shape, arrs, Nothing) =
      (shape, arrs, Nothing)
    onInput (shape, arrs, Just (lam, nes)) =
      (shape, arrs, Just (soacsLambdaToGPU lam, nes))
transformStm _ _ stm = addStm $ soacsStmToGPU stm

transformStms :: FunHasParallelism -> FunSizeParams -> Stms SOACS -> FlattenM ()
transformStms funHasParallelism funSizeParams stms =
  localScope (castScope $ scopeOf stms) $
    fold <$> traverse (transformStm funHasParallelism funSizeParams) stms

transformBody :: FunHasParallelism -> FunSizeParams -> Body SOACS -> FlattenM (Body GPU)
transformBody funHasParallelism funSizeParams (Body () stms res) = buildBody_ $ do
  transformStms funHasParallelism funSizeParams stms
  pure res

transformFunDef ::
  FunHasParallelism ->
  FunSizeParams ->
  Scope SOACS ->
  FunDef SOACS ->
  PassM (FunDef GPU, S.Set DemandFn)
transformFunDef funHasParallelism funSizeParams consts_scope fd = do
  let FunDef
        { funDefBody = body,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  (body', needs) <-
    runFlattenM (scopeOfFParams fparams <> castScope consts_scope) $
      transformBody funHasParallelism funSizeParams body
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
  FunSizeParams ->
  Scope SOACS ->
  S.Set DemandFn ->
  S.Set DemandFn ->
  PassM [FunDef GPU]
liftUntilFixedPoint prog funHasParallelism funSizeParams consts_scope made needed = do
  let made' = made <> needed
  (lifted_funs, new_needed) <-
    fmap (second ((`S.difference` made') . mconcat)) $
      mapAndUnzipM mkDemanded $
        S.toList needed
  if new_needed == mempty
    then pure $ concat lifted_funs
    else
      (concat lifted_funs ++)
        <$> liftUntilFixedPoint prog funHasParallelism funSizeParams consts_scope made' new_needed
  where
    mkDemanded (DemandLifted fname) =
      case find ((== fname) . funDefName) $ progFuns prog of
        Just fundef -> do
          -- TODO: Do not create both versions
          (irreg_fun, needed') <- liftFunDef funHasParallelism funSizeParams consts_scope fundef
          (reg_fun, _) <- liftRegFunDef funHasParallelism funSizeParams consts_scope fundef
          pure ([irreg_fun, reg_fun], needed')
        Nothing -> error $ "mkDemanded: " <> show fname
    mkDemanded (DemandBuiltin b) = pure ([builtinFunDef b], mempty)

transformProg :: Prog SOACS -> PassM (Prog GPU)
transformProg prog = do
  progAfterPreProcessing <- preprocessProg prog
  let consts = progConsts progAfterPreProcessing
      consts_scope = scopeOf consts
      funs = progFuns progAfterPreProcessing
      funParallelism = analyseFunParallelism funs
      size_param_map = analyseFunSizeParams funs
      funHasParallelism fname =
        M.findWithDefault (not $ isBuiltInFunction fname) fname funParallelism
      funSizeParams fname = 
         M.findWithDefault mempty fname size_param_map
  (consts', consts_needs) <-
    runFlattenM mempty $ collectStms_ $ transformStms funHasParallelism funSizeParams consts
  (funs', funs_needs) <-
    second mconcat
      <$> mapAndUnzipM (transformFunDef funHasParallelism funSizeParams consts_scope) funs

  -- Now do fixpoint iteration until all needed functions have been provided.
  lifted_funs <-
    liftUntilFixedPoint
      prog
      funHasParallelism
      funSizeParams
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
