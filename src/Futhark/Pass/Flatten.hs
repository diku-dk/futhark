{-# LANGUAGE TypeFamilies #-}

-- | This pass transforms parallelism expressed with arbitrarily nested SOACs to
-- instead be expressed with limited-nesting SegOps. This is the so-called
-- "flattening transformation" (sometimes called "vectorization", although we do
-- not use that term much in the Futhark compiler).
--
-- This is a sophisticated pass that does various clever things:
--
-- - Detects uniform nesting and flattens it more efficiently than the
--   nonuniform case.
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
-- "irregular maps" that can accept and produce irregular arrays. These
-- irregular maps will then be transformed into flat parallelism based on their
-- contents. If irregular maps contain only a single Stm, then it is fairly
-- straightforward, as we simply implement flattening rules for every single
-- kind of expression. Of course that is also somewhat inefficient, so we want
-- to support multiple Stms for things like scalar code.
--
-- Nomenclature:
--
-- A /map-nest/ is the collection of parallel operations enclosing some code. For
-- simplicity, we say "map-nest" even when the top level parallel operation is
-- actually a redomap or other screma.
--
-- An /irregular array/ is a multidimensional array like '[[1,2],[3]]', where rows
-- have different shapes. These are not directly supported in Futhark or in the
-- Futhark IR, but are encoded in various ways.
--
-- We say that an operation or type in a map-nest is /uniform/ when its size
-- (including internal sizes and sizes of inputs) and control flow is invariant
-- to the map-nest. Converse, it is /nonuniform/ when it is variant. When we
-- distribute a uniform statement, the intermediate results are regular, and
-- otherwise irregular. A statement that uses an irregular array is necessarily
-- nonuniform.
--
-- Take care not to confuse the terms "regular" and "uniform" - we say "regular"
-- only about arrays! "Uniform" is the general concept.
--
-- /Uniform nested parallelism/ is nested parallelism whose size is uniform to
-- the enclosing map nest, and which uses only variables whose types are
-- uniform, and which is enclosed in uniform control flow. /Nonuniform nested
-- parallelism/ is the converse. Many of the optimisations here are about
-- detecting the uniform case. We previously often used the terms "regular
-- nested parallelism" and "irregular nested parallelism", but this is now
-- discouraged, as explained above.
module Futhark.Pass.Flatten (flattenSOACs) where

import Control.Monad
import Data.Bifunctor (second)
import Data.Foldable
import Data.List qualified as L
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
import Futhark.Pass.Flatten.Loop
import Futhark.Pass.Flatten.Match
import Futhark.Pass.Flatten.PreProcess
import Futhark.Pass.Flatten.SOAC
import Futhark.Pass.Flatten.WithAcc
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Futhark.Transform.ToGPU (soacsLambdaToGPU, soacsStmToGPU)
import Prelude hiding (div, quot, rem)

type FunSizeParams = Name -> S.Set Int

-- | The irregularity handling mode requested by a statement, defaulting to the
-- mode already in effect. @#[flattening(sequentialise_nonuniform)]@ asks that
-- nonuniform nested parallelism be sequentialised rather than flattened; see
-- 'SequentialiseIrregularAll'.
irregularityFor :: DistIrregularity -> StmAux a -> DistIrregularity
irregularityFor irreg aux
  | AttrComp "flattening" ["sequentialise_nonuniform"] `inAttrs` stmAuxAttrs aux =
      SequentialiseIrregularAll
  | otherwise = irreg

flattenOpsFor ::
  Attrs ->
  FunHasParallelism ->
  FunSizeParams ->
  DistIrregularity ->
  SegLevel ->
  FlattenOps
flattenOpsFor attrs funHasParallelism funSizeParams irreg lvl =
  FlattenOps
    { flattenSegLevel = lvl,
      flattenIrregularity = irreg,
      flattenFunHasParallelism = funHasParallelism,
      flattenDistStmWith = transformDistStm funSizeParams,
      flattenScalarStmAt = transformScalarStm,
      flattenTopLevelStm = transformTopLevelStm attrs funHasParallelism funSizeParams
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
  insertRepsM (zip (map distResTag distres) $ map Regular vs) env

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

-- | Transform a top-level 'Screma' by treating it as the empty-'Segments' case
-- of a nested one: it is not enclosed in any map-nest, so there are no
-- segments, the mapped arrays are plain regular top-level values
-- ('DistInputFree'), and the results are necessarily regular.
transformTopLevelScrema ::
  Attrs ->
  FunHasParallelism ->
  FunSizeParams ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  FlattenM ()
transformTopLevelScrema attrs funHasParallelism funSizeParams pat aux w arrs form = do
  let irreg = irregularityFor DistributeIrregular aux
      ops = flattenOpsFor attrs funHasParallelism funSizeParams irreg defaultSegLevel
  arr_ts <- mapM lookupType arrs
  -- 'flattenScrema' may bind the names of the pattern it is given (some paths
  -- bind them directly, others only insert reps), so we pass it a fresh pattern
  -- and bind the real pattern names ourselves from the result.
  nested_pat <- renamePat pat
  let inps = zipWith (\arr t -> (arr, DistInputFree arr t)) arrs arr_ts
      res =
        zipWith
          (\i pe -> DistResult (ResTag i) (DistType [] (Rank 0) (patElemType pe)) (patElemName pe))
          [0 ..]
          (patElems nested_pat)
  env <- flattenScrema ops [] (DistEnv mempty) inps res (nested_pat, aux) (w, arrs, form)
  forM_ (zip (patNames pat) res) $ \(pat_v, r) ->
    case resVar (distResTag r) env of
      Regular v ->
        letBindNames [pat_v] $ BasicOp $ SubExp $ Var v
      Irregular _ ->
        error "transformTopLevelScrema: top-level result cannot be irregular"

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
  v <- liftSubExpRegular lvl [w] inps env expected_shape se
  pure (Var v, d)

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

liftRegularRetType :: DistInputs -> SubExp -> [RetType SOACS] -> [RetType GPU]
liftRegularRetType inps w = concat . snd . L.mapAccumL liftType 0
  where
    liftType i rettype =
      let lifted = case rettype of
            Prim pt -> pure $ arrayOf (Prim pt) (Shape [Free w]) Unique
            Array pt shape _ ->
              if needsIrregularRetType inps rettype
                then
                  let num_data = Prim int64
                      segs = arrayOf (Prim int64) (Shape [Free w]) Unique
                      flags = arrayOf (Prim Bool) (Shape [Ext i]) Unique
                      offsets = arrayOf (Prim int64) (Shape [Free w]) Unique
                      elems = arrayOf (Prim pt) (Shape [Ext i]) Unique
                   in [num_data, segs, flags, offsets, elems]
                else
                  pure $ arrayOf (Prim pt) (Shape [Free w] <> shape) Unique
            Acc {} -> error "liftRetType: Acc"
            Mem {} -> error "liftRetType: Mem"
       in (i + length lifted, lifted)

liftFunName :: Name -> Name
liftFunName name = name <> "_lifted"

liftUniformFunName :: Name -> Name
liftUniformFunName name = name <> "_uniform_lifted"

flattenApply ::
  FunSizeParams ->
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  (Pat Type, StmAux ()) ->
  (Name, [(SubExp, Diet)], [(RetType SOACS, RetAls)], Safety) ->
  FlattenM DistEnv
flattenApply funSizeParams lvl segments env inps res (pat, aux) (name, args, rettype, s) =
  case lvl of
    SegThread {} -> do
      let size_positions = funSizeParams name
          indexed_args = zip [0 ..] args
          isSizeArg = (`S.member` size_positions) . fst
          (size_args, value_args) = L.partition isSizeArg indexed_args
      let nonuniform = any (isVariant inps . fst . snd) size_args
          name' = if nonuniform then liftFunName name else liftUniformFunName name
          mode = if nonuniform then NonUniformLift else UniformLift
      demandLifted name mode
      w <- letSubExp "num_segments" =<< toExp (segmentCount segments)

      args' <-
        if nonuniform
          then
            ((w, Observe) :) . concat <$> mapM (liftArg lvl segments w inps env) args
          else do
            value_args' <- mapM (liftRegArg lvl segments w inps env . snd) value_args
            -- We do not lift 'size_args' because they correspond to size
            -- parameters, which are invariant in the uniform case.
            pure $ (w, Observe) : map snd size_args <> value_args'
      args_ts <- mapM (subExpType . fst) args'
      let dietToUnique Consume = Unique
          dietToUnique Observe = Nonunique
          param_ts = zipWith toDecl args_ts $ map (dietToUnique . snd) args'
          rettype' =
            if nonuniform
              then addRetAls param_ts $ liftRetType w $ map fst rettype
              else addRetAls param_ts $ liftRegularRetType inps w $ map fst rettype
      result <- letTupExp (name' <> "_res") $ Apply name' args' rettype' s
      let reps =
            if nonuniform
              then resultToResReps (map fst rettype) result
              -- XXX: This could instead distinguish between regular and
              -- irregular results based on their return types.
              else resultToResRepsByDistResult res result
      reps' <- zipWithM (reshapeLiftedApplyResult segments) (map fst rettype) reps
      insertRepsM (zip (map distResTag res) reps') env
    -- TODO: we currently do not handle intrablock function applications. It
    -- is possible we could do intrablock-level lifting of functions, but
    -- for now, we simply do not generate intrablock kernels if they would
    -- contain calls to parallel functions.
    _ ->
      if all isRegularDistResult res
        then transformScalarStm lvl segments env inps res $ Let pat aux (Apply name args rettype s)
        else error "Unhandled Apply in non SegThread Seglevel"

transformDistStm :: FunSizeParams -> FlattenOps -> Segments -> DistEnv -> DistStm -> FlattenM DistEnv
transformDistStm _ outer_ops segments env (DistStm inps res (ScalarStm stms)) =
  transformScalarStms (flattenSegLevel outer_ops) segments env inps res stms
transformDistStm funSizeParams outer_ops segments env (DistStm inps res (ParallelStm (Let pat aux e))) = do
  case e of
    BasicOp op -> do
      let ~[res'] = res
          ~[pe] = patElems pat
      flattenBasicOp ops segments env (inps, res', pe, aux, op)
    Op (Screma w arrs form) ->
      flattenScrema ops segments env inps res (pat, aux) (w, arrs, form)
    Match scrutinees cases defaultCase rt ->
      flattenMatch ops segments env inps res aux scrutinees cases defaultCase rt
    Apply name args rettype s ->
      flattenApply funSizeParams lvl segments env inps res (pat, aux) (name, args, rettype, s)
    Loop merge (ForLoop i it n) body ->
      flattenLoop ops segments env inps res (pat, aux) (merge, ForLoop i it n, body)
    Loop merge (WhileLoop cond) body -> do
      flattenLoop ops segments env inps res (pat, aux) (merge, WhileLoop cond, body)
    WithAcc inputs lam ->
      flattenWithAcc ops segments env inps res pat aux inputs lam
    Op (Hist w hist_inputs hist_ops bucket_fun) ->
      flattenHist ops segments env inps res (pat, aux) (w, hist_inputs, hist_ops, bucket_fun)
    Op (FlatMap w arrs lam) ->
      flattenFlatMapNested ops segments env inps res aux w arrs lam
    Op (Stream {}) -> error "transformDistStm: Stream should have been removed"
    Op (JVP {}) -> error "Unhandled JVP"
    Op (VJP {}) -> error "Unhandled VJP"
    Op (WithVJP {}) -> error "Unhandled WithVJP"
  where
    lvl = flattenSegLevel outer_ops
    ops =
      outer_ops
        { flattenIrregularity =
            irregularityFor (flattenIrregularity outer_ops) aux
        }

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

liftBody :: Attrs -> FunHasParallelism -> FunSizeParams -> SegLevel -> SubExp -> DistInputs -> DistEnv -> DistStms -> Result -> FlattenM Result
liftBody attrs funHasParallelism funSizeParams lvl w inputs env dstms result = do
  let segments = [w]
      ops = flattenOpsFor attrs funHasParallelism funSizeParams DistributeIrregular lvl
  env' <- foldM (flattenDistStm ops segments) env dstms
  result' <- mapM (liftResult lvl segments inputs env') result
  pure $ concat result'

liftUniformFunBody :: Attrs -> FunHasParallelism -> FunSizeParams -> SegLevel -> SubExp -> DistInputs -> DistEnv -> DistStms -> [RetType SOACS] -> Result -> FlattenM Result
liftUniformFunBody attrs funHasParallelism funSizeParams lvl w inputs env dstms rettype result = do
  let segments = [w]
      ops = flattenOpsFor attrs funHasParallelism funSizeParams DistributeIrregular lvl
  env' <- foldM (flattenDistStm ops segments) env dstms
  concat <$> zipWithM (liftRegResult lvl segments w inputs env') rettype result

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

-- | Impose attributes on the statements of a function body. This is used to
-- impose attributes on top level statements in lifted functions.
imposeAttrsBody :: Attrs -> Body SOACS -> Body SOACS
imposeAttrsBody attrs body =
  body {bodyStms = fmap (imposeAttrs attrs) (bodyStms body)}

liftFunDef ::
  Attrs ->
  FunHasParallelism ->
  FunSizeParams ->
  Scope SOACS ->
  FunDef SOACS ->
  PassM (FunDef GPU, S.Set DemandFn)
liftFunDef attrs funHasParallelism funSizeParams const_scope fd = do
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
        distributeBody DistributeIrregular funHasParallelism const_scope [Var (paramName wp)] inputs $
          imposeAttrsBody attrs body
      env = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
  -- Lift the body of the function and get the results, inserting copies as
  -- necessary to ensure the results are fresh and unique (see 'freshenResult').
  (body', needs) <-
    runFlattenM (castScope const_scope <> scopeOfFParams fparams'') $
      buildBody_ . freshenResult fparams'' $
        liftBody attrs funHasParallelism funSizeParams defaultSegLevel w inputs' env dstms $
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

-- Here we assume that every type size is invariant and therefore every input
-- array is regular. As a result, parameters that correspond to type sizes are
-- not lifted and are also not part of 'DistInput'.
-- A uniformly lifted function can still return irregular arrays. This happens
-- when it returns an array whose dimension size was created in the function
-- body. In other words, the array has an existential size.
liftUniformFunDef ::
  Attrs ->
  FunHasParallelism ->
  FunSizeParams ->
  Scope SOACS ->
  FunDef SOACS ->
  PassM (FunDef GPU, S.Set DemandFn)
liftUniformFunDef attrs funHasParallelism funSizeParams const_scope fd = do
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
  let (inputs', dstms) =
        distributeBody DistributeIrregular funHasParallelism (const_scope <> scopeOfFParams fparam_sizes) [Var (paramName wp)] inputs $
          imposeAttrsBody attrs body
      env = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) value_reps
      rettype' =
        addRetAls (map paramDeclType fparams'') $
          liftRegularRetType inputs' w (map fst rettype)
  -- Lift the body of the function and get the results, inserting copies as
  -- necessary to ensure the results are fresh and unique (see 'freshenResult').
  (body', needs) <-
    runFlattenM (castScope const_scope <> scopeOfFParams fparams'') $
      buildBody_ . freshenResult fparams'' $
        -- XXX: I think function lifting makes it more important to classify invariant
        -- results in bodies. Function bodies can produce values that are
        -- invariant to the map-nest, but at this point there is no opportunity to
        -- hoist them out of the nest.

        liftUniformFunBody attrs funHasParallelism funSizeParams defaultSegLevel w inputs' env dstms (map fst rettype) $
          bodyResult body
  let name = liftUniformFunName $ funDefName fd
  pure
    ( fd
        { funDefName = name,
          funDefBody = body',
          funDefParams = fparams'',
          funDefRetType = rettype'
        },
      needs
    )

transformLambda :: Attrs -> FunHasParallelism -> FunSizeParams -> Lambda SOACS -> FlattenM (Lambda GPU)
transformLambda attrs funHasParallelism funSizeParams (Lambda params ret body) = do
  body' <- localScope (scopeOfLParams params) $ transformBody attrs funHasParallelism funSizeParams body
  pure $ Lambda params ret body'

transformStm :: Attrs -> FunHasParallelism -> FunSizeParams -> Stm SOACS -> FlattenM ()
transformStm attrs funHasParallelism funSizeParams (Let pat aux (Op soac))
  | "sequential_outer" `inAttrs` stmAuxAttrs aux = do
      scope <- askScope
      stms <- runBuilderT_ (FOT.transformSOAC pat soac) (castScope scope)
      transformStms attrs funHasParallelism funSizeParams $ fmap (certify (stmAuxCerts aux)) stms
transformStm _ _ _ stm
  | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) = addStm $ soacsStmToGPU stm
transformStm _ _ _ (Let pat aux (Op (Hist w arrs ops bucket_fun))) =
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
transformStm attrs funHasParallelism funSizeParams (Let pat aux (Op (Screma w arrs form)))
  | shouldDissectForm form =
      error "transformStm: complex Screma survived preprocessing"
  | otherwise =
      transformTopLevelScrema attrs funHasParallelism funSizeParams pat aux w arrs form
transformStm attrs funHasParallelism funSizeParams (Let pat aux (Op (FlatMap w arrs lam))) =
  certifying (stmAuxCerts aux) $ flattenFlatMap ops pat w arrs lam
  where
    irreg = irregularityFor DistributeIrregular aux
    ops = flattenOpsFor attrs funHasParallelism funSizeParams irreg defaultSegLevel
transformStm attrs funHasParallelism funSizeParams (Let pat aux (Loop params form body)) =
  localScope (scopeOfLoopForm form <> scopeOfFParams (map fst params)) $
    addStm . Let pat aux . Loop params form =<< transformBody attrs funHasParallelism funSizeParams body
transformStm attrs funHasParallelism funSizeParams (Let pat aux (Match ses cases def_body ret)) =
  addStm . Let pat aux
    =<< (Match ses <$> mapM onCase cases <*> transformBody attrs funHasParallelism funSizeParams def_body <*> pure ret)
  where
    onCase = traverse (transformBody attrs funHasParallelism funSizeParams)
transformStm attrs funHasParallelism funSizeParams (Let pat aux (WithAcc inputs withacc_lam)) = do
  addStm . Let pat aux . WithAcc (map onInput inputs)
    =<< transformLambda attrs funHasParallelism funSizeParams withacc_lam
  where
    onInput (shape, arrs, Nothing) =
      (shape, arrs, Nothing)
    onInput (shape, arrs, Just (lam, nes)) =
      (shape, arrs, Just (soacsLambdaToGPU lam, nes))
transformStm _ _ _ stm = addStm $ soacsStmToGPU stm

-- | Transform a statement that is not enclosed in any map-nest, whether it
-- occurs in a function body or was synthesised by flattening. This is where the
-- attributes imposed on the pass (see 'flattenSOACs') are put on the statement.
-- Nested statements receive them through the usual attribute propagation.
transformTopLevelStm :: Attrs -> FunHasParallelism -> FunSizeParams -> Stm SOACS -> FlattenM ()
transformTopLevelStm attrs funHasParallelism funSizeParams =
  transformStm attrs funHasParallelism funSizeParams . imposeAttrs attrs

transformStms :: Attrs -> FunHasParallelism -> FunSizeParams -> Stms SOACS -> FlattenM ()
transformStms attrs funHasParallelism funSizeParams stms =
  localScope (castScope $ scopeOf stms) $
    fold <$> traverse (transformTopLevelStm attrs funHasParallelism funSizeParams) stms

transformBody :: Attrs -> FunHasParallelism -> FunSizeParams -> Body SOACS -> FlattenM (Body GPU)
transformBody attrs funHasParallelism funSizeParams (Body () stms res) = buildBody_ $ do
  transformStms attrs funHasParallelism funSizeParams stms
  pure res

transformFunDef ::
  Attrs ->
  FunHasParallelism ->
  FunSizeParams ->
  Scope SOACS ->
  FunDef SOACS ->
  PassM (FunDef GPU, S.Set DemandFn)
transformFunDef attrs funHasParallelism funSizeParams consts_scope fd = do
  let FunDef
        { funDefBody = body,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  (body', needs) <-
    runFlattenM (scopeOfFParams fparams <> castScope consts_scope) $
      transformBody attrs funHasParallelism funSizeParams body
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
  Attrs ->
  FunHasParallelism ->
  FunSizeParams ->
  Scope SOACS ->
  S.Set DemandFn ->
  S.Set DemandFn ->
  PassM [FunDef GPU]
liftUntilFixedPoint prog attrs funHasParallelism funSizeParams consts_scope made needed = do
  let made' = made <> needed
  (lifted_funs, new_needed) <-
    fmap (second ((`S.difference` made') . mconcat)) $
      mapAndUnzipM mkDemanded $
        S.toList needed
  if new_needed == mempty
    then pure lifted_funs
    else
      (lifted_funs ++)
        <$> liftUntilFixedPoint prog attrs funHasParallelism funSizeParams consts_scope made' new_needed
  where
    mkDemanded (DemandLifted fname mode) =
      case find ((== fname) . funDefName) $ progFuns prog of
        Just fundef ->
          case mode of
            UniformLift -> liftUniformFunDef attrs funHasParallelism funSizeParams consts_scope fundef
            NonUniformLift -> liftFunDef attrs funHasParallelism funSizeParams consts_scope fundef
        Nothing -> error $ "mkDemanded: " <> show fname
    mkDemanded (DemandBuiltin b) = pure (builtinFunDef b, mempty)

transformProg :: Attrs -> Prog SOACS -> PassM (Prog GPU)
transformProg attrs prog = do
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
    runFlattenM mempty $ collectStms_ $ transformStms attrs funHasParallelism funSizeParams consts
  (funs', funs_needs) <-
    second mconcat
      <$> mapAndUnzipM (transformFunDef attrs funHasParallelism funSizeParams consts_scope) funs

  -- Now do fixpoint iteration until all needed functions have been provided.
  lifted_funs <-
    liftUntilFixedPoint
      prog
      attrs
      funHasParallelism
      funSizeParams
      consts_scope
      mempty
      (consts_needs <> funs_needs)

  pure $
    prog
      { progConsts = consts',
        progFuns = lifted_funs <> funs'
      }

-- | Transform a SOACS program to a GPU program, using flattening.
--
-- Parameterised by the names of flattening attributes to impose on top-level
-- SOACs that do not carry flattening attributes of their own.
flattenSOACs :: [Name] -> Pass SOACS GPU
flattenSOACs attr_names =
  Pass
    { passName = "flatten",
      passDescription = "Perform full flattening",
      passFunction = transformProg attrs
    }
  where
    attrs =
      foldMap (oneAttr . AttrComp "flattening" . pure . AttrName) attr_names
{-# NOINLINE flattenSOACs #-}
