module Futhark.Pass.Flatten.Loop
  ( transformLoop,
    liftParam,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Monad
import Futhark.Pass.Flatten.SOAC (distResultsToResReps, liftBodyWithDistResults, localiseInputs)
import Futhark.Tools
import Prelude hiding (div, quot, rem)

needsIrregular :: DistInputs -> DistEnv -> S.Set VName -> DeclType -> Bool
needsIrregular inps env loopParamNames t =
  case t of
    Array {} -> any dimIsVariant (arrayDims t)
    _ -> False
  where
    dimIsVariant (Constant _) = False
    dimIsVariant (Var v) = v `S.member` loopParamNames || isVariant inps env (Var v)

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
                irregularD = paramName elems,
                irregularK = Dense
              }
        )
    Acc {} ->
      error "liftParam: Acc"
    Mem {} ->
      error "liftParam: Mem"
  where
    desc = baseName (paramName fparam)

-- Lift a loop parameter and its initial value together.
-- If the parameter is an array whose dimensions are all invariant,
-- we lift it to a regular array. Otherwise we fall back to irregular.
liftLoopParam ::
  SegLevel ->
  Segments ->
  SubExp ->
  DistInputs ->
  DistEnv ->
  S.Set VName ->
  (FParam SOACS, SubExp) ->
  Builder GPU ([FParam GPU], ResRep, [SubExp])
liftLoopParam lvl segments num_segments inps env loopParamNames (fparam, initSE) = do
  let t = declTypeOf fparam
  case t of
    Prim pt -> do
      param <-
        newParam
          (baseName (paramName fparam) <> "_lifted")
          (arrayOf (Prim pt) (segmentsShape segments) Nonunique)
      initV <- liftSubExpRegular lvl segments inps env (segmentsShape segments) initSE
      pure ([param], Regular $ paramName param, [Var initV])
    Array pt _ u
      | needsIrregular inps env loopParamNames t -> do
          (params, rep) <- liftParam num_segments fparam
          initVals <- liftLoopInit lvl segments inps env initSE num_segments
          pure (params, rep, initVals)
      | otherwise -> do
          -- Regular case: all dims are invariant, just add w as outermost dim
          let pShape = segmentsShape segments <> arrayShape t
          p <-
            newParam
              (baseName (paramName fparam) <> "_lifted")
              (arrayOf (Prim pt) pShape u)
          initV <- liftSubExpRegular lvl segments inps env pShape initSE
          pure ([p], Regular $ paramName p, [Var initV])
    Acc {} -> do
      initV <- liftSubExpRegular lvl segments inps env mempty initSE
      let Param attrs v acc_t = fparam
      param <- Param attrs <$> newName v <*> pure acc_t
      pure ([param], Regular $ paramName param, [Var initV])
    Mem {} ->
      error "liftLoopParam: Mem"

liftLoopInit :: SegLevel -> Segments -> DistInputs -> DistEnv -> SubExp -> SubExp -> Builder GPU [SubExp]
liftLoopInit lvl segments inps env se num_segments = do
  (_, rep) <- liftSubExp lvl segments inps env se
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
        t_o <- lookupType offsets
        flags_t <- lookupType flags
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        -- I'm not sure why I need this reshapes
        segs' <- letExp "segs" $ BasicOp $ Reshape segs $ reshapeAll (arrayShape t_o) (Shape [num_segments])
        offsets' <- letExp "offsets" $ BasicOp $ Reshape offsets $ reshapeAll (arrayShape t_o) (Shape [num_segments])
        pure $ map Var [num_data, segs', flags', offsets', elems']

loopResultToResReps :: [DistResult] -> [VName] -> [ResRep]
loopResultToResReps dist_res results =
  snd $
    L.mapAccumL
      ( \rs dist_res' ->
          if isRegularDistResult dist_res'
            then
              let (v : rs') = rs
               in (rs', Regular v)
            else
              let (_ : segs : flags : offsets : elems : rs') = rs
               in (rs', Irregular $ IrregularRep segs flags offsets elems Dense)
      )
      results
      dist_res

liftLoopResult :: SegLevel -> Segments -> SubExp -> DistInputs -> DistEnv -> DistResult -> SubExpRes -> Builder GPU Result
liftLoopResult lvl segments num_segments inps env dist_res res =
  if isRegularDistResult dist_res
    then do
      let (DistType _ _ t) = distResType dist_res
      let expectedShape = segmentsShape segments <> arrayShape t
      v <- liftSubExpRegular lvl segments inps env expectedShape (resSubExp res)
      pure [SubExpRes mempty (Var v)]
    else case resSubExp res of
      Var v -> do
        irreg <- getIrregRep lvl segments env inps v
        map (SubExpRes mempty . Var) <$> mkIrrep irreg
      _ -> undefined
  where
    mkIrrep
      ( IrregularRep
          { irregularS = segs,
            irregularF = flags,
            irregularO = offsets,
            irregularD = elems
          }
        ) = do
        flags_t <- lookupType flags
        t <- lookupType elems
        t_o <- lookupType offsets
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        segs' <- letExp "segs" $ BasicOp $ Reshape segs $ reshapeAll (arrayShape t_o) (Shape [num_segments])
        offsets' <- letExp "offsets" $ BasicOp $ Reshape offsets $ reshapeAll (arrayShape t_o) (Shape [num_segments])
        pure [num_data, segs', flags', offsets', elems']

liftLoopBody :: FlattenOps -> Segments -> SubExp -> DistInputs -> DistEnv -> [DistStm] -> [DistResult] -> Result -> Builder GPU Result
liftLoopBody ops segments num_segments inputs env dstms dist_res result = do
  env' <- foldM (flattenDistStm ops segments) env dstms
  results <- zipWithM (liftLoopResult lvl segments num_segments inputs env') dist_res result
  pure $ concat results
  where
    lvl = flattenSegLevel ops

-- FIXME: this is very similar to the one in Match.hs.
splitInput ::
  SegLevel ->
  Segments ->
  DistInputs ->
  DistEnv ->
  VName ->
  VName ->
  Builder GPU (Type, VName, ResRep)
splitInput lvl segments inps env is v = do
  (t, rep) <- liftSubExpPreserveRep segments inps env (Var v)
  (t,v,) <$> case rep of
    Regular arr ->
      if isAcc t
        then
          pure $ Regular arr
        else do
          n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
          -- isnt' it better to do the segmap over all dims?
          arr' <- letExp "split_arr" <=< segMap lvl (MkSolo n) $ \(MkSolo i) -> do
            idx <- letSubExp "idx" =<< eIndex is [eSubExp i]
            let arr_is = unflattenIndex (segmentDims segments) (pe64 idx)
            subExpsRes . pure <$> (letSubExp "arr" =<< eIndex arr (map toExp arr_is))
          pure $ Regular arr'
    Irregular (IrregularRep segs flags offsets elems _) -> do
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      segs' <- letExp "split_segs" <=< segMap lvl (MkSolo n) $ \(MkSolo i) -> do
        idx <- letExp "idx" =<< eIndex is [eSubExp i]
        subExpsRes . pure <$> (letSubExp "segs" =<< eIndex segs [toExp idx])
      (_, offsets', num_data) <- exScanAndSum lvl segs'
      (_, _, ii1) <- doRepIota lvl segs'
      (_, _, ii2) <- doSegIota lvl segs'
      ~[flags', elems'] <- letTupExp "split_F_data" <=< segMap lvl (MkSolo num_data) $ \(MkSolo i) -> do
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
              irregularD = elems',
              irregularK = Dense
            }

-- transform a for-loop with a variant iteration count into a while-loop
transformForToWhile ::
  FlattenOps ->
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
transformForToWhile ops segments env inps res aux merge i it n body = do
  let old_loop_params = map fst merge
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
  let (inps_dist, dstms) = distributeBody (flattenFunHasParallelism ops) scope segments inps_local synthetic_body

  lifted_res <- liftBodyWithDistResults ops segments inps_dist env_local dstms res (bodyResult synthetic_body)
  lifted_vs <- mapM (letExp "for_variant_res" <=< toExp . resSubExp) lifted_res
  let reps = distResultsToResReps res lifted_vs
  pure $ insertReps (zip (map distResTag res) reps) env

transformLoop ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  (Pat Type, StmAux ()) ->
  ([(Param DeclType, SubExp)], LoopForm, Body SOACS) ->
  Builder GPU DistEnv
transformLoop ops segments env inps res (_pat, aux) (merge, ForLoop i it n, body) = do
  if isVariant inps env n
    then transformForToWhile ops segments env inps res aux merge i it n body
    else do
      let old_loop_params = map fst merge
          old_loop_inits = map snd merge
          loopParamNames = S.fromList $ map paramName old_loop_params

      num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
      (lifted_loop_params, lifted_loop_reps, lifted_init) <-
        unzip3 <$> mapM (liftLoopParam (flattenSegLevel ops) segments num_segments inps env loopParamNames) (zip old_loop_params old_loop_inits)

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
      let (loop_new_inputs', loop_dstms) = distributeBody (flattenFunHasParallelism ops) scope segments loop_new_inputs body

      (loop_body_res, loop_body_stms) <-
        runReaderT
          ( runBuilder $
              liftLoopBody ops segments num_segments loop_new_inputs' loop_env_local loop_dstms res (bodyResult body)
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

      let out_reps = loopResultToResReps res loop_out_vs
      pure $ insertReps (zip (map distResTag res) out_reps) env
--
transformLoop ops segments env inps res (_pat, aux) (merge, WhileLoop cond, body) = do
  -- TODO:
  -- 4) Use reduction rather than scan for any_active
  -- 5) Consider updating the active segment so we don't go over w everytime

  -- inside the body we should compute the indices for which the condition is true and for which it is false, and then distribute the body based on that.
  --  We can then merge the results of the two branches by writing them back to a blank space like we do for the branches of a match.

  let old_loop_params = map fst merge
      old_loop_inits = map snd merge
      loopParamNames = S.fromList $ map paramName old_loop_params
  w <- letSubExp "num_segments" =<< toExp (segmentCount segments)
  (lifted_loop_params, lifted_loop_reps, lifted_init) <-
    unzip3 <$> mapM (liftLoopParam lvl segments w inps env loopParamNames) (zip old_loop_params old_loop_inits)

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
    -- infinite loop
    Nothing -> do
      let build_scope = scopeOfFParams lifted_loop_params'
      let (loop_new_inputs', loop_dstms) =
            distributeBody (flattenFunHasParallelism ops) scope segments loop_new_inputs body
      (loop_body_res, loop_body_stms) <-
        flip runReaderT (scope <> build_scope) . runBuilder $
          liftLoopBody ops segments w loop_new_inputs' loop_env_local loop_dstms res (bodyResult body)
      let loop_body_gpu = Body () loop_body_stms loop_body_res
          loop_exp_gpu = Loop (zip lifted_loop_params' lifted_init') (WhileLoop cond) loop_body_gpu
      loop_out_vs <- certifying (distCerts inps aux env) $ letTupExp "loop_res_out" loop_exp_gpu
      let out_reps = loopResultToResReps res loop_out_vs
      pure $ insertReps (zip (map distResTag res) out_reps) env
    Just (cond_lifted_rep, cond_init) -> do
      let [cond_init_se] = cond_init

      -- Compute initial any_active
      cond_init_arr_v <- letExp "cond_init_arr" $ BasicOp $ SubExp cond_init_se
      let cond_lifted_param = case cond_lifted_rep of
            Regular v -> v
            Irregular {} -> error "WhileLoop condition cannot be irregular"

      -- latter chagne to reduction
      cond_init_arr_t <- lookupType cond_init_arr_v
      cond_init_flat <-
        letExp "cond_init_flat" . BasicOp $
          Reshape cond_init_arr_v $
            reshapeAll (arrayShape cond_init_arr_t) (Shape [w])

      or_lam <- binOpLambda LogOr Bool
      cond_scanned <- genScan lvl "any_scan" (NE.singleton w) or_lam [constant False] [cond_init_flat]
      let [cond_scanned_v] = cond_scanned

      any_active_init <-
        letSubExp "any_active_init"
          =<< eIf
            (toExp $ pe64 w .==. 0)
            (eBody [eSubExp $ constant False])
            (eBody [eIndex cond_scanned_v [toExp $ pe64 w - 1]])

      any_active_param <- newParam "any_active" (Prim Bool)
      let build_scope = scopeOfFParams lifted_loop_params' <> scopeOfFParams [any_active_param]
      -- ‌build body
      (loop_body_res, loop_body_stms) <-
        runReaderT
          ( runBuilder $ do
              -- (num_data, active_inds) <- genFilter cond_lifted_param
              equiv_classes <- letExp "equiv_classes" <=< segMap lvl (MkSolo w) $ \(MkSolo i) -> do
                let seg_is = unflattenIndex (segmentDims segments) (pe64 i)
                c <- letSubExp "c" =<< eIndex cond_lifted_param (map toExp seg_is)
                cls <-
                  letSubExp "cls"
                    =<< eIf
                      (eSubExp c)
                      (eBody [toExp $ intConst Int64 1])
                      (eBody [toExp $ intConst Int64 0])
                pure [subExpRes cls]
              n_cases <- letExp "n_cases" <=< toExp $ intConst Int64 2
              (partition_sizes, partition_offs, partition_inds) <- doPartition lvl n_cases equiv_classes
              inds_t <- lookupType partition_inds

              let getInds nm k = do
                    sz <-
                      letSubExp (nm <> "_sz")
                        =<< eIndex partition_sizes [toExp $ intConst Int64 k]
                    off <-
                      letSubExp (nm <> "_off")
                        =<< eIndex partition_offs [toExp $ intConst Int64 k]
                    inds <-
                      letExp (nm <> "_inds") $
                        BasicOp $
                          Index partition_inds $
                            fullSlice inds_t [DimSlice off sz (intConst Int64 1)]
                    pure (sz, inds)

              (_, inactive_inds) <- getInds "inactive" 0
              (active_size, active_inds) <- getInds "active" 1

              inactive_reps <- forM old_loop_params $ \p -> do
                (_, _, rep) <- splitInput lvl segments loop_new_inputs loop_env_local inactive_inds (paramName p)
                pure rep

              let free_in_body =
                    filter
                      (isVariant loop_new_inputs loop_env_local . Var)
                      (namesToList $ freeIn body)
              free_sizes <-
                foldMap freeIn <$> mapM (lookupInputType loop_new_inputs) free_in_body
              let free_variant_sizes = filter (isVariant loop_new_inputs loop_env_local . Var) (namesToList free_sizes)
                  free_size_vars = nubOrd (free_variant_sizes <> free_in_body)
              (ts, vs, reps) <- unzip3 <$> mapM (splitInput lvl segments loop_new_inputs loop_env_local active_inds) free_size_vars
              let subset_inputs = do
                    (v, t, i) <- zip3 vs ts [0 ..]
                    pure (v, DistInput (ResTag i) t)
                  env_subset = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
              let subset_segments = NE.singleton active_size
              let (subset_inputs', subset_dstms) =
                    distributeBody (flattenFunHasParallelism ops) scope subset_segments subset_inputs body
              env_subset' <- foldM (flattenDistStm ops subset_segments) env_subset subset_dstms
              active_reps <-
                zipWithM
                  (liftDistResultRep lvl subset_segments subset_inputs' env_subset')
                  res
                  (bodyResult body)

              let mergeOneLifted t rep0 rep1
                    | isAcc t = do
                        let (Regular acc_res) = rep1
                        pure [SubExpRes mempty (Var acc_res)]
                    | otherwise =
                        case (rep0, rep1) of
                          (Regular x0, Regular x1) -> do
                            let initial_shape = Shape [w] <> arrayShape t
                            let final_shape = segmentsShape segments <> arrayShape t
                            let pt = elemType t
                            space <- letExp "blank" =<< eBlank (Array pt initial_shape NoUniqueness)

                            out <-
                              foldM
                                (scatterRegular lvl)
                                space
                                [(inactive_inds, x0), (active_inds, x1)]

                            out_type <- arrayShape <$> lookupType out
                            out_reshaped <-
                              letExp "out_reshaped" . BasicOp $
                                Reshape out $
                                  reshapeAll out_type final_shape

                            pure [SubExpRes mempty (Var out_reshaped)]
                          (Irregular ir0, Irregular ir1) -> do
                            segsSpace <-
                              letExp "blank_segs"
                                =<< eBlank (Array int64 (Shape [w]) NoUniqueness)

                            segs <-
                              foldM
                                (scatterRegular lvl)
                                segsSpace
                                [(inactive_inds, irregularS ir0), (active_inds, irregularS ir1)]

                            (_, offsets, num_data) <- exScanAndSum lvl segs

                            let pt = elemType t
                            elemsSpace <-
                              letExp "blank_elems"
                                =<< eBlank (Array pt (Shape [num_data]) NoUniqueness)

                            elems <-
                              foldM
                                (scatterIrregular lvl offsets)
                                elemsSpace
                                [(inactive_inds, ir0), (active_inds, ir1)]

                            flags <- genFlags lvl num_data offsets

                            pure
                              [ SubExpRes mempty num_data,
                                SubExpRes mempty (Var segs),
                                SubExpRes mempty (Var flags),
                                SubExpRes mempty (Var offsets),
                                SubExpRes mempty (Var elems)
                              ]
                          _ -> error "mergeOneLifted: mismatched reps"

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
                    (toExp $ pe64 active_size .==. 0)
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
      let out_reps = loopResultToResReps res loop_out_vs'
      pure $ insertReps (zip (map distResTag res) out_reps) env
  where
    lvl = flattenSegLevel ops
