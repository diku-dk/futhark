module Futhark.Pass.Flatten.Loop
  ( transformLoop,
  )
where

import Control.Monad
import Control.Monad.Reader (runReaderT)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify (simplifyStms)
import Futhark.MonadFreshNames
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.General
import Futhark.Tools
import Prelude hiding (div, quot, rem)

-- | Is this dimension variant to the loop or the outer map context -
-- either because it is itself a loop parameter, or because it is
-- variant in the outer map nest?
variantDim :: DistInputs -> S.Set VName -> SubExp -> Bool
variantDim _ _ Constant {} = False
variantDim inps loopParamNames (Var v) =
  v `S.member` loopParamNames || isVariant inps (Var v)

-- Check whether a loop parameter array needs irregular representation.
-- we need the irregular representation when any of its dimensions are either:
-- a loop parameter name or variant in the outer map context
needsIrregular :: DistInputs -> S.Set VName -> DeclType -> Bool
needsIrregular inps loopParamNames t =
  case t of
    Array {} -> any (variantDim inps loopParamNames) (arrayDims t)
    _ -> False

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
  FlattenM ([FParam GPU], ResRep, [SubExp])
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
      | needsIrregular inps loopParamNames t -> do
          (params, rep) <- liftParam num_segments fparam
          (_, initRep) <- liftSubExp lvl segments inps env initSE
          irreg <- case initRep of
            -- This will not happen.
            Regular v -> mkIrregFromReg lvl segments v
            Irregular irreg -> pure irreg
          initVals <- irregularRepToFlatArrs num_segments irreg
          pure (params, rep, map Var initVals)
      | otherwise -> do
          -- Regular case: all dims are invariant, just add w as outermost dim
          let pShape = segmentsShape segments <> arrayShape t
          p <-
            newParam
              (baseName (paramName fparam) <> "_lifted")
              (arrayOf (Prim pt) pShape u)
          initV <- liftSubExpRegular lvl segments inps env pShape initSE
          -- If the parameter is consumed, we must not consume the
          -- representation array (it may be used by other versions in
          -- multi-versioned code), so insert a copy. The simplifier hopefully
          -- removes it again when consuming the representation directly is
          -- safe.
          initV' <-
            if u == Unique
              then letExp (baseName (paramName fparam) <> "_inter_copy") =<< eCopy (eVar initV)
              else pure initV
          pure ([p], Regular $ paramName p, [Var initV'])
    Acc {} -> do
      initV <- liftSubExpRegular lvl segments inps env mempty initSE
      let Param attrs v acc_t = fparam
      param <- Param attrs <$> newName v <*> pure acc_t
      pure ([param], Regular $ paramName param, [Var initV])
    Mem {} ->
      error "liftLoopParam: Mem"

-- | Construct the body of an interchanged uniform loop: a single
-- Screma mapping the original loop body over the lifted loop
-- parameters (and any other inputs used by the body), transformed as
-- if it were a top-level statement - in particular, it is subject to
-- multi-versioning. The lambda parameters reuse the original names,
-- so the body can be used unchanged. Only usable when all involved
-- values are regular.
interchangedLoopBody ::
  FlattenOps ->
  SubExp ->
  Segments ->
  DistEnv ->
  [(FParam SOACS, FParam GPU)] ->
  DistInputs ->
  StmAux () ->
  Body SOACS ->
  FlattenM (Body GPU)
interchangedLoopBody ops num_segments segments env params free_inps aux body = buildBody_ $ do
  let flatInput name arr t = do
        arr_t <- lookupType arr
        letExp (baseName name <> "_flat") . BasicOp . Reshape arr $
          reshapeAll (arrayShape arr_t) (Shape [num_segments] <> arrayShape t)
      inputArr (DistInputFree arr _) = arr
      inputArr (DistInput rt _) = case resVar rt env of
        Regular arr -> arr
        Irregular {} -> error "interchangedLoopBody: irregular input"
  param_arrs <- forM params $ \(p, lifted_p) ->
    flatInput (paramName p) (paramName lifted_p) (fromDecl (declTypeOf p))
  free_arrs <- forM free_inps $ \(v, inp) ->
    flatInput v (inputArr inp) (distInputType inp)

  let lam_params =
        [Param mempty (paramName p) (fromDecl (declTypeOf p)) | (p, _) <- params]
          ++ [Param mempty v (distInputType inp) | (v, inp) <- free_inps]
      row_ts = [fromDecl (declTypeOf p) | (p, _) <- params]
      lam = Lambda lam_params row_ts body
  pes <- forM (zip params row_ts) $ \((p, _), t) ->
    PatElem
      <$> newName (paramName p)
      <*> pure (t `arrayOfRow` num_segments)
  form <- mapSOAC lam
  let map_stm :: Stm SOACS
      map_stm =
        Let (Pat pes) (aux {stmAuxCerts = mempty}) $
          Op $
            Screma num_segments (param_arrs ++ free_arrs) form

  -- Simplify before transforming. Apart from generally producing
  -- better code, this hoists statements that are invariant to the
  -- mapped values out of the Screma, and in particular any sizes
  -- they compute must be in scope when the Screma is versioned
  -- (e.g. for deciding intrablock feasibility).
  scope <- castScope <$> askScope
  map_stms <- runReaderT (simplifyStms (oneStm map_stm)) (scope :: Scope SOACS)
  mapM_ (flattenTopLevelStm ops) map_stms
  fmap (map (SubExpRes mempty . Var)) . forM (zip pes params) $ \(pe, (p, _)) -> do
    pe_t <- lookupType (patElemName pe)
    let seg_shape = segmentsShape segments <> arrayShape (fromDecl (declTypeOf p))
    letExp (baseName (paramName p) <> "_unflat") . BasicOp . Reshape (patElemName pe) $
      reshapeAll (arrayShape pe_t) seg_shape

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

liftLoopResult :: SegLevel -> Segments -> SubExp -> DistInputs -> DistEnv -> DistResult -> SubExpRes -> FlattenM Result
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
        varsRes <$> irregularRepToFlatArrs num_segments irreg
      _ -> undefined

-- | Distribute the loop body statement by statement and lift the
-- distributed statements, producing the statements and result of the
-- body of the lifted loop. The provided scope is that of the lifted
-- loop parameters (and any loop index); it is brought into scope only
-- after distribution, as the original body cannot reference it.
distributedLoopBody ::
  FlattenOps ->
  Segments ->
  SubExp ->
  Scope GPU ->
  DistInputs ->
  DistEnv ->
  [DistResult] ->
  Body SOACS ->
  FlattenM (Body GPU)
distributedLoopBody ops segments num_segments loop_scope inputs env res body = do
  scope <- askScope
  let lvl = flattenSegLevel ops
      (inputs', dstms) =
        distributeBody (distIrregularityAtLevel lvl) (flattenFunHasParallelism ops) scope segments inputs body
  buildBody_ $ localScope loop_scope $ do
    env' <- foldM (flattenDistStm ops segments) env dstms
    concat <$> zipWithM (liftLoopResult lvl segments num_segments inputs' env') res (bodyResult body)

-- | Make the original loop parameters available as distribution
-- inputs for the loop body, mapped to their lifted representations.
loopBodyInputs :: DistEnv -> DistInputs -> [FParam SOACS] -> [ResRep] -> (DistInputs, DistEnv)
loopBodyInputs env inps old_loop_params lifted_loop_reps =
  let (inps_local, env_local, next) = localiseInputs env inps
      loop_param_inputs =
        zipWith
          (\p j -> (paramName p, DistInput (ResTag j) (paramType p)))
          old_loop_params
          [next ..]
      loop_param_reps =
        zipWith (\j rep -> (ResTag j, rep)) [next ..] lifted_loop_reps
   in ( inps_local <> loop_param_inputs,
        insertReps loop_param_reps env_local
      )

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
  FlattenM DistEnv
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

  reps <- distributeAndFlattenBody ops segments "for_variant_res" env inps res synthetic_body
  pure $ insertReps (zip (map distResTag res) reps) env

transformLoop ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  (Pat Type, StmAux ()) ->
  ([(Param DeclType, SubExp)], LoopForm, Body SOACS) ->
  FlattenM DistEnv
transformLoop ops segments env inps res (_pat, aux) (merge, ForLoop i it n, body) = do
  if isVariant inps n
    then transformForToWhile ops segments env inps res aux merge i it n body
    else do
      let old_loop_params = map fst merge
          loopParamNames = S.fromList $ map paramName old_loop_params

      num_segments <- letSubExp "num_segments" =<< toExp (segmentCount segments)
      (lifted_loop_params, lifted_loop_reps, lifted_init) <-
        unzip3 <$> mapM (liftLoopParam (flattenSegLevel ops) segments num_segments inps env loopParamNames) merge

      let lifted_loop_params' = concat lifted_loop_params
          lifted_init' = concat lifted_init
          (loop_new_inputs, loop_env_local) =
            loopBodyInputs env inps old_loop_params lifted_loop_reps

      let i_param = Param mempty i (Prim (IntType it))
          build_scope = scopeOfFParams lifted_loop_params' <> scopeOfLParams [i_param]

      -- When the loop parameters and all inputs used by the body are regular,
      -- the interchange of the map nest and the loop corresponds to a perfectly
      -- ordinary Screma inside the loop. We then transform that Screma as if
      -- that was what the program looked like in the first place, which in
      -- particular means it is subject to multi-versioning. Otherwise we
      -- distribute the loop body statement by statement.
      let body_free = freeIn body
          free_inps =
            [ (v, inp)
            | (v, inp) <- inps,
              v `nameIn` body_free,
              not $ v `S.member` loopParamNames
            ]
          regularInput (_, inp) =
            not (any (variantDim inps loopParamNames) (arrayDims (distInputType inp)))
              && case inp of
                DistInputFree {} -> True
                DistInput rt _ -> case resVar rt env of
                  Regular {} -> True
                  Irregular {} -> False
          regularRep Regular {} = True
          regularRep Irregular {} = False
          simpleParam p = case declTypeOf p of
            Prim {} -> True
            Array {} -> True
            _ -> False
          -- The interchanged Screma is transformed as a top-level
          -- statement, so this is only possible when we are not
          -- generating in-block code.
          at_host_level = case flattenSegLevel ops of
            SegThreadInBlock {} -> False
            _ -> True
          interchangeable =
            at_host_level
              -- Parameters with variant dimensions are lifted to an
              -- irregular representation, so this also rejects those.
              && all regularRep lifted_loop_reps
              && all isRegularDistResult res
              && all simpleParam old_loop_params
              && all regularInput free_inps

      loop_body_gpu <-
        if interchangeable
          then
            localScope build_scope $
              interchangedLoopBody
                ops
                num_segments
                segments
                env
                (zip old_loop_params lifted_loop_params')
                free_inps
                aux
                body
          else
            distributedLoopBody
              ops
              segments
              num_segments
              build_scope
              loop_new_inputs
              loop_env_local
              res
              body

      let loop_exp_gpu =
            Loop
              (zip lifted_loop_params' lifted_init')
              (ForLoop i it n)
              loop_body_gpu

      -- We must copy the result because otherwise we increase the degree of
      -- aliasing. In a loop, the result aliases the input, because it might run
      -- for zero iterations, but in the original program the result was
      -- produced by 'map', which has no aliases.
      loop_out_vs <-
        mapM (letExp "loop_res_out_copy" <=< eCopy . eVar)
          <=< certifying (distCerts inps aux env)
          $ letTupExp "loop_res_out" loop_exp_gpu

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
      loopParamNames = S.fromList $ map paramName old_loop_params
  w <- letSubExp "num_segments" =<< toExp (segmentCount segments)
  (lifted_loop_params, lifted_loop_reps, lifted_init) <-
    unzip3 <$> mapM (liftLoopParam lvl segments w inps env loopParamNames) merge

  let lifted_loop_params' = concat lifted_loop_params
      lifted_init' = concat lifted_init
      (loop_new_inputs, loop_env_local) =
        loopBodyInputs env inps old_loop_params lifted_loop_reps

  -- find cond_lifted_param in old_lifted_loop_params to get the lifted_loop_reps
  let maybe_cond = lookup cond (zip (map paramName old_loop_params) (zip lifted_loop_reps lifted_init))
  scope <- askScope
  case maybe_cond of
    -- infinite loop
    Nothing -> do
      loop_body_gpu <-
        distributedLoopBody ops segments w (scopeOfFParams lifted_loop_params') loop_new_inputs loop_env_local res body
      let loop_exp_gpu = Loop (zip lifted_loop_params' lifted_init') (WhileLoop cond) loop_body_gpu
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
      loop_body_gpu <-
        buildBody_ . localScope build_scope $ do
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
            (_, _, rep) <- splitInput lvl segments loop_env_local loop_new_inputs inactive_inds mempty (paramName p)
            pure rep

          let free_in_body =
                filter
                  (isVariant loop_new_inputs . Var)
                  (namesToList $ freeIn body)
          free_sizes <-
            foldMap freeIn <$> mapM (lookupInputType loop_new_inputs) free_in_body
          let free_variant_sizes = filter (isVariant loop_new_inputs . Var) (namesToList free_sizes)
              free_size_vars = nubOrd (free_variant_sizes <> free_in_body)
          (ts, vs, reps) <- unzip3 <$> mapM (splitInput lvl segments loop_env_local loop_new_inputs active_inds mempty) free_size_vars
          let subset_inputs = do
                (v, t, i) <- zip3 vs ts [0 ..]
                pure (v, DistInput (ResTag i) t)
              env_subset = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
          let subset_segments = NE.singleton active_size
          let (subset_inputs', subset_dstms) =
                distributeBody (distIrregularityAtLevel (flattenSegLevel ops)) (flattenFunHasParallelism ops) scope subset_segments subset_inputs body
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

      let merge' =
            zip
              (lifted_loop_params' ++ [any_active_param])
              (lifted_init' ++ [any_active_init])
      loop_out_vs <-
        certifying (distCerts inps aux env) $
          letTupExp "loop_res_out" $
            Loop
              merge'
              (WhileLoop (paramName any_active_param))
              loop_body_gpu
      let loop_out_vs' = L.init loop_out_vs
      let out_reps = loopResultToResReps res loop_out_vs'
      pure $ insertReps (zip (map distResTag res) out_reps) env
  where
    lvl = flattenSegLevel ops
