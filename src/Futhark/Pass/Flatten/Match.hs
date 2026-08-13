-- | Flattening of 'Match'.
module Futhark.Pass.Flatten.Match
  ( flattenMatch,
  )
where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.General
import Futhark.Tools

-- Given the indices for which a branch is taken and its body,
-- distribute the statements of the body of that branch.
distributeBranch ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  Body SOACS ->
  M.Map VName ResRep ->
  FlattenM (DistInputs, DistEnv, DistStms)
distributeBranch ops segments env inps is body acc_reps = do
  let lvl = flattenSegLevel ops
      free_in_body = filter (isVariant inps . Var) (namesToList $ freeIn body)
  scope <- askScope
  free_sizes <-
    foldMap freeIn <$> mapM (lookupInputType inps) free_in_body
  let free_variant_sizes = filter (isVariant inps . Var) (namesToList free_sizes)
      free_size_vars = nubOrd (free_variant_sizes <> free_in_body)
  (ts, vs, reps) <-
    unzip3 <$> mapM (splitInput lvl segments env inps is acc_reps) free_size_vars
  let inputs = do
        (v, t, i) <- zip3 vs ts [0 ..]
        pure (v, DistInput (ResTag i) t)
  let env' = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
  let (inputs', dstms) = distributeBodyWith ops scope segments inputs body
  pure (inputs', env', dstms)

-- Given a single result from each branch as well the *unlifted*
-- result type, merge the results of all branches into a single result.
mergeResult ::
  SegLevel ->
  Segments ->
  SubExp ->
  [VName] ->
  [ResRep] ->
  DistResult ->
  FlattenM ResRep
mergeResult lvl segments w iss branchesRep dist_res
  -- Regular case
  | isRegularDistResult dist_res = do
      let (DistType _ _ resType) = distResType dist_res
      if isAcc resType
        then do
          xs <- mapM asRegular branchesRep
          pure $ Regular $ last xs
        else do
          let resultType = Array (elemType resType) (Shape [w] <> arrayShape resType) NoUniqueness
          xs <- mapM asRegular branchesRep
          -- Create the blank space for the result
          resultSpace <- letExp "blank_res" =<< eBlank resultType
          -- Write back the values of each branch to the blank space
          result <- foldM (scatterRegular lvl) resultSpace $ zip iss xs
          result_t <- arrayShape <$> lookupType result
          result' <-
            letExp "match_res_reg" . BasicOp $
              Reshape result (reshapeAll result_t (segmentsShape segments <> arrayShape resType))
          pure $ Regular result'
  -- Irregular case
  | DistType _ _ (Array pt _ _) <- distResType dist_res = do
      branchesIrregRep <- mapM asIrregular branchesRep
      let segsType = Array (IntType Int64) (Shape [w]) NoUniqueness
      -- Create a blank space for the 'segs'
      segsSpace <- letExp "blank_segs" =<< eBlank segsType
      -- Write back the segs of each branch to the blank space
      segs <- foldM (scatterRegular lvl) segsSpace $ zip iss (irregularS <$> branchesIrregRep)
      (_, offsets, num_data) <- exScanAndSum lvl segs
      let resultType = Array pt (Shape [num_data]) NoUniqueness
      -- Create the blank space for the result
      resultSpace <- letExp "blank_res" =<< eBlank resultType
      -- Write back the values of each branch to the blank space
      elems <- foldM (scatterIrregular lvl offsets) resultSpace $ zip iss branchesIrregRep
      flags <- genFlags lvl num_data offsets
      pure $
        Irregular $
          IrregularRep
            { irregularS = segs,
              irregularF = flags,
              irregularO = offsets,
              irregularD = elems,
              irregularK = Dense
            }
  | otherwise = error "mergeResult: non-array irregular result"
  where
    asRegular (Regular v) = pure v
    asRegular _ = error "mergeResult: mismatched reps"

    asIrregular (Irregular irreg) = pure irreg
    asIrregular _ = error "mergeResult: mismatched reps"

-- | Flatten a single branch body of a variant 'Match', but guard its execution
-- on the branch actually being taken by some segment. When a branch receives no
-- segments (its partition is empty) we must not run its flattened code: it may
-- call lifted recursive functions, which would recurse forever on an empty
-- batch. An untaken branch's results are never read ('mergeResult' scatters
-- them back through the branch's empty index array), so we just yield blanks.
--
-- Like 'flattenUniformMatch', the branch is lifted to a 'Result' of flat rep
-- components and the reps recovered with 'distResultsToResReps'; here we
-- additionally wrap it in a @branch_size > 0@ 'Match'.
guardBranch ::
  FlattenOps ->
  SubExp ->
  DistEnv ->
  DistInputs ->
  DistStms ->
  [DistResult] ->
  Result ->
  FlattenM [ResRep]
guardBranch ops branch_size env inputs dstms res result = do
  let branch_segments = [branch_size]
  (taken_body, taken_types) <-
    buildBody $ do
      body_res <- liftBodyWithDistResults ops branch_segments inputs env dstms res result
      ts <- mapM (subExpType . resSubExp) body_res
      pure (body_res, ts)
  -- Blanks for the untaken branch have the same types as the taken branch.
  -- Sizes bound inside the branch (the length of irregular data) are not in
  -- scope here, so we zero them; the 'Match' then makes them existential.
  untaken_body <- buildBody_ $ do
    let blank t = t `setArrayShape` Shape (map (const (intConst Int64 0)) (arrayDims t))
    subExpsRes <$> mapM (letSubExp "blank" <=< eBlank . blank) taken_types
  match_e <-
    eIf
      (eCmpOp (CmpSlt Int64) (eSubExp (intConst Int64 0)) (eSubExp branch_size))
      (pure taken_body)
      (pure untaken_body)
  match_res <- letTupExp "guarded_branch" match_e
  rets <- expExtType match_e
  pure $ distResultsToResReps res $ drop (S.size (shapeContext rets)) match_res

flattenVariantMatch ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  StmAux () ->
  [SubExp] ->
  [Case (Body SOACS)] ->
  Body SOACS ->
  MatchDec ExtType ->
  FlattenM DistEnv
flattenVariantMatch ops segments env inps res _aux scrutinees cases defaultCase _rt = do
  let lvl = flattenSegLevel ops
  w <- letSubExp "w" <=< toExp $ product $ segmentDims segments
  -- We need to partition the indices of the scrutinees by which case they match.
  -- Lift the scrutinees.
  -- If it's a variable, we know it's a scalar and the lifted version will therefore be a regular array.
  lifted_scrutinees <- forM scrutinees $ \scrut -> do
    liftSubExpRegular lvl segments inps env (segmentsShape segments) scrut
  -- Cases for tagging values that match the same branch.
  -- The default case is the 0'th equvalence class.
  let equiv_cases =
        zipWith
          (\(Case pat _) n -> Case pat $ eBody [toExp $ intConst Int64 n])
          cases
          [1 ..]
  let equiv_case_default = eBody [toExp $ intConst Int64 0]
  -- Match the scrutinees againts the branch cases
  equiv_classes <- letExp "equiv_classes" <=< segMap lvl (MkSolo w) $ \(MkSolo i) -> do
    -- unflatten index
    let seg_is = unflattenIndex (segmentDims segments) (pe64 i)
    scruts <- mapM (letSubExp "scruts" <=< flip eIndex (map toExp seg_is)) lifted_scrutinees
    cls <- letSubExp "cls" =<< eMatch scruts equiv_cases equiv_case_default
    pure [subExpRes cls]
  let num_cases = fromIntegral $ length cases + 1
  n_cases <- letExp "n_cases" <=< toExp $ intConst Int64 num_cases
  -- Parition the indices of the scrutinees by their equvalence class such
  -- that (the indices) of the scrutinees belonging to class 0 come first,
  -- then those belonging to class 1 and so on.
  (partition_sizes, partition_offs, partition_inds) <- doPartition lvl n_cases equiv_classes
  inds_t <- lookupType partition_inds
  -- Get the indices of each scrutinee by equivalence class
  branch_info <- forM [0 .. num_cases - 1] $ \i -> do
    num_data <-
      letSubExp ("size" <> nameFromString (show i))
        =<< eIndex partition_sizes [toExp $ intConst Int64 i]
    begin <-
      letSubExp ("idx_begin" <> nameFromString (show i))
        =<< eIndex partition_offs [toExp $ intConst Int64 i]
    inds <-
      letExp ("inds_branch" <> nameFromString (show i)) $
        BasicOp . Index partition_inds $
          fullSlice inds_t [DimSlice begin num_data (intConst Int64 1)]
    pure (num_data, inds)
  let (branch_sizes, inds) = unzip branch_info

  -- Distribute and lift the branch bodies.
  -- We put the default case at the start as it's the 0'th equivalence class
  -- and is therefore the first segment after the partition.
  let branch_bodies = defaultCase : map (\(Case _ body) -> body) cases
  let branch_results = map bodyResult branch_bodies
  -- Accumulator results are threaded from one branch to the next and cannot be
  -- blanked, so we only guard branch execution when no accumulators are
  -- involved. XXX: can we be sure this will never be a problem?
  let hasAcc = any (\dr -> case distResType dr of DistType _ _ t -> isAcc t) res
  -- acc inputs are handled differently, each branch use the result of the previous branch
  (branch_reps, _) <-
    foldM
      ( \(branch_reps_acc, acc_reps) (branch_size, branch_inds, body, result) -> do
          let branch_segments = [branch_size]
          (inputs, env', dstms) <-
            distributeBranch ops segments env inps branch_inds body acc_reps
          reps <-
            if hasAcc
              then do
                env'' <- foldM (flattenDistStm ops branch_segments) env' dstms
                zipWithM (liftDistResultRep lvl branch_segments inputs env'') res result
              else guardBranch ops branch_size env' inputs dstms res result
          let acc_reps' = replaceAccReps acc_reps reps
          pure (branch_reps_acc <> [reps], acc_reps')
      )
      ([], M.empty)
      (L.zip4 branch_sizes inds branch_bodies branch_results)
  -- Merging acc results is done by using the last branch result
  reps <- zipWithM (mergeResult lvl segments w inds) (L.transpose branch_reps) res
  insertRepsM (zip (map distResTag res) reps) env
  where
    findAccCert :: VName -> (VName, DistInput) -> Maybe VName
    findAccCert cert v_inp =
      let (v, inp) = v_inp
       in if isAcc (distInputType inp)
            then case distInputType inp of
              Acc cert' _ _ _ | cert == cert' -> Just v
              _ -> Nothing
            else Nothing

    -- Idealy this should be a singleton
    findAccCerts :: VName -> [VName]
    findAccCerts cert = mapMaybe (findAccCert cert) inps

    replaceAccRep acc_reps (dist_res, rep) =
      let (DistType _ _ t) = distResType dist_res
       in if not $ isAcc t
            then
              acc_reps
            else
              let (Acc cert _ _ _) = t
                  accVars = findAccCerts cert
               in foldl (\m v -> M.insert v rep m) acc_reps accVars
    replaceAccReps acc_reps reps = foldl replaceAccRep acc_reps $ zip res reps

flattenUniformMatch ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  StmAux () ->
  [SubExp] ->
  [Case (Body SOACS)] ->
  Body SOACS ->
  MatchDec ExtType ->
  FlattenM DistEnv
flattenUniformMatch ops segments env inps res aux scrutinees cases defaultCase rt = do
  scope <- askScope
  new_cases <- forM cases $ \(Case c body) -> do
    let (case_body_inputs, case_dstms) =
          distributeBodyWith ops scope segments inps body

    fmap (Case c) . buildBody_ $
      liftBodyWithDistResults ops segments case_body_inputs env case_dstms res (bodyResult body)
  new_default_body <- do
    let (new_default_body_inputs, new_default_dstms) =
          distributeBodyWith ops scope segments inps defaultCase
    buildBody_ $
      liftBodyWithDistResults ops segments new_default_body_inputs env new_default_dstms res (bodyResult defaultCase)

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
  insertRepsM (zip (map distResTag res) reps) env

-- | Flatten a 'Match'
flattenMatch ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  StmAux () ->
  [SubExp] ->
  [Case (Body SOACS)] ->
  Body SOACS ->
  MatchDec ExtType ->
  FlattenM DistEnv
flattenMatch ops segments env inps res aux scrutinees cases defaultCase rt =
  -- 'flattenUniformMatch' keeps the scrutinees in a plain GPU 'Match', which is
  -- only well-scoped when they are invariant to the nest. Whenever a scrutinee is
  -- variant we must partition the segments by branch, even if no branch contains
  -- parallelism (this happens e.g. for a variant conditional with an irregular
  -- result, which cannot be sequentialised into a scalar group).
  if any (isVariant inps) scrutinees
    then flattenVariantMatch ops segments env inps res aux scrutinees cases defaultCase rt
    else flattenUniformMatch ops segments env inps res aux scrutinees cases defaultCase rt
