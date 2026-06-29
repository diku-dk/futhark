-- | Flattening of 'Match'.
module Futhark.Pass.Flatten.Match
  ( transformVariantMatch,
    transformUniformMatch,
  )
where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Monad
import Futhark.Tools

-- Take the elements at index `is` from an input `v`.
splitInput ::
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  M.Map VName ResRep ->
  VName ->
  FlattenM (Type, VName, ResRep)
splitInput lvl segments env inps is acc_reps v = do
  (t, rep0) <- liftSubExpPreserveRep segments inps env (Var v)
  let rep = M.findWithDefault rep0 v acc_reps
  (t,v,) <$> case rep of
    Regular arr -> do
      if isAcc t
        then
          pure $ Regular arr
        else do
          -- In the regular case we just take the elements
          -- of the array given by `is`
          n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
          arr' <- letExp "split_arr" <=< segMap lvl (MkSolo n) $ \(MkSolo i) -> do
            idx <- letSubExp "idx" =<< eIndex is [eSubExp i]
            -- unflatten index
            let arr_is = unflattenIndex (segmentDims segments) (pe64 idx)
            subExpsRes . pure <$> (letSubExp "arr" =<< eIndex arr (map toExp arr_is))
          pure $ Regular arr'
    Irregular (IrregularRep segs flags offsets elems _) -> do
      -- In the irregular case we take the elements
      -- of the `segs` array given by `is` like in the regular case
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      segs' <- letExp "split_segs" <=< segMap lvl (MkSolo n) $ \(MkSolo i) -> do
        idx <- letExp "idx" =<< eIndex is [eSubExp i]
        subExpsRes . pure <$> (letSubExp "segs" =<< eIndex segs [toExp idx])
      -- From this we calculate the offsets and number of elements
      (_, offsets', num_data) <- exScanAndSum lvl segs'
      (_, _, ii1) <- doRepIota lvl segs'
      (_, _, ii2) <- doSegIota lvl segs'
      -- We then take the elements we need from `elems` and `flags`
      -- For each index `i`, we roughly:
      -- Get the offset of the segment we want to copy by indexing
      -- `offsets` through `is` further through `ii1` i.e.
      -- `offset = offsets[is[ii1[i]]]`
      -- We then add `ii2[i]` to `offset`
      -- and use that to index into `elems` and `flags`.
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

-- Given the indices for which a branch is taken and its body,
-- distribute the statements of the body of that branch.
distributeBranch ::
  FunHasParallelism ->
  SegLevel ->
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  Body SOACS ->
  M.Map VName ResRep ->
  FlattenM (DistInputs, DistEnv, [DistStm])
distributeBranch funHasParallelism lvl segments env inps is body acc_reps = do
  let free_in_body = filter (isVariant inps . Var) (namesToList $ freeIn body)
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
  let (inputs', dstms) = distributeBody funHasParallelism scope segments inputs body
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
          xs <- mapM regularBranch branchesRep
          pure $ Regular $ last xs
        else do
          let resultType = Array (elemType resType) (Shape [w] <> arrayShape resType) NoUniqueness
          xs <- mapM regularBranch branchesRep
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
      branchesIrregRep <- mapM irregularBranch branchesRep
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
    regularBranch (Regular v) = pure v
    regularBranch _ = error "mergeResult: mismatched reps"

    irregularBranch (Irregular irreg) = pure irreg
    irregularBranch _ = error "mergeResult: mismatched reps"

transformVariantMatch ::
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
transformVariantMatch ops segments env inps res _aux scrutinees cases defaultCase _rt = do
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
  -- acc inputs are handled differently, each breanch use the result of the previous branch
  (branch_reps, _) <-
    foldM
      ( \(branch_reps_acc, acc_reps) (branch_size, branch_inds, body, result) -> do
          let branch_segments = NE.singleton branch_size
          (inputs, env', dstms) <-
            distributeBranch (flattenFunHasParallelism ops) lvl segments env inps branch_inds body acc_reps
          env'' <- foldM (flattenDistStm ops branch_segments) env' dstms
          reps <- zipWithM (liftDistResultRep lvl branch_segments inputs env'') res result
          let acc_reps' = replaceAccReps acc_reps reps
          pure (branch_reps_acc <> [reps], acc_reps')
      )
      ([], M.empty)
      (L.zip4 branch_sizes inds branch_bodies branch_results)
  -- Merging acc results is done by using the last branch result
  reps <- zipWithM (mergeResult lvl segments w inds) (L.transpose branch_reps) res
  pure $ insertReps (zip (map distResTag res) reps) env
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

transformUniformMatch ::
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
transformUniformMatch ops segments env inps res aux scrutinees cases defaultCase rt = do
  scope <- askScope
  new_cases <- forM cases $ \(Case c body) -> do
    let (case_body_inputs, case_dstms) =
          distributeBody (flattenFunHasParallelism ops) scope segments inps body

    (case_body_res, case_body_stms) <-
      collectStms $
        liftBodyWithDistResults ops segments case_body_inputs env case_dstms res (bodyResult body)
    pure $ Case c $ Body () case_body_stms case_body_res
  new_default_body <- do
    let (new_default_body_inputs, new_default_dstms) =
          distributeBody (flattenFunHasParallelism ops) scope segments inps defaultCase
    (new_default_body_res, new_default_body_stms) <-
      collectStms $
        liftBodyWithDistResults ops segments new_default_body_inputs env new_default_dstms res (bodyResult defaultCase)
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
