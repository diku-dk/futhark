-- | Flattening of 'Match'.
module Futhark.Pass.Flatten.Match
  ( transformMatch,
  )
where

import Control.Monad
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Tuple.Solo
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Monad
import Futhark.Tools

-- Take the elements at index `is` from an input `v`.
splitInput ::
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  VName ->
  Builder GPU (Type, VName, ResRep)
splitInput segments env inps is v = do
  (t, rep) <- liftSubExpPreserveRep segments inps env (Var v)
  (t,v,) <$> case rep of
    Regular arr -> do
      -- In the regular case we just take the elements
      -- of the array given by `is`
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      arr' <- letExp "split_arr" <=< segMap (MkSolo n) $ \(MkSolo i) -> do
        idx <- letSubExp "idx" =<< eIndex is [eSubExp i]
        -- unflatten index
        let arr_is = unflattenIndex (segmentDims segments) (pe64 idx)
        subExpsRes . pure <$> (letSubExp "arr" =<< eIndex arr (map toExp arr_is))
      pure $ Regular arr'
    Irregular (IrregularRep segs flags offsets elems _) -> do
      -- In the irregular case we take the elements
      -- of the `segs` array given by `is` like in the regular case
      n <- letSubExp "n" =<< (toExp . arraySize 0 =<< lookupType is)
      segs' <- letExp "split_segs" <=< segMap (MkSolo n) $ \(MkSolo i) -> do
        idx <- letExp "idx" =<< eIndex is [eSubExp i]
        subExpsRes . pure <$> (letSubExp "segs" =<< eIndex segs [toExp idx])
      -- From this we calculate the offsets and number of elements
      (_, offsets', num_data) <- exScanAndSum segs'
      (_, _, ii1) <- doRepIota segs'
      (_, _, ii2) <- doSegIota segs'
      -- We then take the elements we need from `elems` and `flags`
      -- For each index `i`, we roughly:
      -- Get the offset of the segment we want to copy by indexing
      -- `offsets` through `is` further through `ii1` i.e.
      -- `offset = offsets[is[ii1[i]]]`
      -- We then add `ii2[i]` to `offset`
      -- and use that to index into `elems` and `flags`.
      ~[flags', elems'] <- letTupExp "split_F_data" <=< segMap (MkSolo num_data) $ \(MkSolo i) -> do
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
  Segments ->
  DistEnv ->
  DistInputs ->
  VName ->
  Body SOACS ->
  Builder GPU (DistInputs, DistEnv, [DistStm])
distributeBranch segments env inps is body = do
  let free_in_body = filter (isVariant inps env . Var) (namesToList $ freeIn body)
  (ts, vs, reps) <-
    unzip3 <$> mapM (splitInput segments env inps is) free_in_body
  let inputs = do
        (v, t, i) <- zip3 vs ts [0 ..]
        pure (v, DistInput (ResTag i) t)
  let env' = DistEnv $ M.fromList $ zip (map ResTag [0 ..]) reps
  scope <- askScope
  let (inputs', dstms) = distributeBody scope segments inputs body
  pure (inputs', env', dstms)

-- Given a single result from each branch as well the *unlifted*
-- result type, merge the results of all branches into a single result.
mergeResult ::
  Segments ->
  SubExp ->
  [VName] ->
  [ResRep] ->
  DistResult ->
  Builder GPU ResRep
mergeResult segments w iss branchesRep dist_res
  -- Regular case
  | isRegularDistResult dist_res = do
      let (DistType _ _ resType) = distResType dist_res
          resultType =
            Array (elemType resType) (Shape [w] <> arrayShape resType) NoUniqueness
      xs <- mapM regularBranch branchesRep
      -- Create the blank space for the result
      resultSpace <- letExp "blank_res" =<< eBlank resultType
      -- Write back the values of each branch to the blank space
      result <- foldM scatterRegular resultSpace $ zip iss xs
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
      segs <- foldM scatterRegular segsSpace $ zip iss (irregularS <$> branchesIrregRep)
      (_, offsets, num_data) <- exScanAndSum segs
      let resultType = Array pt (Shape [num_data]) NoUniqueness
      -- Create the blank space for the result
      resultSpace <- letExp "blank_res" =<< eBlank resultType
      -- Write back the values of each branch to the blank space
      elems <- foldM (scatterIrregular offsets) resultSpace $ zip iss branchesIrregRep
      flags <- genFlags num_data offsets
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

transformMatch ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  [SubExp] ->
  [Case (Body SOACS)] ->
  Body SOACS ->
  Builder GPU DistEnv
transformMatch ops segments env inps res scrutinees cases defaultCase = do
  w <- letSubExp "w" <=< toExp $ product $ segmentDims segments
  -- We need to partition the indices of the scrutinees by which case they match.
  -- Lift the scrutinees.
  -- If it's a variable, we know it's a scalar and the lifted version will therefore be a regular array.
  lifted_scrutinees <- forM scrutinees $ \scrut -> do
    liftSubExpRegular segments inps env (segmentsShape segments) scrut
  -- Cases for tagging values that match the same branch.
  -- The default case is the 0'th equvalence class.
  let equiv_cases =
        zipWith
          (\(Case pat _) n -> Case pat $ eBody [toExp $ intConst Int64 n])
          cases
          [1 ..]
  let equiv_case_default = eBody [toExp $ intConst Int64 0]
  -- Match the scrutinees againts the branch cases
  equiv_classes <- letExp "equiv_classes" <=< segMap (MkSolo w) $ \(MkSolo i) -> do
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
  (partition_sizes, partition_offs, partition_inds) <- doPartition n_cases equiv_classes
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
  (branch_inputs, branch_envs, branch_dstms) <-
    unzip3 <$> zipWithM (distributeBranch segments env inps) inds branch_bodies

  let branch_results = map bodyResult branch_bodies
  branch_reps <- forM [0 .. num_cases - 1] $ \i -> do
    let inputs = branch_inputs !! fromIntegral i
    let env' = branch_envs !! fromIntegral i
    let dstms = branch_dstms !! fromIntegral i
    let result = branch_results !! fromIntegral i
        branch_segments = NE.singleton $ branch_sizes !! fromIntegral i
    env'' <- foldM (flattenDistStm ops branch_segments) env' dstms
    zipWithM (liftDistResultRep branch_segments inputs env'') res result

  -- Merge the results of the branches and insert the resulting res reps
  reps <- zipWithM (mergeResult segments w inds) (L.transpose branch_reps) res
  pure $ insertReps (zip (map distResTag res) reps) env
