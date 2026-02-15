{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | Multicore code generation for SegScan. Uses a fairly naive multipass
-- algorithm, with no particular locality optimisations.
module Futhark.CodeGen.ImpGen.Multicore.SegScan
  ( compileSegScan,
  )
where

import Control.Monad
import Data.List (zip4)
import Data.Maybe (isNothing)
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Transform.Rename (renameBody)
import Futhark.Util.IntegralExp (divUp)
import Prelude hiding (quot, rem)

-- This does not correspond with the actual cache size, but the actual cache
-- size may not really be the optimal value either. Ideally this should be
-- exposed as a tuning parameter.
cacheSize :: Imp.TExp Int64
cacheSize = 65536 -- 64 KB

xParams, yParams :: SegBinOp MCMem -> [LParam MCMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

lamBody :: SegBinOp MCMem -> Body MCMem
lamBody = lambdaBody . segBinOpLambda

genBinOpParams :: [SegBinOp MCMem] -> MulticoreGen ()
genBinOpParams scan_ops =
  dScope Nothing $
    scopeOfLParams $
      concatMap (lambdaParams . segBinOpLambda) scan_ops

initialiseLocalPrefixes :: [SegBinOp MCMem] -> [[VName]] -> MulticoreGen ()
initialiseLocalPrefixes scan_ops per_op_prefix_var = do
  scan_ops_renamed <- renameSegBinOp scan_ops
  genBinOpParams scan_ops_renamed
  forM_ (zip scan_ops_renamed per_op_prefix_var) $ \(scan_op, prefix_vars) -> do
    sLoopNest (segBinOpShape scan_op) $ \vec_is ->
      forM_ (zip (segBinOpNeutral scan_op) prefix_vars) $ \(ne, prefix_var) -> do
        copyDWIMFix prefix_var vec_is ne []

updateLocalPrefixes :: [SegBinOp MCMem] -> [[VName]] -> [[VName]] -> [Imp.TExp Int64] -> MulticoreGen ()
updateLocalPrefixes scan_ops per_op_prefix_var per_op_prefix_arr index = do
  forM_ (zip3 scan_ops per_op_prefix_var per_op_prefix_arr) $ \(scan_op, prefix_vars, prefix_arrs) -> do
    let shape = segBinOpShape scan_op
    sLoopNest shape $ \vec_is ->
      forM_ (zip prefix_vars prefix_arrs) $ \(prefix_var, prefix_arr) -> do
        copyDWIMFix prefix_var vec_is (Var prefix_arr) (index ++ vec_is)

genArrays :: [SegBinOp MCMem] -> Name -> Shape -> MulticoreGen [[VName]]
genArrays scan_ops arr_name block_shape = do
  forM scan_ops $ \scan_op ->
    forM (lambdaReturnType $ segBinOpLambda scan_op) $ \t -> do
      let shape = block_shape <> segBinOpShape scan_op <> arrayShape t
      sAllocArray arr_name (elemType t) shape DefaultSpace

genLocalArray :: [SegBinOp MCMem] -> MulticoreGen [[VName]]
genLocalArray scan_ops = do
  forM scan_ops $ \scan_op -> do
    let shape = segBinOpShape scan_op
        ts = lambdaReturnType $ segBinOpLambda scan_op
    forM (zip (xParams scan_op) ts) $ \(p, t) -> do
      case shapeDims shape of
        [] -> pure $ paramName p
        _ -> do
          let pt = elemType t
          sAllocArray "local_acc" pt (shape <> arrayShape t) DefaultSpace

totalBytes :: [SegBinOp MCMem] -> Imp.TExp Int64
totalBytes scan_ops =
  sum
    [ bytesOfShape (elemType t) (arrayShape t <> scan_shape)
    | op <- scan_ops,
      let lam = segBinOpLambda op,
      let scan_shape = segBinOpShape op,
      t <- lambdaReturnType lam
    ]
  where
    bytesOfShape pt sh = primByteSize pt * product (map pe64 (shapeDims sh))

bodyHas :: (Exp MCMem -> Bool) -> GBody MCMem res -> Bool
bodyHas f = any (f' . stmExp) . bodyStms
  where
    f' e
      | f e = True
      | otherwise = isNothing $ walkExpM walker e
    walker =
      identityWalker
        { walkOnBody = const $ guard . not . bodyHas f
        }

-- | Determine whether this kernel body should be recomputed. Involves both
-- correctness checks and (crude) efficiency checks. Basically, recomputation is
-- only safe if nothing is consumed in the body, and considered efficient only
-- if the body has no loops.
shouldRecompute :: KernelBody MCMem -> Bool
shouldRecompute = not . bodyHas bad
  where
    bad (BasicOp Update {}) = True
    bad (BasicOp UpdateAcc {}) = True
    bad (WithAcc {}) = True
    bad Loop {} = True
    bad _ = False

hasAssert :: GBody MCMem res -> Bool
hasAssert = bodyHas isAssert
  where
    isAssert (BasicOp Assert {}) = True
    isAssert _ = False

copyFromDescToLocal :: [SegBinOp MCMem] -> [[VName]] -> [[VName]] -> TV Int64 -> MulticoreGen ()
copyFromDescToLocal scan_ops per_op_local_vars per_op_description_arrays index = do
  forM_ (zip3 scan_ops per_op_local_vars per_op_description_arrays) $ \(scan_op, local_vars, desc_arrs) -> do
    sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
      forM_ (zip local_vars desc_arrs) $ \(local_var, desc_arr) -> do
        copyDWIMFix local_var vec_is (Var desc_arr) (tvExp index : vec_is)

applyScanFromDescToLocal ::
  [SegBinOp MCMem] ->
  [[VName]] ->
  [[VName]] ->
  TV Int64 ->
  MulticoreGen ()
applyScanFromDescToLocal scan_ops per_op_local_vars per_op_description_arrays index = do
  scan_ops_renamed <- renameSegBinOp scan_ops
  genBinOpParams scan_ops_renamed
  forM_ (zip3 scan_ops_renamed per_op_local_vars per_op_description_arrays) $ \(scan_op, local_vars, description_array) -> do
    sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
      forM_ (zip (xParams scan_op) description_array) $ \(xs, desc_var) -> do
        copyDWIMFix (paramName xs) [] (Var desc_var) (tvExp index : vec_is)
      forM_ (zip (yParams scan_op) local_vars) $ \(ys, local_var) -> do
        copyDWIMFix (paramName ys) [] (Var local_var) vec_is
      compileStms mempty (bodyStms $ lamBody scan_op) $ do
        forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) local_vars) $ \(se, local_var) -> do
          copyDWIMFix local_var vec_is se []

applyScanAggregateToPrefix ::
  [SegBinOp MCMem] ->
  [[VName]] ->
  [[VName]] ->
  [[VName]] ->
  TV Int64 ->
  MulticoreGen ()
applyScanAggregateToPrefix scan_ops per_op_local_vars per_op_aggr_arrs prefArrs block_idx = do
  scan_ops_renamed <- renameSegBinOp scan_ops
  genBinOpParams scan_ops_renamed
  forM_ (zip4 scan_ops_renamed per_op_local_vars per_op_aggr_arrs prefArrs) $ \(scan_op, local_vars, aggr_arrs, prifix_arrs) -> do
    sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
      forM_ (zip (xParams scan_op) local_vars) $ \(xs, local_var) -> do
        copyDWIMFix (paramName xs) [] (Var local_var) vec_is
      forM_ (zip (yParams scan_op) aggr_arrs) $ \(ys, aggrArr) -> do
        copyDWIMFix (paramName ys) [] (Var aggrArr) (tvExp block_idx : vec_is)

      compileStms mempty (bodyStms $ lamBody scan_op) $ do
        forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) prifix_arrs) $ \(se, prefix_arr) -> do
          copyDWIMFix prefix_arr (tvExp block_idx : vec_is) se []

seqScanFastPath ::
  Pat LetDecMem ->
  VName ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  [[VName]] ->
  TV Int64 ->
  TV Int64 ->
  [[VName]] ->
  TV Int64 ->
  MulticoreGen ()
seqScanFastPath pat i scan_ops kbody per_op_prefixes_var start chunk_length per_op_prefix_arr block_idx = do
  kbody_renamed <- renameBody kbody
  scan_ops_renamed <- renameSegBinOp scan_ops
  genBinOpParams scan_ops_renamed
  per_op_local_accum <- genLocalArray scan_ops_renamed

  let results = bodyResult kbody_renamed
  let n_scan = segBinOpResults scan_ops_renamed
  let (all_scan_res, map_res) = splitAt n_scan results

  let per_scan_res = segBinOpChunks scan_ops_renamed all_scan_res
  let per_scan_pes = segBinOpChunks scan_ops_renamed $ patElems pat

  forM_ (zip3 scan_ops_renamed per_op_prefixes_var per_op_local_accum) $ \(scan_op, prefix_vars, local_accums) ->
    sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
      forM_ (zip local_accums prefix_vars) $ \(acc, prefix) -> do
        copyDWIMFix acc vec_is (Var prefix) vec_is

  z <- dPrimV "z" (0 :: Imp.TExp Int64)
  sWhile (tvExp z .<. tvExp chunk_length) $ do
    dPrimV_ i (tvExp start + tvExp z)

    compileStms mempty (bodyStms kbody_renamed) $ do
      let map_arrs = drop (segBinOpResults scan_ops_renamed) $ patElems pat

      sComment "write mapped values results to memory" $
        forM_ (zip (map patElemName map_arrs) (map kernelResultSubExp map_res)) $ \(arr, res) ->
          copyDWIMFix arr [tvExp start + tvExp z] res []

      forM_ (zip4 per_scan_pes scan_ops_renamed per_scan_res per_op_local_accum) $ \(pes, scan_op, scan_res, local_accums) ->
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
          forM_ (zip (xParams scan_op) local_accums) $ \(p, acc) ->
            copyDWIMFix (paramName p) [] (Var acc) vec_is
          forM_ (zip (yParams scan_op) scan_res) $ \(py, kr) ->
            copyDWIMFix (paramName py) [] (kernelResultSubExp kr) vec_is
          compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip3 local_accums (map resSubExp $ bodyResult $ lamBody scan_op) pes) $ \(acc, se, pe) -> do
              copyDWIMFix (patElemName pe) ((tvExp start + tvExp z) : vec_is) se []
              copyDWIMFix acc vec_is se []
    z <-- tvExp z + 1

    -- write back local accumulators to prefix arrays
    forM_ (zip3 scan_ops_renamed per_op_prefix_arr per_op_local_accum) $ \(scan_op, prefix_arrs, local_accums) ->
      sLoopNest (segBinOpShape scan_op) $ \vec_is ->
        forM_ (zip local_accums prefix_arrs) $ \(acc, prefix_arr) ->
          copyDWIMFix prefix_arr (tvExp block_idx : vec_is) (Var acc) vec_is

seqScanLB ::
  Pat LetDecMem ->
  VName ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  [[VName]] ->
  TV Int64 ->
  TV Int64 ->
  MulticoreGen ()
seqScanLB pat i scan_ops kbody per_op_prefixes_var start chunk_length = do
  kbody_renamed <- renameBody kbody
  scan_ops_renamed <- renameSegBinOp scan_ops
  genBinOpParams scan_ops_renamed
  per_op_local_accum <- genLocalArray scan_ops_renamed

  let results = bodyResult kbody_renamed
  let n_scan = segBinOpResults scan_ops_renamed
  let (all_scan_res, _) = splitAt n_scan results

  let per_scan_res = segBinOpChunks scan_ops_renamed all_scan_res
  let per_scan_pes = segBinOpChunks scan_ops_renamed $ patElems pat

  forM_ (zip3 scan_ops_renamed per_op_prefixes_var per_op_local_accum) $ \(scan_op, prefix_vars, local_accums) ->
    sLoopNest (segBinOpShape scan_op) $ \vec_is ->
      forM_ (zip local_accums prefix_vars) $ \(acc, prefix) ->
        copyDWIMFix acc vec_is (Var prefix) vec_is

  z <- dPrimV "z" (0 :: Imp.TExp Int64)
  sWhile (tvExp z .<. tvExp chunk_length) $ do
    dPrimV_ i (tvExp start + tvExp z)
    if shouldRecompute kbody_renamed
      then do
        compileStms mempty (bodyStms kbody_renamed) $ do
          forM_ (zip4 per_scan_pes scan_ops_renamed per_scan_res per_op_local_accum) $ \(pes, scan_op, scan_res, local_accums) ->
            sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
              forM_ (zip (xParams scan_op) local_accums) $ \(px, acc) ->
                copyDWIMFix (paramName px) [] (Var acc) vec_is
              forM_ (zip (yParams scan_op) scan_res) $ \(py, kr) ->
                copyDWIMFix (paramName py) [] (kernelResultSubExp kr) vec_is
              compileStms mempty (bodyStms $ lamBody scan_op) $
                forM_ (zip3 (map resSubExp $ bodyResult $ lamBody scan_op) pes local_accums) $ \(se, pe, acc) -> do
                  copyDWIMFix acc vec_is se []
                  copyDWIMFix (patElemName pe) ((tvExp start + tvExp z) : vec_is) se []
        z <-- tvExp z + 1
      else do
        forM_ (zip4 per_scan_pes scan_ops_renamed per_scan_res per_op_local_accum) $ \(pes, scan_op, _, local_accums) ->
          sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
            forM_ (zip (xParams scan_op) local_accums) $ \(px, acc) ->
              copyDWIMFix (paramName px) [] (Var acc) vec_is
            forM_ (zip (yParams scan_op) pes) $ \(py, pe) ->
              -- reading from output array
              copyDWIMFix (paramName py) [] (Var (patElemName pe)) ((tvExp start + tvExp z) : vec_is)
            compileStms mempty (bodyStms $ lamBody scan_op) $ do
              forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) pes) $ \(se, pe) ->
                copyDWIMFix (patElemName pe) ((tvExp start + tvExp z) : vec_is) se []
        z <-- tvExp z + 1

seqAggregate ::
  Pat LetDecMem ->
  VName ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int64 ->
  TV Int64 ->
  [[VName]] ->
  TV Int64 ->
  MulticoreGen ()
seqAggregate pat i scan_ops kbody start chunk_length per_op_aggr_arrs block_idx = do
  scan_ops_renamed <- renameSegBinOp scan_ops
  kbody_renamed <- renameBody kbody
  genBinOpParams scan_ops_renamed
  let results = bodyResult kbody_renamed
  let n_scan = segBinOpResults scan_ops_renamed
  let (all_scan_res, map_res) = splitAt n_scan results
  let per_scan_res = segBinOpChunks scan_ops_renamed all_scan_res
  let per_scan_pes = segBinOpChunks scan_ops_renamed $ patElems pat

  per_op_local_accum <- genLocalArray scan_ops_renamed

  j <- dPrimV "j" (0 :: Imp.TExp Int64)
  sWhile (tvExp j .<. tvExp chunk_length) $ do
    dPrimV_ i (tvExp start + tvExp j)
    compileStms mempty (bodyStms kbody_renamed) $ do
      let map_arrs = drop (segBinOpResults scan_ops_renamed) $ patElems pat
      sComment "write mapped values results to memory" $
        forM_ (zip (map patElemName map_arrs) (map kernelResultSubExp map_res)) $ \(arr, res) ->
          copyDWIMFix arr [tvExp start + tvExp j] res []

      sIf
        (tvExp j .==. 0)
        ( forM_ (zip4 scan_ops_renamed per_scan_res per_op_local_accum per_scan_pes) $
            \(scan_op, scan_res_op, local_accums, pes) -> do
              let shape = segBinOpShape scan_op
              sLoopNest shape $ \vec_is -> do
                forM_ (zip3 scan_res_op local_accums pes) $ \(kr, acc, pe) -> do
                  copyDWIMFix acc vec_is (kernelResultSubExp kr) vec_is
                  unless (shouldRecompute kbody_renamed) $
                    copyDWIMFix (patElemName pe) ((tvExp start + tvExp j) : vec_is) (kernelResultSubExp kr) vec_is
        )
        ( forM_ (zip4 scan_ops_renamed per_scan_res per_op_local_accum per_scan_pes) $
            \(scan_op, scan_res, local_accums, pes) ->
              sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
                forM_ (zip (xParams scan_op) local_accums) $ \(px, acc) ->
                  copyDWIMFix (paramName px) [] (Var acc) vec_is
                forM_ (zip (yParams scan_op) scan_res) $ \(py, kr) ->
                  copyDWIMFix (paramName py) [] (kernelResultSubExp kr) vec_is

                compileStms mempty (bodyStms $ lamBody scan_op) $ do
                  forM_ (zip3 (map resSubExp $ bodyResult $ lamBody scan_op) local_accums pes) $ \(se, acc, pe) -> do
                    copyDWIMFix acc vec_is se []
                    unless (shouldRecompute kbody_renamed) $ copyDWIMFix (patElemName pe) ((tvExp start + tvExp j) : vec_is) se []
        )
    j <-- tvExp j + 1

  --  write local accumulators to aggregate arrays
  forM_ (zip3 scan_ops_renamed per_op_local_accum per_op_aggr_arrs) $ \(scan_op, local_accums, aggr_arrs) ->
    sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
      forM_ (zip local_accums aggr_arrs) $ \(acc, agg) -> do
        copyDWIMFix agg (tvExp block_idx : vec_is) (Var acc) vec_is

load64 ::
  VName ->
  VName ->
  Imp.Count Imp.Elements (Imp.TExp Int32) ->
  MulticoreGen ()
load64 v arr i = sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int64) v arr i

store64 ::
  VName ->
  Imp.Count Imp.Elements (Imp.TExp Int32) ->
  Imp.TExp Int64 ->
  MulticoreGen ()
store64 arr i x = sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) arr i (untyped x)

add64 ::
  TV Int64 ->
  VName ->
  Imp.Count Imp.Elements (Imp.TExp Int32) ->
  Imp.TExp Int64 ->
  MulticoreGen ()
add64 v arr i x = sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar v) arr i (untyped x)

nonsegmentedScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  SegPostOp MCMem ->
  TV Int32 ->
  MulticoreGen ()
nonsegmentedScan
  pat
  (SegSpace fid [(i, n)])
  scan_ops
  kbody
  _post_op -- TODO: use this
  _nsubtasks = do
    let multiplier = 1 -- For playing with.
        blockSize = cacheSize `divUp` (totalBytes scan_ops * multiplier)

    block_no <- dPrimV "nblocks" (pe64 n `divUp` blockSize)

    -- allocate flags/aggr/prefix arrays of length nblocks
    flagsArr <- sAllocArray "scan_flags" int64 (Shape [Var (tvVar block_no)]) DefaultSpace

    let block_shape = Shape [Var (tvVar block_no)]

    aggrArrs <- genArrays scan_ops "scan_aggr" block_shape

    prefArrs <- genArrays scan_ops "scan_pref" block_shape

    work_index <- sAllocArray "work_index" int64 (Shape [intConst Int64 1]) DefaultSpace

    sFor "init" (tvExp block_no) $ \j -> do
      copyDWIMFix flagsArr [j] (intConst Int64 0) []

    copyDWIMFix work_index [0] (intConst Int64 0) []

    fbody <- collect $ do
      seq_flag <- dPrimV "seq_flag" true

      sOp $ Imp.GetTaskId fid

      block_idx <- dPrim "block_idx"
      work_index_loc <- entryArrayLoc <$> lookupArray work_index
      let work_index_loc_name = memLocName work_index_loc

      add64 block_idx work_index_loc_name (Imp.elements 0) 1

      sWhile (tvExp block_idx .<. tvExp block_no) $ do
        start <- dPrimV "start" (tvExp block_idx * blockSize)

        diff_start <- dPrimV "diff_start" (pe64 n - tvExp start)

        chunk_length <- dPrim "chunk_length"
        sIf
          (tvExp diff_start .<. blockSize)
          (chunk_length <-- tvExp diff_start)
          (chunk_length <-- blockSize)

        prefix_seqs <- genArrays scan_ops "seq_prefix" (Shape [])

        flags_loc <- entryArrayLoc <$> lookupArray flagsArr
        let flag_loc_name = memLocName flags_loc
        let block_idx_32 = sExt32 (tvExp block_idx)

        sWhen
          (tvExp seq_flag .==. true)
          ( sIf
              (tvExp block_idx .==. 0)
              (initialiseLocalPrefixes scan_ops prefix_seqs)
              ( do
                  prev_flag <- dPrim "prev_flag" :: MulticoreGen (TV Int64)
                  load64 (tvVar prev_flag) flag_loc_name (Imp.elements $ block_idx_32 - 1)
                  sIf
                    (tvExp prev_flag .==. 2)
                    (updateLocalPrefixes scan_ops prefix_seqs prefArrs [tvExp block_idx - 1])
                    (seq_flag <-- false)
              )
          )

        sIf
          (tvExp seq_flag .==. true)
          ( do
              seqScanFastPath pat i scan_ops kbody prefix_seqs start chunk_length prefArrs block_idx

              store64 flag_loc_name (Imp.elements block_idx_32) 2
          )
          ( do
              seqAggregate pat i scan_ops kbody start chunk_length aggrArrs block_idx

              -- write flag as 1
              store64 flag_loc_name (Imp.elements block_idx_32) 1

              old_flag <- dPrim "old_flag" :: MulticoreGen (TV Int64)
              old_flag <-- 0
              prefix_vars <- genArrays scan_ops "par_prefix" (Shape [])

              lb <- dPrimV "lb" (tvExp block_idx - 1)
              has_acc <- dPrimV "has_acc" (false :: Imp.TExp Bool)

              error_flag <- dPrim "error_flag" :: MulticoreGen (TV Bool)
              sOp $ Imp.GetError (tvVar error_flag)

              sWhile (bNot (tvExp old_flag .==. 2 .||. tvExp error_flag)) $ do
                load64 (tvVar old_flag) flag_loc_name (Imp.elements $ sExt32 $ tvExp lb)

                -- One of the other operators may fail, in which case we have to
                -- bail out to avoid an infinite wait. This is a very rare case,
                -- so only do it when necessary.
                when (any (hasAssert . lambdaBody . segBinOpLambda) scan_ops || hasAssert kbody) $
                  sOp $
                    Imp.GetError (tvVar error_flag)

                sWhen
                  (tvExp old_flag .==. 2)
                  ( do
                      sIf
                        (tvExp has_acc .==. false)
                        (copyFromDescToLocal scan_ops prefix_vars prefArrs lb)
                        (applyScanFromDescToLocal scan_ops prefix_vars prefArrs lb)
                  )
                sWhen
                  (tvExp old_flag .==. 1)
                  ( do
                      sIf
                        (tvExp has_acc .==. false)
                        (copyFromDescToLocal scan_ops prefix_vars aggrArrs lb)
                        (applyScanFromDescToLocal scan_ops prefix_vars aggrArrs lb)
                      has_acc <-- true
                      lb <-- tvExp lb - 1
                  )

              applyScanAggregateToPrefix scan_ops prefix_vars aggrArrs prefArrs block_idx

              store64 flag_loc_name (Imp.elements block_idx_32) 2

              seqScanLB pat i scan_ops kbody prefix_vars start chunk_length
          )

        add64 block_idx work_index_loc_name (Imp.elements 0) 1

    free_params <- freeParams fbody
    emit $ Imp.Op $ Imp.ParLoop "segmap" fbody free_params

-- | Compile a SegScan construct.
compileSegScan ::
  Pat LetDecMem ->
  SegSpace ->
  [Type] ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  SegPostOp MCMem ->
  TV Int32 ->
  MulticoreGen ()
compileSegScan pat space _ts kbody reds post_op nsubtasks
  | [_] <- unSegSpace space =
      nonsegmentedScan pat space reds kbody post_op nsubtasks
  | otherwise =
      error "only nonsegmented scans for now"
