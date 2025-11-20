{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | Multicore code generation for SegScan. Uses a fairly naive multipass
-- algorithm, with no particular locality optimisations.
module Futhark.CodeGen.ImpGen.Multicore.SegScan
  ( compileSegScan,
  )
where

import Control.Monad
import Data.List (zip4, zip5)
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Util.IntegralExp (divUp)
import Prelude hiding (quot, rem)

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
initialiseLocalPrefixes scan_ops prefix_seqs = do
  scan_ops_renamed <- renameSegBinOp scan_ops
  genBinOpParams scan_ops_renamed
  forM_ (zip scan_ops_renamed prefix_seqs) $ \(scan_op, per_op_prefix_seq) -> do
    forM_ (zip (segBinOpNeutral scan_op) per_op_prefix_seq) $ \(ne, prefix_seq) -> do
      sLoopNest (segBinOpShape scan_op) $ \vec_is ->
        copyDWIMFix prefix_seq vec_is ne []

updateLocalPrefixes :: [SegBinOp MCMem] -> [[VName]] -> [[VName]] -> [Imp.TExp Int64] -> MulticoreGen ()
updateLocalPrefixes scan_ops prefix_seqs per_op_prefs index = do
  forM_ (zip3 scan_ops prefix_seqs per_op_prefs) $ \(scan_op, per_op_prefix_seq, per_op_pref) -> do
    forM_ (zip per_op_prefix_seq per_op_pref) $ \(prefix_seq, prefArr) -> do
      let shape = segBinOpShape scan_op
      sLoopNest shape $ \vec_is ->
        copyDWIMFix prefix_seq vec_is (Var prefArr) (index ++ vec_is)


genArrays :: [SegBinOp MCMem] -> Name -> Shape -> MulticoreGen [[VName]]
genArrays scan_ops arr_name block_shape = do
  forM scan_ops $ \scan_op -> do
    let shape = segBinOpShape scan_op
        ts = lambdaReturnType $ segBinOpLambda scan_op
    forM ts $ \t -> do
      sAllocArray arr_name (elemType t) (block_shape <> shape <> arrayShape t) DefaultSpace

-- | Compile a SegScan construct.
compileSegScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen ()
compileSegScan pat space reds kbody nsubtasks
  | [_] <- unSegSpace space =
      nonsegmentedScan pat space reds kbody nsubtasks
  | otherwise =
      error "only nonsegmented scans for now"

seqScan ::
  Pat LetDecMem ->
  VName ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  [[VName]] ->
  TV Int64 ->
  TV Int64 ->
  Maybe ([[VName]], TV Int64) ->
  MulticoreGen ()
seqScan pat i scan_ops kbody per_op_prefixes_var start chunk_length mprefArrs_block_idx = do
  scan_ops2 <- renameSegBinOp scan_ops
  genBinOpParams scan_ops2

  let results = bodyResult kbody
  let n_scan = segBinOpResults scan_ops2
  let (all_scan_res, map_res) = splitAt n_scan results

  let per_scan_res = segBinOpChunks scan_ops2 all_scan_res
  let per_scan_pes = segBinOpChunks scan_ops2 $ patElems pat


  z <- dPrimV "z" (0 :: Imp.TExp Int64)
  sWhile (tvExp z .<. tvExp chunk_length) $ do
    dPrimV_ i (tvExp start + tvExp z)

    compileStms mempty (bodyStms kbody) $ do
      let map_arrs = drop (segBinOpResults scan_ops2) $ patElems pat
      -- recheck this part
      sComment "write mapped values results to memory" $
        forM_ (zip (map patElemName map_arrs) (map kernelResultSubExp map_res)) $ \(arr, res) ->
          copyDWIMFix arr [tvExp start + tvExp z] res []

      case mprefArrs_block_idx of
        Just (prefArrs, block_idx) ->
          forM_ (zip5 per_scan_pes scan_ops2 per_scan_res prefArrs per_op_prefixes_var) $ \(pes, scan_op, scan_res, prefArr, prefixes) ->
            sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
              forM_ (zip (xParams scan_op) prefixes) $ \(p, acc') -> do
                copyDWIMFix (paramName p) [] (Var acc') vec_is
              forM_ (zip (yParams scan_op) scan_res) $ \(py, kr) ->
                copyDWIMFix (paramName py) [] (kernelResultSubExp kr) vec_is
              compileStms mempty (bodyStms $ lamBody scan_op) $ do
                forM_ (zip4 prefixes (map resSubExp $ bodyResult $ lamBody scan_op) pes prefArr) $ \(acc', se, pe, pref) -> do
                  copyDWIMFix (patElemName pe) ((tvExp start + tvExp z) : vec_is) se []
                  copyDWIMFix acc' vec_is se []
                  copyDWIMFix pref (tvExp block_idx : vec_is) se []
        Nothing ->
          forM_ (zip4 per_scan_pes scan_ops2 per_scan_res per_op_prefixes_var) $ \(pes, scan_op, scan_res, prefixes) ->
            sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
              forM_ (zip (xParams scan_op) prefixes) $ \(p, acc') -> do
                copyDWIMFix (paramName p) [] (Var acc') vec_is
              forM_ (zip (yParams scan_op) scan_res) $ \(py, kr) ->
                copyDWIMFix (paramName py) [] (kernelResultSubExp kr) vec_is
              compileStms mempty (bodyStms $ lamBody scan_op) $ do
                forM_ (zip3 prefixes (map resSubExp $ bodyResult $ lamBody scan_op) pes) $ \(acc', se, pe) -> do
                  copyDWIMFix (patElemName pe) ((tvExp start + tvExp z) : vec_is) se []
                  copyDWIMFix acc' vec_is se []
    z <-- tvExp z + 1

copyFromDescToLocal :: [SegBinOp MCMem] -> [[VName]] -> [[VName]] -> TV Int64 -> MulticoreGen ()
copyFromDescToLocal scan_ops local_arrays description_arrays lb = do
  forM_ scan_ops $ \scan_op -> do
    let shape = segBinOpShape scan_op
    forM_ (zip local_arrays description_arrays) $ \(local_array, description_array) -> do
      forM_ (zip local_array description_array) $ \(local_var, desc_var) -> do
        sLoopNest shape $ \vec_is ->
          copyDWIMFix local_var vec_is (Var desc_var) (tvExp lb : vec_is)

applyScanFromDescToLocal ::
  [SegBinOp MCMem] ->
  [[VName]] ->
  [[VName]] ->
  TV Int64 ->
  MulticoreGen ()
applyScanFromDescToLocal scan_ops local_arrays description_arrays lb = do
  forM_ (zip3 scan_ops local_arrays description_arrays) $ \(scan_op, local_array, description_array) -> do
    sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
      forM_ (zip (xParams scan_op) description_array) $ \(xs, desc_var) -> do
        copyDWIMFix (paramName xs) [] (Var desc_var) (tvExp lb : vec_is)
      forM_ (zip (yParams scan_op) local_array) $ \(ys, local_var) -> do
        copyDWIMFix (paramName ys) [] (Var local_var) vec_is
      compileStms mempty (bodyStms $ lamBody scan_op) $ do
        forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) local_array) $ \(op_res, local_var) -> do
          copyDWIMFix local_var vec_is op_res []

applyScanAggregateToPrefix ::
  [SegBinOp MCMem] ->
  [[VName]] ->
  [[VName]] ->
  [[VName]] ->
  TV Int64 ->
  MulticoreGen ()
applyScanAggregateToPrefix scan_ops prefix_vars aggrArrs prefArrs block_idx = do
  forM_ (zip4 scan_ops prefix_vars aggrArrs prefArrs) $ \(scan_op, prefix_vars_op, aggrArrs_op, prefArrs_op) -> do
    sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
      forM_ (zip (xParams scan_op) aggrArrs_op) $ \(xs, aggrArr) -> do
        copyDWIMFix (paramName xs) [] (Var aggrArr) (tvExp block_idx : vec_is)
      forM_ (zip (yParams scan_op) prefix_vars_op) $ \(ys, prefix_var) -> do
        copyDWIMFix (paramName ys) [] (Var prefix_var) vec_is
      compileStms mempty (bodyStms $ lamBody scan_op) $ do
        forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) prefArrs_op) $ \(op_res, prefArr) -> do
          copyDWIMFix prefArr (tvExp block_idx : vec_is) op_res []

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
seqAggregate pat i scan_ops kbody start chunk_length aggrArrs block_idx = do
  let results = bodyResult kbody
  let n_scan = segBinOpResults scan_ops
  let (all_scan_res, map_res) = splitAt n_scan results

  let per_scan_res = segBinOpChunks scan_ops all_scan_res
  scan_ops2 <- renameSegBinOp scan_ops
  genBinOpParams scan_ops2
  dPrimV_ i (tvExp start)
  compileStms mempty (bodyStms kbody) $ do
    let map_arrs = drop (segBinOpResults scan_ops2) $ patElems pat
    -- recheck this part
    sComment "write mapped values results to memory" $
      forM_ (zip (map patElemName map_arrs) (map kernelResultSubExp map_res)) $ \(arr, res) ->
        copyDWIMFix arr [tvExp start] res []

    forM_ (zip3 scan_ops2 per_scan_res aggrArrs) $ \(op, scan_res_op, aggrArr) -> do
      forM_ (zip3 (xParams op) scan_res_op aggrArr) $ \(p, kr, agg) -> do
        let shape = segBinOpShape op
        sLoopNest shape $ \vec_is -> do
          copyDWIMFix agg (tvExp block_idx : vec_is) (kernelResultSubExp kr) vec_is

  j <- dPrimV "j" (1 :: Imp.TExp Int64)
  sWhile (tvExp j .<. tvExp chunk_length) $ do
    dPrimV_ i (tvExp start + tvExp j)
    compileStms mempty (bodyStms kbody) $ do
      let map_arrs = drop (segBinOpResults scan_ops2) $ patElems pat

      -- recheck this part
      sComment "write mapped values results to memory" $
        forM_ (zip (map patElemName map_arrs) (map kernelResultSubExp map_res)) $ \(arr, res) ->
          copyDWIMFix arr [tvExp start + tvExp j] res []

      forM_ (zip3 scan_ops2 per_scan_res aggrArrs) $ \(scan_op, scan_res, aggrArr) ->
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
          forM_ (zip (xParams scan_op) aggrArr) $ \(p, acc') -> do
            copyDWIMFix (paramName p) [] (Var acc') (tvExp block_idx : vec_is)
          forM_ (zip (yParams scan_op) scan_res) $ \(py, kr) ->
            copyDWIMFix (paramName py) [] (kernelResultSubExp kr) vec_is
          compileStms mempty (bodyStms $ lamBody scan_op) $ do
            forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) aggrArr) $ \(se, agg) -> do
              copyDWIMFix agg (tvExp block_idx : vec_is) se []

    j <-- tvExp j + 1

nonsegmentedScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen ()
nonsegmentedScan
  pat
  (SegSpace _ [(i, n)])
  scan_ops
  kbody
  _nsubtasks = do
    let comp_tys = concatMap (lambdaReturnType . segBinOpLambda) scan_ops
    let pts = map elemType comp_tys

    let pt0 = case pts of
          (pt : _) -> pt
          [] -> int32
    let blockSize = cacheSize `divUp` primByteSize pt0

    block_no <- dPrim "nblocks"
    block_no <-- pe64 n `divUp` blockSize

    -- allocate flags/aggr/prefix arrays of length nblocks
    flagsArr <- sAllocArray "scan_flags" int64 (Shape [Var (tvVar block_no)]) DefaultSpace

    let block_shape = Shape [Var (tvVar block_no)]

    aggrArrs <- genArrays scan_ops "scan_aggr" block_shape

    prefArrs <- genArrays scan_ops "scan_pref" block_shape

    work_index <- sAllocArray "work_index" int64 (Shape [intConst Int64 1]) DefaultSpace

    -- initialise
    sFor "init" (tvExp block_no) $ \j -> do
      copyDWIMFix flagsArr [j] (intConst Int64 0) []

    copyDWIMFix work_index [0] (intConst Int64 0) []

    fbody <- collect $ do
      let one = (1 :: Imp.TExp Int64)
      let idx0 = Imp.elements (0 :: Imp.TExp Int32)

      seq_flag <- dPrimV "seq_flag" (true :: Imp.TExp Bool)

      vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
      sOp $ Imp.GetTaskId (tvVar vvv)

      block_idx <- dPrim "block_idx" :: MulticoreGen (TV Int64)
      work_index_loc <- entryArrayLoc <$> lookupArray work_index
      let workF = memLocName work_index_loc

      sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar block_idx) workF idx0 (untyped one)

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
        let memF = memLocName flags_loc
        let block_idx_32 = TPrimExp $ sExt Int32 (untyped $ tvExp block_idx)

        sWhen
          (tvExp seq_flag .==. true)
          ( sIf
              (tvExp block_idx .==. 0)
              ( initialiseLocalPrefixes scan_ops prefix_seqs)
              ( do
                  prev_flag <- dPrim "prev_flag" :: MulticoreGen (TV Int64)
                  sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int64) (tvVar prev_flag) memF (Imp.elements $ block_idx_32 - 1)
                  sIf
                    (tvExp prev_flag .==. 2)
                    (updateLocalPrefixes scan_ops prefix_seqs prefArrs [tvExp block_idx - 1])
                    (seq_flag <-- false)
              )
          )

        sIf
          (tvExp seq_flag .==. true)
          ( do
              seqScan pat i scan_ops kbody prefix_seqs start chunk_length (Just (prefArrs, block_idx))

              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (2 :: Imp.TExp Int64))
          )
          ( do

              seqAggregate pat i scan_ops kbody start chunk_length aggrArrs block_idx

              -- write flag as 1
              old_flag <- dPrim "old_flag" :: MulticoreGen (TV Int64)
              old_flag <-- 0
              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (1 :: Imp.TExp Int64))

              prefix_vars <- genArrays scan_ops "par_prefix" (Shape [])

              lb <- dPrimV "lb" (tvExp block_idx - 1)

              has_acc <- dPrimV "has_acc" (false :: Imp.TExp Bool)

              sWhile (bNot (tvExp old_flag .==. 2)) $ do
                sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int64) (tvVar old_flag) memF (Imp.elements $ TPrimExp $ sExt Int32 (untyped $ tvExp lb))

                sWhen
                  (tvExp old_flag .==. 2)
                  ( do
                      
                      renamed_scan_ops <- renameSegBinOp scan_ops
                      genBinOpParams renamed_scan_ops

                      sIf
                        (tvExp has_acc .==. false)
                        (copyFromDescToLocal renamed_scan_ops prefix_vars prefArrs lb)
                        (applyScanFromDescToLocal renamed_scan_ops prefix_vars prefArrs lb)
                  )
                sWhen
                  (tvExp old_flag .==. 1)
                  ( do
                      renamed_scan_ops <- renameSegBinOp scan_ops
                      genBinOpParams renamed_scan_ops

                      sIf
                        (tvExp has_acc .==. false)
                        (copyFromDescToLocal renamed_scan_ops prefix_vars aggrArrs lb)
                        (applyScanFromDescToLocal renamed_scan_ops prefix_vars aggrArrs lb)
                      has_acc <-- true
                      lb <-- tvExp lb - 1
                  )

              scan_ops1 <- renameSegBinOp scan_ops
              genBinOpParams scan_ops1

              applyScanAggregateToPrefix scan_ops1 prefix_vars aggrArrs prefArrs block_idx

              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (2 :: Imp.TExp Int64))

              seqScan pat i scan_ops kbody prefix_vars start chunk_length Nothing
              pure ()
          )

        sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar block_idx) workF idx0 (untyped one)

    free_params <- freeParams fbody
    emit $ Imp.Op $ Imp.ParLoop "segmap" fbody free_params


    -- forM_ scan_ops $ \scan_op -> do
    --   let ts = lambdaReturnType $ segBinOpLambda scan_op

    --   forM_ ts $ \t -> do
    --     let arshape = arrayShape t
    --     let dims = shapeDims arshape
    --     forM_ (zip [(0 :: Int) ..] dims) $ \(i, d) -> do
    --       emit $
    --         Imp.DebugPrint
    --           "SegScan result shape: "
    --           (Just $ untyped $ pe64 d)
    -- forM_ scan_ops $ \scan_op -> do
    --   let dims = shapeDims $ segBinOpShape scan_op

    --   forM_ (zip [(0 :: Int) ..] dims) $ \(i, d) -> do
    --     emit $
    --       Imp.DebugPrint
    --         "SegScan shape: "
    --         (Just $ untyped $ pe64 d)

    -- forM_ scan_ops $ \scan_op -> do
    --   emit $ Imp.DebugPrint "#############" Nothing
    -- forM_ scan_ops $ \scan_op -> do
    --   emit $ Imp.DebugPrint "SegScan ********" Nothing
