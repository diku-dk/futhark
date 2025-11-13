{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | Multicore code generation for SegScan. Uses a fairly naive multipass
-- algorithm, with no particular locality optimisations.
module Futhark.CodeGen.ImpGen.Multicore.SegScan
  ( compileSegScan,
  )
where

import Control.Monad
import Data.List (zip4)
import Data.Text.Internal.Lazy (chunk)
import Data.Traversable (for)
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Transform.Rename (renameStm)
import Futhark.Util.IntegralExp (divUp, quot)
import Futhark.Util.Pretty (prettyString)
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
  MulticoreGen [SegBinOp MCMem]
seqScan pat i scan_ops kbody per_op_prefixes_var start chunk_length = do
  let results = bodyResult kbody
  let n_scan = segBinOpResults scan_ops
  let (all_scan_res, map_res) = splitAt n_scan results

  let per_scan_res = segBinOpChunks scan_ops all_scan_res
  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat

  scan_ops2 <- renameSegBinOp scan_ops
  genBinOpParams scan_ops2

  forM_ (zip scan_ops2 per_op_prefixes_var) $ \(op, pfxs) ->
    forM_ (zip (xParams op) pfxs) $ \(p, pfx) ->
      copyDWIMFix (paramName p) [] (Var pfx) []
  z <- dPrimV "z" (0 :: Imp.TExp Int64)
  sWhile (tvExp z .<. tvExp chunk_length) $ do
    dPrimV_ i (tvExp start + tvExp z)

    compileStms mempty (bodyStms kbody) $ do
      let map_arrs = drop (segBinOpResults scan_ops) $ patElems pat
      -- recheck this part
      sComment "write mapped values results to memory" $
        forM_ (zip (map patElemName map_arrs) (map kernelResultSubExp map_res)) $ \(arr, res) ->
          copyDWIMFix arr [tvExp start + tvExp z] res []

      forM_ (zip scan_ops2 per_scan_res) $ \(op, scan_res_op) ->
        forM_ (zip (yParams op) scan_res_op) $ \(p, kr) ->
          copyDWIMFix (paramName p) [] (kernelResultSubExp kr) []

    forM_ (zip scan_ops2 per_scan_pes) $ \(op, pes_op) -> do
      compileStms mempty (bodyStms $ lamBody op) $ do
        forM_
          ( zip3
              (map resSubExp $ bodyResult $ lamBody op)
              (xParams op)
              pes_op
          )
          $ \(se, px, pe) -> do
            copyDWIMFix (patElemName pe) [tvExp start + tvExp z] se []
            copyDWIMFix (paramName px) [] se []
    z <-- tvExp z + 1
  pure scan_ops2

seqAggregate ::
  Pat LetDecMem ->
  VName ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int64 ->
  TV Int64 ->
  MulticoreGen [SegBinOp MCMem]
seqAggregate pat i scan_ops kbody start chunk_length = do
  let results = bodyResult kbody
  let n_scan = segBinOpResults scan_ops
  let (all_scan_res, map_res) = splitAt n_scan results

  let per_scan_res = segBinOpChunks scan_ops all_scan_res
  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  scan_ops2 <- renameSegBinOp scan_ops
  genBinOpParams scan_ops2
  dPrimV_ i (tvExp start)
  compileStms mempty (bodyStms kbody) $ do
    forM_ (zip scan_ops2 per_scan_res) $ \(op, scan_res_op) -> do
      forM_ (zip (xParams op) scan_res_op) $ \(p, kr) ->
        copyDWIMFix (paramName p) [] (kernelResultSubExp kr) []

  j <- dPrimV "j" (1 :: Imp.TExp Int64)
  sWhile (tvExp j .<. tvExp chunk_length) $ do
    dPrimV_ i (tvExp start + tvExp j)
    compileStms mempty (bodyStms kbody) $ do
      forM_ (zip scan_ops2 per_scan_res) $ \(op, scan_res_op) -> do
        forM_ (zip (yParams op) scan_res_op) $ \(p, kr) ->
          copyDWIMFix (paramName p) [] (kernelResultSubExp kr) []

    forM_ (zip scan_ops2 per_scan_pes) $ \(op, pes_op) -> do
      compileStms mempty (bodyStms $ lamBody op) $ do
        forM_
          ( zip3
              (map resSubExp $ bodyResult $ lamBody op)
              (xParams op)
              pes_op
          )
          $ \(se, px, pe) -> do
            copyDWIMFix (paramName px) [] se []

    j <-- tvExp j + 1
  pure scan_ops2

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
    let nes = concatMap segBinOpNeutral scan_ops

    let pt0 = case pts of
          (pt : _) -> pt
          [] -> int32
    let blockSize = cacheSize `divUp` primByteSize pt0

    block_no <- dPrim "nblocks"
    block_no <-- pe64 n `divUp` blockSize

    -- allocate flags/aggr/prefix arrays of length nblocks
    flagsArr <- sAllocArray "scan_flags" int64 (Shape [Var (tvVar block_no)]) DefaultSpace

    aggrArrs <- forM (zip [0 ..] pts) $ \(k, pt) ->
      sAllocArray ("scan_aggr_" <> nameFromText (prettyText (show k))) pt (Shape [Var (tvVar block_no)]) DefaultSpace
    prefArrs <- forM (zip [0 ..] pts) $ \(k, pt) ->
      sAllocArray ("scan_pref_" <> nameFromText (prettyText (show k))) pt (Shape [Var (tvVar block_no)]) DefaultSpace

    let per_op_aggr = segBinOpChunks scan_ops aggrArrs
    let per_op_prefs = segBinOpChunks scan_ops prefArrs

    work_index <- sAllocArray "work_index" int64 (Shape [intConst Int64 1]) DefaultSpace

    -- initialise
    sFor "init" (tvExp block_no) $ \j -> do
      copyDWIMFix flagsArr [j] (intConst Int64 0) []
      forM_ (zip aggrArrs nes) $ \(aggrArr, ne) ->
        copyDWIMFix aggrArr [j] ne []
      forM_ (zip prefArrs nes) $ \(prefArr, ne) ->
        copyDWIMFix prefArr [j] ne []

    copyDWIMFix work_index [0] (intConst Int64 0) []

    num_tasks <- dPrimSV "num_tasks" int64
    sOp $ Imp.GetNumTasks $ tvVar num_tasks

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

        -- prefix_seq <- dPrimSV "prefix_seq" pt
        prefix_seqs <- forM (zip [0 ..] pts) $ \(k, pt) ->
          dPrimS ("prefix_seq_" <> nameFromText (prettyText (show k))) pt

        flags_loc <- entryArrayLoc <$> lookupArray flagsArr
        let memF = memLocName flags_loc

        let block_idx_32 = TPrimExp $ sExt Int32 (untyped $ tvExp block_idx)

        sWhen
          (tvExp seq_flag .==. true)
          ( sIf
              (tvExp block_idx .==. 0)
              ( do
                  forM_ (zip prefix_seqs nes) $ \(prefix_seq, p) ->
                    copyDWIMFix prefix_seq [] p []
              )
              ( do
                  prev_flag <- dPrim "prev_flag" :: MulticoreGen (TV Int64)
                  sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int64) (tvVar prev_flag) memF (Imp.elements $ block_idx_32 - 1)
                  sIf
                    (tvExp prev_flag .==. 2)
                    ( forM_ (zip prefix_seqs prefArrs) $ \(prefix_seq, prefArr) ->
                        copyDWIMFix prefix_seq [] (Var prefArr) [tvExp block_idx - 1]
                    )
                    (seq_flag <-- false)
              )
          )

        sIf
          (tvExp seq_flag .==. true)
          ( do
              let per_op_prefix = segBinOpChunks scan_ops prefix_seqs

              new_scan_ops <- seqScan pat i scan_ops kbody per_op_prefix start chunk_length

              forM_ (zip new_scan_ops per_op_prefs) $ \(op, prefAs) -> do
                forM_ (zip prefAs (xParams op)) $ \(prefA, xp) ->
                  copyDWIMFix prefA [tvExp block_idx] (Var $ paramName xp) []

              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (2 :: Imp.TExp Int64))
          )
          ( do
              new_scan_ops <- seqAggregate pat i scan_ops kbody start chunk_length

              -- write to aggr array
              forM_ (zip new_scan_ops per_op_aggr) $ \(scan_op, aggrArr) ->
                forM_ (zip (xParams scan_op) aggrArr) $ \(p, aggrA) ->
                  copyDWIMFix aggrA [tvExp block_idx] (Var $ paramName p) []
              -- write flag as 1
              old_flag <- dPrim "old_flag" :: MulticoreGen (TV Int64)
              old_flag <-- 0
              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (1 :: Imp.TExp Int64))

              prefix_vars <- forM (zip [0 ..] pts) $ \(k, pt) ->
                dPrimS ("prefix_" <> nameFromText (prettyText (show k))) pt

              let per_op_prefixes_var = segBinOpChunks scan_ops prefix_vars

              lb <- dPrimV "lb" (tvExp block_idx - 1)

              has_acc <- dPrimV "has_acc" (false :: Imp.TExp Bool)

              sWhile (bNot (tvExp old_flag .==. 2)) $ do
                sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int64) (tvVar old_flag) memF (Imp.elements $ TPrimExp $ sExt Int32 (untyped $ tvExp lb))

                vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
                sOp $ Imp.GetTaskId (tvVar vvv)

                sWhen
                  (tvExp old_flag .==. 2)
                  ( do
                      genBinOpParams scan_ops

                      sIf
                        (tvExp has_acc .==. false)
                        ( forM_ (zip prefix_vars prefArrs) $ \(prefix_var, prefArr) ->
                            copyDWIMFix prefix_var [] (Var prefArr) [tvExp lb]
                        )
                        ( do
                            forM_ (zip3 scan_ops per_op_prefixes_var per_op_prefs) $ \(scan_op, prefix_vars, prefArrs) ->
                              forM_ (zip4 (xParams scan_op) (yParams scan_op) prefArrs prefix_vars) $ \(xs, ys, prefArr, prefix_var) -> do
                                copyDWIMFix (paramName xs) [] (Var prefArr) [tvExp lb]
                                copyDWIMFix (paramName ys) [] (Var prefix_var) []

                            forM_ (zip scan_ops per_op_prefixes_var) $ \(scan_op, prefix_vars) ->
                              compileStms mempty (bodyStms $ lamBody scan_op) $ do
                                forM_
                                  ( zip
                                      (map resSubExp $ bodyResult $ lamBody scan_op)
                                      prefix_vars
                                  )
                                  $ \(op_res, prefix_var) -> do
                                    copyDWIMFix prefix_var [] op_res []
                        )
                  )
                sWhen
                  (tvExp old_flag .==. 1)
                  ( do
                      genBinOpParams scan_ops
                      sIf
                        (tvExp has_acc .==. false)
                        ( forM_ (zip prefix_vars aggrArrs) $ \(prefix, aggrArr) ->
                            copyDWIMFix prefix [] (Var aggrArr) [tvExp lb]
                        )
                        ( do
                            forM_ (zip3 scan_ops per_op_prefixes_var per_op_aggr) $ \(scan_op, prefix_vars, argAggr) ->
                              forM_ (zip4 (xParams scan_op) (yParams scan_op) argAggr prefix_vars) $ \(xs, ys, aggrArr, prefix_var) -> do
                                copyDWIMFix (paramName xs) [] (Var aggrArr) [tvExp lb]
                                copyDWIMFix (paramName ys) [] (Var prefix_var) []

                            forM_ (zip scan_ops per_op_prefixes_var) $ \(scan_op, prefix_vars) ->
                              compileStms mempty (bodyStms $ lamBody scan_op) $ do
                                forM_
                                  ( zip
                                      (map resSubExp $ bodyResult $ lamBody scan_op)
                                      prefix_vars
                                  )
                                  $ \(op_res, prefix_var) -> do
                                    copyDWIMFix prefix_var [] op_res []
                        )

                      has_acc <-- true
                      lb <-- tvExp lb - 1
                  )

              scan_ops1 <- renameSegBinOp scan_ops
              genBinOpParams scan_ops1

              forM_ (zip3 scan_ops1 per_op_prefixes_var per_op_aggr) $ \(scan_op, prefix_vars, argAggr) ->
                forM_ (zip4 (xParams scan_op) (yParams scan_op) argAggr prefix_vars) $ \(xs, ys, aggrArr, prefix_var) -> do
                  copyDWIMFix (paramName xs) [] (Var aggrArr) [tvExp block_idx]
                  copyDWIMFix (paramName ys) [] (Var prefix_var) []

              forM_ (zip scan_ops1 per_op_prefs) $ \(scan_op, prefArrs) ->
                compileStms mempty (bodyStms $ lamBody scan_op) $ do
                  forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) prefArrs) $ \(op_res, prefArr) -> do
                    copyDWIMFix prefArr [tvExp block_idx] op_res []

              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (2 :: Imp.TExp Int64))

              _ <- seqScan pat i scan_ops kbody per_op_prefixes_var start chunk_length
              pure ()
          )

        sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar block_idx) workF idx0 (untyped one)

      vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
      sOp $ Imp.GetTaskId (tvVar vvv)

    free_params <- freeParams fbody
    emit $ Imp.Op $ Imp.ParLoop "segmap" fbody free_params
