module Futhark.CodeGen.ImpGen.Multicore.SegScan
  ( compileSegScan,
  )
where

import Control.Monad
import Data.List (zip4)
import Debug.Trace
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.Construct (fullSlice)
import Futhark.IR.MCMem
import Futhark.IR.SOACS.SOAC (groupScatterResults)
import Futhark.IR.SegOp (splitPostOpResults)
import Futhark.Util.IntegralExp (quot, rem)
import Prelude hiding (quot, rem)

debug :: (Show a) => a -> a
debug x = traceShow x x

-- Compile a SegScan construct
compileSegScan ::
  Pat LetDecMem ->
  SegSpace ->
  [Type] ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  SegPostOp MCMem ->
  TV Int32 ->
  MulticoreGen Imp.MCCode
compileSegScan pat space ts kbody reds post_op nsubtasks
  | [_] <- unSegSpace space =
      nonsegmentedScan pat space ts kbody reds post_op nsubtasks
  | otherwise =
      segmentedScan pat space kbody reds post_op

xParams, yParams :: SegBinOp MCMem -> [LParam MCMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

lamBody :: SegBinOp MCMem -> Body MCMem
lamBody = lambdaBody . segBinOpLambda

-- Arrays for storing worker results.
carryArrays :: String -> TV Int32 -> [SegBinOp MCMem] -> MulticoreGen [[VName]]
carryArrays s nsubtasks segops =
  forM segops $ \(SegBinOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
      let pt = elemType t
          full_shape =
            Shape [Var (tvVar nsubtasks)]
              <> shape
              <> arrayShape t
      sAllocArray s pt full_shape DefaultSpace

nonsegmentedScan ::
  Pat LetDecMem ->
  SegSpace ->
  [Type] ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  SegPostOp MCMem ->
  TV Int32 ->
  MulticoreGen Imp.MCCode
nonsegmentedScan pat space ts kbody scan_ops post_op nsubtasks = do
  emit $ Imp.DebugPrint "nonsegmented segScan" Nothing
  collect $ do
    -- Are we working with nested arrays
    let dims = map (shapeDims . segBinOpShape) scan_ops
    -- Are we only working on scalars
    let scalars = all (all (primType . typeOf . paramDec) . (lambdaParams . segBinOpLambda)) scan_ops && all null dims
    -- Do we have nested vector operations
    let vectorize = [] `notElem` dims

    let param_types = concatMap (map paramType . (lambdaParams . segBinOpLambda)) scan_ops
    let no_array_param = all primType param_types

    let shpT op = (segBinOpShape op,) <$> lambdaReturnType (segBinOpLambda op)
    let scan_ts = concatMap shpT scan_ops
    let shpOfT t s =
          arrayShape $
            foldr (flip arrayOfRow) (arrayOfShape t s) $
              segSpaceDims space

    scan_out <- forM scan_ts $ \(s, t) ->
      sAllocArray "scan_out" (elemType t) (shpOfT t s) DefaultSpace

    map_out <- forM (drop (segBinOpResults scan_ops) ts) $ \t ->
      sAllocArray "map_out" (elemType t) (shpOfT t mempty) DefaultSpace

    let (scanStage1, scanStage3)
          | scalars = (scanStage1Scalar, scanStage3Scalar)
          | vectorize && no_array_param = (scanStage1Nested, scanStage3Nested)
          | otherwise = (scanStage1Fallback, scanStage3Fallback)
    emit $ Imp.DebugPrint "Scan stage 1" Nothing
    scanStage1 space kbody scan_ops scan_out map_out

    -- let nsubtasks' = tvExp nsubtasks
    scan_ops2 <- renameSegBinOp scan_ops
    emit $ Imp.DebugPrint "Scan stage 2" Nothing
    carries <- scanStage2 nsubtasks space scan_ops2 scan_out
    scan_ops3 <- renameSegBinOp scan_ops
    emit $ Imp.DebugPrint "Scan stage 3" Nothing
    scanStage3 pat space scan_ops3 carries post_op scan_out map_out

-- Different ways to generate code for a scan loop
data ScanLoopType
  = ScanSeq -- Fully sequential
  | ScanNested -- Nested vectorized map
  | ScanScalar -- Vectorized scan over scalars

-- Given a scan type, return a function to inject into the loop body
getScanLoop ::
  ScanLoopType ->
  (Imp.TExp Int64 -> MulticoreGen ()) ->
  MulticoreGen ()
getScanLoop ScanScalar = generateUniformizeLoop
getScanLoop _ = \body -> body 0

-- Given a scan type, return a function to extract a scalar from a vector
getExtract :: ScanLoopType -> Imp.TExp Int64 -> MulticoreGen Imp.MCCode -> MulticoreGen ()
getExtract ScanSeq = \_ body -> body >>= emit
getExtract _ = extractVectorLane

genBinOpParams :: [SegBinOp MCMem] -> MulticoreGen ()
genBinOpParams scan_ops =
  dScope Nothing $
    scopeOfLParams $
      concatMap (lambdaParams . segBinOpLambda) scan_ops

genLocalAccsStage1 :: [SegBinOp MCMem] -> MulticoreGen [[VName]]
genLocalAccsStage1 scan_ops = do
  forM scan_ops $ \scan_op -> do
    let shape = segBinOpShape scan_op
        ts = lambdaReturnType $ segBinOpLambda scan_op
    forM (zip3 (xParams scan_op) (segBinOpNeutral scan_op) ts) $ \(p, ne, t) -> do
      acc <- -- update accumulator to have type decoration
        case shapeDims shape of
          [] -> pure $ paramName p
          _ -> do
            let pt = elemType t
            sAllocArray "local_acc" pt (shape <> arrayShape t) DefaultSpace

      -- Now neutral-initialise the accumulator.
      sLoopNest (segBinOpShape scan_op) $ \vec_is ->
        copyDWIMFix acc vec_is ne []

      pure acc

getNestLoop ::
  ScanLoopType ->
  Shape ->
  ([Imp.TExp Int64] -> MulticoreGen ()) ->
  MulticoreGen ()
getNestLoop ScanNested = sLoopNestVectorized
getNestLoop _ = sLoopNest

applyScanOps ::
  ScanLoopType ->
  [VName] ->
  SegSpace ->
  [SubExp] ->
  [SegBinOp MCMem] ->
  [[VName]] ->
  ImpM MCMem HostEnv Imp.Multicore ()
applyScanOps typ scan_out space all_scan_res scan_ops local_accs = do
  let per_scan_res = segBinOpChunks scan_ops all_scan_res
      per_scan_out = segBinOpChunks scan_ops scan_out
  let (is, _) = unzip $ unSegSpace space

  -- Potential vector load and then do sequential scan
  getScanLoop typ $ \j ->
    forM_ (zip4 per_scan_out scan_ops per_scan_res local_accs) $ \(outs, scan_op, scan_res, acc) ->
      getNestLoop typ (segBinOpShape scan_op) $ \vec_is -> do
        sComment "Read accumulator" $
          forM_ (zip (xParams scan_op) acc) $ \(p, acc') -> do
            copyDWIMFix (paramName p) [] (Var acc') vec_is
        sComment "Read next values" $
          forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
            getExtract typ j $
              collect $
                copyDWIMFix (paramName p) [] se vec_is
        -- Scan body
        sComment "Scan op body" $
          compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip3 acc outs $ map resSubExp $ bodyResult $ lamBody scan_op) $
              \(acc', out, se) -> do
                copyDWIMFix out (map Imp.le64 is ++ vec_is) se []
                copyDWIMFix acc' vec_is se []

-- Generate a loop which performs a potentially vectorized scan on the
-- result of a kernel body.
genScanLoop ::
  ScanLoopType ->
  SegSpace ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  [[VName]] ->
  [VName] ->
  [VName] ->
  Imp.TExp Int64 ->
  ImpM MCMem HostEnv Imp.Multicore ()
genScanLoop typ space kbody scan_ops local_accs scan_out map_out i = do
  let (all_scan_res, map_res) =
        splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
  let (is, ns) = unzip $ unSegSpace space
      ns' = map pe64 ns
  let map_subexps = kernelResultSubExp <$> map_res
  zipWithM_ dPrimV_ is $ unflattenIndex ns' i
  compileStms mempty (kernelBodyStms kbody) $ do
    sComment "write mapped values results to memory" $
      forM_ (zip map_out map_subexps) $ \(name, subexp) -> do
        let is' = map (Imp.le64 . fst) $ unSegSpace space
        copyDWIMFix name is' subexp []
    sComment "Apply scan op" $
      applyScanOps typ scan_out space (map kernelResultSubExp all_scan_res) scan_ops local_accs

scanStage1Scalar ::
  SegSpace ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  [VName] ->
  [VName] ->
  MulticoreGen ()
scanStage1Scalar space kbody scan_ops scan_out map_out = do
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    genBinOpParams scan_ops
    local_accs <- genLocalAccsStage1 scan_ops
    inISPC $
      generateChunkLoop "SegScan" Vectorized $
        genScanLoop ScanScalar space kbody scan_ops local_accs scan_out map_out
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_1" fbody free_params

scanStage1Nested ::
  SegSpace ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  [VName] ->
  [VName] ->
  MulticoreGen ()
scanStage1Nested space kbody scan_ops scan_out map_out = do
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    local_accs <- genLocalAccsStage1 scan_ops

    inISPC $ do
      genBinOpParams scan_ops
      generateChunkLoop "SegScan" Scalar $
        genScanLoop ScanNested space kbody scan_ops local_accs scan_out map_out

  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_1" fbody free_params

scanStage1Fallback ::
  SegSpace ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  [VName] ->
  [VName] ->
  MulticoreGen ()
scanStage1Fallback space kbody scan_ops scan_out map_out = do
  -- Stage 1 : each thread partially scans a chunk of the input
  -- Writes directly to the resulting array
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    genBinOpParams scan_ops
    local_accs <- genLocalAccsStage1 scan_ops

    generateChunkLoop "SegScan" Scalar $
      genScanLoop ScanSeq space kbody scan_ops local_accs scan_out map_out
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_1" fbody free_params

scanStage2 ::
  TV Int32 ->
  SegSpace ->
  [SegBinOp MCMem] ->
  [VName] ->
  MulticoreGen [[VName]]
scanStage2 nsubtasks space scan_ops scan_out = do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map pe64 ns
      per_scan_out = segBinOpChunks scan_ops scan_out
      nsubtasks' = sExt64 $ tvExp nsubtasks

  dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
  offset <- dPrimV "offset" (0 :: Imp.TExp Int64)
  let offset' = tvExp offset
  offset_index <- dPrimV "offset_index" (0 :: Imp.TExp Int64)
  let offset_index' = tvExp offset_index

  -- Parameters used to find the chunk sizes
  -- Perhaps get this information from ``scheduling information``
  -- instead of computing it manually here.
  let iter_pr_subtask = product ns_64 `quot` nsubtasks'
      remainder = product ns_64 `rem` nsubtasks'

  carries <- carryArrays "scan_stage_2_carry" nsubtasks scan_ops
  sComment "carry-in for first chunk is neutral" $
    forM_ (zip scan_ops carries) $ \(scan_op, carry) ->
      sLoopNest (segBinOpShape scan_op) $ \vec_is ->
        forM_ (zip carry $ segBinOpNeutral scan_op) $ \(carry', ne) ->
          copyDWIMFix carry' (0 : vec_is) ne []

  -- Perform sequential scan over the last element of each chunk
  sComment "scan carries" $ sFor "i" (nsubtasks' - 1) $ \i -> do
    offset <-- iter_pr_subtask
    sWhen (sExt64 i .<. remainder) (offset <-- offset' + 1)
    offset_index <-- offset_index' + offset'
    zipWithM_ dPrimV_ is $ unflattenIndex ns_64 $ sExt64 offset_index'

    forM_ (zip3 per_scan_out scan_ops carries) $ \(outs, scan_op, carry) ->
      sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
        sComment "Read carry" $
          forM_ (zip (xParams scan_op) carry) $ \(p, carry') ->
            copyDWIMFix (paramName p) [] (Var carry') (i : vec_is)

        sComment "Read next values" $
          forM_ (zip (yParams scan_op) outs) $ \(p, out) ->
            copyDWIMFix (paramName p) [] (Var out) ((offset_index' - 1) : vec_is)

        compileStms mempty (bodyStms $ lamBody scan_op) $
          forM_ (zip carry $ map resSubExp $ bodyResult $ lamBody scan_op) $ \(carry', se) -> do
            copyDWIMFix carry' ((i + 1) : vec_is) se []

  -- Return the array of carries for each chunk.
  pure carries

scanStage3Scalar ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  [[VName]] ->
  SegPostOp MCMem ->
  [VName] ->
  [VName] ->
  MulticoreGen ()
scanStage3Scalar pat space scan_ops per_scan_carries post_op scan_out map_out = do
  let per_scan_out = segBinOpChunks scan_ops scan_out
      (scan_pars, map_pars) = splitAt (segBinOpResults scan_ops) $ lambdaParams $ segPostOpLambda post_op
      per_scan_post_par = segBinOpChunks scan_ops scan_pars
      (is, ns) = unzip $ unSegSpace space
      ns' = map pe64 ns
      (idxs, vals, map_res) = splitPostOpResults post_op $ fmap resSubExp $ bodyResult $ lambdaBody $ segPostOpLambda post_op
      groups = groupScatterResults (segPostOpScatterSpec post_op) (idxs <> vals)
      (pat_scatter, pat_map) = splitAt (length groups) $ patElems pat

  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId $ segFlat space

    inISPC $ do
      genBinOpParams scan_ops
      sComment "load carry-in" $
        forM_ (zip per_scan_carries scan_ops) $ \(op_carries, scan_op) ->
          forM_ (zip (xParams scan_op) op_carries) $ \(p, carries) ->
            copyDWIMFix (paramName p) [] (Var carries) [le64 (segFlat space)]
      generateChunkLoop "SegScan" Vectorized $ \i -> do
        dScope Nothing $
          scopeOfLParams $
            lambdaParams $
              segPostOpLambda post_op
        zipWithM_ dPrimV_ is $ unflattenIndex ns' i
        sComment "load partial result" $
          forM_ (zip per_scan_out scan_ops) $ \(outs, scan_op) ->
            forM_ (zip (yParams scan_op) outs) $ \(p, out) ->
              copyDWIMFix (paramName p) [] (Var out) (map le64 is)
        sComment "combine carry with partial result" $
          forM_ (zip per_scan_post_par scan_ops) $ \(ps, scan_op) ->
            compileStms mempty (bodyStms $ lamBody scan_op) $
              forM_ (zip ps $ bodyResult $ lambdaBody $ segBinOpLambda scan_op) $ \(p, res) ->
                copyDWIMFix (paramName p) [] (resSubExp res) []
        sComment "read kernel body map results" $
          forM_ (zip map_pars map_out) $ \(par, out) ->
            copyDWIMFix (paramName par) [] (Var out) (map le64 is)
        sComment "compute post op." $
          compileStms mempty (bodyStms $ lambdaBody $ segPostOpLambda post_op) $ do
            sComment "write scatter values." $
              forM_ (zip pat_scatter groups) $ \(pe, (_, arr, idxs_vals)) ->
                forM_ idxs_vals $ \(is', val) -> do
                  arr_t <- lookupType arr
                  let rws' = map pe64 $ arrayDims arr_t
                      slice' = fmap pe64 $ fullSlice arr_t $ map DimFix is'
                  sWhen (inBounds slice' rws') $
                    copyDWIM (patElemName pe) (unSlice slice') val []
            sComment "write mapped values" $
              forM_ (zip pat_map map_res) $ \(pe, res) ->
                copyDWIMFix (patElemName pe) (map le64 is) res []

  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_3" body free_params

scanStage3Nested ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  [[VName]] ->
  SegPostOp MCMem ->
  [VName] ->
  [VName] ->
  MulticoreGen ()
scanStage3Nested pat space scan_ops per_scan_carries post_op scan_out map_out = do
  let per_scan_out = segBinOpChunks scan_ops scan_out
      (scan_pars, map_pars) = splitAt (segBinOpResults scan_ops) $ lambdaParams $ segPostOpLambda post_op
      per_scan_pars = segBinOpChunks scan_ops scan_pars
      (is, ns) = unzip $ unSegSpace space
      ns' = map pe64 ns
      (idxs, vals, map_res) = splitPostOpResults post_op $ fmap resSubExp $ bodyResult $ lambdaBody $ segPostOpLambda post_op
      groups = groupScatterResults (segPostOpScatterSpec post_op) (idxs <> vals)
      (pat_scatter, pat_map) = splitAt (length groups) $ patElems pat
  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    generateChunkLoop "SegScan" Scalar $ \i -> do
      genBinOpParams scan_ops
      zipWithM_ dPrimV_ is $ unflattenIndex ns' i
      dScope Nothing $
        scopeOfLParams $
          lambdaParams $
            segPostOpLambda post_op

      forM_ (zip4 per_scan_pars per_scan_out per_scan_carries scan_ops) $ \(pars, outs, op_carries, scan_op) -> do
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
          sComment "load carry-in" $
            forM_ (zip (xParams scan_op) op_carries) $ \(p, carries) ->
              copyDWIMFix (paramName p) [] (Var carries) (le64 (segFlat space) : vec_is)

          sComment "load partial result" $
            forM_ (zip (yParams scan_op) outs) $ \(p, out) ->
              copyDWIMFix (paramName p) [] (Var out) (map le64 is ++ vec_is)

          sComment "combine carry with partial result" $
            compileStms mempty (bodyStms $ lamBody scan_op) $
              forM_ (zip pars $ map resSubExp $ bodyResult $ lamBody scan_op) $ \(par, se) ->
                copyDWIMFix (paramName par) vec_is se []

      sComment "copy map out to params" $
        forM_ (zip map_out map_pars) $ \(out, par) ->
          copyDWIMFix (paramName par) [] (Var out) []

      sComment "compute post op" $
        compileStms mempty (bodyStms $ lambdaBody $ segPostOpLambda post_op) $ do
          sComment "write scatter values." $
            forM_ (zip pat_scatter groups) $ \(pe, (_, arr, idxs_vals)) ->
              forM_ idxs_vals $ \(is', val) -> do
                arr_t <- lookupType arr
                let rws' = map pe64 $ arrayDims arr_t
                    slice' = fmap pe64 $ fullSlice arr_t $ map DimFix is'
                sWhen (inBounds slice' rws') $
                  copyDWIM (patElemName pe) (unSlice slice') val []
          sComment "write mapped values" $
            forM_ (zip pat_map map_res) $ \(pe, res) ->
              copyDWIMFix (patElemName pe) (map le64 is) res []

  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_3" body free_params

scanStage3Fallback ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  [[VName]] ->
  SegPostOp MCMem ->
  [VName] ->
  [VName] ->
  MulticoreGen ()
scanStage3Fallback pat space scan_ops per_scan_carries post_op scan_out map_out = do
  let per_scan_out = segBinOpChunks scan_ops scan_out
      (scan_pars, map_pars) = splitAt (segBinOpResults scan_ops) $ lambdaParams $ segPostOpLambda post_op
      per_scan_pars = segBinOpChunks scan_ops scan_pars
      (is, ns) = unzip $ unSegSpace space
      ns' = map pe64 ns
      (idxs, vals, map_res) = splitPostOpResults post_op $ fmap resSubExp $ bodyResult $ lambdaBody $ segPostOpLambda post_op
      groups = groupScatterResults (segPostOpScatterSpec post_op) (idxs <> vals)
      (pat_scatter, pat_map) = splitAt (length groups) $ patElems pat
  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    genBinOpParams scan_ops

    generateChunkLoop "SegScan" Scalar $ \i -> do
      genBinOpParams scan_ops
      zipWithM_ dPrimV_ is $ unflattenIndex ns' i
      dScope Nothing $
        scopeOfLParams $
          lambdaParams $
            segPostOpLambda post_op

      forM_ (zip4 per_scan_pars per_scan_out per_scan_carries scan_ops) $ \(pars, outs, op_carries, scan_op) -> do
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
          sComment "load carry-in" $
            forM_ (zip (xParams scan_op) op_carries) $ \(p, carries) ->
              copyDWIMFix (paramName p) [] (Var carries) (le64 (segFlat space) : vec_is)

          sComment "load partial result" $
            forM_ (zip (yParams scan_op) outs) $ \(p, out) ->
              copyDWIMFix (paramName p) [] (Var out) (map le64 is ++ vec_is)

          sComment "combine carry with partial result" $
            compileStms mempty (bodyStms $ lamBody scan_op) $
              forM_ (zip pars $ map resSubExp $ bodyResult $ lamBody scan_op) $ \(par, se) ->
                copyDWIMFix (paramName par) vec_is se []

      sComment "copy map out to params" $
        forM_ (zip map_out map_pars) $ \(out, par) ->
          copyDWIMFix (paramName par) [] (Var out) []

      sComment "compute post op" $
        compileStms mempty (bodyStms $ lambdaBody $ segPostOpLambda post_op) $ do
          sComment "write scatter values." $
            forM_ (zip pat_scatter groups) $ \(pe, (_, arr, idxs_vals)) ->
              forM_ idxs_vals $ \(is', val) -> do
                arr_t <- lookupType arr
                let rws' = map pe64 $ arrayDims arr_t
                    slice' = fmap pe64 $ fullSlice arr_t $ map DimFix is'
                sWhen (inBounds slice' rws') $
                  copyDWIM (patElemName pe) (unSlice slice') val []
          sComment "write mapped values" $
            forM_ (zip pat_map map_res) $ \(pe, res) ->
              copyDWIMFix (patElemName pe) (map le64 is) res []

  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_3" body free_params

-- Note: This isn't currently used anywhere.
-- This implementation for a Segmented scan only
-- parallelize over the segments and each segment is
-- scanned sequentially.
segmentedScan ::
  Pat LetDecMem ->
  SegSpace ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  SegPostOp MCMem ->
  MulticoreGen Imp.MCCode
segmentedScan pat space kbody scan_ops post_op = do
  emit $ Imp.DebugPrint "segmented segScan" Nothing
  collect $ do
    body <- compileSegScanBody pat space kbody scan_ops post_op
    free_params <- freeParams body
    emit $ Imp.Op $ Imp.ParLoop "seg_scan" body free_params

compileSegScanBody ::
  Pat LetDecMem ->
  SegSpace ->
  KernelBody MCMem ->
  [SegBinOp MCMem] ->
  SegPostOp MCMem ->
  MulticoreGen Imp.MCCode
compileSegScanBody pat space kbody scan_ops post_op = collect $ do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map pe64 ns

  dPrim_ (segFlat space) int64
  sOp $ Imp.GetTaskId (segFlat space)

  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  generateChunkLoop "SegScan" Scalar $ \segment_i -> do
    forM_ (zip scan_ops per_scan_pes) $ \(scan_op, scan_pes) -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan_op
      let (scan_x_params, scan_y_params) = splitAt (length $ segBinOpNeutral scan_op) $ (lambdaParams . segBinOpLambda) scan_op

      forM_ (zip scan_x_params $ segBinOpNeutral scan_op) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

      let inner_bound = last ns_64

      -- Perform a sequential scan over the segment ``segment_i``
      sFor "i" inner_bound $ \i -> do
        zipWithM_ dPrimV_ (init is) $ unflattenIndex (init ns_64) segment_i
        dPrimV_ (last is) i
        compileStms mempty (kernelBodyStms kbody) $ do
          let (scan_res, map_res) = splitAt (length $ segBinOpNeutral scan_op) $ kernelBodyResult kbody
          sComment "write to-scan values to parameters" $
            forM_ (zip scan_y_params scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) []

          sComment "write mapped values results to memory" $
            forM_ (zip (drop (length $ segBinOpNeutral scan_op) $ patElems pat) map_res) $ \(pe, se) ->
              copyDWIMFix (patElemName pe) (map Imp.le64 is) (kernelResultSubExp se) []

          sComment "combine with carry and write to memory" $
            compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scan_op) $
              forM_ (zip3 scan_x_params scan_pes $ map resSubExp $ bodyResult $ lambdaBody $ segBinOpLambda scan_op) $ \(p, pe, se) -> do
                copyDWIMFix (patElemName pe) (map Imp.le64 is) se []
                copyDWIMFix (paramName p) [] se []
