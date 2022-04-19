module Futhark.CodeGen.ImpGen.Multicore.SegScan
  ( compileSegScan,
  )
where

import Control.Monad
import Data.List (zip4)
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Util.IntegralExp (quot, rem)
import Prelude hiding (quot, rem)
import Debug.Trace (traceM, trace)

-- Compile a SegScan construct
compileSegScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen Imp.MCCode
compileSegScan pat space reds kbody nsubtasks
  | [_] <- unSegSpace space =
      nonsegmentedScan pat space reds kbody nsubtasks
  | otherwise =
      segmentedScan pat space reds kbody

xParams, yParams :: SegBinOp MCMem -> [LParam MCMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

lamBody :: SegBinOp MCMem -> Body MCMem
lamBody = lambdaBody . segBinOpLambda

-- Arrays for storing worker results.
resultArrays :: String -> [SegBinOp MCMem] -> MulticoreGen [[VName]]
resultArrays s segops =
  forM segops $ \(SegBinOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
      let pt = elemType t
          full_shape = shape <> arrayShape t
      sAllocArray s pt full_shape DefaultSpace

nonsegmentedScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen Imp.MCCode
nonsegmentedScan pat space scan_ops kbody nsubtasks = do
  emit $ Imp.DebugPrint "nonsegmented segScan" Nothing
  collect $ do
    -- Are we working with nested arrays
    let dims = map (shapeDims . segBinOpShape) scan_ops
    let isScalar x = case x of MemPrim _ -> True; _ -> False
    -- Are we only working on scalars
    let scalars = all (all (isScalar . paramDec) . (lambdaParams . segBinOpLambda)) scan_ops && all (==[]) dims
    -- Do we have nested vector operations    
    let vectorize = [] `notElem` dims

    let (scanStage1, scanStage3)
         | scalars = trace "scalar" (scalar, scalarStage3)
         | vectorize = trace "vec" (vectorized, vectorizedStage3)
         | otherwise = trace "other" (fallback, fallbackStage3)
    scanStage1 pat space kbody scan_ops

    let nsubtasks' = tvExp nsubtasks
    sWhen (nsubtasks' .>. 1) $ do
      scan_ops2 <- renameSegBinOp scan_ops
      scanStage2 pat nsubtasks space scan_ops2 kbody
      scan_ops3 <- renameSegBinOp scan_ops
      --scanStage3 False scalars pat space scan_ops3 kbody
      scanStage3 pat space kbody scan_ops3


data ScanType = Fallback | Vectorized | Scalar
-- Given a scan type indicating if we are generating a kernel, give a function
-- to inject into the loop body
getScanLoop ::
  ScanType
  -> (Imp.TExp Int64 -> MulticoreGen ())
  -> MulticoreGen ()
getScanLoop Scalar = generateUniformizeLoop
getScanLoop _ = \body -> body 0

-- Given a boolean indicating if we are generating a kernel, return a function
-- to extract a scalar from a vector
getExtract :: Bool -> Imp.TExp Int64 -> MulticoreGen Imp.MCCode -> MulticoreGen ()
getExtract True = extractVectorLane
getExtract False = \_ body -> body >>= emit

genBinOpParams :: [SegBinOp MCMem] -> MulticoreGen ()
genBinOpParams scan_ops = dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops

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
  ScanType
  -> Shape
  -> ([Imp.TExp Int64] -> MulticoreGen ())
  -> MulticoreGen ()
getNestLoop Vectorized = sLoopNestVectorized
getNestLoop _ = sLoopNest
-- Generate a loop which performs a potentially vectorized scan.
-- The @kernel@ flag controls indicates whether we are generating code
-- for an external kernel, and @mapout@ indicates whether this loop
-- is fused with a map.
genScanLoop ::
  ScanType
  -> Pat LetDecMem
  -> SegSpace
  -> KernelBody MCMem
  -> [SegBinOp MCMem]
  -> [[VName]]
  -> Bool
  -> Bool
  -> Imp.TExp Int64
  -> ImpM MCMem HostEnv Imp.Multicore ()
genScanLoop typ pat space kbody scan_ops local_accs mapout kernel i = do
  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res = segBinOpChunks scan_ops all_scan_res
      per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  let (is, ns) = unzip $ unSegSpace space
      ns' = map toInt64Exp ns

  zipWithM_ dPrimV_ is $ unflattenIndex ns' i
  compileStms mempty (kernelBodyStms kbody) $ do
    -- Potential vector load and then do sequential scan
    getScanLoop typ $ \j -> do
      when mapout $
        sComment "write mapped values results to memory" $ do
          let map_arrs = drop (segBinOpResults scan_ops) $ patElems pat
          zipWithM_ (compileThreadResult space) map_arrs map_res
      forM_ (zip4 per_scan_pes scan_ops per_scan_res local_accs) $ \(pes, scan_op, scan_res, acc) ->
        getNestLoop typ (segBinOpShape scan_op) $ \vec_is -> do
          -- Read accum value
          forM_ (zip (xParams scan_op) acc) $ \(p, acc') -> do   
            copyDWIMFix (paramName p) [] (Var acc') vec_is
          -- Read next value
          sComment "Read next values" $
            forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
              getExtract kernel j $ collect $
                copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is
          -- Scan body
          sComment "Scan body" $
            compileStms mempty (bodyStms $ lamBody scan_op) $
              forM_ (zip3 acc pes $ map resSubExp $ bodyResult $ lamBody scan_op) $
                \(acc', pe, se) -> do
                  copyDWIMFix (patElemName pe) (map Imp.le64 is ++ vec_is) se []
                  copyDWIMFix acc' vec_is se []

scanStage1' ::
  SegSpace
  -> [SegBinOp MCMem]
  -> ([[VName]] -> [[VName]] -> MulticoreGen ())
   -> MulticoreGen ()
scanStage1' space scan_ops f_loop =  do
  -- Stage 1 : each thread partially scans a chunk of the input
  -- Writes directly to the resulting array
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    -- Make one set of uniform params
    genBinOpParams scan_ops
    local_accs_uni <- genLocalAccsStage1 scan_ops

    scan_ops_vari <- renameSegBinOp scan_ops
    -- Make one set of potentially varying params
    genBinOpParams scan_ops_vari
    local_accs_vari <- genLocalAccsStage1 scan_ops_vari
    traceM $ "stage1 vari1 " ++ pretty local_accs_vari
    traceM $ "stage1 uni1 " ++ pretty local_accs_uni

    f_loop local_accs_uni local_accs_vari
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_1" fbody free_params

scalar ::
  Pat LetDecMem
  -> SegSpace
  -> KernelBody MCMem
  -> [SegBinOp MCMem]
  -> MulticoreGen ()
scalar pat space kbody scan_ops = do
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    -- Make one set of uniform params
    genBinOpParams scan_ops
    local_accs <- genLocalAccsStage1 scan_ops
    inISPC $ generateChunkLoop "SegScan" True $
      genScanLoop Scalar pat space kbody scan_ops local_accs True True
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_1" fbody free_params  

vectorized ::
  Pat LetDecMem
  -> SegSpace
  -> KernelBody MCMem
  -> [SegBinOp MCMem]
  ->  MulticoreGen ()
vectorized pat space kbody scan_ops = do
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)
    
    -- Make one set of uniform params        
    inISPC $ generateChunkLoop "SegScan" False $ \i -> do
      genBinOpParams scan_ops
      local_accs <- genLocalAccsStage1 scan_ops
      genScanLoop Vectorized pat space kbody scan_ops local_accs True True i

  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_1" fbody free_params  

fallback ::
  Pat LetDecMem
  -> SegSpace
  -> KernelBody MCMem
  -> [SegBinOp MCMem]
  ->  MulticoreGen ()
fallback pat space kbody scan_ops  = do
    -- Stage 1 : each thread partially scans a chunk of the input
  -- Writes directly to the resulting array
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    -- Make one set of uniform params
    genBinOpParams scan_ops
    local_accs <- genLocalAccsStage1 scan_ops

    inISPC $ generateChunkLoop "SegScan" False $
      genScanLoop Fallback pat space kbody scan_ops local_accs True False
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_1" fbody free_params

scanStage2 ::
  Pat LetDecMem ->
  TV Int32 ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen ()
scanStage2 pat nsubtasks space scan_ops kbody = do
  emit $ Imp.DebugPrint "nonsegmentedScan stage 2" Nothing
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map toInt64Exp ns
      per_scan_pes = segBinOpChunks scan_ops $ patElems pat
      nsubtasks' = tvExp nsubtasks

  dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
  offset <- dPrimV "offset" (0 :: Imp.TExp Int64)
  let offset' = tvExp offset
  offset_index <- dPrimV "offset_index" (0 :: Imp.TExp Int64)
  let offset_index' = tvExp offset_index

  -- Parameters used to find the chunk sizes
  -- Perhaps get this information from ``scheduling information``
  -- instead of computing it manually here.
  let iter_pr_subtask = product ns_64 `quot` sExt64 nsubtasks'
      remainder = product ns_64 `rem` sExt64 nsubtasks'

  accs <- resultArrays "scan_stage_2_accum" scan_ops
  forM_ (zip scan_ops accs) $ \(scan_op, acc) ->
    sLoopNest (segBinOpShape scan_op) $ \vec_is ->
      forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
        copyDWIMFix acc' vec_is ne []

  -- Perform sequential scan over the last element of each chunk
  sFor "i" (nsubtasks' - 1) $ \i -> do
    offset <-- iter_pr_subtask
    sWhen (sExt64 i .<. remainder) (offset <-- offset' + 1)
    offset_index <-- offset_index' + offset'
    zipWithM_ dPrimV_ is $ unflattenIndex ns_64 $ sExt64 offset_index'

    compileStms mempty (kernelBodyStms kbody) $
      forM_ (zip3 per_scan_pes scan_ops accs) $ \(pes, scan_op, acc) ->
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
          sComment "Read carry in" $
            forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
              copyDWIMFix (paramName p) [] (Var acc') vec_is

          sComment "Read next values" $
            forM_ (zip (yParams scan_op) pes) $ \(p, pe) ->
              copyDWIMFix (paramName p) [] (Var $ patElemName pe) ((offset_index' - 1) : vec_is)

          compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip3 acc pes $ map resSubExp $ bodyResult $ lamBody scan_op) $
              \(acc', pe, se) -> do
                copyDWIMFix (patElemName pe) ((offset_index' - 1) : vec_is) se []
                copyDWIMFix acc' vec_is se []

genLocalAccsStage3 :: [SegBinOp MCMem] -> [[PatElem LetDecMem]]-> MulticoreGen [[VName]]
genLocalAccsStage3 scan_ops per_scan_pes = 
  forM (zip scan_ops per_scan_pes) $ \(scan_op, pes) -> do
      let shape = segBinOpShape scan_op
          ts = lambdaReturnType $ segBinOpLambda scan_op
      forM (zip4 (xParams scan_op) pes ts $ segBinOpNeutral scan_op) $ \(p, pe, t, ne) -> do
        acc <-
          case shapeDims shape of
            [] -> pure $ paramName p
            _ -> do
              let pt = elemType t
              sAllocArray "local_acc" pt (shape <> arrayShape t) DefaultSpace

        -- Initialise the accumulator with neutral from previous chunk.
        -- or read neutral if first ``iter``
        (start, _end) <- getLoopBounds
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
          let read_carry_in =
                copyDWIMFix acc vec_is (Var $ patElemName pe) (start - 1 : vec_is)
              read_neutral =
                copyDWIMFix acc vec_is ne []
          sIf (start .==. 0) read_neutral read_carry_in
        pure acc

scanStage3' ::
  Pat LetDecMem
  -> SegSpace
  ->[ SegBinOp MCMem]
  -> ([[VName]] -> [[VName]] -> MulticoreGen ())
  -> MulticoreGen ()
scanStage3' pat space scan_ops f = do
  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  --traceM $ pretty per_scan_pes
  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    genBinOpParams scan_ops
    local_accs_uni <- genLocalAccsStage3 scan_ops per_scan_pes    
    
    scan_ops_vari <- renameSegBinOp scan_ops
    genBinOpParams scan_ops_vari  
    local_accs_vari <- genLocalAccsStage3 scan_ops_vari per_scan_pes

    f local_accs_uni local_accs_vari
  free_params' <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_3" body free_params'

scalarStage3 ::
  Pat LetDecMem
  -> SegSpace
  -> KernelBody MCMem
  -> [SegBinOp MCMem]
  -> MulticoreGen ()
scalarStage3 pat space kbody scan_ops = do
  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  --traceM $ pretty per_scan_pes
  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    genBinOpParams scan_ops
    local_accs <- genLocalAccsStage3 scan_ops per_scan_pes 

    inISPC $ generateChunkLoop "SegScan" True $
      genScanLoop Scalar pat space kbody scan_ops local_accs True True
  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_3" body free_params  

vectorizedStage3 ::
  Pat LetDecMem
  -> SegSpace
  -> KernelBody MCMem
  -> [SegBinOp MCMem]
  ->  MulticoreGen ()
vectorizedStage3 pat space kbody scan_ops = do
  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  --traceM $ pretty per_scan_pes
  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    inISPC $ generateChunkLoop "SegScan" False $ \i -> do
      genBinOpParams scan_ops
      local_accs <- genLocalAccsStage3 scan_ops per_scan_pes 
      genScanLoop Vectorized pat space kbody scan_ops local_accs True True i

  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_3" body free_params  

fallbackStage3 ::
  Pat LetDecMem
  -> SegSpace
  -> KernelBody MCMem
  -> [SegBinOp MCMem]
  ->  MulticoreGen ()
fallbackStage3 pat space kbody scan_ops  = do
  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  --traceM $ pretty per_scan_pes
  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    genBinOpParams scan_ops
    local_accs <- genLocalAccsStage3 scan_ops per_scan_pes 

    inISPC $ generateChunkLoop "SegScan" False $
      genScanLoop Fallback pat space kbody scan_ops local_accs True False
  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_3" body free_params


-- This implementation for a Segmented scan only
-- parallelize over the segments and each segment is
-- scanned sequentially.
segmentedScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen Imp.MCCode
segmentedScan pat space scan_ops kbody = do
  emit $ Imp.DebugPrint "segmented segScan" Nothing
  collect $ do
    body <- compileSegScanBody pat space scan_ops kbody
    free_params <- freeParams body
    emit $ Imp.Op $ Imp.ParLoop "seg_scan" body free_params

compileSegScanBody ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen Imp.MCCode
compileSegScanBody pat space scan_ops kbody = collect $ do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map toInt64Exp ns

  dPrim_ (segFlat space) int64
  sOp $ Imp.GetTaskId (segFlat space)

  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  generateChunkLoop "SegScan" True $ \segment_i -> do
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
