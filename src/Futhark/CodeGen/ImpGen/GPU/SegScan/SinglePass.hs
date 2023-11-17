{-# LANGUAGE TypeFamilies #-}

-- | Code generation for segmented and non-segmented scans.  Uses a
-- fast single-pass algorithm, but which only works on NVIDIA GPUs and
-- with some constraints on the operator.  We use this when we can.
module Futhark.CodeGen.ImpGen.GPU.SegScan.SinglePass (compileSegScan) where

import Control.Monad
import Data.List (zip4, zip7)
import Data.Maybe
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.IR.GPUMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Transform.Rename
import Futhark.Util (mapAccumLM, takeLast)
import Futhark.Util.IntegralExp (IntegralExp (mod, rem), divUp, quot)
import Prelude hiding (mod, quot, rem)

xParams, yParams :: SegBinOp GPUMem -> [LParam GPUMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

alignTo :: (IntegralExp a) => a -> a -> a
alignTo x a = (x `divUp` a) * a

createLocalArrays ::
  Count GroupSize SubExp ->
  SubExp ->
  [PrimType] ->
  InKernelGen (VName, [VName], [VName], VName, [VName])
createLocalArrays (Count groupSize) chunk types = do
  let groupSizeE = pe64 groupSize
      workSize = pe64 chunk * groupSizeE
      prefixArraysSize =
        foldl (\acc tySize -> alignTo acc tySize + tySize * groupSizeE) 0 $
          map primByteSize types
      maxTransposedArraySize =
        foldl1 sMax64 $ map (\ty -> workSize * primByteSize ty) types

      warpSize :: (Num a) => a
      warpSize = 32
      maxWarpExchangeSize =
        foldl (\acc tySize -> alignTo acc tySize + tySize * fromInteger warpSize) 0 $
          map primByteSize types
      maxLookbackSize = maxWarpExchangeSize + warpSize
      size = Imp.bytes $ maxLookbackSize `sMax64` prefixArraysSize `sMax64` maxTransposedArraySize

  (_, byteOffsets) <-
    mapAccumLM
      ( \off tySize -> do
          off' <- dPrimVE "byte_offsets" $ alignTo off tySize + pe64 groupSize * tySize
          pure (off', off)
      )
      0
      $ map primByteSize types

  (_, warpByteOffsets) <-
    mapAccumLM
      ( \off tySize -> do
          off' <- dPrimVE "warp_byte_offset" $ alignTo off tySize + warpSize * tySize
          pure (off', off)
      )
      warpSize
      $ map primByteSize types

  sComment "Allocate reusable shared memory" $ pure ()

  localMem <- sAlloc "local_mem" size (Space "local")
  transposeArrayLength <- dPrimV "trans_arr_len" workSize

  sharedId <- sArrayInMem "shared_id" int32 (Shape [constant (1 :: Int32)]) localMem

  transposedArrays <-
    forM types $ \ty ->
      sArrayInMem
        "local_transpose_arr"
        ty
        (Shape [tvSize transposeArrayLength])
        localMem

  prefixArrays <-
    forM (zip byteOffsets types) $ \(off, ty) -> do
      let off' = off `quot` primByteSize ty
      sArray
        "local_prefix_arr"
        ty
        (Shape [groupSize])
        localMem
        $ LMAD.iota off' [pe64 groupSize]

  warpscan <- sArrayInMem "warpscan" int8 (Shape [constant (warpSize :: Int64)]) localMem
  warpExchanges <-
    forM (zip warpByteOffsets types) $ \(off, ty) -> do
      let off' = off `quot` primByteSize ty
      sArray
        "warp_exchange"
        ty
        (Shape [constant (warpSize :: Int64)])
        localMem
        $ LMAD.iota off' [warpSize]

  pure (sharedId, transposedArrays, prefixArrays, warpscan, warpExchanges)

statusX, statusA, statusP :: (Num a) => a
statusX = 0
statusA = 1
statusP = 2

inBlockScanLookback ::
  KernelConstants ->
  Imp.TExp Int64 ->
  VName ->
  [VName] ->
  Lambda GPUMem ->
  InKernelGen ()
inBlockScanLookback constants arrs_full_size flag_arr arrs scan_lam = everythingVolatile $ do
  flg_x <- dPrim "flg_x" p_int8
  flg_y <- dPrim "flg_y" p_int8
  let flg_param_x = Param mempty (tvVar flg_x) (MemPrim p_int8)
      flg_param_y = Param mempty (tvVar flg_y) (MemPrim p_int8)
      flg_y_exp = tvExp flg_y
      statusP_e = statusP :: Imp.TExp Int8
      statusX_e = statusX :: Imp.TExp Int8

  dLParams (lambdaParams scan_lam)

  skip_threads <- dPrim "skip_threads" int32
  let in_block_thread_active =
        tvExp skip_threads .<=. in_block_id
      actual_params = lambdaParams scan_lam
      (x_params, y_params) =
        splitAt (length actual_params `div` 2) actual_params
      y_to_x =
        forM_ (zip x_params y_params) $ \(x, y) ->
          when (primType (paramType x)) $
            copyDWIM (paramName x) [] (Var (paramName y)) []
      y_to_x_flg =
        copyDWIM (tvVar flg_x) [] (Var (tvVar flg_y)) []

  -- Set initial y values
  sComment "read input for in-block scan" $ do
    zipWithM_ readInitial (flg_param_y : y_params) (flag_arr : arrs)
    -- Since the final result is expected to be in x_params, we may
    -- need to copy it there for the first thread in the block.
    sWhen (in_block_id .==. 0) $ do
      y_to_x
      y_to_x_flg

  when array_scan barrier

  let op_to_x = do
        sIf
          (flg_y_exp .==. statusP_e .||. flg_y_exp .==. statusX_e)
          ( do
              y_to_x_flg
              y_to_x
          )
          (compileBody' x_params $ lambdaBody scan_lam)

  sComment "in-block scan (hopefully no barriers needed)" $ do
    skip_threads <-- 1

    sWhile (tvExp skip_threads .<. block_size) $ do
      sWhen in_block_thread_active $ do
        sComment "read operands" $
          zipWithM_
            (readParam (sExt64 $ tvExp skip_threads))
            (flg_param_x : x_params)
            (flag_arr : arrs)
        sComment "perform operation" op_to_x

        sComment "write result" $
          sequence_ $
            zipWith3
              writeResult
              (flg_param_x : x_params)
              (flg_param_y : y_params)
              (flag_arr : arrs)

      skip_threads <-- tvExp skip_threads * 2
  where
    p_int8 = IntType Int8
    block_size = 32
    block_id = ltid32 `quot` block_size
    in_block_id = ltid32 - block_id * block_size
    ltid32 = kernelLocalThreadId constants
    ltid = sExt64 ltid32
    gtid = sExt64 $ kernelGlobalThreadId constants
    array_scan = not $ all primType $ lambdaReturnType scan_lam
    barrier
      | array_scan =
          sOp $ Imp.Barrier Imp.FenceGlobal
      | otherwise =
          sOp $ Imp.Barrier Imp.FenceLocal

    readInitial p arr
      | primType $ paramType p =
          copyDWIMFix (paramName p) [] (Var arr) [ltid]
      | otherwise =
          copyDWIMFix (paramName p) [] (Var arr) [gtid]
    readParam behind p arr
      | primType $ paramType p =
          copyDWIMFix (paramName p) [] (Var arr) [ltid - behind]
      | otherwise =
          copyDWIMFix (paramName p) [] (Var arr) [gtid - behind + arrs_full_size]

    writeResult x y arr
      | primType $ paramType x = do
          copyDWIMFix arr [ltid] (Var $ paramName x) []
          copyDWIM (paramName y) [] (Var $ paramName x) []
      | otherwise =
          copyDWIM (paramName y) [] (Var $ paramName x) []

-- | Compile 'SegScan' instance to host-level code with calls to a
-- single-pass kernel.
compileSegScan ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  SegBinOp GPUMem ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegScan pat lvl space scan_op map_kbody = do
  attrs <- lvlKernelAttrs lvl
  let Pat all_pes = pat

      scanop_nes = segBinOpNeutral scan_op

      n = product $ map pe64 $ segSpaceDims space

      tys = map (\(Prim pt) -> pt) $ lambdaReturnType $ segBinOpLambda scan_op
      tys_sizes = map primByteSize tys

      sumT, maxT :: Integer
      sumT = sum tys_sizes
      sumT' = sum (map (max 4 . primByteSize) tys) `div` 4
      maxT = maximum tys_sizes

      -- TODO: Make these constants dynamic by querying device
      k_reg = 64
      k_mem = 95

      mem_constraint = max k_mem sumT `div` maxT
      reg_constraint = (k_reg - 1 - sumT') `div` (2 * sumT')

      chunk :: (Num a) => a
      chunk = fromIntegral $ max 1 $ min mem_constraint reg_constraint

      group_size_e = pe64 $ unCount $ kAttrGroupSize attrs
      num_physgroups_e = pe64 $ unCount $ kAttrNumGroups attrs

  num_virtgroups <-
    tvSize <$> dPrimV "num_virtgroups" (n `divUp` (group_size_e * chunk))
  let num_virtgroups_e = pe64 num_virtgroups

  num_virt_threads <-
    dPrimVE "num_virt_threads" $ num_virtgroups_e * group_size_e

  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      segmented = length dims' > 1
      not_segmented_e = fromBool $ not segmented
      segment_size = last dims'

  let debug_ s v = emit $ Imp.DebugPrint s $ Just $ untyped (v :: Imp.TExp Int32)
  debug_ "Sequential elements per thread (chunk) " chunk
  debug_ "Memory constraint" $ fromIntegral mem_constraint
  debug_ "Register constraint" $ fromIntegral reg_constraint
  debug_ "sumT'" $ fromIntegral sumT'

  statusFlags <- sAllocArray "status_flags" int8 (Shape [num_virtgroups]) (Space "device")
  sReplicate statusFlags $ intConst Int8 statusX

  (aggregateArrays, incprefixArrays) <-
    fmap unzip $
      forM tys $ \ty ->
        (,)
          <$> sAllocArray "aggregates" ty (Shape [num_virtgroups]) (Space "device")
          <*> sAllocArray "incprefixes" ty (Shape [num_virtgroups]) (Space "device")

  global_id <- genZeroes "global_dynid" 1

  sKernelThread "segscan" (segFlat space) attrs $ do
    constants <- kernelConstants <$> askEnv

    (sharedId, transposedArrays, prefixArrays, warpscan, exchanges) <-
      createLocalArrays (kAttrGroupSize attrs) (intConst Int64 chunk) tys

    -- We wrap the entire kernel body in a virtualisation loop to handle the
    -- case where we do not have enough workgroups to cover the iteration space.
    -- Dynamic group indexing has no implication on this, since each group
    -- simply fetches a new dynamic ID upon entry into the virtualisation loop.
    --
    -- We could use virtualiseGroups, but this introduces a barrier which is
    -- redundant in this case, and also we don't need to base virtual group IDs
    -- on the loop variable, but rather on the dynamic IDs.
    physgroup_id <- dPrim "physgroup_id" int32
    sOp $ Imp.GetGroupId (tvVar physgroup_id) 0
    iters <-
      dPrimVE "virtloop_bound" $
        (num_virtgroups_e - tvExp physgroup_id)
          `divUp` num_physgroups_e

    sFor "virtloop_i" iters $ const $ do
      dyn_id <- dPrim "dynamic_id" int32
      sComment "First thread in block fetches this block's dynamic_id" $ do
        sWhen (kernelLocalThreadId constants .==. 0) $ do
          (globalIdMem, _, globalIdOff) <- fullyIndexArray global_id [0]
          sOp $
            Imp.Atomic DefaultSpace $
              Imp.AtomicAdd
                Int32
                (tvVar dyn_id)
                globalIdMem
                (Count $ unCount globalIdOff)
                (untyped (1 :: Imp.TExp Int32))
          sComment "Set dynamic id for this block" $ do
            copyDWIMFix sharedId [0] (tvSize dyn_id) []

          sComment "First thread in last (virtual) block resets global dynamic_id" $ do
            sWhen (tvExp dyn_id .==. num_virtgroups_e - 1) $
              copyDWIMFix global_id [0] (intConst Int32 0) []

      let local_barrier = Imp.Barrier Imp.FenceLocal
          local_fence = Imp.MemFence Imp.FenceLocal
          global_fence = Imp.MemFence Imp.FenceGlobal

      sOp local_barrier
      copyDWIMFix (tvVar dyn_id) [] (Var sharedId) [0]
      sOp local_barrier

      blockOff <-
        dPrimV "blockOff" $
          sExt64 (tvExp dyn_id) * chunk * group_size_e -- kernelGroupSize constants
      sgmIdx <- dPrimVE "sgm_idx" $ tvExp blockOff `mod` segment_size
      boundary <-
        dPrimVE "boundary" $
          sExt32 $
            sMin64 (chunk * group_size_e) (segment_size - sgmIdx)
      segsize_compact <-
        dPrimVE "segsize_compact" $
          sExt32 $
            sMin64 (chunk * group_size_e) segment_size
      privateArrays <-
        forM tys $ \ty ->
          sAllocArray
            "private"
            ty
            (Shape [intConst Int64 chunk])
            (ScalarSpace [intConst Int64 chunk] ty)

      sComment "Load and map" $
        sFor "i" chunk $ \i -> do
          -- The map's input index
          virt_tid <-
            dPrimVE "virt_tid" $
              tvExp blockOff
                + sExt64 (kernelLocalThreadId constants)
                + i * kernelGroupSize constants
          dIndexSpace (zip gtids dims') virt_tid
          -- Perform the map
          let in_bounds =
                compileStms mempty (kernelBodyStms map_kbody) $ do
                  let (all_scan_res, map_res) =
                        splitAt (segBinOpResults [scan_op]) $ kernelBodyResult map_kbody

                  -- Write map results to their global memory destinations
                  forM_ (zip (takeLast (length map_res) all_pes) map_res) $ \(dest, src) ->
                    copyDWIMFix (patElemName dest) (map Imp.le64 gtids) (kernelResultSubExp src) []

                  -- Write to-scan results to private memory.
                  forM_ (zip privateArrays $ map kernelResultSubExp all_scan_res) $ \(dest, src) ->
                    copyDWIMFix dest [i] src []

              out_of_bounds =
                forM_ (zip privateArrays scanop_nes) $ \(dest, ne) ->
                  copyDWIMFix dest [i] ne []

          sIf (virt_tid .<. n) in_bounds out_of_bounds

      sOp $ Imp.ErrorSync Imp.FenceLocal
      sComment "Transpose scan inputs" $ do
        forM_ (zip transposedArrays privateArrays) $ \(trans, priv) -> do
          sFor "i" chunk $ \i -> do
            sharedIdx <-
              dPrimVE "sharedIdx" $
                sExt64 (kernelLocalThreadId constants)
                  + i * kernelGroupSize constants
            copyDWIMFix trans [sharedIdx] (Var priv) [i]
          sOp local_barrier
          sFor "i" chunk $ \i -> do
            sharedIdx <- dPrimV "sharedIdx" $ kernelLocalThreadId constants * chunk + i
            copyDWIMFix priv [sExt64 i] (Var trans) [sExt64 $ tvExp sharedIdx]
          sOp local_barrier

      sComment "Per thread scan" $ do
        -- We don't need to touch the first element, so only m-1
        -- iterations here.
        globalIdx <-
          dPrimVE "gidx" $
            (kernelLocalThreadId constants * chunk) + 1
        sFor "i" (chunk - 1) $ \i -> do
          let xs = map paramName $ xParams scan_op
              ys = map paramName $ yParams scan_op
          -- determine if start of segment
          new_sgm <-
            if segmented
              then dPrimVE "new_sgm" $ (globalIdx + sExt32 i - boundary) `mod` segsize_compact .==. 0
              else pure false
          -- skip scan of first element in segment
          sUnless new_sgm $ do
            forM_ (zip4 privateArrays xs ys tys) $ \(src, x, y, ty) -> do
              dPrim_ x ty
              dPrim_ y ty
              copyDWIMFix x [] (Var src) [i]
              copyDWIMFix y [] (Var src) [i + 1]

            compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scan_op) $
              forM_ (zip privateArrays $ map resSubExp $ bodyResult $ lambdaBody $ segBinOpLambda scan_op) $ \(dest, res) ->
                copyDWIMFix dest [i + 1] res []

      sComment "Publish results in shared memory" $ do
        forM_ (zip prefixArrays privateArrays) $ \(dest, src) ->
          copyDWIMFix dest [sExt64 $ kernelLocalThreadId constants] (Var src) [chunk - 1]
        sOp local_barrier

      let crossesSegment = do
            guard segmented
            Just $ \from to ->
              let from' = (from + 1) * chunk - 1
                  to' = (to + 1) * chunk - 1
               in (to' - from') .>. (to' + segsize_compact - boundary) `mod` segsize_compact

      scan_op1 <- renameLambda $ segBinOpLambda scan_op

      accs <- mapM (dPrim "acc") tys
      sComment "Scan results (with warp scan)" $ do
        groupScan
          crossesSegment
          num_virt_threads
          (kernelGroupSize constants)
          scan_op1
          prefixArrays

        sOp $ Imp.ErrorSync Imp.FenceLocal

        let firstThread acc prefixes =
              copyDWIMFix (tvVar acc) [] (Var prefixes) [sExt64 (kernelGroupSize constants) - 1]
            notFirstThread acc prefixes =
              copyDWIMFix (tvVar acc) [] (Var prefixes) [sExt64 (kernelLocalThreadId constants) - 1]
        sIf
          (kernelLocalThreadId constants .==. 0)
          (zipWithM_ firstThread accs prefixArrays)
          (zipWithM_ notFirstThread accs prefixArrays)

        sOp local_barrier

      prefixes <- forM (zip scanop_nes tys) $ \(ne, ty) ->
        dPrimV "prefix" $ TPrimExp $ toExp' ty ne
      blockNewSgm <- dPrimVE "block_new_sgm" $ sgmIdx .==. 0
      sComment "Perform lookback" $ do
        sWhen (blockNewSgm .&&. kernelLocalThreadId constants .==. 0) $ do
          everythingVolatile $
            forM_ (zip accs incprefixArrays) $ \(acc, incprefixArray) ->
              copyDWIMFix incprefixArray [tvExp dyn_id] (tvSize acc) []
          sOp global_fence
          everythingVolatile $
            copyDWIMFix statusFlags [tvExp dyn_id] (intConst Int8 statusP) []
          forM_ (zip scanop_nes accs) $ \(ne, acc) ->
            copyDWIMFix (tvVar acc) [] ne []
        -- end sWhen

        let warpSize = kernelWaveSize constants
        sWhen (bNot blockNewSgm .&&. kernelLocalThreadId constants .<. warpSize) $ do
          sWhen (kernelLocalThreadId constants .==. 0) $ do
            sIf
              (not_segmented_e .||. boundary .==. sExt32 (group_size_e * chunk))
              ( do
                  everythingVolatile $
                    forM_ (zip aggregateArrays accs) $ \(aggregateArray, acc) ->
                      copyDWIMFix aggregateArray [tvExp dyn_id] (tvSize acc) []
                  sOp global_fence
                  everythingVolatile $
                    copyDWIMFix statusFlags [tvExp dyn_id] (intConst Int8 statusA) []
              )
              ( do
                  everythingVolatile $
                    forM_ (zip incprefixArrays accs) $ \(incprefixArray, acc) ->
                      copyDWIMFix incprefixArray [tvExp dyn_id] (tvSize acc) []
                  sOp global_fence
                  everythingVolatile $
                    copyDWIMFix statusFlags [tvExp dyn_id] (intConst Int8 statusP) []
              )
            everythingVolatile $
              copyDWIMFix warpscan [0] (Var statusFlags) [tvExp dyn_id - 1]
          -- sWhen
          sOp local_fence

          status <- dPrim "status" int8 :: InKernelGen (TV Int8)
          copyDWIMFix (tvVar status) [] (Var warpscan) [0]

          sIf
            (tvExp status .==. statusP)
            ( sWhen (kernelLocalThreadId constants .==. 0) $
                everythingVolatile $
                  forM_ (zip prefixes incprefixArrays) $ \(prefix, incprefixArray) ->
                    copyDWIMFix (tvVar prefix) [] (Var incprefixArray) [tvExp dyn_id - 1]
            )
            ( do
                readOffset <-
                  dPrimV "readOffset" $
                    sExt32 $
                      tvExp dyn_id - sExt64 (kernelWaveSize constants)
                let loopStop = warpSize * (-1)
                    sameSegment readIdx
                      | segmented =
                          let startIdx = sExt64 (tvExp readIdx + 1) * kernelGroupSize constants * chunk - 1
                           in tvExp blockOff - startIdx .<=. sgmIdx
                      | otherwise = true
                sWhile (tvExp readOffset .>. loopStop) $ do
                  readI <- dPrimV "read_i" $ tvExp readOffset + kernelLocalThreadId constants
                  aggrs <- forM (zip scanop_nes tys) $ \(ne, ty) ->
                    dPrimV "aggr" $ TPrimExp $ toExp' ty ne
                  flag <- dPrimV "flag" (statusX :: Imp.TExp Int8)
                  everythingVolatile . sWhen (tvExp readI .>=. 0) $ do
                    sIf
                      (sameSegment readI)
                      ( do
                          copyDWIMFix (tvVar flag) [] (Var statusFlags) [sExt64 $ tvExp readI]
                          sIf
                            (tvExp flag .==. statusP)
                            ( forM_ (zip incprefixArrays aggrs) $ \(incprefix, aggr) ->
                                copyDWIMFix (tvVar aggr) [] (Var incprefix) [sExt64 $ tvExp readI]
                            )
                            ( sWhen (tvExp flag .==. statusA) $ do
                                forM_ (zip aggrs aggregateArrays) $ \(aggr, aggregate) ->
                                  copyDWIMFix (tvVar aggr) [] (Var aggregate) [sExt64 $ tvExp readI]
                            )
                      )
                      (copyDWIMFix (tvVar flag) [] (intConst Int8 statusP) [])
                  -- end sIf
                  -- end sWhen

                  forM_ (zip exchanges aggrs) $ \(exchange, aggr) ->
                    copyDWIMFix exchange [sExt64 $ kernelLocalThreadId constants] (tvSize aggr) []
                  copyDWIMFix warpscan [sExt64 $ kernelLocalThreadId constants] (tvSize flag) []

                  -- execute warp-parallel reduction but only if the last read flag in not STATUS_P
                  copyDWIMFix (tvVar flag) [] (Var warpscan) [sExt64 warpSize - 1]
                  sWhen (tvExp flag .<. (2 :: Imp.TExp Int8)) $ do
                    lam' <- renameLambda scan_op1
                    inBlockScanLookback
                      constants
                      num_virt_threads
                      warpscan
                      exchanges
                      lam'

                  -- all threads of the warp read the result of reduction
                  copyDWIMFix (tvVar flag) [] (Var warpscan) [sExt64 warpSize - 1]
                  forM_ (zip aggrs exchanges) $ \(aggr, exchange) ->
                    copyDWIMFix (tvVar aggr) [] (Var exchange) [sExt64 warpSize - 1]
                  -- update read offset
                  sIf
                    (tvExp flag .==. statusP)
                    (readOffset <-- loopStop)
                    ( sWhen (tvExp flag .==. statusA) $ do
                        readOffset <-- tvExp readOffset - zExt32 warpSize
                    )

                  -- update prefix if flag different than STATUS_X:
                  sWhen (tvExp flag .>. (statusX :: Imp.TExp Int8)) $ do
                    lam <- renameLambda scan_op1
                    let (xs, ys) = splitAt (length tys) $ map paramName $ lambdaParams lam
                    forM_ (zip xs aggrs) $ \(x, aggr) -> dPrimV_ x (tvExp aggr)
                    forM_ (zip ys prefixes) $ \(y, prefix) -> dPrimV_ y (tvExp prefix)
                    compileStms mempty (bodyStms $ lambdaBody lam) $
                      forM_ (zip3 prefixes tys $ map resSubExp $ bodyResult $ lambdaBody lam) $
                        \(prefix, ty, res) -> prefix <-- TPrimExp (toExp' ty res)
                  sOp local_fence
            )

          -- end sWhile
          -- end sIf
          sWhen (kernelLocalThreadId constants .==. 0) $ do
            scan_op2 <- renameLambda scan_op1
            let xs = map paramName $ take (length tys) $ lambdaParams scan_op2
                ys = map paramName $ drop (length tys) $ lambdaParams scan_op2
            sWhen (boundary .==. sExt32 (group_size_e * chunk)) $ do
              forM_ (zip xs prefixes) $ \(x, prefix) -> dPrimV_ x $ tvExp prefix
              forM_ (zip ys accs) $ \(y, acc) -> dPrimV_ y $ tvExp acc
              compileStms mempty (bodyStms $ lambdaBody scan_op2) $
                everythingVolatile $
                  forM_ (zip incprefixArrays $ map resSubExp $ bodyResult $ lambdaBody scan_op2) $
                    \(incprefixArray, res) -> copyDWIMFix incprefixArray [tvExp dyn_id] res []
              sOp global_fence
              everythingVolatile $ copyDWIMFix statusFlags [tvExp dyn_id] (intConst Int8 statusP) []
            forM_ (zip exchanges prefixes) $ \(exchange, prefix) ->
              copyDWIMFix exchange [0] (tvSize prefix) []
            forM_ (zip3 accs tys scanop_nes) $ \(acc, ty, ne) ->
              tvVar acc <~~ toExp' ty ne
        -- end sWhen
        -- end sWhen

        sWhen (bNot $ tvExp dyn_id .==. 0) $ do
          sOp local_barrier
          forM_ (zip exchanges prefixes) $ \(exchange, prefix) ->
            copyDWIMFix (tvVar prefix) [] (Var exchange) [0]
          sOp local_barrier
      -- end sWhen
      -- end sComment

      scan_op3 <- renameLambda scan_op1
      scan_op4 <- renameLambda scan_op1

      sComment "Distribute results" $ do
        let (xs, ys) = splitAt (length tys) $ map paramName $ lambdaParams scan_op3
            (xs', ys') = splitAt (length tys) $ map paramName $ lambdaParams scan_op4

        forM_ (zip7 prefixes accs xs xs' ys ys' tys) $
          \(prefix, acc, x, x', y, y', ty) -> do
            dPrim_ x ty
            dPrim_ y ty
            dPrimV_ x' $ tvExp prefix
            dPrimV_ y' $ tvExp acc

        sIf
          (kernelLocalThreadId constants * chunk .<. boundary .&&. bNot blockNewSgm)
          ( compileStms mempty (bodyStms $ lambdaBody scan_op4) $
              forM_ (zip3 xs tys $ map resSubExp $ bodyResult $ lambdaBody scan_op4) $
                \(x, ty, res) -> x <~~ toExp' ty res
          )
          (forM_ (zip xs accs) $ \(x, acc) -> copyDWIMFix x [] (Var $ tvVar acc) [])
        -- calculate where previous thread stopped, to determine number of
        -- elements left before new segment.
        stop <-
          dPrimVE "stopping_point" $
            segsize_compact - (kernelLocalThreadId constants * chunk - 1 + segsize_compact - boundary) `rem` segsize_compact
        sFor "i" chunk $ \i -> do
          sWhen (sExt32 i .<. stop - 1) $ do
            forM_ (zip privateArrays ys) $ \(src, y) ->
              -- only include prefix for the first segment part per thread
              copyDWIMFix y [] (Var src) [i]
            compileStms mempty (bodyStms $ lambdaBody scan_op3) $
              forM_ (zip privateArrays $ map resSubExp $ bodyResult $ lambdaBody scan_op3) $
                \(dest, res) ->
                  copyDWIMFix dest [i] res []

      sComment "Transpose scan output and Write it to global memory in coalesced fashion" $ do
        forM_ (zip3 transposedArrays privateArrays $ map patElemName all_pes) $ \(locmem, priv, dest) -> do
          -- sOp local_barrier
          sFor "i" chunk $ \i -> do
            sharedIdx <-
              dPrimV "sharedIdx" $
                sExt64 (kernelLocalThreadId constants * chunk) + i
            copyDWIMFix locmem [tvExp sharedIdx] (Var priv) [i]
          sOp local_barrier
          sFor "i" chunk $ \i -> do
            flat_idx <-
              dPrimVE "flat_idx" $
                tvExp blockOff
                  + kernelGroupSize constants * i
                  + sExt64 (kernelLocalThreadId constants)
            dIndexSpace (zip gtids dims') flat_idx
            sWhen (flat_idx .<. n) $ do
              copyDWIMFix
                dest
                (map Imp.le64 gtids)
                (Var locmem)
                [sExt64 $ flat_idx - tvExp blockOff]
          sOp local_barrier
{-# NOINLINE compileSegScan #-}
