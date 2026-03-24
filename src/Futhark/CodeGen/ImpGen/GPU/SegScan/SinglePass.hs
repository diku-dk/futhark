{-# LANGUAGE TypeFamilies #-}

-- | Code generation for segmented and non-segmented scans.  Uses a
-- fast single-pass algorithm, but which only works on NVIDIA GPUs and
-- with some constraints on the operator.  We use this when we can.
module Futhark.CodeGen.ImpGen.GPU.SegScan.SinglePass (compileSegScan) where

import Control.Monad
import Data.List (zip4, zip7)
import Data.Map qualified as M
import Data.Maybe
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.IR.GPUMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Transform.Rename
import Futhark.Util (mapAccumLM)
import Futhark.Util.IntegralExp (IntegralExp (mod, rem), divUp, nextMul, quot)
import Prelude hiding (mod, quot, rem)

xParams, yParams :: SegBinOp GPUMem -> [LParam GPUMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

-- | Given available register, thread block size, scan parameter
-- types, and map parameter types, compute the largest available chunk
-- size given the parameters for which we want chunking and the
-- available resources.
getScanChunkSize :: SubExp -> [Type] -> [Type] -> CallKernelGen Imp.KernelConstExp
getScanChunkSize tblock_size scan_types map_types = do
  tblock_size_exp <-
    case tblock_size of
      Constant v -> pure $ ValueExp v
      Var name -> do
        vtable <- getVTable
        x <- isConstExp vtable $ LeafExp name $ IntType Int64
        case x of
          Just a -> pure a
          Nothing ->
            pure $ LeafExp (Imp.SizeMaxConst SizeThreadBlock) (IntType Int64)
  let max_block_mem = Imp.SizeMaxConst SizeSharedMemory
      max_block_reg = Imp.SizeMaxConst SizeRegisters
      min_bound_tblock_size =
        isInt64 $ ValueExp $ IntValue $ Int64Value 256
      bounded_tblock_size =
        sMax64 (isInt64 tblock_size_exp) min_bound_tblock_size
      k_mem = le64 max_block_mem `quot` bounded_tblock_size
      k_reg = le64 max_block_reg `quot` bounded_tblock_size

      scanned = map elemType $ filter primType scan_types
      mapped = map elemType $ filter primType map_types

      scanned_sizes = map primByteSize scanned
      scanned_sum_sizes = sum scanned_sizes
      scanned_max_size = maximum scanned_sizes

      reg_scan_sum_sizes =
        sum (map (sMax64 4 . primByteSize) scanned) `quot` 4

      reg_map_sum_sizes =
        sum (map (sMax64 4 . primByteSize) mapped) `quot` 4

      mem_constraint =
        max k_mem scanned_sum_sizes `quot` scanned_max_size

      baseline_regs =
        1 + reg_scan_sum_sizes + reg_map_sum_sizes

      per_item_regs =
        2 * reg_scan_sum_sizes + reg_map_sum_sizes

      reg_constraint =
        (k_reg - baseline_regs) `quot` per_item_regs

  pure $ untyped $ sMax64 1 $ sMin64 mem_constraint reg_constraint

createLocalArrays ::
  Count BlockSize SubExp ->
  SubExp ->
  [PrimType] ->
  InKernelGen (VName, [VName], [VName], VName, [VName])
createLocalArrays (Count block_size) chunk types = do
  let block_sizeE = pe64 block_size
      workSize = pe64 chunk * block_sizeE
      prefixArraysSize =
        foldl (\acc tySize -> nextMul acc tySize + tySize * block_sizeE) 0 $
          map primByteSize types
      maxTransposedArraySize =
        foldl1 sMax64 $ map (\ty -> workSize * primByteSize ty) types

      warp_size :: (Num a) => a
      warp_size = 32
      maxWarpExchangeSize =
        foldl (\acc tySize -> nextMul acc tySize + tySize * fromInteger warp_size) 0 $
          map primByteSize types
      maxLookbackSize = maxWarpExchangeSize + warp_size
      size = Imp.bytes $ maxLookbackSize `sMax64` prefixArraysSize `sMax64` maxTransposedArraySize

  (_, byteOffsets) <-
    mapAccumLM
      ( \off tySize -> do
          off' <- dPrimVE "byte_offsets" $ nextMul off tySize + pe64 block_size * tySize
          pure (off', off)
      )
      0
      $ map primByteSize types

  (_, warpByteOffsets) <-
    mapAccumLM
      ( \off tySize -> do
          off' <- dPrimVE "warp_byte_offset" $ nextMul off tySize + warp_size * tySize
          pure (off', off)
      )
      warp_size
      $ map primByteSize types

  sComment "Allocate reusable shared memory" $ pure ()

  localMem <- sAlloc "local_mem" size (Space "shared")
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
        (Shape [block_size])
        localMem
        $ LMAD.iota off' [pe64 block_size]

  warpscan <- sArrayInMem "warpscan" int8 (Shape [constant (warp_size :: Int64)]) localMem
  warpExchanges <-
    forM (zip warpByteOffsets types) $ \(off, ty) -> do
      let off' = off `quot` primByteSize ty
      sArray
        "warp_exchange"
        ty
        (Shape [constant (warp_size :: Int64)])
        localMem
        $ LMAD.iota off' [warp_size]

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
  flg_x :: TV Int8 <- dPrim "flg_x"
  flg_y :: TV Int8 <- dPrim "flg_y"
  let flg_param_x = Param mempty (tvVar flg_x) (MemPrim p_int8)
      flg_param_y = Param mempty (tvVar flg_y) (MemPrim p_int8)
      flg_y_exp = tvExp flg_y
      statusP_e = statusP :: Imp.TExp Int8
      statusX_e = statusX :: Imp.TExp Int8

  dLParams (lambdaParams scan_lam)

  skip_threads <- dPrim "skip_threads"
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

    writeResult x y arr = do
      when (isPrimParam x) $
        copyDWIMFix arr [ltid] (Var $ paramName x) []
      copyDWIM (paramName y) [] (Var $ paramName x) []

-- | Calculate the number of u64 words needed to store n bits
bitArrayWords :: Imp.KernelConstExp -> Imp.KernelConstExp
bitArrayWords n = untyped $ isInt64 n `divUp` 64

-- | Set a bit in a bit array stored as u64 words
setBitInBitArray :: Imp.TExp Int64 -> VName -> Imp.TExp Int64 -> SubExp -> InKernelGen ()
setBitInBitArray chunk bit_array idx bool_val = do
  word_idx <- dPrimV "word_idx" (0 :: Imp.TExp Int64)
  bit_idx <- dPrimV "bit_idx" idx

  sIf
    (chunk .<=. 64)
    ( do
        word_idx <-- 0
        bit_idx <-- idx
    )
    ( do
        word_idx <-- idx `quot` 64
        bit_idx <-- idx `rem` 64
    )

  -- Read current word
  current_word <- dPrimV "current_word" (0 :: Imp.TExp Int64)
  copyDWIMFix (tvVar current_word) [] (Var bit_array) [tvExp word_idx]

  -- Convert bool to int64 (0 or 1) using zero extension
  bool_as_int <-
    dPrimVE "bool_as_int" $
      TPrimExp $
        zExt Int64 $
          toExp' Bool bool_val

  -- Create mask and update word
  let bit_mask = 1 .<<. tvExp bit_idx
      cleared_word = tvExp current_word .&. (bit_mask .^. (-1))
      set_bit = (bool_as_int .&. 1) .<<. tvExp bit_idx
      new_word = cleared_word .|. set_bit

  new_word_var <- dPrimV "new_word" new_word
  copyDWIMFix bit_array [tvExp word_idx] (Var $ tvVar new_word_var) []

-- | Get a bit from a bit array stored as u64 words, storing result in destination
getBitFromBitArray :: Imp.TExp Int64 -> VName -> VName -> Imp.TExp Int64 -> InKernelGen ()
getBitFromBitArray chunk dest bit_array idx = do
  word_idx <- dPrimV "word_idx" (0 :: Imp.TExp Int64)
  bit_idx <- dPrimV "bit_idx" idx

  sIf
    (chunk .<=. 64)
    ( do
        word_idx <-- 0
        bit_idx <-- idx
    )
    ( do
        word_idx <-- idx `quot` 64
        bit_idx <-- idx `rem` 64
    )

  -- Read word containing the bit
  word <- dPrimV "bit_word_read" (0 :: Imp.TExp Int64)
  copyDWIMFix (tvVar word) [] (Var bit_array) [tvExp word_idx]

  -- Extract bit: (word >> bitIdx) & 1
  let extracted_bit = (tvExp word .>>. tvExp bit_idx) .&. 1
      bool_val = extracted_bit .==. 1

  bool_var <- dPrimV "bool_val" bool_val
  copyDWIMFix dest [] (Var $ tvVar bool_var) []

-- | Helper to determine if a type should use bit array representation
shouldUseBitArray :: Type -> Bool
shouldUseBitArray t = primType t && elemType t == Bool

-- | Compile 'SegScan' instance to host-level code with calls to a
-- single-pass kernel.
compileSegScan ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  [Type] ->
  SegBinOp GPUMem ->
  KernelBody GPUMem ->
  SegPostOp GPUMem ->
  CallKernelGen ()
compileSegScan pat lvl space ts scan_op map_kbody post_op = do
  attrs <- lvlKernelAttrs lvl
  let scanop_nes = segBinOpNeutral scan_op

      n = product $ map pe64 $ segSpaceDims space

      scan_tys' = lambdaReturnType $ segBinOpLambda scan_op
      map_tys' = drop (length $ segBinOpNeutral scan_op) ts

      scan_tys = map elemType scan_tys'

      tblock_size_e = pe64 $ unCount $ kAttrBlockSize attrs
      num_phys_blocks_e = pe64 $ unCount $ kAttrNumBlocks attrs

  chunk_const <-
    getScanChunkSize (unCount $ kAttrBlockSize attrs) scan_tys' $
      filter (not . shouldUseBitArray) map_tys'
  chunk_v <- dPrim "chunk_size"
  let chunk_name = nameFromText $ prettyText $ tvVar chunk_v
  addTuningParam chunk_name Nothing
  emit . Imp.GetUserParam (tvVar chunk_v) chunk_name . isInt64
    =<< kernelConstToExp chunk_const
  let chunk_constexp = LeafExp (Imp.SizeUserParam chunk_name chunk_const) int64
      chunk = tvExp chunk_v

  let num_words_const = bitArrayWords chunk_const
  num_words <-
    dPrimV "num_bit_words"
      . isInt64
      =<< kernelConstToExp num_words_const

  num_virt_blocks <-
    tvSize <$> dPrimV "num_virt_blocks" (n `divUp` (tblock_size_e * chunk))
  let num_virt_blocks_e = pe64 num_virt_blocks

  num_virt_threads <-
    dPrimVE "num_virt_threads" $ num_virt_blocks_e * tblock_size_e

  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      segmented = length dims' > 1
      not_segmented_e = fromBool $ not segmented
      segment_size = last dims'

  emit $ Imp.DebugPrint "Sequential elements per thread (chunk)" $ Just $ untyped chunk

  statusFlags <- sAllocArray "status_flags" int8 (Shape [num_virt_blocks]) (Space "device")
  sReplicate statusFlags $ intConst Int8 statusX

  (aggregateArrays, incprefixArrays) <-
    fmap unzip $
      forM scan_tys $ \ty ->
        (,)
          <$> sAllocArray "aggregates" ty (Shape [num_virt_blocks]) (Space "device")
          <*> sAllocArray "incprefixes" ty (Shape [num_virt_blocks]) (Space "device")

  global_id <- genZeroes "global_dynid" 1

  let attrs' =
        attrs
          { kAttrConstExps =
              M.fromList
                [ (tvVar chunk_v, chunk_constexp),
                  (tvVar num_words, num_words_const)
                ]
          }

  map_global_chunks <-
    forM map_tys' $ \t ->
      if isAcc t || primType t
        then pure Nothing
        else
          Just
            <$> sAllocArray
              "global"
              (elemType t)
              ( Shape
                  [ unCount $ kAttrNumBlocks attrs,
                    unCount $ kAttrBlockSize attrs,
                    tvSize chunk_v
                  ]
                  <> arrayShape t
              )
              (Space "device")
  sKernelThread "segscan" (segFlat space) attrs' $ do
    chunk32 <- dPrimVE "chunk_size_32b" $ sExt32 $ tvExp chunk_v

    constants <- kernelConstants <$> askEnv

    let ltid32 = kernelLocalThreadId constants
        ltid = sExt64 ltid32

    (sharedId, transposedArrays, prefixArrays, warpscan, exchanges) <-
      createLocalArrays (kAttrBlockSize attrs) (tvSize chunk_v) scan_tys

    -- We wrap the entire kernel body in a virtualisation loop to
    -- handle the case where we do not have enough thread blocks to
    -- cover the iteration space. Dynamic block indexing has no
    -- implication on this, since each block simply fetches a new
    -- dynamic ID upon entry into the virtualisation loop.
    --
    -- We could use virtualiseBlocks, but this introduces a barrier which is
    -- redundant in this case, and also we don't need to base virtual block IDs
    -- on the loop variable, but rather on the dynamic IDs.
    phys_block_id <- dPrim "phys_block_id"
    sOp $ Imp.GetBlockId (tvVar phys_block_id) 0
    iters <-
      dPrimVE "virtloop_bound" $
        (num_virt_blocks_e - tvExp phys_block_id)
          `divUp` num_phys_blocks_e

    sFor "virtloop_i" iters $ const $ do
      dyn_id <- dPrim "dynamic_id"
      sComment "First thread in block fetches this block's dynamic_id" $ do
        sWhen (ltid32 .==. 0) $ do
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
            sWhen (tvExp dyn_id .==. num_virt_blocks_e - 1) $
              copyDWIMFix global_id [0] (intConst Int32 0) []

      let local_barrier = Imp.Barrier Imp.FenceLocal
          local_fence = Imp.MemFence Imp.FenceLocal
          global_fence = Imp.MemFence Imp.FenceGlobal

      sOp local_barrier
      copyDWIMFix (tvVar dyn_id) [] (Var sharedId) [0]
      sOp local_barrier

      block_offset <-
        dPrimVE "block_offset" $
          sExt64 (tvExp dyn_id) * chunk * tblock_size_e
      sgm_idx <- dPrimVE "sgm_idx" $ block_offset `mod` segment_size
      boundary <-
        dPrimVE "boundary" $
          sExt32 $
            sMin64 (chunk * tblock_size_e) (segment_size - sgm_idx)
      segsize_compact <-
        dPrimVE "segsize_compact" $
          sExt32 $
            sMin64 (chunk * tblock_size_e) segment_size
      scan_private_chunks <-
        forM scan_tys $ \ty ->
          sAllocArray
            "private"
            ty
            (Shape [tvSize chunk_v])
            (ScalarSpace [tvSize chunk_v] ty)

      map_private_chunks <-
        forM map_tys' $ \t ->
          if isAcc t || not (primType t)
            then pure Nothing
            else
              if shouldUseBitArray t
                then do
                  Just
                    <$> sAllocArray
                      "private_bits"
                      int64
                      (Shape [tvSize num_words])
                      (ScalarSpace [tvSize num_words] int64)
                else
                  Just
                    <$> sAllocArray
                      "private"
                      (elemType t)
                      (Shape [tvSize chunk_v])
                      (ScalarSpace [tvSize chunk_v] (elemType t))

      thd_offset <- dPrimVE "thd_offset" $ block_offset + ltid

      sComment "Load and map" $
        sFor "i" chunk $ \i -> do
          -- The map's input index
          virt_tid <- dPrimVE "virt_tid" $ thd_offset + i * tblock_size_e
          dIndexSpace (zip gtids dims') virt_tid
          -- Perform the map
          let in_bounds =
                compileStms mempty (bodyStms map_kbody) $ do
                  let (all_scan_res, map_res) =
                        splitAt (segBinOpResults [scan_op]) $ bodyResult map_kbody

                  -- Write map results to memory.
                  forM_ (zip4 map_private_chunks map_global_chunks (map kernelResultSubExp map_res) map_tys') $
                    \(priv_dest, glob_dest, src, ty) -> do
                      case priv_dest of
                        Just d
                          | shouldUseBitArray ty ->
                              setBitInBitArray chunk d i src
                        Just d ->
                          copyDWIMFix d [i] src []
                        Nothing -> pure ()

                      maybe (pure ()) (\d -> copyDWIMFix d [tvExp phys_block_id, ltid, i] src []) glob_dest

                  -- Write to-scan results to private memory.
                  forM_ (zip scan_private_chunks $ map kernelResultSubExp all_scan_res) $ \(dest, src) ->
                    copyDWIMFix dest [i] src []

              out_of_bounds =
                forM_ (zip scan_private_chunks scanop_nes) $ \(dest, ne) ->
                  copyDWIMFix dest [i] ne []

          sIf (virt_tid .<. n) in_bounds out_of_bounds

      sOp $ Imp.ErrorSync Imp.FenceLocal
      sComment "Transpose scan inputs" $ do
        forM_ (zip transposedArrays scan_private_chunks) $ \(trans, priv) -> do
          sFor "i" chunk $ \i -> do
            sharedIdx <- dPrimVE "sharedIdx" $ ltid + i * tblock_size_e
            copyDWIMFix trans [sharedIdx] (Var priv) [i]
          sOp local_barrier
          sFor "i" chunk $ \i -> do
            sharedIdx <- dPrimV "sharedIdx" $ ltid * chunk + i
            copyDWIMFix priv [sExt64 i] (Var trans) [sExt64 $ tvExp sharedIdx]
          sOp local_barrier

      sComment "Per thread scan" $ do
        -- We don't need to touch the first element, so only m-1
        -- iterations here.
        sFor "i" (chunk - 1) $ \i -> do
          let xs = map paramName $ xParams scan_op
              ys = map paramName $ yParams scan_op
          -- determine if start of segment
          new_sgm <-
            if segmented
              then do
                gidx <- dPrimVE "gidx" $ (ltid32 * chunk32) + 1
                dPrimVE "new_sgm" $ (gidx + sExt32 i - boundary) `mod` segsize_compact .==. 0
              else pure false
          -- skip scan of first element in segment
          sUnless new_sgm $ do
            forM_ (zip4 scan_private_chunks xs ys scan_tys) $ \(src, x, y, ty) -> do
              dPrim_ x ty
              dPrim_ y ty
              copyDWIMFix x [] (Var src) [i]
              copyDWIMFix y [] (Var src) [i + 1]

            compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scan_op) $
              forM_ (zip scan_private_chunks $ map resSubExp $ bodyResult $ lambdaBody $ segBinOpLambda scan_op) $ \(dest, res) ->
                copyDWIMFix dest [i + 1] res []

      sComment "Publish results in shared memory" $ do
        forM_ (zip prefixArrays scan_private_chunks) $ \(dest, src) ->
          copyDWIMFix dest [ltid] (Var src) [chunk - 1]
        sOp local_barrier

      let crossesSegment = do
            guard segmented
            Just $ \from to ->
              let from' = (from + 1) * chunk32 - 1
                  to' = (to + 1) * chunk32 - 1
               in (to' - from') .>. (to' + segsize_compact - boundary) `mod` segsize_compact

      scan_op1 <- renameLambda $ segBinOpLambda scan_op

      accs <- mapM (dPrimSV "acc") scan_tys
      sComment "Scan results (with warp scan)" $ do
        blockScan
          crossesSegment
          tblock_size_e
          num_virt_threads
          scan_op1
          prefixArrays

        sOp $ Imp.ErrorSync Imp.FenceLocal

        let firstThread acc prefixes =
              copyDWIMFix (tvVar acc) [] (Var prefixes) [sExt64 tblock_size_e - 1]
            notFirstThread acc prefixes =
              copyDWIMFix (tvVar acc) [] (Var prefixes) [ltid - 1]
        sIf
          (ltid32 .==. 0)
          (zipWithM_ firstThread accs prefixArrays)
          (zipWithM_ notFirstThread accs prefixArrays)

        sOp local_barrier

      prefixes <- forM (zip scanop_nes scan_tys) $ \(ne, ty) ->
        dPrimV "prefix" $ TPrimExp $ toExp' ty ne
      blockNewSgm <- dPrimVE "block_new_sgm" $ sgm_idx .==. 0
      sComment "Perform lookback" $ do
        sWhen (blockNewSgm .&&. ltid32 .==. 0) $ do
          everythingVolatile $
            forM_ (zip accs incprefixArrays) $ \(acc, incprefixArray) ->
              copyDWIMFix incprefixArray [tvExp dyn_id] (tvSize acc) []
          sOp global_fence
          everythingVolatile $
            copyDWIMFix statusFlags [tvExp dyn_id] (intConst Int8 statusP) []
          forM_ (zip scanop_nes accs) $ \(ne, acc) ->
            copyDWIMFix (tvVar acc) [] ne []
        -- end sWhen

        let warp_size = kernelWaveSize constants
        sWhen (bNot blockNewSgm .&&. ltid32 .<. warp_size) $ do
          sWhen (ltid32 .==. 0) $ do
            sIf
              (not_segmented_e .||. boundary .==. sExt32 (tblock_size_e * chunk))
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

          status :: TV Int8 <- dPrim "status"
          copyDWIMFix (tvVar status) [] (Var warpscan) [0]

          sIf
            (tvExp status .==. statusP)
            ( sWhen (ltid32 .==. 0) $
                everythingVolatile $
                  forM_ (zip prefixes incprefixArrays) $ \(prefix, incprefixArray) ->
                    copyDWIMFix (tvVar prefix) [] (Var incprefixArray) [tvExp dyn_id - 1]
            )
            ( do
                readOffset <-
                  dPrimV "readOffset" $
                    sExt32 $
                      tvExp dyn_id - sExt64 (kernelWaveSize constants)
                let loopStop = warp_size * (-1)
                    sameSegment readIdx
                      | segmented =
                          let startIdx = sExt64 (tvExp readIdx + 1) * tblock_size_e * chunk - 1
                           in block_offset - startIdx .<=. sgm_idx
                      | otherwise = true
                sWhile (tvExp readOffset .>. loopStop) $ do
                  readI <- dPrimV "read_i" $ tvExp readOffset + ltid32
                  aggrs <- forM (zip scanop_nes scan_tys) $ \(ne, ty) ->
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
                    copyDWIMFix exchange [ltid] (tvSize aggr) []
                  copyDWIMFix warpscan [ltid] (tvSize flag) []

                  -- execute warp-parallel reduction but only if the last read flag in not STATUS_P
                  copyDWIMFix (tvVar flag) [] (Var warpscan) [sExt64 warp_size - 1]
                  sWhen (tvExp flag .<. statusP) $ do
                    lam' <- renameLambda scan_op1
                    inBlockScanLookback
                      constants
                      num_virt_threads
                      warpscan
                      exchanges
                      lam'

                  -- all threads of the warp read the result of reduction
                  copyDWIMFix (tvVar flag) [] (Var warpscan) [sExt64 warp_size - 1]
                  forM_ (zip aggrs exchanges) $ \(aggr, exchange) ->
                    copyDWIMFix (tvVar aggr) [] (Var exchange) [sExt64 warp_size - 1]
                  -- update read offset
                  sIf
                    (tvExp flag .==. statusP)
                    (readOffset <-- loopStop)
                    ( sWhen (tvExp flag .==. statusA) $ do
                        readOffset <-- tvExp readOffset - zExt32 warp_size
                    )

                  -- update prefix if flag different than STATUS_X:
                  sWhen (tvExp flag .>. statusX) $ do
                    lam <- renameLambda scan_op1
                    let (xs, ys) = splitAt (length scan_tys) $ map paramName $ lambdaParams lam
                    forM_ (zip xs aggrs) $ \(x, aggr) -> dPrimV_ x (tvExp aggr)
                    forM_ (zip ys prefixes) $ \(y, prefix) -> dPrimV_ y (tvExp prefix)
                    compileStms mempty (bodyStms $ lambdaBody lam) $
                      forM_ (zip3 prefixes scan_tys $ map resSubExp $ bodyResult $ lambdaBody lam) $
                        \(prefix, ty, res) -> prefix <-- TPrimExp (toExp' ty res)
                  sOp local_fence
            )

          -- end sWhile
          -- end sIf
          sWhen (ltid32 .==. 0) $ do
            scan_op2 <- renameLambda scan_op1
            let xs = map paramName $ take (length scan_tys) $ lambdaParams scan_op2
                ys = map paramName $ drop (length scan_tys) $ lambdaParams scan_op2
            sWhen (boundary .==. sExt32 (tblock_size_e * chunk)) $ do
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
            forM_ (zip3 accs scan_tys scanop_nes) $ \(acc, ty, ne) ->
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
        let (xs, ys) = splitAt (length scan_tys) $ map paramName $ lambdaParams scan_op3
            (xs', ys') = splitAt (length scan_tys) $ map paramName $ lambdaParams scan_op4

        forM_ (zip7 prefixes accs xs xs' ys ys' scan_tys) $
          \(prefix, acc, x, x', y, y', ty) -> do
            dPrim_ x ty
            dPrim_ y ty
            dPrimV_ x' $ tvExp prefix
            dPrimV_ y' $ tvExp acc

        sIf
          (ltid32 * chunk32 .<. boundary .&&. bNot blockNewSgm)
          ( compileStms mempty (bodyStms $ lambdaBody scan_op4) $
              forM_ (zip3 xs scan_tys $ map resSubExp $ bodyResult $ lambdaBody scan_op4) $
                \(x, ty, res) -> x <~~ toExp' ty res
          )
          (forM_ (zip xs accs) $ \(x, acc) -> copyDWIMFix x [] (Var $ tvVar acc) [])
        -- calculate where previous thread stopped, to determine number of
        -- elements left before new segment.
        stop <-
          dPrimVE "stopping_point" $
            segsize_compact - (ltid32 * chunk32 - 1 + segsize_compact - boundary) `rem` segsize_compact
        sFor "i" chunk $ \i -> do
          sWhen (sExt32 i .<. stop - 1) $ do
            forM_ (zip scan_private_chunks ys) $ \(src, y) ->
              -- only include prefix for the first segment part per thread
              copyDWIMFix y [] (Var src) [i]
            compileStms mempty (bodyStms $ lambdaBody scan_op3) $
              forM_ (zip scan_private_chunks $ map resSubExp $ bodyResult $ lambdaBody scan_op3) $
                \(dest, res) ->
                  copyDWIMFix dest [i] res []

      sComment "Transpose scan output and to write it later in coalesced fashion to global memory" $ do
        forM_ (zip transposedArrays scan_private_chunks) $ \(locmem, priv) -> do
          -- sOp local_barrier
          sFor "i" chunk $ \i -> do
            sharedIdx <-
              dPrimV "sharedIdx" $
                sExt64 (ltid * chunk) + i
            copyDWIMFix locmem [tvExp sharedIdx] (Var priv) [i]
          sOp local_barrier
          sFor "i" chunk $ \i -> do
            flat_idx <- dPrimVE "flat_idx" $ thd_offset + i * tblock_size_e
            dIndexSpace (zip gtids dims') flat_idx
            sWhen (flat_idx .<. n) $ do
              copyDWIMFix
                priv
                [i]
                (Var locmem)
                [sExt64 $ flat_idx - block_offset]
          sOp local_barrier

      let (scan_pars, map_pars) =
            splitAt (length $ segBinOpNeutral scan_op) $
              map paramName $
                lambdaParams $
                  segPostOpLambda post_op

      sComment "Compute post op and write to global memory." $ do
        sFor "i" chunk $ \i -> do
          flat_idx <- dPrimVE "flat_idx" $ thd_offset + i * tblock_size_e
          sWhen (flat_idx .<. n) $ do
            dIndexSpace (zip gtids dims') flat_idx

            dScope Nothing $
              scopeOfLParams $
                lambdaParams $
                  segPostOpLambda post_op

            sComment "bind scan results to post lambda params" $ do
              forM_ (zip scan_pars scan_private_chunks) $ \(par, priv) ->
                copyDWIMFix par [] (Var priv) [i]

            sComment "bind map results to post lamda params" $
              forM_ (zip4 map_pars map_private_chunks map_global_chunks map_tys') $
                \(par, priv, glob, ty) -> do
                  case priv of
                    Just p
                      | shouldUseBitArray ty ->
                          getBitFromBitArray chunk par p i
                    Just p ->
                      copyDWIMFix par [] (Var p) [i]
                    Nothing -> pure ()

                  maybe (pure ()) (\g -> copyDWIMFix par [] (Var g) [tvExp phys_block_id, ltid, i]) glob

            let res = fmap resSubExp $ bodyResult $ lambdaBody $ segPostOpLambda post_op
            sComment "compute post op." $ do
              compileStms mempty (bodyStms $ lambdaBody $ segPostOpLambda post_op) $
                sComment "write mapped values" $ do
                  forM_ (zip (patElems pat) res) $ \(pe, subexp) ->
                    copyDWIMFix (patElemName pe) (map le64 gtids) subexp []

      sOp local_barrier
{-# NOINLINE compileSegScan #-}
