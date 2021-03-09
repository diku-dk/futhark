{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for segmented and non-segmented scans.  Uses a
-- fast single-pass algorithm, but which only works on NVIDIA GPUs and
-- with some constraints on the operator.  We use this when we can.
module Futhark.CodeGen.ImpGen.Kernels.SegScan.SegSinglePass (compileSegScan) where

import Control.Monad.Except
import Data.List (zip4)
import Data.Maybe
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.IR.KernelsMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Transform.Rename
import Futhark.Util (takeLast)
import Futhark.Util.IntegralExp (IntegralExp (mod, div), divUp, quot)
import Prelude hiding (quot)

xParams, yParams :: SegBinOp KernelsMem -> [LParam KernelsMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

alignTo :: IntegralExp a => a -> a -> a
alignTo x a = (x `divUp` a) * a

createLocalArrays ::
  Count GroupSize SubExp ->
  SubExp ->
  [PrimType] ->
  InKernelGen (VName, [VName], [VName], VName, VName, [VName])
createLocalArrays (Count groupSize) m types = do
  let groupSizeE = toInt64Exp groupSize
      workSize = toInt64Exp m * groupSizeE
      prefixArraysSize =
        foldl (\acc tySize -> alignTo acc tySize + tySize * groupSizeE) 0 $
          map primByteSize types
      maxTransposedArraySize =
        foldl1 sMax64 $ map (\ty -> workSize * primByteSize ty) types

      warpSize :: Num a => a
      warpSize = 32
      maxWarpExchangeSize =
        foldl (\acc tySize -> alignTo acc tySize + tySize * fromInteger warpSize) 0 $
          map primByteSize types
      maxLookbackSize = maxWarpExchangeSize + warpSize
      size = Imp.bytes $ maxLookbackSize `sMax64` prefixArraysSize `sMax64` maxTransposedArraySize

      varTE :: TV Int64 -> TPrimExp Int64 VName
      varTE = le64 . tvVar

  byteOffsets <-
    mapM (fmap varTE . dPrimV "byte_offsets") $
      scanl (\off tySize -> alignTo off tySize + toInt64Exp groupSize * tySize) 0 $
        map primByteSize types

  warpByteOffsets <-
    mapM (fmap varTE . dPrimV "warp_byte_offset") $
      scanl (\off tySize -> alignTo off tySize + warpSize * tySize) warpSize $
        map primByteSize types

  sComment "Allocate reused shared memeory" $ return ()

  localMem <- sAlloc "local_mem" size (Space "local")
  transposeArrayLength <- dPrimV "trans_arr_len" workSize

  sharedId <- sArrayInMem "shared_id" int32 (Shape [constant (1 :: Int32)]) localMem
  sharedReadOffset <- sArrayInMem "shared_read_offset" int32 (Shape [constant (1 :: Int32)]) localMem

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
        $ ArrayIn localMem $ IxFun.iotaOffset off' [pe64 groupSize]

  warpscan <- sArrayInMem "warpscan" int8 (Shape [constant (warpSize :: Int64)]) localMem
  warpExchanges <-
    forM (zip warpByteOffsets types) $ \(off, ty) -> do
      let off' = off `quot` primByteSize ty
      sArray
        "warp_exchange"
        ty
        (Shape [constant (warpSize :: Int64)])
        $ ArrayIn localMem $ IxFun.iotaOffset off' [warpSize]

  return (sharedId, transposedArrays, prefixArrays, sharedReadOffset, warpscan, warpExchanges)
-- | Compile 'SegScan' instance to host-level code with calls to a
-- single-pass kernel.
compileSegScan ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  SegBinOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegScan pat lvl space scanOp kbody = do
  let Pattern _ all_pes = pat
      group_size = toInt64Exp <$> segGroupSize lvl
      n = product $ map toInt64Exp $ segSpaceDims space
      m :: Num a => a
      m = 9
      num_groups = Count (n `divUp` (unCount group_size * m))
      num_threads = unCount num_groups * unCount group_size
      (mapIdx, mapSpaceExp) = head $ unSegSpace space
      (innerIdx, innerExp) = unSegSpace space !! 1
      scanOpNe = segBinOpNeutral scanOp
      tys = map (\(Prim pt) -> pt) $ lambdaReturnType $ segBinOpLambda scanOp
      statusX, statusA, statusP :: Num a => a
      statusX = 0
      statusA = 1
      statusP = 2
      makeStatusUsed flag used = tvExp flag .|. (tvExp used .<<. 2)
      unmakeStatusUsed :: TV Int8 -> TV Int8 -> TV Int8 -> InKernelGen ()
      unmakeStatusUsed flagUsed flag used = do
        used <-- tvExp flagUsed .>>. 2
        flag <-- tvExp flagUsed .&. 3

  -- Allocate the shared memory for output component
  numThreads <- dPrimV "numThreads" num_threads
  numGroups <- dPrimV "numGroups" $ unCount num_groups

  globalId <- sStaticArray "id_counter" (Space "device") int32 $ Imp.ArrayZeros 1
  statusFlags <- sAllocArray "status_flags" int8 (Shape [tvSize numGroups]) (Space "device")
  (aggregateArrays, incprefixArrays) <-
    fmap unzip $
      forM tys $ \ty ->
        (,) <$> sAllocArray "aggregates" ty (Shape [tvSize numGroups]) (Space "device")
          <*> sAllocArray "incprefixes" ty (Shape [tvSize numGroups]) (Space "device")

  sReplicate statusFlags $ intConst Int8 statusX

  sKernelThread "segscan" num_groups group_size (segFlat space) $ do
    constants <- kernelConstants <$> askEnv

    (sharedId, transposedArrays, prefixArrays, sharedReadOffset, warpscan, exchanges) <-
      createLocalArrays (segGroupSize lvl) (intConst Int64 m) tys

    dynamicId <- dPrim "dynamic_id" int32 :: ImpM lore r op (TV Int64)
    sWhen (kernelLocalThreadId constants .==. 0) $ do
      (globalIdMem, _, globalIdOff) <- fullyIndexArray globalId [0]
      sOp $
        Imp.Atomic DefaultSpace $
          Imp.AtomicAdd
            Int32
            (tvVar dynamicId)
            globalIdMem
            (Count $ unCount globalIdOff)
            (untyped (1 :: Imp.TExp Int32))
      copyDWIMFix sharedId [0] (tvSize dynamicId) []

    let localBarrier = Imp.Barrier Imp.FenceLocal
        localFence = Imp.MemFence Imp.FenceLocal
        globalFence = Imp.MemFence Imp.FenceGlobal

    sOp localBarrier
    copyDWIMFix (tvVar dynamicId) [] (Var sharedId) [0]
    sOp localBarrier

    blockOff <-
      dPrimV "blockOff" $
        sExt64 (tvExp dynamicId) * m * kernelGroupSize constants

    privateArrays <-
      forM tys $ \ty ->
        sAllocArray
          "private"
          ty
          (Shape [intConst Int64 m])
          (ScalarSpace [intConst Int64 m] ty)

    sComment "Load and map" $
      sFor "i" m $ \i -> do
        let phys_tid = segFlat space
        -- The map's input index
        dPrimV_ phys_tid $
          tvExp blockOff + sExt64 (kernelLocalThreadId constants)
            + i * kernelGroupSize constants
        dPrimV_ mapIdx $ toInt64Exp mapSpaceExp
        dPrimV_ innerIdx $ toInt64Exp innerExp
        -- Perform the map
        let in_bounds =
              compileStms mempty (kernelBodyStms kbody) $ do
                let (all_scan_res, map_res) = splitAt (segBinOpResults [scanOp]) $ kernelBodyResult kbody

                -- Write map results to their global memory destinations
                forM_ (zip (takeLast (length map_res) all_pes) map_res) $ \(dest, src) ->
                  copyDWIMFix (patElemName dest) [Imp.vi64 phys_tid] (kernelResultSubExp src) []

                -- Write to-scan results to private memory.
                forM_ (zip privateArrays $ map kernelResultSubExp all_scan_res) $ \(dest, src) ->
                  copyDWIMFix dest [i] src []

            out_of_bounds =
              forM_ (zip privateArrays scanOpNe) $ \(dest, ne) ->
                copyDWIMFix dest [i] ne []

        sIf (Imp.vi64 phys_tid .<. n) in_bounds out_of_bounds

    sComment "Transpose scan inputs" $ do
      forM_ (zip transposedArrays privateArrays) $ \(trans, priv) -> do
        sOp localBarrier
        sFor "i" m $ \i -> do
          sharedIdx <-
            dPrimVE "sharedIdx" $
              sExt64 (kernelLocalThreadId constants)
                + i * kernelGroupSize constants
          copyDWIMFix trans [sharedIdx] (Var priv) [i]
        sOp localBarrier
        sFor "i" m $ \i -> do
          sharedIdx <- dPrimV "sharedIdx" $ kernelLocalThreadId constants * m + i
          copyDWIMFix priv [sExt64 i] (Var trans) [sExt64 $ tvExp sharedIdx]
      sOp localBarrier

    sComment "Per thread scan" $
      -- We don't need to touch the first element, so only m-1
      -- iterations here.
      sFor "i" (m -1) $ \i -> do
        let xs = map paramName $ xParams scanOp
            ys = map paramName $ yParams scanOp
        -- calculate global index
        globalIdx <-
          dPrimVE "gidx" $
            tvExp blockOff + sExt64 (kernelLocalThreadId constants * m) + i + 1
        -- determine if start of segment
        isNewSgm <-
          dPrimVE "new_sgm" $
            Futhark.Util.IntegralExp.mod globalIdx (toInt64Exp innerExp) .==. 0
        -- skip scan of first element in segment
        sUnless isNewSgm $
          forM_ (zip privateArrays $ zip3 xs ys tys) $ \(src, (x, y, ty)) -> do
            dPrim_ x ty
            dPrim_ y ty
            copyDWIMFix x [] (Var src) [i]
            copyDWIMFix y [] (Var src) [i + 1]

            compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scanOp) $
              forM_ (zip privateArrays $ bodyResult $ lambdaBody $ segBinOpLambda scanOp) $ \(dest, res) ->
                copyDWIMFix dest [i + 1] res []

    sComment "Publish results in shared memory" $ do
      forM_ (zip prefixArrays privateArrays) $ \(dest, src) ->
        copyDWIMFix dest [sExt64 $ kernelLocalThreadId constants] (Var src) [m - 1]
      sOp localBarrier

    scanOp' <- renameLambda $ segBinOpLambda scanOp

    accs <- mapM (dPrim "acc") tys
    sComment "Scan results (with warp scan)" $ do
      groupScan
        Nothing -- TODO
        (tvExp numThreads)
        (kernelGroupSize constants)
        scanOp'
        prefixArrays

      sOp localBarrier
      let firstThread acc prefixes =
            copyDWIMFix (tvVar acc) [] (Var prefixes) [sExt64 (kernelGroupSize constants) - 1]
          notFirstThread acc prefixes =
            copyDWIMFix (tvVar acc) [] (Var prefixes) [sExt64 (kernelLocalThreadId constants) - 1]
      sIf
        (kernelLocalThreadId constants .==. 0)
        (zipWithM_ firstThread accs prefixArrays)
        (zipWithM_ notFirstThread accs prefixArrays)

      sOp localBarrier
    prefixes <- forM (zip scanOpNe tys) $ \(ne, ty) ->
      dPrimV "prefix" $ TPrimExp $ toExp' ty ne
    sComment "Perform lookback" $ do
      sWhen (tvExp dynamicId .==. 0 .&&. kernelLocalThreadId constants .==. 0) $ do
        everythingVolatile $
          forM_ (zip incprefixArrays accs) $ \(incprefixArray, acc) ->
            copyDWIMFix incprefixArray [tvExp dynamicId] (tvSize acc) []
        sOp globalFence
        everythingVolatile $
          copyDWIMFix statusFlags [tvExp dynamicId] (intConst Int8 statusP) []
        forM_ (zip scanOpNe accs) $ \(ne, acc) ->
          copyDWIMFix (tvVar acc) [] ne []

    -- Parallel reduction during lookback should be performed here --
    scanOp''''' <- renameLambda scanOp'
    scanOp'''''' <- renameLambda scanOp'

    sComment "Distribute results" $ do
      let (xs, ys) = splitAt (length tys) $ map paramName $ lambdaParams scanOp'''''
          (xs', ys') = splitAt (length tys) $ map paramName $ lambdaParams scanOp''''''

      forM_ (zip4 (zip prefixes accs) (zip xs xs') (zip ys ys') tys) $
        \((prefix, acc), (x, x'), (y, y'), ty) -> do
          dPrim_ x ty
          dPrim_ y ty
          dPrimV_ x' $ tvExp prefix
          dPrimV_ y' $ tvExp acc

      compileStms mempty (bodyStms $ lambdaBody scanOp'''''') $
        forM_ (zip3 xs tys $ bodyResult $ lambdaBody scanOp'''''') $
          \(x, ty, res) -> x <~~ toExp' ty res

      sFor "i" m $ \i -> do
        forM_ (zip privateArrays ys) $ \(src, y) ->
          copyDWIMFix y [] (Var src) [i]

        compileStms mempty (bodyStms $ lambdaBody scanOp''''') $
          forM_ (zip privateArrays $ bodyResult $ lambdaBody scanOp''''') $
            \(dest, res) ->
              copyDWIMFix dest [i] res []

    sComment "Transpose scan output" $ do
      forM_ (zip transposedArrays privateArrays) $ \(trans, priv) -> do
        sOp localBarrier
        sFor "i" m $ \i -> do
          sharedIdx <-
            dPrimV "sharedIdx" $
              sExt64 (kernelLocalThreadId constants * m) + i
          copyDWIMFix trans [tvExp sharedIdx] (Var priv) [i]
        sOp localBarrier
        sFor "i" m $ \i -> do
          sharedIdx <-
            dPrimV "sharedIdx" $
              kernelLocalThreadId constants
                + sExt32 (kernelGroupSize constants * i)
          copyDWIMFix priv [i] (Var trans) [sExt64 $ tvExp sharedIdx]
      sOp localBarrier

    sComment "Write block scan results to global memory" $
      forM_ (zip (map patElemName all_pes) privateArrays) $ \(dest, src) ->
        sFor "i" m $ \i -> do
          flatIdx <-
            dPrimV "flatIdx" $
              tvExp blockOff + kernelGroupSize constants * i
                + sExt64 (kernelLocalThreadId constants)
          sWhen (tvExp flatIdx .<. n) $
            -- Gives rank error
            let outerIdx = tvExp flatIdx `Futhark.Util.IntegralExp.div` toInt64Exp innerExp
                sgmIdx = tvExp flatIdx `Futhark.Util.IntegralExp.mod` toInt64Exp innerExp
            in copyDWIMFix dest [outerIdx, sgmIdx] (Var src) [i]

    sComment "If this is the last block, reset the dynamicId" $
      sWhen (tvExp dynamicId .==. unCount num_groups - 1) $
        copyDWIMFix globalId [0] (constant (0 :: Int32)) []