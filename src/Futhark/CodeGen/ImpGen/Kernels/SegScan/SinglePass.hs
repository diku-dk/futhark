{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for segmented and non-segmented scans.  Uses a
-- fast single-pass algorithm, but which only works on NVIDIA GPUs and
-- with some constraints on the operator.  We use this when we can.
module Futhark.CodeGen.ImpGen.Kernels.SegScan.SinglePass (compileSegScan) where

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
import Futhark.Util.IntegralExp (IntegralExp, div, divUp)
import Prelude hiding (quot, rem)

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
  let workSize = toInt64Exp m * groupSizeE
  let prefixArraysSize = foldl (\acc tySize -> alignTo acc tySize + tySize * groupSizeE) 0 $ map primByteSize types
  let byteOffsets = scanl (\off tySize -> alignTo off tySize + pe64 groupSize * tySize) 0 $ map primByteSize types
  let maxTransposedArraySize = foldl1 sMax64 $ map (\ty -> workSize * primByteSize ty) types
  let warpSize :: Num a => a
      warpSize = 32
  let maxWarpExchangeSize =
        foldl (\acc tySize -> alignTo acc tySize + tySize * fromInteger warpSize) 0 $ map primByteSize types
  let warpByteOffsets =
        scanl (\off tySize -> alignTo off tySize + warpSize * tySize) warpSize $
          map primByteSize types
  let maxLookbackSize = maxWarpExchangeSize + warpSize
  let size = Imp.bytes $ sMax64 maxLookbackSize $ sMax64 prefixArraysSize maxTransposedArraySize

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
      let off' = off `Futhark.Util.IntegralExp.div` primByteSize ty
      sArray
        "local_prefix_arr"
        ty
        (Shape [groupSize])
        $ ArrayIn localMem $ IxFun.iotaOffset off' [pe64 groupSize]

  warpscan <- sArrayInMem "warpscan" int8 (Shape [constant (warpSize :: Int64)]) localMem
  warpExchanges <-
    forM (zip warpByteOffsets types) $ \(off, ty) -> do
      let off' = off `Futhark.Util.IntegralExp.div` primByteSize ty
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
  [SegBinOp KernelsMem] ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegScan pat lvl space scans kbody = sWhen (0 .<. n) $ do
  emit $ Imp.DebugPrint "\n# SegScan" Nothing

  let Pattern _ all_pes = pat
      group_size = toInt64Exp <$> segGroupSize lvl
      num_groups = Count (n `divUp` (unCount group_size * sExt64 tM))
      num_threads = unCount num_groups * unCount group_size
      (mapIdx, _) = head $ unSegSpace space
      m :: Int32
      m = 9
      tM :: Imp.TExp Int32
      tM = 9
      scanOp = head scans
      scanOpNe = segBinOpNeutral scanOp
      tys = map (\(Prim pt) -> pt) $ lambdaReturnType $ segBinOpLambda scanOp
      statusX = 0 :: Imp.TExp Int8
      statusA = 1 :: Imp.TExp Int8
      statusP = 2 :: Imp.TExp Int8
      sStatusX = constant (0 :: Int8)
      sStatusA = constant (1 :: Int8)
      sStatusP = constant (2 :: Int8)
      makeStatusUsed flag used = tvExp flag .|. TPrimExp (BinOpExp (Shl Int8) (untyped $ tvExp used) (ValueExp $ value (2 :: Int8)))
      unmakeStatusUsed :: TV Int8 -> TV Int8 -> TV Int8 -> InKernelGen ()
      unmakeStatusUsed flagUsed flag used = do
        used <-- TPrimExp (BinOpExp (LShr Int8) (untyped $ tvExp flagUsed) (ValueExp $ value (2 :: Int8)))
        flag <-- tvExp flagUsed .&. (3 :: Imp.TExp Int8)

  -- Allocate the shared memory for output component
  numThreads <- dPrimV "numThreads" num_threads
  numGroups <- dPrimV "numGroups" $ unCount num_groups

  globalId <- sStaticArray "id_counter" (Space "device") int32 $ Imp.ArrayZeros 1
  -- TODO: Should be zeroed.
  statusFlags <- sAllocArray "status_flags" int8 (Shape [tvSize numGroups]) (Space "device")
  (aggregateArrays, incprefixArrays) <-
    unzip
      <$> forM
        tys
        ( \ty ->
            (,) <$> sAllocArray "aggregates" ty (Shape [tvSize numGroups]) (Space "device")
              <*> sAllocArray "incprefixes" ty (Shape [tvSize numGroups]) (Space "device")
        )

  sKernelThread "segscan" num_groups group_size (segFlat space) $ do
    constants <- kernelConstants <$> askEnv

    (sharedId, transposedArrays, prefixArrays, sharedReadOffset, warpscan, exchanges) <-
      createLocalArrays (segGroupSize lvl) (constant m) tys

    dynamicId <- dPrim "dynamic_id" int32
    sWhen (kernelLocalThreadId constants .==. 0) $ do
      (globalIdMem, _, globalIdOff) <- fullyIndexArray globalId [0]
      sOp $
        Imp.Atomic DefaultSpace $
          Imp.AtomicAdd
            Int32
            (tvVar dynamicId)
            globalIdMem
            (Count $ unCount globalIdOff)
            (toExp' int32 $ constant (1 :: Int32))
      copyDWIMFix sharedId [0] (tvSize dynamicId) []
      everythingVolatile $ copyDWIMFix statusFlags [tvExp dynamicId] sStatusX []

    let localBarrier = Imp.Barrier Imp.FenceLocal
        localFence = Imp.MemFence Imp.FenceLocal
        globalFence = Imp.MemFence Imp.FenceGlobal

    sOp localBarrier
    copyDWIMFix (tvVar dynamicId) [] (Var sharedId) [0]
    sOp localBarrier

    blockOff <-
      dPrimV "blockOff" $
        sExt64 (tvExp dynamicId) * sExt64 tM * kernelGroupSize constants

    privateArrays <-
      forM tys $ \ty ->
        sAllocArray
          "private"
          ty
          (Shape [constant m])
          (ScalarSpace [constant m] ty)

    sComment "Load and map" $
      sFor "i" (sExt64 tM) $ \i -> do
        -- The map's input index
        dPrimV_ mapIdx $
          tvExp blockOff + sExt64 (kernelLocalThreadId constants)
            + i * kernelGroupSize constants
        -- Perform the map
        let in_bounds =
              compileStms mempty (kernelBodyStms kbody) $ do
                let (all_scan_res, map_res) = splitAt (segBinOpResults scans) $ kernelBodyResult kbody

                -- Write map results to their global memory destinations
                forM_ (zip (takeLast (length map_res) all_pes) map_res) $ \(dest, src) ->
                  copyDWIMFix (patElemName dest) [Imp.vi64 mapIdx] (kernelResultSubExp src) []

                -- Write to-scan results to private memory.
                forM_ (zip privateArrays $ map kernelResultSubExp all_scan_res) $ \(dest, src) ->
                  copyDWIMFix dest [i] src []

            out_of_bounds =
              forM_ (zip privateArrays scanOpNe) $ \(dest, ne) ->
                copyDWIMFix dest [i] ne []

        sIf (Imp.vi64 mapIdx .<. n) in_bounds out_of_bounds

    sComment "Transpose scan inputs" $ do
      forM_ (zip transposedArrays privateArrays) $ \(trans, priv) -> do
        sOp localBarrier
        sFor "i" (sExt64 tM) $ \i -> do
          sharedIdx <-
            dPrimVE "sharedIdx" $
              sExt64 (kernelLocalThreadId constants)
                + i * kernelGroupSize constants
          copyDWIMFix trans [sharedIdx] (Var priv) [i]
        sOp localBarrier
        sFor "i" tM $ \i -> do
          sharedIdx <- dPrimV "sharedIdx" $ kernelLocalThreadId constants * tM + i
          copyDWIMFix priv [sExt64 i] (Var trans) [sExt64 $ tvExp sharedIdx]
      sOp localBarrier

    sComment "Per thread scan" $
      sFor "i" tM $ \i -> do
        let xs = map paramName $ xParams scanOp
            ys = map paramName $ yParams scanOp
            nes = segBinOpNeutral scanOp

        forM_ (zip privateArrays $ zip4 xs ys nes tys) $ \(src, (x, y, ne, ty)) -> do
          dPrim_ x ty
          dPrim_ y ty
          sIf
            (i .==. 0)
            (copyDWIMFix x [] ne [])
            (copyDWIMFix x [] (Var src) [sExt64 i - 1])
          copyDWIMFix y [] (Var src) [sExt64 i]

        compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scanOp) $
          forM_ (zip privateArrays $ bodyResult $ lambdaBody $ segBinOpLambda scanOp) $ \(dest, res) ->
            copyDWIMFix dest [sExt64 i] res []

    sComment "Publish results in shared memory" $ do
      forM_ (zip prefixArrays privateArrays) $ \(dest, src) ->
        copyDWIMFix dest [sExt64 $ kernelLocalThreadId constants] (Var src) [sExt64 tM - 1]
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

      forM_
        (zip accs prefixArrays)
        ( \(acc, prefixes) ->
            sIf
              (kernelLocalThreadId constants .==. 0)
              (copyDWIMFix (tvVar acc) [] (Var prefixes) [sExt64 (kernelGroupSize constants) - 1])
              (copyDWIMFix (tvVar acc) [] (Var prefixes) [sExt64 (kernelLocalThreadId constants) - 1])
        )

      sOp localBarrier

    prefixes <- forM (zip scanOpNe tys) (\(ne, ty) -> dPrimV "prefix" $ TPrimExp $ toExp' ty ne)
    sComment "Perform lookback" $ do
      sWhen (tvExp dynamicId .==. 0 .&&. kernelLocalThreadId constants .==. 0) $ do
        everythingVolatile $
          forM_
            (zip incprefixArrays accs)
            ( \(incprefixArray, acc) ->
                copyDWIMFix incprefixArray [tvExp dynamicId] (tvSize acc) []
            )
        sOp globalFence
        everythingVolatile $
          copyDWIMFix statusFlags [tvExp dynamicId] sStatusP []
        forM_
          (zip scanOpNe accs)
          ( \(ne, acc) ->
              copyDWIMFix (tvVar acc) [] ne []
          )
      -- end sWhen

      let warpSize = kernelWaveSize constants
      sWhen (bNot (tvExp dynamicId .==. 0) .&&. kernelLocalThreadId constants .<. warpSize) $ do
        sWhen (kernelLocalThreadId constants .==. 0) $ do
          everythingVolatile $
            forM_
              (zip aggregateArrays accs)
              ( \(aggregateArray, acc) ->
                  copyDWIMFix aggregateArray [tvExp dynamicId] (tvSize acc) []
              )
          sOp globalFence
          everythingVolatile $
            copyDWIMFix statusFlags [tvExp dynamicId] sStatusA []
          copyDWIMFix warpscan [0] (Var statusFlags) [tvExp dynamicId - 1]
        -- sWhen
        sOp localFence

        status <- dPrim "status" int8
        copyDWIMFix (tvVar status) [] (Var warpscan) [0]

        sIf
          (tvExp status .==. statusP)
          ( sWhen (kernelLocalThreadId constants .==. 0) $
              everythingVolatile $
                forM_ (zip prefixes incprefixArrays) $
                  \(prefix, incprefixArray) -> copyDWIMFix (tvVar prefix) [] (Var incprefixArray) [tvExp dynamicId - 1]
          )
          ( do
              readOffset <-
                dPrimV "readOffset" $
                  sExt32 $ tvExp dynamicId - sExt64 (kernelWaveSize constants)
              let loopStop = warpSize * (-1)
              sWhile (tvExp readOffset .>. loopStop) $ do
                readI <- dPrimV "read_i" $ tvExp readOffset + kernelLocalThreadId constants
                aggrs <- forM (zip scanOpNe tys) $
                  \(ne, ty) -> dPrimV "aggr" $ TPrimExp $ toExp' ty ne
                flag <- dPrimV "flag" statusX
                used <- dPrimV "used" (0 :: Imp.TExp Int8)
                everythingVolatile $
                  sWhen (tvExp readI .>=. 0) $ do
                    copyDWIMFix (tvVar flag) [] (Var statusFlags) [sExt64 $ tvExp readI]
                    sIf
                      (tvExp flag .==. statusP)
                      ( forM_
                          (zip incprefixArrays aggrs)
                          (\(incprefix, aggr) -> copyDWIMFix (tvVar aggr) [] (Var incprefix) [sExt64 $ tvExp readI])
                      )
                      $ sWhen (tvExp flag .==. statusA) $ do
                        forM_
                          (zip aggrs aggregateArrays)
                          (\(aggr, aggregate) -> copyDWIMFix (tvVar aggr) [] (Var aggregate) [sExt64 $ tvExp readI])
                        used <-- (1 :: Imp.TExp Int8)
                -- end sIf
                -- end sWhen
                forM_ (zip exchanges aggrs) $
                  \(exchange, aggr) ->
                    copyDWIMFix exchange [sExt64 $ kernelLocalThreadId constants] (tvSize aggr) []
                tmp <- dPrimV "tmp" $ makeStatusUsed flag used
                copyDWIMFix warpscan [sExt64 $ kernelLocalThreadId constants] (tvSize tmp) []
                sOp localFence

                (warpscanMem, warpscanSpace, warpscanOff) <-
                  fullyIndexArray warpscan [sExt64 warpSize - 1]
                flag <-- TPrimExp (Imp.index warpscanMem warpscanOff int8 warpscanSpace Imp.Volatile)
                sWhen (kernelLocalThreadId constants .==. 0) $ do
                  -- TODO: This is a single-threaded reduce
                  sIf
                    (bNot $ tvExp flag .==. statusP)
                    ( do
                        scanOp'' <- renameLambda scanOp'
                        let agg1s = map paramName $ take (length tys) $ lambdaParams scanOp''
                            agg2s = map paramName $ drop (length tys) $ lambdaParams scanOp''

                        forM_ (zip3 agg1s scanOpNe tys) $
                          \(agg1, ne, ty) -> dPrimV_ agg1 $ TPrimExp $ toExp' ty ne
                        zipWithM_ dPrim_ agg2s tys

                        flag1 <- dPrimV "flag1" statusX
                        flag2 <- dPrim "flag2" int8
                        used1 <- dPrimV "used1" (0 :: Imp.TExp Int8)
                        used2 <- dPrim "used2" int8
                        sFor "i" warpSize $ \i -> do
                          copyDWIMFix (tvVar flag2) [] (Var warpscan) [sExt64 i]
                          unmakeStatusUsed flag2 flag2 used2
                          forM_ (zip agg2s exchanges) $
                            \(agg2, exchange) ->
                              copyDWIMFix agg2 [] (Var exchange) [sExt64 i]
                          sIf
                            (bNot $ tvExp flag2 .==. statusA)
                            ( do
                                flag1 <-- tvExp flag2
                                used1 <-- tvExp used2
                                forM_
                                  (zip3 agg1s tys agg2s)
                                  (\(agg1, ty, agg2) -> agg1 <~~ toExp' ty (Var agg2))
                            )
                            ( do
                                used1 <-- tvExp used1 + tvExp used2
                                compileStms mempty (bodyStms $ lambdaBody scanOp'') $
                                  forM_
                                    (zip3 agg1s tys $ bodyResult $ lambdaBody scanOp'')
                                    (\(agg1, ty, res) -> agg1 <~~ toExp' ty res)
                            )
                        flag <-- tvExp flag1
                        used <-- tvExp used1
                        forM_ (zip3 aggrs tys agg1s) (\(aggr, ty, agg1) -> tvVar aggr <~~ toExp' ty (Var agg1))
                    )
                    -- else
                    ( forM_
                        (zip aggrs exchanges)
                        $ \(aggr, exchange) ->
                          copyDWIMFix
                            (tvVar aggr)
                            []
                            (Var exchange)
                            [sExt64 warpSize - 1]
                    )
                  -- end sIf
                  sIf
                    (tvExp flag .==. statusP)
                    (readOffset <-- loopStop)
                    (readOffset <-- tvExp readOffset - zExt32 (tvExp used))
                  copyDWIMFix sharedReadOffset [0] (tvSize readOffset) []
                  scanOp''' <- renameLambda scanOp'
                  let xs = map paramName $ take (length tys) $ lambdaParams scanOp'''
                      ys = map paramName $ drop (length tys) $ lambdaParams scanOp'''
                  forM_ (zip xs aggrs) (\(x, aggr) -> dPrimV_ x (tvExp aggr))
                  forM_ (zip ys prefixes) (\(y, prefix) -> dPrimV_ y (tvExp prefix))
                  compileStms mempty (bodyStms $ lambdaBody scanOp''') $
                    forM_
                      (zip3 prefixes tys $ bodyResult $ lambdaBody scanOp''')
                      (\(prefix, ty, res) -> prefix <-- TPrimExp (toExp' ty res))
                -- end sWhen
                sOp localFence
                copyDWIMFix (tvVar readOffset) [] (Var sharedReadOffset) [0]
          )
        -- end sWhile
        -- end sIf
        sWhen (kernelLocalThreadId constants .==. 0) $ do
          scanOp'''' <- renameLambda scanOp'
          let xs = map paramName $ take (length tys) $ lambdaParams scanOp''''
              ys = map paramName $ drop (length tys) $ lambdaParams scanOp''''
          forM_ (zip xs accs) (\(x, acc) -> dPrimV_ x (tvExp acc))
          forM_ (zip ys prefixes) (\(y, prefix) -> dPrimV_ y (tvExp prefix))
          compileStms mempty (bodyStms $ lambdaBody scanOp'''') $
            everythingVolatile $
              forM_
                (zip incprefixArrays $ bodyResult $ lambdaBody scanOp'''')
                (\(incprefixArray, res) -> copyDWIMFix incprefixArray [tvExp dynamicId] res [])
          sOp globalFence
          everythingVolatile $ copyDWIMFix statusFlags [tvExp dynamicId] sStatusP []
          forM_ (zip exchanges prefixes) (\(exchange, prefix) -> copyDWIMFix exchange [0] (tvSize prefix) [])
          forM_ (zip3 accs tys scanOpNe) (\(acc, ty, ne) -> tvVar acc <~~ toExp' ty ne)
      -- end sWhen
      -- end sWhen

      sWhen (bNot $ tvExp dynamicId .==. 0) $ do
        sOp localBarrier
        forM_ (zip exchanges prefixes) (\(exchange, prefix) -> copyDWIMFix (tvVar prefix) [] (Var exchange) [0])
        sOp localBarrier
    -- end sWhen
    -- end sComment

    scanOp''''' <- renameLambda scanOp'
    scanOp'''''' <- renameLambda scanOp'

    sComment "Distribute results" $ do
      let xs = map paramName $ take (length tys) $ lambdaParams scanOp'''''
          ys = map paramName $ drop (length tys) $ lambdaParams scanOp'''''
          xs' = map paramName $ take (length tys) $ lambdaParams scanOp''''''
          ys' = map paramName $ drop (length tys) $ lambdaParams scanOp''''''

      forM_ (zip4 (zip prefixes accs) (zip xs xs') (zip ys ys') tys) $
        \((prefix, acc), (x, x'), (y, y'), ty) -> do
          dPrim_ x ty
          dPrim_ y ty
          dPrimV_ x' (tvExp acc)
          dPrimV_ y' (tvExp prefix)

      compileStms mempty (bodyStms $ lambdaBody scanOp'''''') $
        forM_
          (zip3 xs tys $ bodyResult $ lambdaBody scanOp'''''')
          (\(x, ty, res) -> x <~~ toExp' ty res)

      sFor "i" (sExt64 tM) $ \i -> do
        forM_ (zip privateArrays ys) $ \(src, y) ->
          copyDWIMFix y [] (Var src) [i]

        compileStms mempty (bodyStms $ lambdaBody scanOp''''') $
          forM_ (zip privateArrays $ bodyResult $ lambdaBody scanOp''''') $
            \(dest, res) ->
              copyDWIMFix dest [i] res []

    sComment "Transpose scan output" $ do
      forM_ (zip transposedArrays privateArrays) $ \(trans, priv) -> do
        sOp localBarrier
        sFor "i" (sExt64 tM) $ \i -> do
          sharedIdx <-
            dPrimV "sharedIdx" $
              sExt64 (kernelLocalThreadId constants * tM) + i
          copyDWIMFix trans [tvExp sharedIdx] (Var priv) [i]
        sOp localBarrier
        sFor "i" (sExt64 tM) $ \i -> do
          sharedIdx <-
            dPrimV "sharedIdx" $
              kernelLocalThreadId constants
                + sExt32 (kernelGroupSize constants * i)
          copyDWIMFix priv [i] (Var trans) [sExt64 $ tvExp sharedIdx]
      sOp localBarrier

    sComment "Write block scan results to global memory" $
      forM_ (zip (map patElemName all_pes) privateArrays) $ \(dest, src) ->
        sFor "i" (sExt64 tM) $ \i -> do
          dPrimV_ mapIdx $
            tvExp blockOff + kernelGroupSize constants * i
              + sExt64 (kernelLocalThreadId constants)
          sWhen (Imp.vi64 mapIdx .<. n) $
            copyDWIMFix dest [Imp.vi64 mapIdx] (Var src) [i]

    sComment "If this is the last block, reset the dynamicId" $
      sWhen (tvExp dynamicId .==. unCount num_groups - 1) $
        copyDWIMFix globalId [0] (constant (0 :: Int32)) []
  where
    n = product $ map toInt64Exp $ segSpaceDims space
