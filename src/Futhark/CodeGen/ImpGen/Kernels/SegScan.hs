{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for segmented and non-segmented scans.  Uses a
-- fairly inefficient two-pass algorithm.
module Futhark.CodeGen.ImpGen.Kernels.SegScan (compileSegScan) where

import Control.Monad.Except
import Control.Monad.State
import Data.List (delete, find, foldl', zip4)
import Data.Maybe
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.IR.KernelsMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Transform.Rename
import Futhark.Util (takeLast)
import Futhark.Util.IntegralExp (divUp, quot, rem, div)
import Prelude hiding (quot, rem)
import System.IO.Unsafe (unsafePerformIO)
import Futhark.IR.Prop.Constants (constant)

xParams, yParams :: SegBinOp KernelsMem -> [LParam KernelsMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

createLocalArrays ::
  Count GroupSize SubExp ->
  SubExp ->
  [PrimType] ->
  InKernelGen (VName, [VName], [VName], VName, VName, [VName])
createLocalArrays (Count groupSize) m types = do
  let groupSizeE = (toInt64Exp groupSize)
  let workSize = (toInt64Exp m) * groupSizeE
  let prefixArraysSize = foldl (\acc tySize -> (alignTo acc tySize) + tySize * groupSizeE) 0 $ map primByteSize types
  let byteOffsets = scanl (\off tySize -> (alignTo off tySize) + (pe32 groupSize) * tySize) 0 $ map primByteSize types
  let maxTransposedArraySize = foldl1 sMax64 $ map (\ty -> workSize * primByteSize ty) types
  let warpSize = (32 :: Int32)
  let warpSizeE = 32
  let warpSize64 = 32
  let maxWarpExchangeSize = foldl (\acc tySize -> (alignTo acc tySize) + tySize * warpSizeE) 0 $ map primByteSize types
  let warpByteOffsets = scanl (\off tySize -> (alignTo off tySize) + warpSize64 * tySize) warpSize64 $ map primByteSize types
  let maxLookbackSize = maxWarpExchangeSize + warpSizeE
  let size = Imp.bytes $ sMax64 maxLookbackSize $ sMax64 prefixArraysSize maxTransposedArraySize

  sComment "Allocate reused shared memeory" $ return ()

  localMem <- sAlloc "local_mem" size (Space "local")
  transposeArrayLength <- dPrimV "trans_arr_len" $ workSize

  sharedId <- sArrayInMem "shared_id" int32 (Shape [constant (1 :: Int32)]) localMem
  sharedReadOffset <- sArrayInMem "shared_read_offset" int32 (Shape [constant (1 :: Int32)]) localMem

  transposedArrays <-
    mapM (\ty -> do
             sArrayInMem "local_transpose_arr"
                         ty
                         (Shape [tvSize transposeArrayLength])
                         localMem)
         types

  prefixArrays <-
    mapM (\(off, ty) -> do
          let off' = off `Futhark.Util.IntegralExp.div` primByteSize ty
          sArray "local_prefix_arr"
                 ty
                 (Shape [groupSize])
                 $ ArrayIn localMem $ IxFun.iotaOffset off' [pe32 $ groupSize])
    $ zip byteOffsets types

  warpscan <- sArrayInMem "warpscan" int8 (Shape [constant warpSize]) localMem
  warpExchanges <-
    mapM (\(off, ty) -> do
            let off' = off `Futhark.Util.IntegralExp.div` primByteSize ty
            sArray "warp_exchange"
                   ty
                   (Shape [constant warpSize])
                   $ ArrayIn localMem $ IxFun.iotaOffset off' [warpSize64])
      (zip warpByteOffsets types)

  return (sharedId, transposedArrays, prefixArrays, sharedReadOffset, warpscan, warpExchanges)
  where
    alignTo x a = (x `divUp` a) * a

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  [SegBinOp KernelsMem] ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegScan pat lvl space scans kbody = sWhen (0 .<. n) $ do
  emit $ Imp.DebugPrint "\n# SegScan" Nothing

  let (Pattern _ all_pes) =
        unsafePerformIO (do putStrLn ("pat: " ++ show pat
                                       ++ "\nlvl: " ++ show lvl
                                       ++ "\nspace: " ++ show space
                                       ++ "\nscans: " ++ show scans
                                       ++ "\nkbody: " ++ show kbody)
                            return pat)

  let group_size    = toInt32Exp <$> segGroupSize lvl
      num_groups    = Count (n `divUp` ((unCount group_size) * tM))
      num_memset_groups
                    = Count ((unCount num_groups) `divUp` ((unCount group_size) * tM))
      num_threads   = (unCount num_groups) * (unCount group_size)
      res           = patElemName $ last all_pes
      (mapIdx, dim) = head $ unSegSpace space
      m :: Int32
      m             = 9
      tM :: Imp.TExp Int32
      tM            = 9
      scanOp        = head scans
      scanOpNe      = segBinOpNeutral scanOp
      tys           = map (\(Prim pt) -> pt) $ lambdaReturnType $ segBinOpLambda scanOp
      statusX       = 0 :: Imp.TExp Int8
      statusA       = 1 :: Imp.TExp Int8
      statusP       = 2 :: Imp.TExp Int8
      sStatusX      = constant (0 :: Int8)
      sStatusA      = constant (1 :: Int8)
      sStatusP      = constant (2 :: Int8)
      makeStatusUsed flag used = (tvExp flag) .|. (TPrimExp $ BinOpExp (Shl Int8) (untyped $ tvExp used) (ValueExp $ value (2 :: Int8)))
      unmakeStatusUsed :: TV Int8 -> TV Int8 -> TV Int8 -> InKernelGen ()
      unmakeStatusUsed flagUsed flag used = do
        used <-- (TPrimExp $ BinOpExp (LShr Int8) (untyped $ tvExp flagUsed) (ValueExp $ value (2 :: Int8)))
        flag <-- (tvExp flagUsed) .&. (3 :: Imp.TExp Int8)
      waveSizeE     = 32
      waveSize      = constant (32 :: Int32)

  -- Allocate the shared memory for output component
  numThreads <- dPrimV "numThreads" num_threads
  numGroups <- dPrimV "numGroups" $ unCount num_groups

  globalId <- sStaticArray "id_counter" (Space "device") int32 $ Imp.ArrayZeros 1
  -- TODO: Should be zeroed.
  statusFlags <- sAllocArray "status_flags" int8 (Shape [tvSize numGroups]) (Space "device")
  (aggregateArrays, incprefixArrays) <-
    unzip <$> forM tys
                (\ty -> (,) <$> (sAllocArray "aggregates" ty (Shape [tvSize numGroups]) (Space "device"))
                            <*> (sAllocArray "incprefixes" ty (Shape [tvSize numGroups]) (Space "device")))

  -- Set all status flags to statusX and set the dynamic id to zero before starting the scan
  sKernelThread "memset" num_memset_groups group_size (segFlat space) $ do
    constants <- kernelConstants <$> askEnv

    forM_ [0..m] $ \i -> do
      dst <- dPrimV "dst" ((kernelGlobalThreadId constants) + (kernelNumThreads constants * (TPrimExp $ toExp' int32 $ constant i)))
      sWhen ((tvExp dst) .<. unCount num_groups) $
        copyDWIMFix statusFlags [tvExp dst] sStatusX []

    sWhen ((kernelGlobalThreadId constants) .==. 0) $
      copyDWIMFix globalId [0] (constant (0 :: Int32)) []

  sKernelThread "segscan" num_groups group_size (segFlat space) $ do
    constants  <- kernelConstants <$> askEnv

    (sharedId, transposedArrays, prefixArrays, sharedReadOffset, warpscan, exchanges) <-
      createLocalArrays (segGroupSize lvl) (constant m) tys

    dynamicId <- dPrim "dynamic_id" int32
    sWhen (kernelLocalThreadId constants .==. 0) $ do
      (globalIdMem, _, globalIdOff) <- fullyIndexArray globalId [0]
      sOp $ Imp.Atomic DefaultSpace $
        Imp.AtomicAdd Int32
                      (tvVar dynamicId)
                      globalIdMem
                      (Count $ sExt32 $ unCount globalIdOff)
                      (toExp' int32 $ constant (1 :: Int32))
      copyDWIMFix sharedId [0] (tvSize dynamicId) []
      copyDWIMFix statusFlags [tvExp dynamicId] sStatusX []

    let localBarrier = Imp.Barrier Imp.FenceLocal
    let globalBarrier = Imp.Barrier Imp.FenceGlobal
    let localFence = Imp.MemFence Imp.FenceLocal
    let globalFence = Imp.MemFence Imp.FenceGlobal
    sOp localBarrier

    copyDWIMFix (tvVar dynamicId) [] (Var sharedId) [0]

    blockOff   <- dPrimV "blockOff" $ (tvExp dynamicId) * tM * (kernelGroupSize constants)

    privateArrays <-
      mapM (\ty -> sAllocArray "private"
                               ty
                               (Shape [constant m])
                               (ScalarSpace [constant m] ty))
           tys


    sComment "Load and map" $
      sFor "i" tM $ \i -> do
        -- The map's input index
        dPrimV_ mapIdx $ (tvExp blockOff) + (kernelLocalThreadId constants) + i * (kernelGroupSize constants)
        -- Perform the map
        -- TODO: Write neutral elements to private arrays
        sWhen ((Imp.vi32 mapIdx) .<. n) $ do
          compileStms mempty (kernelBodyStms kbody) $
            do let (all_scan_res, map_res) = splitAt (segBinOpResults scans) $ kernelBodyResult kbody
               forM_ (zip (takeLast (length map_res) all_pes) map_res) $ \(dest, src) -> do
                 -- Write map results to their global memory destinations
                 copyDWIMFix (patElemName dest) [Imp.vi32 mapIdx] (kernelResultSubExp src) []

               forM_ (zip privateArrays $ map kernelResultSubExp all_scan_res) $ \(dest, (Var src)) -> do
                 copyDWIMFix dest [i] (Var src) []

    sComment "Transpose scan inputs" $ do
      forM_ (zip transposedArrays privateArrays) $ \(trans, priv) -> do
        sOp localBarrier
        sFor "i" tM $ \i -> do
          sharedIdx <- dPrimV "sharedIdx" $ (kernelLocalThreadId constants) + i * (kernelGroupSize constants)
          copyDWIMFix trans [tvExp sharedIdx] (Var priv) [i]
        sOp localBarrier
        sFor "i" tM $ \i -> do
          sharedIdx <- dPrimV "sharedIdx" $ (kernelLocalThreadId constants) * tM + i
          copyDWIMFix priv [i] (Var trans) [tvExp sharedIdx]
      sOp localBarrier

    sComment "Per thread scan" $ do
      sFor "i" tM $ \i -> do
        let xs  = map paramName $ xParams scanOp
            ys  = map paramName $ yParams scanOp
            nes = segBinOpNeutral scanOp

        mapM_
          (\(src, (x, y, ne, ty)) ->
             do dPrim_ x ty
                dPrim_ y ty
                sIf (i .==. 0)
                  (copyDWIMFix x [] ne [])
                  (copyDWIMFix x [] (Var src) [i - 1])
                copyDWIMFix y [] (Var src) [i])
          $ zip privateArrays $ zip4 xs ys nes tys

        compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scanOp) $
          mapM_
            (\(dest, res) ->
               copyDWIMFix dest [i] res [])
            $ zip privateArrays $ bodyResult $ lambdaBody $ segBinOpLambda scanOp

    sComment "Publish results in shared memory" $ do
      mapM_ (\(dest, src) ->
               copyDWIMFix dest [kernelLocalThreadId constants] (Var src) [tM - 1])
            $ zip prefixArrays privateArrays
      sOp localBarrier

    scanOp' <- renameLambda $ segBinOpLambda scanOp

    accs <- forM tys (\ty -> dPrim "acc" ty)
    sComment "Scan results (with warp scan)" $ do
      groupScan
        Nothing -- TODO
        (tvExp numThreads)
        (kernelGroupSize constants)
        scanOp'
        prefixArrays

      sOp localBarrier

      forM_ (zip accs prefixArrays)
        (\(acc, prefixes) ->
           sIf ((kernelLocalThreadId constants) .==. 0)
               (copyDWIMFix (tvVar acc) [] (Var prefixes) [(kernelGroupSize constants) - 1])
               (copyDWIMFix (tvVar acc) [] (Var prefixes) [(kernelLocalThreadId constants) - 1]))

      sOp localBarrier

    prefixes <- forM (zip scanOpNe tys) (\(ne, ty) -> dPrimV "prefix" $ TPrimExp $ toExp' ty ne)
    sComment "Perform lookback" $ do
      sWhen ((tvExp dynamicId) .==. 0 .&&. (kernelLocalThreadId constants) .==. 0) $ do
        forM_ (zip incprefixArrays accs)
          (\(incprefixArray, acc) ->
             copyDWIMFix incprefixArray [tvExp dynamicId] (tvSize acc) [])
        sOp globalFence
        copyDWIMFix statusFlags [tvExp dynamicId] sStatusP []
        forM_ (zip scanOpNe accs)
          (\(ne, acc) ->
             copyDWIMFix (tvVar acc) [] ne [])
      -- end sWhen

      sWhen ((bNot ((tvExp dynamicId) .==. 0)) .&&. (kernelLocalThreadId constants) .<. waveSizeE) $ do
        warpSize <- dPrimV "warpsize" $ kernelWaveSize constants

        sWhen ((kernelLocalThreadId constants) .==. 0) $ do
          forM_ (zip aggregateArrays accs)
            (\(aggregateArray, acc) ->
               copyDWIMFix aggregateArray [tvExp dynamicId] (tvSize acc) [])
          sOp globalFence
          copyDWIMFix statusFlags [tvExp dynamicId] sStatusA []
          copyDWIMFix warpscan [0] (Var statusFlags) [(tvExp dynamicId) - 1]
        -- sWhen
        sOp localFence

        status <- dPrim "status" int8
        copyDWIMFix (tvVar status) [] (Var warpscan) [0]

        sIf ((tvExp status) .==. statusP)
            (sWhen ((kernelLocalThreadId constants) .==. 0)
              $ forM_ (zip prefixes incprefixArrays)
                  $ \(prefix, incprefixArray) -> copyDWIMFix (tvVar prefix) [] (Var incprefixArray) [(tvExp dynamicId) - 1])
            (do readOffset <- dPrimV "readOffset" (tvExp dynamicId - (kernelWaveSize constants))
                let loopStop = (tvExp warpSize) * (-1)
                sWhile ((tvExp readOffset) .>. loopStop) $ do
                  readI <- dPrimV "read_i" $ (tvExp readOffset) + (kernelLocalThreadId constants)
                  aggrs <- forM (zip scanOpNe tys) (\(ne, ty) -> dPrimV "aggr" $ TPrimExp $ toExp' ty ne)
                  flag <- dPrimV "flag" $ statusX
                  used <- dPrimV "used" $ (0 :: Imp.TExp Int8)
                  sWhen ((tvExp readI) .>=. 0) $ do
                    copyDWIMFix (tvVar flag) [] (Var statusFlags) [tvExp readI]
                    sIf ((tvExp flag) .==. statusP)
                        (forM_ (zip incprefixArrays aggrs)
                           (\(incprefix, aggr) -> copyDWIMFix (tvVar aggr) [] (Var incprefix) [tvExp readI]))
                        $ sWhen ((tvExp flag) .==. statusA) $ do
                            forM_ (zip aggrs aggregateArrays)
                              (\(aggr, aggregate) -> copyDWIMFix (tvVar aggr) [] (Var aggregate) [tvExp readI])
                            used <-- (1 :: Imp.TExp Int8)
                    -- end sIf
                  -- end sWhen
                  forM_ (zip exchanges aggrs) $
                    \(exchange, aggr) ->
                      copyDWIMFix exchange [kernelLocalThreadId constants] (tvSize aggr) []
                  tmp <- dPrimV "tmp" $ makeStatusUsed flag used
                  copyDWIMFix warpscan [kernelLocalThreadId constants] (tvSize tmp) []
                  sOp localFence

                  (warpscanMem, warpscanSpace, warpscanOff) <- fullyIndexArray warpscan [tvExp warpSize - 1]
                  flag <-- (TPrimExp $ Imp.index warpscanMem warpscanOff int8 warpscanSpace Imp.Volatile)
                  sWhen ((kernelLocalThreadId constants) .==. 0) $ do
                    -- TODO: This is a single-threaded reduce
                    sIf (bNot $ (tvExp flag) .==. statusP) (do
                      scanOp'' <- renameLambda scanOp'
                      let agg1s = map paramName $ take (length tys) $ lambdaParams scanOp''
                          agg2s = map paramName $ drop (length tys) $ lambdaParams scanOp''

                      forM_ (zip3 agg1s scanOpNe tys) (\(agg1, ne, ty) -> dPrimV_ agg1 $ TPrimExp $ toExp' ty ne)
                      forM_ (zip agg2s tys) (\(agg2, ty) -> dPrim_ agg2 ty)

                      flag1 <- dPrimV "flag1" statusX
                      flag2 <- dPrim "flag2" int8
                      used1 <- dPrimV "used1" (0 :: Imp.TExp Int8)
                      used2 <- dPrim "used2" int8
                      sFor "i" (tvExp warpSize) (\i -> do
                        copyDWIMFix (tvVar flag2) [] (Var warpscan) [i]
                        unmakeStatusUsed flag2 flag2 used2
                        forM_ (zip agg2s exchanges)
                          (\(agg2, exchange) -> copyDWIMFix agg2 [] (Var exchange) [i])
                        sIf (bNot $ (tvExp flag2) .==. statusA)
                            (do flag1 <-- (tvExp flag2)
                                used1 <-- (tvExp used2)
                                forM_ (zip3 agg1s tys agg2s)
                                  (\(agg1, ty, agg2) -> agg1 <~~ toExp' ty (Var agg2)))
                            (do used1 <-- (tvExp used1) + (tvExp used2)
                                compileStms mempty (bodyStms $ lambdaBody scanOp'') $
                                  forM_ (zip3 agg1s tys $ bodyResult $ lambdaBody scanOp'')
                                    (\(agg1, ty, res) -> agg1 <~~ toExp' ty res)))
                        -- end sIf
                      -- end sFor
                      flag <-- (tvExp flag1)
                      used <-- (tvExp used1)
                      forM_ (zip3 aggrs tys agg1s) (\(aggr, ty, agg1) -> (tvVar aggr) <~~ toExp' ty (Var agg1)))
                      -- else
                      (forM_ (zip aggrs exchanges)
                         (\(aggr, exchange) -> copyDWIMFix (tvVar aggr) [] (Var exchange) [tvExp warpSize - 1]))
                    -- end sIf
                    sIf ((tvExp flag) .==. statusP)
                        (readOffset <-- loopStop)
                        (readOffset <-- (tvExp readOffset) - (zExt32 $ tvExp used))
                    copyDWIMFix sharedReadOffset [0] (tvSize readOffset) []
                    scanOp''' <- renameLambda scanOp'
                    let xs = map paramName $ take (length tys) $ lambdaParams scanOp'''
                        ys = map paramName $ drop (length tys) $ lambdaParams scanOp'''
                    forM_ (zip xs aggrs) (\(x, aggr) -> dPrimV_ x (tvExp aggr))
                    forM_ (zip ys prefixes) (\(y, prefix) -> dPrimV_ y (tvExp prefix))
                    compileStms mempty (bodyStms $ lambdaBody scanOp''') $
                      forM_ (zip3 prefixes tys $ bodyResult $ lambdaBody scanOp''')
                        (\(prefix, ty, res) -> prefix <-- (TPrimExp $ toExp' ty res))
                  -- end sWhen
                  sOp localFence
                  copyDWIMFix (tvVar readOffset) [] (Var sharedReadOffset) [0])
               -- end sWhile
        -- end sIf
        sWhen ((kernelLocalThreadId constants) .==. 0) $ do
          scanOp'''' <- renameLambda scanOp'
          let xs = map paramName $ take (length tys) $ lambdaParams scanOp''''
              ys = map paramName $ drop (length tys) $ lambdaParams scanOp''''
          forM_ (zip xs accs) (\(x, acc) -> dPrimV_ x (tvExp acc))
          forM_ (zip ys prefixes) (\(y, prefix) -> dPrimV_ y (tvExp prefix))
          compileStms mempty (bodyStms $ lambdaBody scanOp'''') $
            forM_ (zip incprefixArrays $ bodyResult $ lambdaBody scanOp'''')
              (\(incprefixArray, res) -> copyDWIMFix incprefixArray [tvExp dynamicId] res [])
          sOp globalFence
          copyDWIMFix statusFlags [tvExp dynamicId] sStatusP []
          forM_ (zip exchanges prefixes) (\(exchange, prefix) -> copyDWIMFix exchange [0] (tvSize prefix) [])
          forM_ (zip3 accs tys scanOpNe) (\(acc, ty, ne) -> (tvVar acc) <~~ toExp' ty ne)
        -- end sWhen
      -- end sWhen

      sWhen (bNot $ (tvExp dynamicId) .==. 0) $ do
        sOp localBarrier
        forM_ (zip exchanges prefixes) (\(exchange, prefix) -> copyDWIMFix (tvVar prefix) [] (Var exchange) [0])
        sOp localBarrier
      -- end sWhen
    -- end sComment

    scanOp''''' <- renameLambda scanOp'
    scanOp'''''' <- renameLambda scanOp'

    sComment "Distribute results" $ do
      let xs  = map paramName $ take (length tys) $ lambdaParams scanOp'''''
          ys  = map paramName $ drop (length tys) $ lambdaParams scanOp'''''
          xs' = map paramName $ take (length tys) $ lambdaParams scanOp''''''
          ys' = map paramName $ drop (length tys) $ lambdaParams scanOp''''''

      mapM_
        (\((prefix, acc), (x, x'), (y, y'), ty) ->
          do dPrim_ x ty
             dPrim_ y ty
             dPrimV_ x' (tvExp acc)
             dPrimV_ y' (tvExp prefix))
        $ zip4 (zip prefixes accs) (zip xs xs') (zip ys ys') tys

      compileStms mempty (bodyStms $ lambdaBody scanOp'''''') $ do
        forM_ (zip3 xs tys $ bodyResult $ lambdaBody scanOp'''''')
          (\(x, ty, res) -> x <~~ toExp' ty res)

      sFor "i" tM $ \i -> do
        mapM_
          (\(src, y) ->
             copyDWIMFix y [] (Var src) [i])
          $ zip privateArrays ys

        compileStms mempty (bodyStms $ lambdaBody scanOp''''') $
          mapM_
            (\(dest, res) ->
               copyDWIMFix dest [i] res [])
            $ zip privateArrays $ bodyResult $ lambdaBody scanOp'''''

    sComment "Write block scan results to global memory" $
      forM_ (zip (map patElemName all_pes) privateArrays) $ \(dest, src) -> do
        sFor "i" tM $ \i -> do
          dPrimV_ mapIdx $ (tvExp blockOff) + (kernelLocalThreadId constants) * (tM) + i
          sWhen ((Imp.vi32 mapIdx) .<. n) $ do
            copyDWIMFix dest [Imp.vi32 mapIdx] (Var src) [i]
  where
    n = product $ map toInt32Exp $ segSpaceDims space
