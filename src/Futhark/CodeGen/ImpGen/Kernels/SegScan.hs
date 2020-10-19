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

-- Aggressively try to reuse memory for different SegBinOps, because
-- we will run them sequentially after another.
makeLocalArrays ::
  Count GroupSize SubExp ->
  SubExp ->
  [SegBinOp KernelsMem] ->
  InKernelGen [[VName]]
makeLocalArrays (Count group_size) num_threads scans = do
  (arrs, mems_and_sizes) <- runStateT (mapM onScan scans) mempty
  let maxSize sizes = Imp.bytes $ foldl' sMax64 1 $ map Imp.unCount sizes
  forM_ mems_and_sizes $ \(sizes, mem) ->
    sAlloc_ mem (maxSize sizes) (Space "local")
  return arrs
  where
    onScan (SegBinOp _ scan_op nes _) = do
      let (scan_x_params, _scan_y_params) =
            splitAt (length nes) $ lambdaParams scan_op
      (arrs, used_mems) <- fmap unzip $
        forM scan_x_params $ \p ->
          case paramDec p of
            MemArray pt shape _ (ArrayIn mem _) -> do
              let shape' = Shape [num_threads] <> shape
              arr <-
                lift $
                  sArray "scan_arr" pt shape' $
                    ArrayIn mem $ IxFun.iota $ map pe32 $ shapeDims shape'
              return (arr, [])
            _ -> do
              let pt = elemType $ paramType p
                  shape = Shape [group_size]
              (sizes, mem') <- getMem pt shape
              arr <- lift $ sArrayInMem "scan_arr" pt shape mem'
              return (arr, [(sizes, mem')])
      modify (<> concat used_mems)
      return arrs

    getMem pt shape = do
      let size = typeSize $ Array pt shape NoUniqueness
      mems <- get
      case (find ((size `elem`) . fst) mems, mems) of
        (Just mem, _) -> do
          modify $ delete mem
          return mem
        (Nothing, (size', mem) : mems') -> do
          put mems'
          return (size : size', mem)
        (Nothing, []) -> do
          mem <- lift $ sDeclareMem "scan_arr_mem" $ Space "local"
          return ([size], mem)

type CrossesSegment = Maybe (Imp.TExp Int32 -> Imp.TExp Int32 -> Imp.TExp Bool)

localArrayIndex :: KernelConstants -> Type -> Imp.TExp Int32
localArrayIndex constants t =
  if primType t
    then kernelLocalThreadId constants
    else kernelGlobalThreadId constants

barrierFor :: Lambda KernelsMem -> (Bool, Imp.Fence, InKernelGen ())
barrierFor scan_op = (array_scan, fence, sOp $ Imp.Barrier fence)
  where
    array_scan = not $ all primType $ lambdaReturnType scan_op
    fence
      | array_scan = Imp.FenceGlobal
      | otherwise = Imp.FenceLocal

xParams, yParams :: SegBinOp KernelsMem -> [LParam KernelsMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

writeToScanValues ::
  [VName] ->
  ([PatElem KernelsMem], SegBinOp KernelsMem, [KernelResult]) ->
  InKernelGen ()
writeToScanValues gtids (pes, scan, scan_res)
  | shapeRank (segBinOpShape scan) > 0 =
    forM_ (zip pes scan_res) $ \(pe, res) ->
      copyDWIMFix
        (patElemName pe)
        (map Imp.vi32 gtids)
        (kernelResultSubExp res)
        []
  | otherwise =
    forM_ (zip (yParams scan) scan_res) $ \(p, res) ->
      copyDWIMFix (paramName p) [] (kernelResultSubExp res) []

readToScanValues ::
  [Imp.TExp Int32] ->
  [PatElem KernelsMem] ->
  SegBinOp KernelsMem ->
  InKernelGen ()
readToScanValues is pes scan
  | shapeRank (segBinOpShape scan) > 0 =
    forM_ (zip (yParams scan) pes) $ \(p, pe) ->
      copyDWIMFix (paramName p) [] (Var (patElemName pe)) is
  | otherwise =
    return ()

readCarries ::
  Imp.TExp Int32 ->
  [Imp.TExp Int32] ->
  [Imp.TExp Int32] ->
  [PatElem KernelsMem] ->
  SegBinOp KernelsMem ->
  InKernelGen ()
readCarries chunk_offset dims' vec_is pes scan
  | shapeRank (segBinOpShape scan) > 0 = do
    ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
    -- We may have to reload the carries from the output of the
    -- previous chunk.
    sIf
      (chunk_offset .>. 0 .&&. ltid .==. 0)
      ( do
          let is = unflattenIndex dims' $ chunk_offset - 1
          forM_ (zip (xParams scan) pes) $ \(p, pe) ->
            copyDWIMFix (paramName p) [] (Var (patElemName pe)) (is ++ vec_is)
      )
      ( forM_ (zip (xParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
          copyDWIMFix (paramName p) [] ne []
      )
  | otherwise =
    return ()

-- | Produce partially scanned intervals; one per workgroup.
scanStage1 ::
  Pattern KernelsMem ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  SegSpace ->
  [SegBinOp KernelsMem] ->
  KernelBody KernelsMem ->
  CallKernelGen (TV Int32, Imp.TExp Int32, CrossesSegment)
scanStage1 (Pattern _ all_pes) num_groups group_size space scans kbody = do
  let num_groups' = fmap toInt32Exp num_groups
      group_size' = fmap toInt32Exp group_size
  num_threads <- dPrimV "num_threads" $ unCount num_groups' * unCount group_size'

  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map toInt32Exp dims
  let num_elements = product dims'
      elems_per_thread = num_elements `divUp` tvExp num_threads
      elems_per_group = unCount group_size' * elems_per_thread

  let crossesSegment =
        case reverse dims' of
          segment_size : _ : _ -> Just $ \from to ->
            (to - from) .>. (to `rem` segment_size)
          _ -> Nothing

  sKernelThread "scan_stage1" num_groups' group_size' (segFlat space) $ do
    constants <- kernelConstants <$> askEnv
    all_local_arrs <- makeLocalArrays group_size (tvSize num_threads) scans

    -- The variables from scan_op will be used for the carry and such
    -- in the big chunking loop.
    forM_ scans $ \scan -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan
      forM_ (zip (xParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

    sFor "j" elems_per_thread $ \j -> do
      chunk_offset <-
        dPrimV "chunk_offset" $
          kernelGroupSize constants * j
            + kernelGroupId constants * elems_per_group
      flat_idx <-
        dPrimV "flat_idx" $
          tvExp chunk_offset + kernelLocalThreadId constants
      -- Construct segment indices.
      zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ tvExp flat_idx

      let per_scan_pes = segBinOpChunks scans all_pes

          in_bounds =
            foldl1 (.&&.) $ zipWith (.<.) (map Imp.vi32 gtids) dims'

          when_in_bounds = compileStms mempty (kernelBodyStms kbody) $ do
            let (all_scan_res, map_res) =
                  splitAt (segBinOpResults scans) $ kernelBodyResult kbody
                per_scan_res =
                  segBinOpChunks scans all_scan_res

            sComment "write to-scan values to parameters" $
              mapM_ (writeToScanValues gtids) $
                zip3 per_scan_pes scans per_scan_res

            sComment "write mapped values results to global memory" $
              forM_ (zip (takeLast (length map_res) all_pes) map_res) $ \(pe, se) ->
                copyDWIMFix
                  (patElemName pe)
                  (map Imp.vi32 gtids)
                  (kernelResultSubExp se)
                  []

      sComment "threads in bounds read input" $
        sWhen in_bounds when_in_bounds

      forM_ (zip3 per_scan_pes scans all_local_arrs) $
        \(pes, scan@(SegBinOp _ scan_op nes vec_shape), local_arrs) ->
          sComment "do one intra-group scan operation" $ do
            let rets = lambdaReturnType scan_op
                scan_x_params = xParams scan
                (array_scan, fence, barrier) = barrierFor scan_op

            when array_scan barrier

            sLoopNest vec_shape $ \vec_is -> do
              sComment "maybe restore some to-scan values to parameters, or read neutral" $
                sIf
                  in_bounds
                  ( do
                      readToScanValues (map Imp.vi32 gtids ++ vec_is) pes scan
                      readCarries (tvExp chunk_offset) dims' vec_is pes scan
                  )
                  ( forM_ (zip (yParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
                      copyDWIMFix (paramName p) [] ne []
                  )

              sComment "combine with carry and write to local memory" $
                compileStms mempty (bodyStms $ lambdaBody scan_op) $
                  forM_ (zip3 rets local_arrs (bodyResult $ lambdaBody scan_op)) $
                    \(t, arr, se) -> copyDWIMFix arr [localArrayIndex constants t] se []

              let crossesSegment' = do
                    f <- crossesSegment
                    Just $ \from to ->
                      let from' = from + tvExp chunk_offset
                          to' = to + tvExp chunk_offset
                       in f from' to'

              sOp $ Imp.ErrorSync fence

              -- We need to avoid parameter name clashes.
              scan_op_renamed <- renameLambda scan_op
              groupScan
                crossesSegment'
                (tvExp num_threads)
                (kernelGroupSize constants)
                scan_op_renamed
                local_arrs

              sComment "threads in bounds write partial scan result" $
                sWhen in_bounds $
                  forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
                    copyDWIMFix
                      (patElemName pe)
                      (map Imp.vi32 gtids ++ vec_is)
                      (Var arr)
                      [localArrayIndex constants t]

              barrier

              let load_carry =
                    forM_ (zip local_arrs scan_x_params) $ \(arr, p) ->
                      copyDWIMFix
                        (paramName p)
                        []
                        (Var arr)
                        [ if primType $ paramType p
                            then kernelGroupSize constants - 1
                            else (kernelGroupId constants + 1) * kernelGroupSize constants - 1
                        ]
                  load_neutral =
                    forM_ (zip nes scan_x_params) $ \(ne, p) ->
                      copyDWIMFix (paramName p) [] ne []

              sComment "first thread reads last element as carry-in for next iteration" $ do
                crosses_segment <- dPrimVE "crosses_segment" $
                  case crossesSegment of
                    Nothing -> false
                    Just f ->
                      f
                        ( tvExp chunk_offset
                            + kernelGroupSize constants -1
                        )
                        ( tvExp chunk_offset
                            + kernelGroupSize constants
                        )
                should_load_carry <-
                  dPrimVE "should_load_carry" $
                    kernelLocalThreadId constants .==. 0 .&&. bNot crosses_segment
                sWhen should_load_carry load_carry
                when array_scan barrier
                sUnless should_load_carry load_neutral

              barrier

  return (num_threads, elems_per_group, crossesSegment)

scanStage2 ::
  Pattern KernelsMem ->
  TV Int32 ->
  Imp.TExp Int32 ->
  Count NumGroups SubExp ->
  CrossesSegment ->
  SegSpace ->
  [SegBinOp KernelsMem] ->
  CallKernelGen ()
scanStage2 (Pattern _ all_pes) stage1_num_threads elems_per_group num_groups crossesSegment space scans = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map toInt32Exp dims

  -- Our group size is the number of groups for the stage 1 kernel.
  let group_size = Count $ unCount num_groups
      group_size' = fmap toInt32Exp group_size

  let crossesSegment' = do
        f <- crossesSegment
        Just $ \from to ->
          f ((from + 1) * elems_per_group - 1) ((to + 1) * elems_per_group - 1)

  sKernelThread "scan_stage2" 1 group_size' (segFlat space) $ do
    constants <- kernelConstants <$> askEnv
    per_scan_local_arrs <- makeLocalArrays group_size (tvSize stage1_num_threads) scans
    let per_scan_rets = map (lambdaReturnType . segBinOpLambda) scans
        per_scan_pes = segBinOpChunks scans all_pes

    flat_idx <-
      dPrimV "flat_idx" $
        (kernelLocalThreadId constants + 1) * elems_per_group - 1
    -- Construct segment indices.
    zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ tvExp flat_idx

    forM_ (zip4 scans per_scan_local_arrs per_scan_rets per_scan_pes) $
      \(SegBinOp _ scan_op nes vec_shape, local_arrs, rets, pes) ->
        sLoopNest vec_shape $ \vec_is -> do
          let glob_is = map Imp.vi32 gtids ++ vec_is

              in_bounds =
                foldl1 (.&&.) $ zipWith (.<.) (map Imp.vi32 gtids) dims'

              when_in_bounds = forM_ (zip3 rets local_arrs pes) $ \(t, arr, pe) ->
                copyDWIMFix
                  arr
                  [localArrayIndex constants t]
                  (Var $ patElemName pe)
                  glob_is

              when_out_of_bounds = forM_ (zip3 rets local_arrs nes) $ \(t, arr, ne) ->
                copyDWIMFix arr [localArrayIndex constants t] ne []
              (_, _, barrier) =
                barrierFor scan_op

          sComment "threads in bound read carries; others get neutral element" $
            sIf in_bounds when_in_bounds when_out_of_bounds

          barrier

          groupScan
            crossesSegment'
            (tvExp stage1_num_threads)
            (kernelGroupSize constants)
            scan_op
            local_arrs

          sComment "threads in bounds write scanned carries" $
            sWhen in_bounds $
              forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
                copyDWIMFix
                  (patElemName pe)
                  glob_is
                  (Var arr)
                  [localArrayIndex constants t]

scanStage3 ::
  Pattern KernelsMem ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  Imp.TExp Int32 ->
  CrossesSegment ->
  SegSpace ->
  [SegBinOp KernelsMem] ->
  CallKernelGen ()
scanStage3 (Pattern _ all_pes) num_groups group_size elems_per_group crossesSegment space scans = do
  let num_groups' = fmap toInt32Exp num_groups
      group_size' = fmap toInt32Exp group_size
      (gtids, dims) = unzip $ unSegSpace space
      dims' = map toInt32Exp dims
  required_groups <-
    dPrimVE "required_groups" $
      product dims' `divUp` unCount group_size'

  sKernelThread "scan_stage3" num_groups' group_size' (segFlat space) $
    virtualiseGroups SegVirt required_groups $ \virt_group_id -> do
      constants <- kernelConstants <$> askEnv

      -- Compute our logical index.
      flat_idx <-
        dPrimVE "flat_idx" $
          virt_group_id * unCount group_size'
            + kernelLocalThreadId constants
      zipWithM_ dPrimV_ gtids $ unflattenIndex dims' flat_idx

      -- Figure out which group this element was originally in.
      orig_group <- dPrimV "orig_group" $ flat_idx `quot` elems_per_group
      -- Then the index of the carry-in of the preceding group.
      carry_in_flat_idx <-
        dPrimV "carry_in_flat_idx" $
          tvExp orig_group * elems_per_group - 1
      -- Figure out the logical index of the carry-in.
      let carry_in_idx = unflattenIndex dims' $ tvExp carry_in_flat_idx

      -- Apply the carry if we are not in the scan results for the first
      -- group, and are not the last element in such a group (because
      -- then the carry was updated in stage 2), and we are not crossing
      -- a segment boundary.
      let in_bounds =
            foldl1 (.&&.) $ zipWith (.<.) (map Imp.vi32 gtids) dims'
          crosses_segment =
            fromMaybe false $
              crossesSegment
                <*> pure (tvExp carry_in_flat_idx)
                <*> pure flat_idx
          is_a_carry = flat_idx .==. (tvExp orig_group + 1) * elems_per_group - 1
          no_carry_in = tvExp orig_group .==. 0 .||. is_a_carry .||. crosses_segment

      let per_scan_pes = segBinOpChunks scans all_pes
      sWhen in_bounds $
        sUnless no_carry_in $
          forM_ (zip per_scan_pes scans) $
            \(pes, SegBinOp _ scan_op nes vec_shape) -> do
              dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
              let (scan_x_params, scan_y_params) =
                    splitAt (length nes) $ lambdaParams scan_op

              sLoopNest vec_shape $ \vec_is -> do
                forM_ (zip scan_x_params pes) $ \(p, pe) ->
                  copyDWIMFix
                    (paramName p)
                    []
                    (Var $ patElemName pe)
                    (carry_in_idx ++ vec_is)

                forM_ (zip scan_y_params pes) $ \(p, pe) ->
                  copyDWIMFix
                    (paramName p)
                    []
                    (Var $ patElemName pe)
                    (map Imp.vi32 gtids ++ vec_is)

                compileBody' scan_x_params $ lambdaBody scan_op

                forM_ (zip scan_x_params pes) $ \(p, pe) ->
                  copyDWIMFix
                    (patElemName pe)
                    (map Imp.vi32 gtids ++ vec_is)
                    (Var $ paramName p)
                    []


createLocalArrays ::
  Count GroupSize SubExp ->
  SubExp ->
  [PrimType] ->
  InKernelGen ([VName], [VName])
createLocalArrays (Count groupSize) m types = do
  let groupSizeE = (toInt64Exp groupSize)
  let workSize = (toInt64Exp m) * groupSizeE
  let prefixArraysSize = foldl (\acc tySize -> (alignTo acc tySize) + tySize * groupSizeE) 0 $ map primByteSize types
  let byteOffsets = scanl (\off tySize -> (alignTo off tySize) + (pe32 groupSize) * tySize) 0 $ map primByteSize types
  let maxTransposedArraySize = foldl1 sMax64 $ map (\ty -> workSize * primByteSize ty) types
  let size = Imp.bytes $ sMax64 prefixArraysSize maxTransposedArraySize

  sComment "Allocate reused shared memeory" $ return ()

  localMem <- sAlloc "local_mem" size (Space "local")
  transposeArrayLength <- dPrimV "trans_arr_len" $ workSize

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

  return (transposedArrays, prefixArrays)
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

  let num_groups    = toInt32Exp <$> segNumGroups lvl
      group_size    = toInt32Exp <$> segGroupSize lvl
      num_threads   = (unCount num_groups) * (unCount group_size)
      res           = patElemName $ last all_pes
      (mapIdx, dim) = head $ unSegSpace space
      m :: Int32
      m             = 9
      tM :: Imp.TExp Int32
      tM            = 9
      scanOp        = head scans
      scanOpNe      = head $ segBinOpNeutral scanOp
      tys           = map (\(Prim pt) -> pt) $ lambdaReturnType $ segBinOpLambda scanOp

  -- Allocate the shared memory for output component
  numThreads <- dPrimV "numThreads" num_threads
  globalId <- sStaticArray "id_counter" (Space "device") int32 $ Imp.ArrayZeros 1

  -- TODO: Use dynamic block id instead of the static one
  sKernelThread "segscan" num_groups group_size (segFlat space) $ do
    constants  <- kernelConstants <$> askEnv

    dynamicId <- dPrim "dynamic_id" int32
    sWhen (kernelLocalThreadId constants .==. 0) $
      sOp $ Imp.Atomic (Space "global") $
        Imp.AtomicAdd Int32
                      (tvVar dynamicId)
                      globalId
                      (Count 0)
                      (toExp' int32 $ constant (1 :: Int32))

    blockOff   <- dPrimV "blockOff" $ (tvExp dynamicId) * tM * (kernelGroupSize constants)
    (transposedArrays, prefixArrays) <-
      createLocalArrays (segGroupSize lvl) (constant m) tys

    privateArrays <-
      mapM (\ty -> sAllocArray "private"
                               ty
                               (Shape [constant m])
                               (ScalarSpace [constant m] ty))
           tys

    let barrier = Imp.Barrier Imp.FenceLocal

    sComment "Load and map" $
      sFor "i" tM $ \i -> do
        -- The map's input index
        dPrimV_ mapIdx $ (tvExp blockOff) + (kernelLocalThreadId constants) + i * (kernelGroupSize constants)
        -- Perform the map
        compileStms mempty (kernelBodyStms kbody) $
          do let (all_scan_res, map_res) = splitAt (segBinOpResults scans) $ kernelBodyResult kbody
             forM_ (zip (takeLast (length map_res) all_pes) map_res) $ \(dest, src) -> do
               -- Write map results to their global memory destinations
               copyDWIMFix (patElemName dest) [Imp.vi32 mapIdx] (kernelResultSubExp src) []

             forM_ (zip privateArrays $ map kernelResultSubExp all_scan_res) $ \(dest, (Var src)) -> do
               copyDWIMFix dest [i] (Var src) []

    sComment "Transpose scan inputs" $
      forM_ (zip transposedArrays privateArrays) $ \(trans, priv) -> do
        sFor "i" tM $ \i -> do
          sharedIdx <- dPrimV "sharedIdx" $ (kernelLocalThreadId constants) + i * (kernelGroupSize constants)
          copyDWIMFix trans [tvExp sharedIdx] (Var priv) [i]
        sOp barrier
        sFor "i" tM $ \i -> do
          sharedIdx <- dPrimV "sharedIdx" $ (kernelLocalThreadId constants) * tM + i
          copyDWIMFix priv [i] (Var trans) [tvExp sharedIdx]

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
      sOp barrier


    scanOp' <- renameLambda $ segBinOpLambda scanOp

    sComment "Scan results (with warp scan)" $ do
      groupScan
        Nothing -- TODO
        (tvExp numThreads)
        (kernelGroupSize constants)
        scanOp'
        prefixArrays

    scanOp'' <- renameLambda scanOp'

    -- TODO: Distribution is broken
    sComment "Distribute results" $ do
      sWhen ((kernelLocalThreadId constants) .>. 0) $ do
        let xs = map paramName $ take (length tys) $ lambdaParams scanOp''
            ys = map paramName $ drop (length tys) $ lambdaParams scanOp''

        mapM_
          (\(src, x, y, ty) ->
            do dPrim_ x ty
               dPrim_ y ty
               copyDWIMFix x [] (Var src) [(kernelLocalThreadId constants) - 1])
          $ zip4 prefixArrays xs ys tys

        sFor "i" tM $ \i -> do
          mapM_
            (\(src, y) ->
               copyDWIMFix y [] (Var src) [i])
            $ zip privateArrays ys

          compileStms mempty (bodyStms $ lambdaBody scanOp'') $
            mapM_
              (\(dest, res) ->
                 copyDWIMFix dest [i] res [])
              $ zip privateArrays $ bodyResult $ lambdaBody scanOp''

    sComment "Write block scan results to global memory" $
      forM_ (zip (map patElemName all_pes) privateArrays) $ \(dest, src) -> do
        sFor "i" tM $ \i -> do
          dPrimV_ mapIdx $ (tvExp blockOff) + (kernelLocalThreadId constants) * (tM) + i
          sWhen ((Imp.vi32 mapIdx) .<. n) $ do
            copyDWIMFix dest [Imp.vi32 mapIdx] (Var src) [i]
  where
    n = product $ map toInt32Exp $ segSpaceDims space
