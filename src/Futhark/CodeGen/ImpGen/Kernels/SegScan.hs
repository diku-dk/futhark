{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Code generation for segmented and non-segmented scans.  Uses a
-- fairly inefficient two-pass algorithm.
module Futhark.CodeGen.ImpGen.Kernels.SegScan
  ( compileSegScan )
  where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Data.List (delete, find, foldl', zip4)

import Prelude hiding (quot, rem)

import Futhark.Transform.Rename
import Futhark.IR.KernelsMem
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Util.IntegralExp (divUp, quot, rem)
import Futhark.Util (takeLast)

-- Aggressively try to reuse memory for different SegBinOps, because
-- we will run them sequentially after another.
makeLocalArrays :: Count GroupSize SubExp -> SubExp -> [SegBinOp KernelsMem]
                -> InKernelGen [[VName]]
makeLocalArrays (Count group_size) num_threads scans = do
  (arrs, mems_and_sizes) <- runStateT (mapM onScan scans) mempty
  let maxSize sizes =
        Imp.bytes $ foldl' (Imp.BinOpExp (SMax Int32)) 1 $
        map Imp.unCount sizes
  forM_ mems_and_sizes $ \(sizes, mem) ->
    sAlloc_ mem (maxSize sizes) (Space "local")
  return arrs

  where onScan (SegBinOp _ scan_op nes _) = do
          let (scan_x_params, _scan_y_params) =
                splitAt (length nes) $ lambdaParams scan_op
          (arrs, used_mems) <- fmap unzip $ forM scan_x_params $ \p ->
            case paramDec p of
              MemArray pt shape _ (ArrayIn mem _) -> do
                let shape' = Shape [num_threads] <> shape
                arr <- lift $ sArray "scan_arr" pt shape' $
                  ArrayIn mem $ IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape'
                return (arr, [])
              _ -> do
                let ElemPrim pt = elemType $ paramType p
                    shape = Shape [group_size]
                (sizes, mem') <- getMem pt shape
                arr <- lift $ sArrayInMem "scan_arr" pt shape mem'
                return (arr, [(sizes, mem')])
          modify (<>concat used_mems)
          return arrs

        getMem pt shape = do
          let size = typeSize $ Array (ElemPrim pt) shape NoUniqueness
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

type CrossesSegment = Maybe (Imp.Exp -> Imp.Exp -> Imp.Exp)

localArrayIndex :: KernelConstants -> Type -> Imp.Exp
localArrayIndex constants t =
  if primType t
  then kernelLocalThreadId constants
  else kernelGlobalThreadId constants

barrierFor :: Lambda KernelsMem -> (Bool, Imp.Fence, InKernelGen ())
barrierFor scan_op = (array_scan, fence, sOp $ Imp.Barrier fence)
  where array_scan = not $ all primType $ lambdaReturnType scan_op
        fence | array_scan = Imp.FenceGlobal
              | otherwise = Imp.FenceLocal

xParams, yParams :: SegBinOp KernelsMem -> [LParam KernelsMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

writeToScanValues :: [VName]
                  -> ([PatElem KernelsMem], SegBinOp KernelsMem, [KernelResult])
                  -> InKernelGen ()
writeToScanValues gtids (pes, scan, scan_res)
  | shapeRank (segBinOpShape scan) > 0 =
      forM_ (zip pes scan_res) $ \(pe, res) ->
      copyDWIMFix (patElemName pe) (map Imp.vi32 gtids)
      (kernelResultSubExp res) []
  | otherwise =
      forM_ (zip (yParams scan) scan_res) $ \(p, res) ->
      copyDWIMFix (paramName p) [] (kernelResultSubExp res) []

readToScanValues :: [Imp.Exp] -> [PatElem KernelsMem] -> SegBinOp KernelsMem
                 -> InKernelGen ()
readToScanValues is pes scan
  | shapeRank (segBinOpShape scan) > 0 =
      forM_ (zip (yParams scan) pes) $ \(p, pe) ->
      copyDWIMFix (paramName p) [] (Var (patElemName pe)) is
  | otherwise =
      return ()

readCarries :: Imp.Exp -> [Imp.Exp] -> [Imp.Exp]
            -> [PatElem KernelsMem]
            -> SegBinOp KernelsMem
            -> InKernelGen ()
readCarries chunk_offset dims' vec_is pes scan
  | shapeRank (segBinOpShape scan) > 0 = do
      ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
      -- We may have to reload the carries from the output of the
      -- previous chunk.
      sIf (chunk_offset .>. 0 .&&. ltid .==. 0)
        (do let is = unflattenIndex dims' $ chunk_offset - 1
            forM_ (zip (xParams scan) pes) $ \(p, pe) ->
              copyDWIMFix (paramName p) [] (Var (patElemName pe)) (is++vec_is))
        (forM_ (zip (xParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
            copyDWIMFix (paramName p) [] ne [])
  | otherwise =
      return ()

-- | Produce partially scanned intervals; one per workgroup.
scanStage1 :: Pattern KernelsMem
           -> Count NumGroups SubExp -> Count GroupSize SubExp -> SegSpace
           -> [SegBinOp KernelsMem]
           -> KernelBody KernelsMem
           -> CallKernelGen (VName, Imp.Exp, CrossesSegment)
scanStage1 (Pattern _ all_pes) num_groups group_size space scans kbody = do
  num_groups' <- traverse toExp num_groups
  group_size' <- traverse toExp group_size
  num_threads <- dPrimV "num_threads" $
                 unCount num_groups' * unCount group_size'

  let (gtids, dims) = unzip $ unSegSpace space
  dims' <- mapM toExp dims
  let num_elements = product dims'
      elems_per_thread = num_elements `divUp` Imp.vi32 num_threads
      elems_per_group = unCount group_size' * elems_per_thread

  let crossesSegment =
        case reverse dims' of
          segment_size : _ : _ -> Just $ \from to ->
            (to-from) .>. (to `rem` segment_size)
          _ -> Nothing

  sKernelThread "scan_stage1" num_groups' group_size' (segFlat space) $ do
    constants <- kernelConstants <$> askEnv
    all_local_arrs <- makeLocalArrays group_size (Var num_threads) scans

    -- The variables from scan_op will be used for the carry and such
    -- in the big chunking loop.
    forM_ scans $ \scan -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan
      forM_ (zip (xParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

    sFor "j" elems_per_thread $ \j -> do
      chunk_offset <- dPrimV "chunk_offset" $
                      kernelGroupSize constants * j +
                      kernelGroupId constants * elems_per_group
      flat_idx <- dPrimV "flat_idx" $
                  Imp.var chunk_offset int32 + kernelLocalThreadId constants
      -- Construct segment indices.
      zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ Imp.vi32 flat_idx

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
              copyDWIMFix (patElemName pe) (map Imp.vi32 gtids)
              (kernelResultSubExp se) []

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
            sIf in_bounds
            (do readToScanValues (map Imp.vi32 gtids++vec_is) pes scan
                readCarries (Imp.vi32 chunk_offset) dims' vec_is pes scan)
            (forM_ (zip (yParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
                copyDWIMFix (paramName p) [] ne [])

          sComment "combine with carry and write to local memory" $
            compileStms mempty (bodyStms $ lambdaBody scan_op) $
            forM_ (zip3 rets local_arrs (bodyResult $ lambdaBody scan_op)) $
            \(t, arr, se) -> copyDWIMFix arr [localArrayIndex constants t] se []

          let crossesSegment' = do
                f <- crossesSegment
                Just $ \from to ->
                  let from' = from + Imp.var chunk_offset int32
                      to' = to + Imp.var chunk_offset int32
                  in f from' to'

          sOp $ Imp.ErrorSync fence

          -- We need to avoid parameter name clashes.
          scan_op_renamed <- renameLambda scan_op
          groupScan crossesSegment'
            (Imp.vi32 num_threads)
            (kernelGroupSize constants) scan_op_renamed local_arrs

          sComment "threads in bounds write partial scan result" $
            sWhen in_bounds $ forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
            copyDWIMFix (patElemName pe) (map Imp.vi32 gtids++vec_is)
            (Var arr) [localArrayIndex constants t]

          barrier

          let load_carry =
                forM_ (zip local_arrs scan_x_params) $ \(arr, p) ->
                copyDWIMFix (paramName p) [] (Var arr)
                [if primType $ paramType p
                 then kernelGroupSize constants - 1
                 else (kernelGroupId constants+1) * kernelGroupSize constants - 1]
              load_neutral =
                forM_ (zip nes scan_x_params) $ \(ne, p) ->
                copyDWIMFix (paramName p) [] ne []

          sComment "first thread reads last element as carry-in for next iteration" $ do
            crosses_segment <- dPrimVE "crosses_segment" $
              case crossesSegment of
                Nothing -> false
                Just f -> f (Imp.var chunk_offset int32 +
                             kernelGroupSize constants-1)
                            (Imp.var chunk_offset int32 +
                             kernelGroupSize constants)
            should_load_carry <- dPrimVE "should_load_carry" $
              kernelLocalThreadId constants .==. 0 .&&. UnOpExp Not crosses_segment
            sWhen should_load_carry load_carry
            when array_scan barrier
            sUnless should_load_carry load_neutral

          barrier

  return (num_threads, elems_per_group, crossesSegment)

scanStage2 :: Pattern KernelsMem
           -> VName -> Imp.Exp -> Count NumGroups SubExp -> CrossesSegment -> SegSpace
           -> [SegBinOp KernelsMem]
           -> CallKernelGen ()
scanStage2 (Pattern _ all_pes) stage1_num_threads elems_per_group num_groups crossesSegment space scans = do
  let (gtids, dims) = unzip $ unSegSpace space
  dims' <- mapM toExp dims

  -- Our group size is the number of groups for the stage 1 kernel.
  let group_size = Count $ unCount num_groups
  group_size' <- traverse toExp group_size

  let crossesSegment' = do
        f <- crossesSegment
        Just $ \from to ->
          f ((from + 1) * elems_per_group - 1) ((to + 1) * elems_per_group - 1)

  sKernelThread  "scan_stage2" 1 group_size' (segFlat space) $ do
    constants <- kernelConstants <$> askEnv
    per_scan_local_arrs <- makeLocalArrays group_size (Var stage1_num_threads) scans
    let per_scan_rets = map (lambdaReturnType . segBinOpLambda) scans
        per_scan_pes = segBinOpChunks scans all_pes

    flat_idx <- dPrimV "flat_idx" $
      (kernelLocalThreadId constants + 1) * elems_per_group - 1
    -- Construct segment indices.
    zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ Imp.var flat_idx int32

    forM_ (zip4 scans per_scan_local_arrs per_scan_rets per_scan_pes) $
      \(SegBinOp _ scan_op nes vec_shape, local_arrs, rets, pes) ->
        sLoopNest vec_shape $ \vec_is -> do
        let glob_is = map Imp.vi32 gtids ++ vec_is

            in_bounds =
              foldl1 (.&&.) $ zipWith (.<.) (map Imp.vi32 gtids) dims'

            when_in_bounds = forM_ (zip3 rets local_arrs pes) $ \(t, arr, pe) ->
              copyDWIMFix arr [localArrayIndex constants t]
              (Var $ patElemName pe) glob_is

            when_out_of_bounds = forM_ (zip3 rets local_arrs nes) $ \(t, arr, ne) ->
              copyDWIMFix arr [localArrayIndex constants t] ne []
            (_, _, barrier) =
              barrierFor scan_op

        sComment "threads in bound read carries; others get neutral element" $
          sIf in_bounds when_in_bounds when_out_of_bounds

        barrier

        groupScan crossesSegment'
          (Imp.vi32 stage1_num_threads) (kernelGroupSize constants) scan_op local_arrs

        sComment "threads in bounds write scanned carries" $
          sWhen in_bounds $ forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
          copyDWIMFix (patElemName pe) glob_is
          (Var arr) [localArrayIndex constants t]

scanStage3 :: Pattern KernelsMem
           -> Count NumGroups SubExp -> Count GroupSize SubExp
           -> Imp.Exp -> CrossesSegment -> SegSpace
           -> [SegBinOp KernelsMem]
           -> CallKernelGen ()
scanStage3 (Pattern _ all_pes) num_groups group_size elems_per_group crossesSegment space scans = do
  num_groups' <- traverse toExp num_groups
  group_size' <- traverse toExp group_size
  let (gtids, dims) = unzip $ unSegSpace space
  dims' <- mapM toExp dims
  required_groups <- dPrimVE "required_groups" $
                     product dims' `divUp` unCount group_size'

  sKernelThread "scan_stage3" num_groups' group_size' (segFlat space) $
    virtualiseGroups SegVirt required_groups $ \virt_group_id -> do
    constants <- kernelConstants <$> askEnv

    -- Compute our logical index.
    flat_idx <- dPrimVE "flat_idx" $
                Imp.vi32 virt_group_id * unCount group_size' +
                kernelLocalThreadId constants
    zipWithM_ dPrimV_ gtids $ unflattenIndex dims' flat_idx

    -- Figure out which group this element was originally in.
    orig_group <- dPrimV "orig_group" $ flat_idx `quot` elems_per_group
    -- Then the index of the carry-in of the preceding group.
    carry_in_flat_idx <- dPrimV "carry_in_flat_idx" $
                         Imp.var orig_group int32 * elems_per_group - 1
    -- Figure out the logical index of the carry-in.
    let carry_in_idx = unflattenIndex dims' $ Imp.var carry_in_flat_idx int32

    -- Apply the carry if we are not in the scan results for the first
    -- group, and are not the last element in such a group (because
    -- then the carry was updated in stage 2), and we are not crossing
    -- a segment boundary.
    let in_bounds =
          foldl1 (.&&.) $ zipWith (.<.) (map Imp.vi32 gtids) dims'
        crosses_segment = fromMaybe false $
          crossesSegment <*>
            pure (Imp.var carry_in_flat_idx int32) <*>
            pure flat_idx
        is_a_carry = flat_idx .==.
                     (Imp.var orig_group int32 + 1) * elems_per_group - 1
        no_carry_in = Imp.var orig_group int32 .==. 0 .||. is_a_carry .||. crosses_segment

    let per_scan_pes = segBinOpChunks scans all_pes
    sWhen in_bounds $ sUnless no_carry_in $
      forM_ (zip per_scan_pes scans) $
      \(pes, SegBinOp _ scan_op nes vec_shape) -> do
        dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
        let (scan_x_params, scan_y_params) =
              splitAt (length nes) $ lambdaParams scan_op

        sLoopNest vec_shape $ \vec_is -> do
          forM_ (zip scan_x_params pes) $ \(p, pe) ->
            copyDWIMFix (paramName p) []
            (Var $ patElemName pe) (carry_in_idx++vec_is)

          forM_ (zip scan_y_params pes) $ \(p, pe) ->
            copyDWIMFix (paramName p) []
            (Var $ patElemName pe) (map Imp.vi32 gtids++vec_is)

          compileBody' scan_x_params $ lambdaBody scan_op

          forM_ (zip scan_x_params pes) $ \(p, pe) ->
            copyDWIMFix (patElemName pe) (map Imp.vi32 gtids++vec_is)
            (Var $ paramName p) []

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan :: Pattern KernelsMem
               -> SegLevel -> SegSpace
               -> [SegBinOp KernelsMem]
               -> KernelBody KernelsMem
               -> CallKernelGen ()
compileSegScan pat lvl space scans kbody = do
  emit $ Imp.DebugPrint "\n# SegScan" Nothing

  -- Since stage 2 involves a group size equal to the number of groups
  -- used for stage 1, we have to cap this number to the maximum group
  -- size.
  stage1_max_num_groups <-
    dPrim "stage1_max_num_groups" int32
  sOp $ Imp.GetSizeMax stage1_max_num_groups SizeGroup

  stage1_num_groups <-
    fmap (Imp.Count . Var) $ dPrimV "stage1_num_groups" $
    Imp.BinOpExp (SMin Int32) (Imp.vi32 stage1_max_num_groups) $
    toExp' int32 $ Imp.unCount $ segNumGroups lvl

  (stage1_num_threads, elems_per_group, crossesSegment) <-
    scanStage1 pat stage1_num_groups (segGroupSize lvl) space scans kbody

  emit $ Imp.DebugPrint "elems_per_group" $ Just elems_per_group

  scanStage2 pat stage1_num_threads elems_per_group stage1_num_groups crossesSegment space scans
  scanStage3 pat (segNumGroups lvl) (segGroupSize lvl) elems_per_group crossesSegment space scans
