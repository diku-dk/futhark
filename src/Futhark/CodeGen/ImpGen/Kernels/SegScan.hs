{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Kernels.SegScan
  ( compileSegScan )
  where

import Control.Monad.Except
import Data.Maybe
import Data.List

import Prelude hiding (quot, rem)

import Futhark.Transform.Rename
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)

makeLocalArrays :: Count GroupSize SubExp -> SubExp -> [SubExp] -> Lambda ExplicitMemory
                -> InKernelGen [VName]
makeLocalArrays (Count group_size) num_threads nes scan_op = do
  let (scan_x_params, _scan_y_params) =
        splitAt (length nes) $ lambdaParams scan_op
  forM scan_x_params $ \p ->
    case paramAttr p of
      MemArray pt shape _ (ArrayIn mem _) -> do
        let shape' = Shape [num_threads] <> shape
        sArray "scan_arr" pt shape' $
          ArrayIn mem $ IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape'
      _ -> do
        let pt = elemType $ paramType p
            shape = Shape [group_size]
        sAllocArray "scan_arr" pt shape $ Space "local"

type CrossesSegment = Maybe (Imp.Exp -> Imp.Exp -> Imp.Exp)

localArrayIndex :: KernelConstants -> Type -> Imp.Exp
localArrayIndex constants t =
  if primType t
  then kernelLocalThreadId constants
  else kernelGlobalThreadId constants

-- | Produce partially scanned intervals; one per workgroup.
scanStage1 :: Pattern ExplicitMemory
           -> Count NumGroups SubExp -> Count GroupSize SubExp -> SegSpace
           -> Lambda ExplicitMemory -> [SubExp]
           -> KernelBody ExplicitMemory
           -> CallKernelGen (VName, Imp.Exp, CrossesSegment)
scanStage1 (Pattern _ pes) num_groups group_size space scan_op nes kbody = do
  num_groups' <- traverse toExp num_groups
  group_size' <- traverse toExp group_size
  num_threads <- dPrimV "num_threads" $
                 unCount num_groups' * unCount group_size'

  let (gtids, dims) = unzip $ unSegSpace space
      rets = lambdaReturnType scan_op
  dims' <- mapM toExp dims
  let num_elements = product dims'
      elems_per_thread = num_elements `quotRoundingUp` Imp.vi32 num_threads
      elems_per_group = unCount group_size' * elems_per_thread

  -- Squirrel away a copy of the operator with unique names that we
  -- can pass to groupScan.
  scan_op_renamed <- renameLambda scan_op

  let crossesSegment =
        case reverse dims' of
          segment_size : _ : _ -> Just $ \from to ->
            (to-from) .>. (to `rem` segment_size)
          _ -> Nothing

  sKernelThread "scan_stage1" num_groups' group_size' (segFlat space) $ \constants -> do
    local_arrs <- makeLocalArrays group_size (Var num_threads) nes scan_op

    -- The variables from scan_op will be used for the carry and such
    -- in the big chunking loop.
    dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
    let (scan_x_params, scan_y_params) =
          splitAt (length nes) $ lambdaParams scan_op

    forM_ (zip scan_x_params nes) $ \(p, ne) ->
      copyDWIMFix (paramName p) [] ne []

    sFor "j" elems_per_thread $ \j -> do
      chunk_offset <- dPrimV "chunk_offset" $
                      kernelGroupSize constants * j +
                      kernelGroupId constants * elems_per_group
      flat_idx <- dPrimV "flat_idx" $
                  Imp.var chunk_offset int32 + kernelLocalThreadId constants
      -- Construct segment indices.
      zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ Imp.var flat_idx int32

      let in_bounds =
            foldl1 (.&&.) $ zipWith (.<.) (map (`Imp.var` int32) gtids) dims'
          when_in_bounds = compileStms mempty (kernelBodyStms kbody) $ do
            let (scan_res, map_res) = splitAt (length nes) $ kernelBodyResult kbody
            sComment "write to-scan values to parameters" $
              forM_ (zip scan_y_params scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
            sComment "write mapped values results to global memory" $
              forM_ (zip (drop (length nes) pes) map_res) $ \(pe, se) ->
              copyDWIMFix (patElemName pe) (map (`Imp.var` int32) gtids)
              (kernelResultSubExp se) []
          when_out_of_bounds = forM_ (zip scan_y_params nes) $ \(p, ne) ->
            copyDWIMFix (paramName p) [] ne []

      sComment "threads in bounds read input; others get neutral element" $
        sIf in_bounds when_in_bounds when_out_of_bounds

      sComment "combine with carry and write to local memory" $
        compileStms mempty (bodyStms $ lambdaBody scan_op) $
        forM_ (zip3 rets local_arrs (bodyResult $ lambdaBody scan_op)) $ \(t, arr, se) ->
        copyDWIMFix arr [localArrayIndex constants t] se []

      let crossesSegment' = do
            f <- crossesSegment
            Just $ \from to ->
              let from' = from + Imp.var chunk_offset int32
                  to' = to + Imp.var chunk_offset int32
              in f from' to'

      sOp $ Imp.ErrorSync fence

      groupScan constants crossesSegment'
        (Imp.vi32 num_threads)
        (kernelGroupSize constants) scan_op_renamed local_arrs

      sComment "threads in bounds write partial scan result" $
        sWhen in_bounds $ forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
        copyDWIMFix (patElemName pe) (map (`Imp.var` int32) gtids)
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

  where array_scan = not $ all primType $ lambdaReturnType scan_op
        fence | array_scan = Imp.FenceGlobal
              | otherwise = Imp.FenceLocal
        barrier = sOp $ Imp.Barrier fence


scanStage2 :: Pattern ExplicitMemory
           -> VName -> Imp.Exp -> Count NumGroups SubExp -> CrossesSegment -> SegSpace
           -> Lambda ExplicitMemory -> [SubExp]
           -> CallKernelGen ()
scanStage2 (Pattern _ pes) stage1_num_threads elems_per_group num_groups crossesSegment space scan_op nes = do
  -- Our group size is the number of groups for the stage 1 kernel.
  let group_size = Count $ unCount num_groups
  group_size' <- traverse toExp group_size

  let (gtids, dims) = unzip $ unSegSpace space
      rets = lambdaReturnType scan_op
  dims' <- mapM toExp dims
  let crossesSegment' = do
        f <- crossesSegment
        Just $ \from to ->
          f ((from + 1) * elems_per_group - 1) ((to + 1) * elems_per_group - 1)

  sKernelThread  "scan_stage2" 1 group_size' (segFlat space) $ \constants -> do
    local_arrs <- makeLocalArrays group_size (Var stage1_num_threads) nes scan_op

    flat_idx <- dPrimV "flat_idx" $
      (kernelLocalThreadId constants + 1) * elems_per_group - 1
    -- Construct segment indices.
    zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ Imp.var flat_idx int32

    let in_bounds =
          foldl1 (.&&.) $ zipWith (.<.) (map (`Imp.var` int32) gtids) dims'
        when_in_bounds = forM_ (zip3 rets local_arrs pes) $ \(t, arr, pe) ->
          copyDWIMFix arr [localArrayIndex constants t]
          (Var $ patElemName pe) $ map (`Imp.var` int32) gtids
        when_out_of_bounds = forM_ (zip3 rets local_arrs nes) $ \(t, arr, ne) ->
          copyDWIMFix arr [localArrayIndex constants t] ne []

    sComment "threads in bound read carries; others get neutral element" $
      sIf in_bounds when_in_bounds when_out_of_bounds

    groupScan constants crossesSegment'
      (Imp.vi32 stage1_num_threads) (kernelGroupSize constants) scan_op local_arrs

    sComment "threads in bounds write scanned carries" $
      sWhen in_bounds $ forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
      copyDWIMFix (patElemName pe) (map (`Imp.var` int32) gtids)
      (Var arr) [localArrayIndex constants t]

scanStage3 :: Pattern ExplicitMemory
           -> Count NumGroups SubExp -> Count GroupSize SubExp
           -> Imp.Exp -> CrossesSegment -> SegSpace
           -> Lambda ExplicitMemory -> [SubExp]
           -> CallKernelGen ()
scanStage3 (Pattern _ pes) num_groups group_size elems_per_group crossesSegment space scan_op nes = do
  num_groups' <- traverse toExp num_groups
  group_size' <- traverse toExp group_size
  let (gtids, dims) = unzip $ unSegSpace space
  dims' <- mapM toExp dims
  required_groups <- dPrimVE "required_groups" $
                     product dims' `quotRoundingUp` unCount group_size'

  sKernelThread "scan_stage3" num_groups' group_size' (segFlat space) $ \constants ->
    virtualiseGroups constants SegVirt required_groups $ \virt_group_id -> do
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
    let crosses_segment = fromMaybe false $
          crossesSegment <*>
            pure (Imp.var carry_in_flat_idx int32) <*>
            pure flat_idx
        is_a_carry = flat_idx .==.
                     (Imp.var orig_group int32 + 1) * elems_per_group - 1
        no_carry_in = Imp.var orig_group int32 .==. 0 .||. is_a_carry .||. crosses_segment

    sWhen (kernelThreadActive constants) $ sUnless no_carry_in $ do
      dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
      let (scan_x_params, scan_y_params) =
            splitAt (length nes) $ lambdaParams scan_op
      forM_ (zip scan_x_params pes) $ \(p, pe) ->
        copyDWIMFix (paramName p) [] (Var $ patElemName pe) carry_in_idx
      forM_ (zip scan_y_params pes) $ \(p, pe) ->
        copyDWIMFix (paramName p) [] (Var $ patElemName pe) $ map (`Imp.var` int32) gtids
      compileBody' scan_x_params $ lambdaBody scan_op
      forM_ (zip scan_x_params pes) $ \(p, pe) ->
        copyDWIMFix (patElemName pe) (map (`Imp.var` int32) gtids) (Var $ paramName p) []

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan :: Pattern ExplicitMemory
               -> SegLevel -> SegSpace
               -> Lambda ExplicitMemory -> [SubExp]
               -> KernelBody ExplicitMemory
               -> CallKernelGen ()
compileSegScan pat lvl space scan_op nes kbody = do
  emit $ Imp.DebugPrint "\n# SegScan" Nothing

  (stage1_num_threads, elems_per_group, crossesSegment) <-
    scanStage1 pat (segNumGroups lvl) (segGroupSize lvl) space scan_op nes kbody

  emit $ Imp.DebugPrint "elems_per_group" $ Just elems_per_group

  scan_op' <- renameLambda scan_op
  scan_op'' <- renameLambda scan_op
  scanStage2 pat stage1_num_threads elems_per_group (segNumGroups lvl) crossesSegment space scan_op' nes
  scanStage3 pat (segNumGroups lvl) (segGroupSize lvl) elems_per_group crossesSegment space scan_op'' nes
