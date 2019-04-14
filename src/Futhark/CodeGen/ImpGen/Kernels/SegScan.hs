{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Kernels.SegScan
  ( compileSegScan )
  where

import Control.Monad.Except
import Data.Maybe
import Data.List

import Prelude hiding (quot, rem)

import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.CodeGen.ImpGen (sFor, sComment, sIf, sWhen, sUnless,
                               sOp,
                               (<--), dPrimV, dPrimV_)
import Futhark.CodeGen.ImpGen.Kernels.Base
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)

makeLocalArrays :: KernelConstants -> [SubExp] -> Lambda InKernel
                -> CallKernelGen [VName]
makeLocalArrays constants nes scan_op = do
  let (scan_x_params, _scan_y_params) =
        splitAt (length nes) $ lambdaParams scan_op
  forM scan_x_params $ \p ->
    case paramAttr p of
      MemArray pt shape _ (ArrayIn mem _) -> do
        num_threads <- dPrimV "num_threads" $ kernelNumThreads constants
        let shape' = Shape [Var num_threads] <> shape
        ImpGen.sArray "scan_arr" pt shape' $
          ArrayIn mem $ IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape'
      _ -> do
        group_size <- dPrimV "group_size" $ kernelGroupSize constants
        let pt = elemType $ paramType p
            shape = Shape [Var group_size]
        ImpGen.sAllocArray "scan_arr" pt shape $ Space "local"

type CrossesSegment = Maybe (Imp.Exp -> Imp.Exp -> Imp.Exp)

-- | Produce partially scanned intervals; one per workgroup.
scanStage1 :: Pattern ExplicitMemory
           -> KernelSpace
           -> Lambda InKernel -> [SubExp]
           -> Body InKernel
           -> CallKernelGen (Imp.Exp, CrossesSegment)
scanStage1 (Pattern _ pes) space scan_op nes body = do
  (base_constants, init_constants) <- kernelInitialisationSetSpace space $ return ()
  let (gtids, dims) = unzip $ spaceDimensions space
  dims' <- mapM ImpGen.compileSubExp dims
  let constants = base_constants { kernelThreadActive = true }
      num_elements = product dims'
      elems_per_thread = num_elements `quotRoundingUp` kernelNumThreads constants
      elems_per_group = kernelGroupSize constants * elems_per_thread

  -- Squirrel away a copy of the operator with unique names that we
  -- can pass to groupScan.
  scan_op_renamed <- renameLambda scan_op

  local_arrs <- makeLocalArrays constants nes scan_op

  let crossesSegment =
        case reverse dims' of
          segment_size : _ : _ -> Just $ \from to ->
            (to-from) .>. (to `rem` segment_size)
          _ -> Nothing

  sKernel constants "scan_stage1" $ allThreads constants $ do
    init_constants

    -- The variables from scan_op will be used for the carry and such
    -- in the big chunking loop.
    ImpGen.dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
    let (scan_x_params, scan_y_params) =
          splitAt (length nes) $ lambdaParams scan_op

    forM_ (zip scan_x_params nes) $ \(p, ne) ->
      ImpGen.copyDWIM (paramName p) [] ne []

    j <- newVName "j"
    sFor j Int32 elems_per_thread $ do
      chunk_offset <- dPrimV "chunk_offset" $
                      kernelGroupSize constants * Imp.var j int32 +
                      kernelGroupId constants * elems_per_group
      flat_idx <- dPrimV "flat_idx" $
                  Imp.var chunk_offset int32 + kernelLocalThreadId constants
      -- Construct segment indices.
      zipWithM_ (<--) gtids $ unflattenIndex dims' $ Imp.var flat_idx int32

      let in_bounds =
            foldl1 (.&&.) $ zipWith (.<.) (map (`Imp.var` int32) gtids) dims'
          when_in_bounds = ImpGen.compileStms mempty (stmsToList $ bodyStms body) $ do
            let (scan_res, map_res) = splitAt (length nes) $ bodyResult body
            sComment "write to-scan values to parameters" $
              forM_ (zip scan_y_params scan_res) $ \(p, se) ->
              ImpGen.copyDWIM (paramName p) [] se []
            sComment "write mapped values results to global memory" $
              forM_ (zip (drop (length nes) pes) map_res) $ \(pe, se) ->
              ImpGen.copyDWIM (patElemName pe) (map (`Imp.var` int32) gtids) se []
          when_out_of_bounds = forM_ (zip scan_y_params nes) $ \(p, ne) ->
            ImpGen.copyDWIM (paramName p) [] ne []

      sComment "threads in bounds read input; others get neutral element" $
        sIf in_bounds when_in_bounds when_out_of_bounds

      sComment "combine with carry and write to local memory" $
        ImpGen.compileStms mempty (stmsToList $ bodyStms $ lambdaBody scan_op) $
        forM_ (zip local_arrs $ bodyResult $ lambdaBody scan_op) $ \(arr, se) ->
          ImpGen.copyDWIM arr [kernelLocalThreadId constants] se []

      let crossesSegment' = do
            f <- crossesSegment
            Just $ \from to ->
              let from' = from + Imp.var chunk_offset int32
                  to' = to + Imp.var chunk_offset int32
              in f from' to'

      groupScan constants crossesSegment'
        (kernelGroupSize constants) scan_op_renamed local_arrs

      sComment "threads in bounds write partial scan result" $
        sWhen in_bounds $ forM_ (zip pes local_arrs) $ \(pe, arr) ->
        ImpGen.copyDWIM (patElemName pe) (map (`Imp.var` int32) gtids)
        (Var arr) [kernelLocalThreadId constants]

      sOp Imp.LocalBarrier

      let load_carry =
            forM_ (zip local_arrs scan_x_params) $ \(arr, p) ->
            ImpGen.copyDWIM (paramName p) [] (Var arr) [kernelGroupSize constants - 1]
          load_neutral =
            forM_ (zip nes scan_x_params) $ \(ne, p) ->
            ImpGen.copyDWIM (paramName p) [] ne []

      sComment "first thread reads last element as carry-in for next iteration" $
        sWhen (kernelLocalThreadId constants .==. 0) $
        case crossesSegment of Nothing -> load_carry
                               Just f -> sIf (f (Imp.var chunk_offset int32 +
                                                 kernelGroupSize constants-1)
                                                (Imp.var chunk_offset int32 +
                                                 kernelGroupSize constants))
                                         load_neutral load_carry

      sOp Imp.LocalBarrier

  return (elems_per_group, crossesSegment)

scanStage2 :: Pattern ExplicitMemory
           -> Imp.Exp -> CrossesSegment -> KernelSpace
           -> Lambda InKernel -> [SubExp]
           -> CallKernelGen ()
scanStage2 (Pattern _ pes) elems_per_group crossesSegment space scan_op nes = do
  -- A single group, with one thread for each group in stage 1.
  group_size <- ImpGen.compileSubExp $ spaceNumGroups space
  (constants, init_constants) <-
    kernelInitialisationSimple 1 group_size Nothing

  local_arrs <- makeLocalArrays constants nes scan_op

  let (gtids, dims) = unzip $ spaceDimensions space
  dims' <- mapM ImpGen.compileSubExp dims
  let crossesSegment' = do
        f <- crossesSegment
        Just $ \from to ->
          f ((from + 1) * elems_per_group - 1) ((to + 1) * elems_per_group - 1)

  sKernel constants "scan_stage2" $ do
    init_constants
    flat_idx <- dPrimV "flat_idx" $
      (kernelLocalThreadId constants + 1) * elems_per_group - 1
    -- Construct segment indices.
    zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ Imp.var flat_idx int32

    let in_bounds =
          foldl1 (.&&.) $ zipWith (.<.) (map (`Imp.var` int32) gtids) dims'
        when_in_bounds = forM_ (zip local_arrs pes) $ \(arr, pe) ->
          ImpGen.copyDWIM arr [kernelLocalThreadId constants]
          (Var $ patElemName pe) $ map (`Imp.var` int32) gtids
        when_out_of_bounds = forM_ (zip local_arrs nes) $ \(arr, ne) ->
          ImpGen.copyDWIM arr [kernelLocalThreadId constants] ne []

    sComment "threads in bound read carries; others get neutral element" $
      sIf in_bounds when_in_bounds when_out_of_bounds

    groupScan constants crossesSegment'
      (kernelGroupSize constants) scan_op local_arrs

    sComment "threads in bounds write scanned carries" $
      sWhen in_bounds $ forM_ (zip pes local_arrs) $ \(pe, arr) ->
      ImpGen.copyDWIM (patElemName pe) (map (`Imp.var` int32) gtids)
      (Var arr) [kernelLocalThreadId constants]

scanStage3 :: Pattern ExplicitMemory
           -> Imp.Exp -> CrossesSegment -> KernelSpace
           -> Lambda InKernel -> [SubExp]
           -> CallKernelGen ()
scanStage3 (Pattern _ pes) elems_per_group crossesSegment space scan_op nes = do
  let (gtids, dims) = unzip $ spaceDimensions space
  dims' <- mapM ImpGen.compileSubExp dims
  (constants, init_constants) <- simpleKernelConstants (product dims') "scan"
  sKernel constants "scan_stage3" $ do
    init_constants
    -- Compute our logical index.
    zipWithM_ ImpGen.dPrimV_ gtids $ unflattenIndex dims' $ kernelGlobalThreadId constants
    -- Figure out which group this element was originally in.
    orig_group <- dPrimV "orig_group" $
                  kernelGlobalThreadId constants `quot` elems_per_group
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
            pure (kernelGlobalThreadId constants)
        is_a_carry = kernelGlobalThreadId constants .==.
                     (Imp.var orig_group int32 + 1) * elems_per_group - 1
        no_carry_in = Imp.var orig_group int32 .==. 0 .||. is_a_carry .||. crosses_segment

    sWhen (kernelThreadActive constants) $ sUnless no_carry_in $ do
      ImpGen.dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
      let (scan_x_params, scan_y_params) =
            splitAt (length nes) $ lambdaParams scan_op
      forM_ (zip scan_x_params pes) $ \(p, pe) ->
        ImpGen.copyDWIM (paramName p) [] (Var $ patElemName pe) carry_in_idx
      forM_ (zip scan_y_params pes) $ \(p, pe) ->
        ImpGen.copyDWIM (paramName p) [] (Var $ patElemName pe) $ map (`Imp.var` int32) gtids
      ImpGen.compileBody' scan_x_params $ lambdaBody scan_op
      forM_ (zip scan_x_params pes) $ \(p, pe) ->
        ImpGen.copyDWIM (patElemName pe) (map (`Imp.var` int32) gtids) (Var $ paramName p) []

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan :: Pattern ExplicitMemory
               -> KernelSpace
               -> Lambda InKernel -> [SubExp]
               -> Body InKernel
               -> CallKernelGen ()
compileSegScan pat space scan_op nes body = do
  (elems_per_group, crossesSegment) <- scanStage1 pat space scan_op nes body

  ImpGen.emit $ Imp.DebugPrint "elems_per_group" int32 elems_per_group

  scan_op' <- renameLambda scan_op
  scan_op'' <- renameLambda scan_op
  scanStage2 pat elems_per_group crossesSegment space scan_op' nes
  scanStage3 pat elems_per_group crossesSegment space scan_op'' nes
