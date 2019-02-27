{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Kernels.SegGenRed
  ( compileSegGenRed )
  where

import Control.Monad.Except
import Data.Maybe
import Data.List

import Prelude hiding (quot, rem)

import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.CodeGen.ImpGen ((<--),
                               sFor, sComment, sWhen,
                               dPrim_, dPrimV)
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)
import Futhark.Util (chunks, mapAccumLM)

compileSegGenRed :: Pattern ExplicitMemory
                 -> KernelSpace
                 -> [GenReduceOp InKernel]
                 -> Body InKernel
                 -> CallKernelGen ()
compileSegGenRed (Pattern _ pes) space ops body = do
  (base_constants, init_constants) <- kernelInitialisationSetSpace space $ return ()
  let constants = base_constants { kernelThreadActive = true }
      (space_is, space_sizes) = unzip $ spaceDimensions space
      i32_to_i64 = ConvOpExp (SExt Int32 Int64)
      space_sizes_64 = map (i32_to_i64 . ImpGen.compileSubExpOfType int32) space_sizes
      total_w_64 = product space_sizes_64

  elems_per_thread_64 <- dPrimV "elems_per_thread_64" $
                         total_w_64 `quotRoundingUp`
                         ConvOpExp (SExt Int32 Int64) (kernelNumThreads constants)

  -- We need a separate lock array if the opterators are not all of a
  -- particularly simple form that permits pure atomic operations.
  (_, do_ops) <- mapAccumLM prepareAtomicUpdate Nothing ops

  sKernel constants "seggenred" $ allThreads constants $ do
    init_constants

    i <- newVName "i"

    -- Compute subhistogram index for each thread, per histogram.
    subhisto_inds <- forM ops $ \op ->
      dPrimV "subhisto_ind" $
      kernelGlobalThreadId constants `quot`
      (kernelNumThreads constants `quotRoundingUp`
       ImpGen.compileSubExpOfType int32 (genReduceNumHistograms op))

    sFor i Int64 (Imp.var elems_per_thread_64 int64) $ do
      -- Compute the offset into the input and output.  To this a
      -- thread can add its local ID to figure out which element it is
      -- responsible for.  The calculation is done with 64-bit
      -- integers to avoid overflow, but the final segment indexes are
      -- 32 bit.
      offset <- dPrimV "offset" $
                (i32_to_i64 (kernelGroupId constants) *
                 (Imp.var elems_per_thread_64 int64 *
                  i32_to_i64 (kernelGroupSize constants)))
                + (Imp.var i int64 * i32_to_i64 (kernelGroupSize constants))

      j <- dPrimV "j" $ Imp.var offset int64 + i32_to_i64 (kernelLocalThreadId constants)

      -- Construct segment indices.
      let setIndex v e = do dPrim_ v int32
                            v <-- e
      zipWithM_ setIndex space_is $
        map i32_to_i64 . unflattenIndex space_sizes_64 $ Imp.var j int64

      -- We execute the bucket function once and update each histogram serially.
      -- We apply the bucket function if j=offset+ltid is less than
      -- num_elements.  This also involves writing to the mapout
      -- arrays.
      let input_in_bounds = Imp.var j int64 .<. total_w_64
          num_red_res = length ops + sum (map (length . genReduceNeutral) ops)

      sWhen input_in_bounds $ ImpGen.compileStms mempty (stmsToList $ bodyStms body) $ do
        let (red_res, map_res) = splitAt num_red_res $ bodyResult body
            (_red_pes, map_pes) = splitAt num_red_res pes

        sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, se) ->
          ImpGen.copyDWIM (patElemName pe)
          (map ((`Imp.var` int32) . fst) $ kernelDimensions constants) se []

        let (buckets, vs) = splitAt (length ops) red_res
            perOp :: [a] -> [[a]]
            perOp = chunks $ map (length . genReduceDest) ops

        sComment "perform atomic updates" $
          forM_ (zip5 ops do_ops buckets (perOp vs) subhisto_inds) $
          \(GenReduceOp dest_w _ _ _ _, do_op, bucket, vs', subhisto_ind) -> do

            let bucket' = ImpGen.compileSubExpOfType int32 bucket
                dest_w' = ImpGen.compileSubExpOfType int32 dest_w
                bucket_in_bounds = 0 .<=. bucket' .&&. bucket' .<. dest_w'
                bucket_is = map Var (init space_is) ++ [Var subhisto_ind, bucket]

            sWhen bucket_in_bounds $ do_op bucket_is vs'

prepareAtomicUpdate :: Maybe Locking -> GenReduceOp InKernel
                    -> CallKernelGen (Maybe Locking,
                                      [SubExp] -> [SubExp] -> ImpGen.ImpM InKernel Imp.KernelOp ())
prepareAtomicUpdate l op =
  case (l, atomicUpdateLocking $ genReduceOp op) of
    (_, Left f) -> return (l, f $ genReduceDest op)
    (Just l', Right f) -> return (l, f l' $ genReduceDest op)
    (Nothing, Right f) -> do
      -- The number of locks used here is too low, but since we are
      -- currently forced to inline a huge list, I'm keeping it down
      -- for now.  Some quick experiments suggested that it has little
      -- impact anyway (maybe the locking case is just too slow).
      --
      -- A fun solution would also be to use a simple hashing
      -- algorithm to ensure good distribution of locks.
      let num_locks = 1000
      locks <-
        ImpGen.sStaticArray "genred_locks" (Space "device") int32 $
        replicate num_locks $ IntValue $ Int32Value 0
      let l' = Locking locks 0 1 0 (`rem` fromIntegral num_locks)
      return (Just l', f l' $ genReduceDest op)
