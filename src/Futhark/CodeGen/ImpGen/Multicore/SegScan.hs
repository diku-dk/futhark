-- | Multicore code generation for SegScan. Uses a fairly naive multipass
-- algorithm, with no particular locality optimisations.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Futhark.CodeGen.ImpGen.Multicore.SegScan
  ( compileSegScan,
  )
where

import Control.Monad
import Data.List (zip4)
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Util.IntegralExp (quot, divUp)
import Prelude hiding (quot, rem)
import Futhark.Util.Pretty (prettyString)



-- the final implementation should use this
blockSize64 :: Imp.TExp Int64
blockSize64 = 2



xParams, yParams :: SegBinOp MCMem -> [LParam MCMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

lamBody :: SegBinOp MCMem -> Body MCMem
lamBody = lambdaBody . segBinOpLambda

genBinOpParams :: [SegBinOp MCMem] -> MulticoreGen ()
genBinOpParams scan_ops =
  dScope Nothing $
    scopeOfLParams $
      concatMap (lambdaParams . segBinOpLambda) scan_ops

-- | Compile a SegScan construct.
compileSegScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen ()
compileSegScan pat space reds kbody nsubtasks
  | [_] <- unSegSpace space =
      nonsegmentedScan pat space reds kbody nsubtasks
  | otherwise =
      error "only nonsegmented scans for now"

nonsegmentedScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen ()
nonsegmentedScan
  (Pat [pe])
  (SegSpace _ [(i, n)])
  [scan_op]
  (Body _ kstms [Returns _ _ res])
  _nsubtasks = do
    emit $ Imp.DebugPrint "nonsegmented segScan" Nothing

    genBinOpParams [scan_op]


    nblocks <- dPrim "nblocks"
    nblocks <-- ((pe64 n + blockSize64 - 1) `quot` blockSize64)

    -- for now using head and considering only one
    let pt  = elemType (head (lambdaReturnType (segBinOpLambda scan_op)))
    let ne = head (segBinOpNeutral scan_op)

    -- allocate flags/aggr/prefix arrays of length nblocks
    let nblocks_se = Var (tvVar nblocks)
    flagsArr <- sAllocArray "scan_flags" int8 (Shape [nblocks_se]) DefaultSpace
    aggrArr  <- sAllocArray "scan_aggr"  pt    (Shape [nblocks_se]) DefaultSpace
    prefArr  <- sAllocArray "scan_pref"  pt    (Shape [nblocks_se]) DefaultSpace

    acc_mem <- sAlloc "acc_mem" (Imp.bytes 8) Imp.DefaultSpace
    emit $ Imp.Write acc_mem
                    (Imp.elements (0 :: Imp.TExp Int64)) 
                    (IntType Int64)
                    Imp.DefaultSpace
                    Imp.Nonvolatile
                    (untyped (0 :: Imp.TExp Int64))


    -- initialise
    sFor "init" (tvExp nblocks) $ \j -> do
      copyDWIMFix flagsArr [j] (intConst Int8 0) []
      copyDWIMFix aggrArr  [j] ne []
      copyDWIMFix prefArr  [j] ne []

    num_tasks <- dPrimSV "num_tasks" int64
    sOp $ Imp.GetNumTasks $ tvVar num_tasks
    blockSize643 <- dPrim "blockSize64"
    blockSize643 <-- (pe64 n  `divUp` tvExp num_tasks)


    -- let's create the seg
    fbody <- collect $ do
      blockID <- dPrim "blockID" :: MulticoreGen (TV Int64)
      sOp $ Imp.GetTaskId (tvVar blockID)
      emit $ Imp.DebugPrint "the number of blocks" (Just $ untyped (tvExp num_tasks))
      chunk_no <- dPrimV "chunk_no" (tvExp blockID * tvExp blockSize643)
      chunk_length <- dPrimV "chunk_length" (min (tvExp blockSize643) (pe64 n - tvExp chunk_no))

      sFor "j" (tvExp chunk_length) $ \j -> do
        let one = (1 :: Imp.TExp Int64)
        oldVal <- dPrim "oldVal" :: MulticoreGen (TV Int64)
        -- acc <- dPrim "acc" :: MulticoreGen (TV Int64)
        -- acc <-- (0 :: Imp.TExp Int64)
        -- accMem <- sAlloc "acc" (Imp.bytes 8) Imp.DefaultSpace

        let idx0 = Imp.elements (0 :: Imp.TExp Int32)

        -- sOp $ Imp.Atomic (Imp.AtomicAdd Int64 (tvVar oldVal) accLili (Imp.elements (1 :: Imp.TExp Int32)) (untyped one) )
        -- print the value of accmem
        -- emit $ Imp.DebugPrint "entering the loop" Nothing
        sWhen (tvExp chunk_no + j .<. pe64 n) $ do
          sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar oldVal) acc_mem idx0 (untyped one)
          emit $ Imp.DebugPrint "the value of accmem" (Just $ untyped (tvExp oldVal))


          i_val <- dPrimV "i" (tvExp chunk_no + j)
          dPrimV_ i (tvExp i_val)
          emit $ Imp.DebugPrint "the value of loop index" (Just $ untyped (tvExp i_val))
          compileStms mempty kstms $ do
            forM_ [res] $ \se -> copyDWIMFix (patElemName pe) [tvExp i_val] se []

      -- emit $ Imp.DebugPrint "Inside block loop" (Just $ untyped (tvExp blockID))
      -- emit $ Imp.DebugPrint "HEllo World" (Just $ untyped (tvExp nblocks))
      -- emit $ Imp.DebugPrint "GetNumTasks =" (Just $ untyped (tvExp nt))

    free_params <- freeParams fbody
    emit $ Imp.Op $ Imp.ParLoop "segmap" fbody free_params



    -- forM_ (zip (xParams scan_op) (segBinOpNeutral scan_op)) $ \(p, ne) ->
    --   copyDWIMFix (paramName p) [] ne []

    -- emit $ Imp.DebugPrint ("KERNEL BODY:\n" ++ prettyString kstms) Nothing
    -- emit $ Imp.DebugPrint ("KERNEL Res:\n" ++ prettyString res) Nothing
    -- emit $ Imp.DebugPrint ("Lambda BODY:\n" ++ prettyString (bodyStms $ lamBody scan_op)) Nothing

    -- sFor "j" (pe64 n) $ \j -> do
    --   dPrimV_ i j
    --   compileStms mempty kstms $ do
    --     forM_ (zip (yParams scan_op) [res]) $ \(p, se) ->
    --       copyDWIMFix (paramName p) [] se []

    --   compileStms mempty (bodyStms $ lamBody scan_op) $ do
    --     forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) (xParams scan_op))  $ \(se,px) -> do 
    --       copyDWIMFix (patElemName pe) [j] se []
    --       copyDWIMFix (paramName px) [] se [] 





      -- fbody <- collect $ do
      --   -- Get the current block ID for this parallel task
      --   bid <- dPrim "block_id" :: MulticoreGen (TV Int64)
      --   sOp $ Imp.GetTaskId (tvVar bid)

      --   emit $ Imp.DebugPrint "Inside block loop" (Just $ untyped (tvExp bid))

      --   -- Sequential inner loop over elements in this block
      --   sFor "j" blockSize64 $ \j -> do
      --     sComment "process element j of this block" $
      --       emit $ Imp.DebugPrint "  element" (Just $ untyped j)

      -- free_params <- freeParams fbody


      -- emit $ Imp.Op $ Imp.ParLoop "segmap" fbody free_params



