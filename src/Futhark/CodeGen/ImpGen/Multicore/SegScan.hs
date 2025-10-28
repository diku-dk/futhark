{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | Multicore code generation for SegScan. Uses a fairly naive multipass
-- algorithm, with no particular locality optimisations.
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
import Futhark.Transform.Rename (renameStm)
import Futhark.Util.IntegralExp (divUp, quot)
import Futhark.Util.Pretty (prettyString)
import Prelude hiding (quot, rem)

-- the final implementation should use this
blockSize :: Imp.TExp Int64
blockSize = 4096

-- 29467μs
-- 11102μs


-- 3422μs
-- 2937μs
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

    -- genBinOpParams [scan_op]

    block_no <- dPrim "nblocks"
    block_no <-- pe64 n `divUp` blockSize
    emit $ Imp.DebugPrint "number of blocks" (Just $ untyped (tvExp block_no))


    -- for now using head and considering only one
    let pt = elemType (head (lambdaReturnType (segBinOpLambda scan_op)))
    let ne = head (segBinOpNeutral scan_op)
    -- exne <- toExp ne

    -- emit $ Imp.DebugPrint "the neutral elements" (Just exne)

    -- allocate flags/aggr/prefix arrays of length nblocks
    flagsArr <- sAllocArray "scan_flags" int8 (Shape [Var (tvVar block_no)]) DefaultSpace
    aggrArr <- sAllocArray "scan_aggr" pt (Shape [Var (tvVar block_no)]) DefaultSpace
    prefArr <- sAllocArray "scan_pref" pt (Shape [Var (tvVar block_no)]) DefaultSpace

    work_index <- sAlloc "work_index" (Imp.bytes 8) Imp.DefaultSpace
    emit $
      Imp.Write
        work_index
        (Imp.elements (0 :: Imp.TExp Int64))
        (IntType Int64)
        Imp.DefaultSpace
        Imp.Nonvolatile
        (untyped (0 :: Imp.TExp Int64))

    -- initialise
    sFor "init" (tvExp block_no) $ \j -> do
      copyDWIMFix flagsArr [j] (intConst Int8 0) []
      copyDWIMFix aggrArr [j] ne []
      copyDWIMFix prefArr [j] ne []

    num_tasks <- dPrimSV "num_tasks" int64
    sOp $ Imp.GetNumTasks $ tvVar num_tasks
    emit $ Imp.DebugPrint "the number of tasks" (Just $ untyped (tvExp num_tasks))

    -- let's create the seg
    fbody <- collect $ do
      let one = (1 :: Imp.TExp Int64)
      let idx0 = Imp.elements (0 :: Imp.TExp Int32)

      -- Maybe we don't need this and can just get it from number of subtask?
      seq_flag <- dPrimV "seq_flag" (true :: Imp.TExp Bool)
      vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
      sOp $ Imp.GetTaskId (tvVar vvv)
      emit $ Imp.DebugPrint "the task id" (Just $ untyped (tvExp vvv))

      block_idx <- dPrim "block_idx" :: MulticoreGen (TV Int64)
      sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar block_idx) work_index idx0 (untyped one)
      
      sWhile (tvExp block_idx .<. tvExp block_no) $ do
        start <- dPrimV "start" (tvExp block_idx * blockSize)
        chunk_length <- dPrimV "chunk_length" (min blockSize (pe64 n - tvExp start))
        emit $ Imp.DebugPrint "the value of start" (Just $ untyped (tvExp start))
        prefix_seq <- dPrimSV "prefix_seq" pt
        flags_loc <- entryArrayLoc <$> lookupArray flagsArr
        let memF = memLocName flags_loc
        let block_idx_32 = TPrimExp $ sExt Int32 (untyped $ tvExp block_idx)
        -- emit $ Imp.DebugPrint "reacehd here" Nothing

        sWhen
          (tvExp seq_flag .==. true)
          ( sIf
              (tvExp block_idx .==. 0)
              ( do
                  forM_ (segBinOpNeutral scan_op) $ \p ->
                    copyDWIMFix (tvVar prefix_seq) [] p []
              )
              ( do
                  prev_flag <- dPrim "prev_flag" :: MulticoreGen (TV Int8)
                  -- not sure if this is a good idea
                  -- prefix_loc <- entryArrayLoc <$> lookupArray prefArr
                  -- let memP = memLocName prefix_loc
                  -- probably need to change this
                  -- simulating a read with add 0?
                  sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int8) (tvVar prev_flag) memF (Imp.elements $ block_idx_32 - 1)
                  -- copyDWIMFix (tvVar prefix) [] (Var prefArr)  [tvExp block_idx - 1]
                  sIf
                    (tvExp prev_flag .==. 2)
                    (copyDWIMFix (tvVar prefix_seq) [] (Var prefArr) [tvExp block_idx - 1])
                    (seq_flag <-- false)
              )
          )
        
        sIf
          (tvExp seq_flag .==. true)
          ( do
              j <- dPrimV "j" (0 :: Imp.TExp Int64)
              genBinOpParams [scan_op]
              forM_ (xParams scan_op) $ \p ->
                copyDWIMFix (paramName p) [] (Var $ tvVar prefix_seq) []

              sWhile (tvExp j .<. tvExp chunk_length) $ do
                dPrimV_ i (tvExp start + tvExp j)

                compileStms mempty kstms $ do
                  forM_ (zip (yParams scan_op) [res]) $ \(p, se) ->
                    copyDWIMFix (paramName p) [] se []

                compileStms mempty (bodyStms $ lamBody scan_op) $ do
                  forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) (xParams scan_op)) $ \(se, px) -> do
                    copyDWIMFix (patElemName pe) [tvExp start + tvExp j] se []
                    copyDWIMFix (paramName px) [] se []

                j <-- tvExp j + 1
              -- TODO: again head now that we need to change
              copyDWIMFix prefArr [tvExp block_idx] (Var $ paramName $ head $ xParams scan_op) []
              -- copyDWIMFix flagsArr [tvExp block_idx] (intConst Int8 2) []
              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int8) memF (Imp.elements block_idx_32) (untyped (2 :: Imp.TExp Int8))

              emit $ Imp.DebugPrint "the value of start after seq scan is done" (Just $ untyped (tvExp start))
              emit $ Imp.DebugPrint "the value of index of block" (Just $ untyped block_idx_32)
              emit $ Imp.DebugPrint "the value of prefix in the seq scan"  (Just $ untyped (tvExp prefix_seq))
              vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
              sOp $ Imp.GetTaskId (tvVar vvv)
              emit $ Imp.DebugPrint "the task id" (Just $ untyped (tvExp vvv))
          )
          ( do
              -- compute the aggregate for this block
              -- a bit dirty hack. we assigne the first element of the block to xparams
              
              
              genBinOpParams [scan_op]
              dPrimV_ i (tvExp start)
              compileStms mempty kstms $ do
                  forM_ (zip (xParams scan_op) [res]) $ \(p, se) ->
                    copyDWIMFix (paramName p) [] se []

              emit $ Imp.DebugPrint "reacehd here" Nothing

              j <- dPrimV "j" (1 :: Imp.TExp Int64)
              sWhile (tvExp j .<. tvExp chunk_length) $ do
                dPrimV_ i (tvExp start + tvExp j)
                compileStms mempty kstms $ do
                  forM_ (zip (yParams scan_op) [res]) $ \(p, se) ->
                    copyDWIMFix (paramName p) [] se []
                compileStms mempty (bodyStms $ lamBody scan_op) $ do
                  forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) (xParams scan_op)) $ \(se, px) -> do
                    copyDWIMFix (paramName px) [] se []
                j <-- tvExp j + 1
              -- write to aggr array
              copyDWIMFix aggrArr [tvExp block_idx] (Var $ paramName $ head $ xParams scan_op) []
              -- write flag as 1
              old_flag <- dPrim "old_flag" :: MulticoreGen (TV Int8)
              old_flag <-- 0
              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int8) memF (Imp.elements block_idx_32) (untyped (1 :: Imp.TExp Int8))

              -- should change this. not Int64 but the type of scan op
              prefix <- dPrimS "prefix" pt
              lb <- dPrimV "lb" (tvExp block_idx - 1)
              
              has_acc <- dPrimV "has_acc" (false :: Imp.TExp Bool)



              sWhile (bNot (tvExp old_flag .==. 2)) $ do
                sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int8) (tvVar old_flag) memF (Imp.elements $ TPrimExp $ sExt Int32 (untyped $ tvExp lb))

                vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
                sOp $ Imp.GetTaskId (tvVar vvv)
                emit $ Imp.DebugPrint "the task id in the loop" (Just $ untyped (tvExp vvv))


                sWhen
                  (tvExp old_flag .==. 2)
                  ( do
                      -- emit $ Imp.DebugPrint "the lb value" (Just $ untyped (tvExp lb))
                      -- emit $ Imp.DebugPrint "the flag value" (Just $ untyped (tvExp old_flag))
                      emit $ Imp.DebugPrint "the block that is ready for use" (Just $ untyped (tvExp lb))
                      emit $ Imp.DebugPrint "the task id that got what it needed" (Just $ untyped (tvExp vvv))
                      emit $ Imp.DebugPrint "the block id that got what needed" (Just $ untyped (tvExp block_idx))

                      genBinOpParams [scan_op]

                      sIf 
                        (tvExp has_acc .==. false)
                        (copyDWIMFix prefix [] (Var prefArr) [tvExp lb])
                        ( do
                        forM_ (zip (xParams scan_op) (yParams scan_op)) $ \(xs, ys) -> do
                          copyDWIMFix (paramName xs) [] (Var prefArr) [tvExp lb]
                          copyDWIMFix (paramName ys) [] (Var prefix) []
                        
                        
                        compileStms mempty (bodyStms $ lamBody scan_op) $ do
                          forM_ (map resSubExp $ bodyResult $ lamBody scan_op) $ \ op_res -> do
                            copyDWIMFix prefix [] op_res []
                        )
                      -- prefix_lb <- dPrim "prefix_lb" :: MulticoreGen (TV Int32)
                      -- copyDWIMFix (tvVar prefix_lb) [] (Var prefArr) [tvExp lb]
                      -- prefix <-- tvExp prefix + tvExp prefix_lb
                  )
                sWhen
                  (tvExp old_flag .==. 1)
                  ( do
                      genBinOpParams [scan_op]
                      sIf 
                        (tvExp has_acc .==. false)
                        ( copyDWIMFix prefix [] (Var aggrArr) [tvExp lb])
                        ( do
                        forM_ (zip (xParams scan_op) (yParams scan_op)) $ \(xs, ys) -> do
                          copyDWIMFix (paramName xs) [] (Var aggrArr) [tvExp lb]
                          copyDWIMFix (paramName ys) [] (Var prefix) []
  
                        
                        compileStms mempty (bodyStms $ lamBody scan_op) $ do
                          forM_ (map resSubExp $ bodyResult $ lamBody scan_op) $ \ op_res -> do
                            copyDWIMFix prefix [] op_res []
                        )
                      
                      
                      -- agg_lb <- dPrim "agg_lb" :: MulticoreGen (TV Int32)
                      -- copyDWIMFix (tvVar agg_lb) [] (Var aggrArr) [tvExp lb]
                      -- prefix <-- tvExp prefix + tvExp agg_lb
                      
                      has_acc <-- true
                      lb <-- tvExp lb - 1
                      emit $ Imp.DebugPrint "Are we coming here!!!!!!" (Just $ untyped (tvExp block_idx))
                  )



              scan_ops1 <- renameSegBinOp [scan_op]
              genBinOpParams scan_ops1
              let scan_op1 = head scan_ops1
              forM_ (zip (xParams scan_op1) (yParams scan_op1)) $ \(xs, ys) -> do
                          copyDWIMFix (paramName xs) [] (Var aggrArr) [tvExp block_idx]
                          copyDWIMFix (paramName ys) [] (Var prefix) []
              compileStms mempty (bodyStms $ lamBody scan_op1) $ do
                  forM_ (map resSubExp $ bodyResult $ lamBody scan_op1) $ \ op_res -> do
                    copyDWIMFix prefArr [tvExp block_idx] op_res []
                      
              
              emit $ Imp.DebugPrint "the start value that is ready" (Just $ untyped (tvExp start))

              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int8) memF (Imp.elements block_idx_32) (untyped (2 :: Imp.TExp Int8))


              scan_ops2 <- renameSegBinOp [scan_op]
              genBinOpParams scan_ops2
              let scan_op2 = head scan_ops2
              -- Todo: need to change this
              forM_ (xParams scan_op2) $ \p ->
                copyDWIMFix (paramName p) [] (Var prefix) []

              z <- dPrimV "z" (0 :: Imp.TExp Int64)
              sWhile (tvExp z .<. tvExp chunk_length) $ do
                dPrimV_ i (tvExp start + tvExp z)
                compileStms mempty kstms $ do
                  forM_ (zip (yParams scan_op2) [res]) $ \(p, se) ->
                    copyDWIMFix (paramName p) [] se []
                compileStms mempty (bodyStms $ lamBody scan_op2) $ do
                  forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op2) (xParams scan_op2)) $ \(se, px) -> do
                    copyDWIMFix (patElemName pe) [tvExp start + tvExp z] se []
                    copyDWIMFix (paramName px) [] se []
                z <-- tvExp z + 1
          )

        sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar block_idx) work_index idx0 (untyped one)
      vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
      sOp $ Imp.GetTaskId (tvVar vvv)
      emit $ Imp.DebugPrint "the tak id done working low key" (Just $ untyped (tvExp vvv))


    free_params <- freeParams fbody
    emit $ Imp.Op $ Imp.ParLoop "segmap" fbody free_params
