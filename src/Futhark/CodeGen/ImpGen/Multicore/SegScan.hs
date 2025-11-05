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

blockSize :: Imp.TExp Int64
blockSize = 16384 


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

    -- genBinOpParams [scan_op]

    block_no <- dPrim "nblocks"
    block_no <-- pe64 n `divUp` blockSize


    -- for now using head and considering only one
    let pt = elemType (head (lambdaReturnType (segBinOpLambda scan_op)))
    let ne = head (segBinOpNeutral scan_op)
    -- exne <- toExp ne


    -- allocate flags/aggr/prefix arrays of length nblocks
    flagsArr <- sAllocArray "scan_flags" int64 (Shape [Var (tvVar block_no)]) DefaultSpace
    aggrArr <- sAllocArray "scan_aggr" pt (Shape [Var (tvVar block_no)]) DefaultSpace
    prefArr <- sAllocArray "scan_pref" pt (Shape [Var (tvVar block_no)]) DefaultSpace

    work_index <- sAllocArray "work_index" int64 (Shape [intConst Int64 1]) DefaultSpace

    -- work_index <- sAlloc "work_index" (Imp.bytes 8) Imp.DefaultSpace
    -- emit $
    --   Imp.Write
    --     work_index
    --     (Imp.elements (0 :: Imp.TExp Int64))
    --     (IntType Int64)
    --     Imp.DefaultSpace
    --     Imp.Nonvolatile
    --     (untyped (0 :: Imp.TExp Int64))

    -- initialise
    sFor "init" (tvExp block_no) $ \j -> do
      copyDWIMFix flagsArr [j] (intConst Int64 0) []
      copyDWIMFix aggrArr [j] ne []
      copyDWIMFix prefArr [j] ne []
    
    copyDWIMFix work_index [0] (intConst Int64 0) []

     -- launch parallel tasks

    num_tasks <- dPrimSV "num_tasks" int64
    sOp $ Imp.GetNumTasks $ tvVar num_tasks
    
    fbody <- collect $ do
      let one = (1 :: Imp.TExp Int64)
      let idx0 = Imp.elements (0 :: Imp.TExp Int32)

      seq_flag <- dPrimV "seq_flag" (true :: Imp.TExp Bool)
      
      vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
      sOp $ Imp.GetTaskId (tvVar vvv)

      block_idx <- dPrim "block_idx" :: MulticoreGen (TV Int64)
      work_index_loc <- entryArrayLoc <$> lookupArray work_index
      let workF = memLocName work_index_loc
      
      sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar block_idx) workF idx0 (untyped one)
      
      sWhile (tvExp block_idx .<. tvExp block_no) $ do
        start <- dPrimV "start" (tvExp block_idx * blockSize)
        
        diff_start <- dPrimV "wtf" (pe64 n - tvExp start)

        chunk_length <- dPrim "chunk_length" 
        sIf (tvExp diff_start .<. blockSize)
          (chunk_length <-- tvExp diff_start)
          (chunk_length <-- blockSize)

        
        
        prefix_seq <- dPrimSV "prefix_seq" pt
        flags_loc <- entryArrayLoc <$> lookupArray flagsArr
        let memF = memLocName flags_loc


        
        let block_idx_32 = TPrimExp $ sExt Int32 (untyped $ tvExp block_idx)

        sWhen
          (tvExp seq_flag .==. true)
          ( sIf
              (tvExp block_idx .==. 0)
              ( do
                  forM_ (segBinOpNeutral scan_op) $ \p ->
                    copyDWIMFix (tvVar prefix_seq) [] p []
              )
              ( do
                  prev_flag <- dPrim "prev_flag" :: MulticoreGen (TV Int64)
                  -- not sure if this is a good idea
                  -- prefix_loc <- entryArrayLoc <$> lookupArray prefArr
                  -- let memP = memLocName prefix_loc
                  -- probably need to change this
                  -- simulating a read with add 0?
                  sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int64) (tvVar prev_flag) memF (Imp.elements $ block_idx_32 - 1)
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
              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (2 :: Imp.TExp Int64))
          )
          ( do
              -- compute the aggregate for this block
              -- a bit dirty hack. we assigne the first element of the block to xparams
              
              
              genBinOpParams [scan_op]
              dPrimV_ i (tvExp start)
              compileStms mempty kstms $ do
                  forM_ (zip (xParams scan_op) [res]) $ \(p, se) ->
                    copyDWIMFix (paramName p) [] se []


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
              old_flag <- dPrim "old_flag" :: MulticoreGen (TV Int64)
              old_flag <-- 0
              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (1 :: Imp.TExp Int64))

              -- should change this. not Int64 but the type of scan op
              prefix <- dPrimS "prefix" pt
              lb <- dPrimV "lb" (tvExp block_idx - 1)
              
              has_acc <- dPrimV "has_acc" (false :: Imp.TExp Bool)



              sWhile (bNot (tvExp old_flag .==. 2)) $ do
                sOp $ Imp.Atomic $ Imp.AtomicLoad (IntType Int64) (tvVar old_flag) memF (Imp.elements $ TPrimExp $ sExt Int32 (untyped $ tvExp lb))

                vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
                sOp $ Imp.GetTaskId (tvVar vvv)


                sWhen
                  (tvExp old_flag .==. 2)
                  ( do

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
                      
              

              sOp $ Imp.Atomic $ Imp.AtomicStore (IntType Int64) memF (Imp.elements block_idx_32) (untyped (2 :: Imp.TExp Int64))


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

        sOp $ Imp.Atomic $ Imp.AtomicAdd Int64 (tvVar block_idx) workF idx0 (untyped one)
      vvv <- dPrim "vvv" :: MulticoreGen (TV Int64)
      sOp $ Imp.GetTaskId (tvVar vvv)


    free_params <- freeParams fbody
    emit $ Imp.Op $ Imp.ParLoop "segmap" fbody free_params


