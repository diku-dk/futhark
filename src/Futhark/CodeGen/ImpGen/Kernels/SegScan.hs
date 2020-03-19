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
-- import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
-- import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)

-- one pass scan will be implemented here
compileSegScan :: Pattern ExplicitMemory
               -> SegLevel      --At which level the *body* of a SegOp executes.
               -> SegSpace                            -- Index space of a SegOp.
               -> Lambda ExplicitMemory
               -> [SubExp]
               -> KernelBody ExplicitMemory
               -> CallKernelGen ()
compileSegScan  (Pattern _ pes)
                lvl
                space
                scan_op
                nes
                kbody = do
    let (gtids, dims) = unzip $ unSegSpace space
    arraysize <- toExp $ head dims
    let gtid = head gtids

    let num_groups = segNumGroups lvl
    num_groups' <- traverse toExp $ segNumGroups lvl
    let group_size = segGroupSize lvl
    group_size' <- traverse toExp $ segGroupSize lvl



    let (hxp, _hyp) = splitAt (length nes) $ lambdaParams scan_op

    aggregates <- forM hxp $ \p -> do
            let pt = elemType $ paramType p
            sAllocArray "aggregates" pt (Shape $ [unCount num_groups]) $ Space "device"

    incprefix <- forM hxp $ \p -> do
            let pt = elemType $ paramType p
            sAllocArray "incprefix" pt (Shape $ [unCount num_groups]) $ Space "device"


    statusflgs <- sAllocArray "statusflgs" int8 (Shape [unCount num_groups]) (Space "device")

    g_dyn_id <- sAllocArray "dyn_id" int32 (Shape [intConst Int32 1]) (Space "device")
    copyDWIMFix g_dyn_id [0] (intConst Int32 0) []
    (global_id, _, _) <- fullyIndexArray g_dyn_id [0]
    -- dyn_id <- dPrimV "dyn_id" 0
  -- sKernelThread :: String
                -- -> Count NumGroups Imp.Exp -> Count GroupSize Imp.Exp
                -- -> VName
                -- -> (KernelConstants -> InKernelGen ())
                -- -> CallKernelGen ()
      -- incprefix <- sAllocArray "incprefix" (Prim (FloatType Float32)) (Shape [num_groups]) (Space "device")

    sKernelThread "my_scan" num_groups' group_size' (segFlat space) $ \constants -> do

      let ltid = kernelLocalThreadId constants
      let waveSize = kernelWaveSize constants

      block_id <- sAllocArray "block_id" int32 (Shape [intConst Int32 1]) (Space "local")

      sWhen (ltid .==. 0) $ do
        copyDWIMFix statusflgs [0] (intConst Int8 0) []
        reg_dyn_id <- dPrimV "reg_dyn_id" 0
        sOp $ Imp.Atomic DefaultSpace $ Imp.AtomicAdd reg_dyn_id global_id 0 1
        copyDWIMFix block_id [0] (Var reg_dyn_id) []

      sOp Imp.LocalBarrier
      wG_ID <- dPrim "wG_ID" int32
      copyDWIMFix wG_ID [] (Var block_id) [0]


    -- This is what I would like to use: type from input array
      -- let (input_elm, []) = splitAt (length nes) $ kernelBodyResult kbody
      -- let (Count gsz) = group_size
      -- arr_ts <- mapM lookupType input_elm
      -- loc_arr <- forM input_elm $ \p -> do
      --       let pt = subExpType $ kernelResultSubExp p
      --           shape = Shape $ [gsz]
      --       sAllocArray "my_array" pt shape $ Space "local"

      let (xp, _yp) = splitAt (length nes) $ lambdaParams scan_op
      let (Count gsz) = group_size
      exchange <- forM xp $ \p -> do
            let pt = elemType $ paramType p
                shape = Shape $ [gsz]
            sAllocArray "exchange" pt shape $ Space "local"



      let copyNpad = do
            compileStms mempty (kernelBodyStms kbody) $ do
              let (input_elm, []) = splitAt (length nes) $ kernelBodyResult kbody

              forM_ (zip exchange input_elm) $ \(arr, input) ->
                copyDWIMFix arr [ltid] (kernelResultSubExp input) []

      let padding = do
            forM_ (zip exchange nes) $ \(arr, neutral) ->
              copyDWIMFix arr [ltid] neutral []

--       -- let global_tid = Imp.vi32 group_id * unCount group_size' + kernelLocalThreadId constants

      -- sIf (kernelGlobalThreadId constants .<. arraysize)
      let dgtid = Imp.vi32 wG_ID * unCount group_size' + ltid

      dPrimV_ gtid dgtid

      sIf (dgtid.<. arraysize)
          copyNpad
          padding

      ress <- forM nes $ \ne -> do
                ne' <- toExp ne
                dPrimV "res" ne'

      -- p = 1
      p1 <- dPrim "p1" int32
      p1 <-- 1
      let p1Var = Imp.var p1 int32

      -- while p < array.size
      sWhile (p1Var .<. arraysize) $ do

        -- if tid >= p then res = op array[tid-p] array[tid]
        sWhen (ltid .>=. p1Var) $ do

          dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
          let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op

          forM_ (zip scan_x_params exchange) $ \(param, arr) ->
            copyDWIMFix (paramName param) [] (Var arr) [ltid]

          forM_ (zip scan_y_params exchange) $ \(param, arr) ->
            copyDWIMFix (paramName param) [] (Var arr) [ltid - p1Var]

          compileStms mempty (bodyStms $ lambdaBody scan_op) $ do
            forM_ (zip ress (bodyResult $ lambdaBody scan_op)) $ \(res, sr) ->
              copyDWIMFix res [] sr []

        sOp Imp.LocalBarrier

        -- if tid >= p then array[tid] = res
        sWhen (ltid .>=. p1Var) $ do
          forM_ (zip exchange ress) $ \(arr, res) ->
            copyDWIMFix arr [ltid] (Var res) []
        sOp Imp.LocalBarrier

        p1 <-- p1Var * 2


      -- get in the chunk
      chunk <- forM xp $ \para -> do
            let pt = elemType $ paramType para
            dPrim "chunk" pt
      forM_ (zip chunk exchange) $ \(ch, ex) ->
            copyDWIMFix ch [] (Var ex) [ltid]


      -- int32_t prev_ind = (tid == 0) ? (get_local_size(0) - 1) : (tid - 1);
      let my_group_size = kernelGroupSize constants
      prev_ind <- dPrim "prev_ind" int32
      sIf (ltid .==. 0) (prev_ind <-- my_group_size - 1) (prev_ind <-- ltid - 1)

      acc <- forM xp $ \para -> do
            let pt = elemType $ paramType para
            dPrim "acc" pt

      forM_ (zip acc exchange) $ \(a, e) ->
        copyDWIMFix a [] (Var e) [Imp.var prev_ind int32]

      prefix <- forM nes $ \ne -> do
          ne' <- toExp ne
          dPrimV "prefix" ne'

      -- first group, first warp, first lane
      sWhen (Imp.var wG_ID int32 .==. 0 .&&. ltid .==. 0) $ do
        -- scanwarp <- sAllocArray "scanwarp" int8 (Shape [unCount num_groups]) (Space "local")
        forM_ (zip incprefix acc) $ \(i, a) ->
          copyDWIMFix i [0] (Var a) []
        sOp Imp.MemFenceGlobal
        copyDWIMFix statusflgs [0] (intConst Int8 2) []
        forM_ (zip acc nes) $ \(a, neutral) ->
          copyDWIMFix a [] neutral []

      -- WG_ID != 0, first warp, all lanes
      -- sWhen (Imp.var wG_ID int32 ./=. 0 .&&. ltid .<. kernelWaveSize) $ do
      sUnless (Imp.var wG_ID int32 .==. 0 .||. ltid .>=. waveSize) $ do

        -- This local allocation does not reuse the memory.
        --   can be investigated at the optimization phase.
        --   Note: the Shape should have the size of the wave size not just the hard coded 32.
        warpscan <- sAllocArray "warpscan" int8 (Shape [intConst Int32 32]) (Space "local")
        sWhen (ltid .==. 0) $ do
          forM_ (zip aggregates acc) $ \(ag, ac) ->
            copyDWIMFix ag [Imp.var wG_ID int32] (Var ac) []
          sOp Imp.MemFenceGlobal
          copyDWIMFix statusflgs [Imp.var wG_ID int32] (intConst Int8 1) []
          copyDWIMFix warpscan [0] (Var statusflgs) [Imp.var wG_ID int32 - 1]

        sOp Imp.MemFenceGlobal
        -- if (warpscan[0] == STATUS_P && tid == 0) prefix = incprefix[WG_ID-1];
        status1 <- dPrim "status1" int8
        copyDWIMFix status1 [] (Var warpscan) [0]
        sIf (Imp.var status1 int8 .==. 2)
          -- if
          (sWhen (ltid .==. 0) (forM_ (zip prefix incprefix) $ \(pre,inc) ->
            copyDWIMFix pre [] (Var inc) [Imp.var wG_ID int32 - 1])) $ do

          -- else
          read_offset <- dPrim "read_offset" int32
          read_offset <-- Imp.var wG_ID int32 - waveSize
          loop_stop <- dPrim "loop_stop" int32
          loop_stop <-- -1 * waveSize

          -- while (read_offset > LOOP_STOP) {
          sWhile (Imp.var read_offset int32 .>. Imp.var loop_stop int32) $ do
            readi <- dPrim "readi" int32
            readi <-- Imp.var read_offset int32 + ltid
            let readiExp = Imp.var readi int32
            aggr <- forM nes $ \ne -> do
              ne' <- toExp ne
              dPrimV "aggr" ne'
            flag <- dPrim "flag" int8
            flag <-- 0
            let flagExp = Imp.var flag int32
            used <- dPrim "used" int8
            used <-- 0
            sWhen (readiExp .>=. 0) $ do
              copyDWIMFix flag [] (Var statusflgs) [readiExp]
              sWhen (flagExp .==. 2) $ do -- STATUS_P
                forM_ (zip aggr incprefix) $ \(ag,inc) ->
                  copyDWIMFix ag [] (Var inc) [readiExp]
              sWhen (flagExp .==. 1) $ do -- STATUS_A
                forM_ (zip aggr aggregates) $ \(ag,ags) ->
                  copyDWIMFix ag [] (Var ags) [readiExp]
                used <-- 1
            -- exchange[tid]       = aggr;
            forM_ (zip exchange aggr) $ \(ex,ag) ->
              copyDWIMFix ex [ltid] (Var ag) []
            -- warpscan[tid]       = mkStatusUsed(used, flag);
            combined_flg <- dPrim "combined_flg" int8
            -- TODO: The msb is signed and have to be mult with -1 or bit shiftet or something
            -- combined_flg <-- (Imp.var used int8 * 4) .|. Imp.var flag int8
            combined_flg <-- Imp.BinOpExp (Shl Int8) (Imp.var used int8) 2 .|. Imp.var flag int8
            copyDWIMFix warpscan [ltid] (Var combined_flg) []
            sOp Imp.MemFenceGlobal


            -- Perform reduce
            wsmone <- dPrim "wsmone" int8
            -- wsmone <-- 0
            copyDWIMFix wsmone [] (Var warpscan) [waveSize-1]
            sUnless (Imp.var wsmone int32 .==. 2) $ do -- STATUS_P

              -- #pragma unroll -- TODO unroll
              -- for(uint32_t i=0; i<lgWARP; i++) {
              --     const uint32_t p = (1<<i);
              --     if( lane >= p ) binOpInLocMem( sh_data, sh_status, lane-p, lane );}

              -- p = 1
              p <- dPrim "p" int32
              p <-- 1
              let pVar = Imp.var p int32

              -- while p < array.size
              sWhile (pVar .<. waveSize) $ do

                -- if tid >= p then res = op array[tid-p] array[tid]
                sWhen (ltid .>=. pVar) $ do
                  let acc_th = ltid - pVar
                  let cur_th = ltid
                  agg1 <- forM nes $ \ne -> do
                    ne' <- toExp ne
                    dPrimV "agg1" ne'
                  forM_ (zip agg1 exchange) $ \(ag, ex) ->
                    copyDWIMFix ag [] (Var ex) [acc_th]

                  agg2 <- forM nes $ \ne -> do
                    ne' <- toExp ne
                    dPrimV "agg2" ne'
                  forM_ (zip agg2 exchange) $ \(ag, ex) ->
                    copyDWIMFix ag [] (Var ex) [cur_th]

                  usd1 <- dPrim "usd1" int8
                  usd2 <- dPrim "usd2" int8
                  stat1 <- dPrim "stat1" int8
                  stat2 <- dPrim "stat2" int8
                  tmp <- dPrim "tmp" int8
                  let tmpVar = Imp.var tmp int32
                  copyDWIMFix tmp [] (Var warpscan) [acc_th]

                  stat1 <-- tmpVar .&. 3
                  usd1 <-- Imp.BinOpExp (LShr Int8) tmpVar 2

                  copyDWIMFix tmp [] (Var warpscan) [cur_th]
                  stat2 <-- tmpVar .&. 3
                  usd2 <-- Imp.BinOpExp (LShr Int8) tmpVar 2

                  sUnless (Imp.var stat2 int32 .==. 1) $ do
                    forM_ (zip agg1 nes) $ \(ag, ne) -> do
                      ne' <- toExp ne
                      ag <-- ne'
                    usd1 <-- 0
                    stat1 <-- Imp.var stat2 int8
                  usd1 <-- Imp.var usd1 int8 + Imp.var usd2 int8
                  usd1 <-- Imp.BinOpExp (Shl Int8) (Imp.var usd1 int8) 2
                  usd1 <-- Imp.var usd1 int8 .|. Imp.var stat1 int8
                  copyDWIMFix warpscan [cur_th] (Var usd1) []

                  dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
                  let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op

                  forM_ (zip scan_x_params agg1) $ \(param, ag) ->
                    copyDWIMFix (paramName param) [] (Var ag) []

                  forM_ (zip scan_y_params agg2) $ \(param, ag) ->
                    copyDWIMFix (paramName param) [] (Var ag) []

                  compileStms mempty (bodyStms $ lambdaBody scan_op) $ do
                    forM_ (zip exchange (bodyResult $ lambdaBody scan_op)) $ \(ex, sr) ->
                      copyDWIMFix ex [cur_th] sr []

                  -- forM_ (zip exchange ress) $ \(ex, res) ->
                  --   copyDWIMFix ex [acc_th] (Var res) []

                p <-- pVar * 2

            sOp Imp.MemFenceGlobal
            sWhen (ltid .==.0) $ do
              -- read result from local data after warp reduce
              usedflg_val <- dPrim "usedflg_val" int8
              copyDWIMFix usedflg_val [] (Var warpscan) [waveSize-1]
              flag <-- Imp.var usedflg_val int8 .&. 3 -- get status
              sIf (flagExp .==. 2)
                (read_offset <-- Imp.var loop_stop int32) $ do-- EXIT
                used <-- Imp.BinOpExp (LShr Int8) (Imp.var usedflg_val int8) 2 -- get used: usd_flg >> 2
                read_offset <-- Imp.var read_offset int32 - Imp.var used int32
              copyDWIMFix block_id [0] (Var read_offset) []
              -- update prefix
              forM_ (zip aggr exchange) $ \(ag,ex) ->
                copyDWIMFix ag [] (Var ex) [waveSize-1]

              dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
              let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op

              forM_ (zip scan_x_params aggr) $ \(param, agg) ->
                copyDWIMFix (paramName param) [] (Var agg) []

              forM_ (zip scan_y_params prefix) $ \(param, pre) ->
                copyDWIMFix (paramName param) [] (Var pre) []

              compileStms mempty (bodyStms $ lambdaBody scan_op) $ do
                forM_ (zip prefix (bodyResult $ lambdaBody scan_op)) $ \(pre, sr) ->
                  copyDWIMFix pre [] sr []
            sOp Imp.MemFenceGlobal
            copyDWIMFix read_offset [] (Var block_id) [0]

        sWhen (ltid .==. 0) $ do
          dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
          let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op

          forM_ (zip scan_x_params prefix) $ \(param, pre) ->
            copyDWIMFix (paramName param) [] (Var pre) []

          forM_ (zip scan_y_params acc) $ \(param, ac) ->
            copyDWIMFix (paramName param) [] (Var ac) []

          compileStms mempty (bodyStms $ lambdaBody scan_op) $ do
            forM_ (zip incprefix (bodyResult $ lambdaBody scan_op)) $ \(ipre, sr) ->
              copyDWIMFix ipre [Imp.var wG_ID int32] sr []

          sOp Imp.MemFenceGlobal

          copyDWIMFix statusflgs [Imp.var wG_ID int32] (intConst Int8 2) [] -- STATUS_P
          forM_ (zip exchange prefix) $ \(exc, pre) ->
            copyDWIMFix exc [0] (Var pre) []
          forM_ (zip acc nes) $ \(a, ne) ->
            copyDWIMFix a [] ne []

      sUnless (Imp.var wG_ID int32 .==. 0) $ do
        sOp Imp.LocalBarrier
        forM_ (zip prefix exchange) $ \(pre,exc) ->
          copyDWIMFix pre [] (Var exc) [0]
        sOp Imp.LocalBarrier


      -- myacc <- forM nes $ \ne -> do
      --           ne' <- toExp ne
      --           dPrimV "myacc" ne'

      -- dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
      -- let (scan_x_params1, scan_y_params1) = splitAt (length nes) $ lambdaParams scan_op
      -- forM_ (zip scan_x_params1 prefix) $ \(param, pre) ->
      --   copyDWIMFix (paramName param) [] (Var pre) []
      -- forM_ (zip scan_y_params1 acc) $ \(param, ac) ->
      --   copyDWIMFix (paramName param) [] (Var ac) []
      -- compileStms mempty (bodyStms $ lambdaBody scan_op) $ do
      --   forM_ (zip myacc (bodyResult $ lambdaBody scan_op)) $ \(ma, sr) ->
      --     copyDWIMFix ma [] sr []

      scan_op_renamed <- renameLambda scan_op
      dScope Nothing $ scopeOfLParams $ lambdaParams scan_op_renamed
      let (scan_x_params2, scan_y_params2) = splitAt (length nes) $ lambdaParams scan_op_renamed
      forM_ (zip scan_x_params2 prefix) $ \(param, ma) ->
        copyDWIMFix (paramName param) [] (Var ma) []
      forM_ (zip scan_y_params2 chunk) $ \(param, ch) ->
        copyDWIMFix (paramName param) [] (Var ch) []
      compileStms mempty (bodyStms $ lambdaBody scan_op_renamed) $ do
        forM_ (zip exchange (bodyResult $ lambdaBody scan_op_renamed)) $ \(e, sr) ->
          copyDWIMFix e [ltid] sr []


      sWhen (dgtid .<. arraysize) $ do
        forM_ (zip pes exchange) $ \(dest, arr) ->
          copyDWIMFix (patElemName dest) [dgtid] (Var arr) [ltid]






      --

      return ()
