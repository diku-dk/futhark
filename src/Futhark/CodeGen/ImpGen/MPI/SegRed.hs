module Futhark.CodeGen.ImpGen.MPI.SegRed
  ( compileSegRed,
    compileSegRed',
  )
where

import Control.Monad
import qualified Futhark.CodeGen.ImpCode.MPI as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.MPI.Base
import Futhark.IR.MCMem
import Prelude hiding (quot, rem)
import Debug.Trace

type DoSegBody = (([(SubExp, [Imp.TExp Int64])] -> MPIGen ()) -> MPIGen ())

-- | Generate code for a SegRed construct
compileSegRed ::
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MPIGen Imp.Code
compileSegRed pat space reds kbody
  | [(gtid, _w)] <- unSegSpace space,
    [SegBinOp _ lam nes (Shape [])] <- reds = collect $ do

    -- Declaration of the accumulator variables
    acc_vs <- forM (lambdaReturnType lam) $ \(Prim pt) ->
      fmap tvVar $ dPrim "acc" pt

    -- Assign each acumulator value to the corresponding neutral element 
    forM_ (zip acc_vs nes) $ \(acc_v, ne) -> do
      ne' <- toExp ne
      -- "<~~" Untyped assignment.
      acc_v <~~ ne'

    -- Version Alpha
    -- Step A : Parralel loop for stage 1 reduction
    stage_one_idx <- dPrim "iter" int64
    tmp <- collect $ do 
      dPrim_ gtid int64
      copyDWIM gtid [] (Var . tvVar $ stage_one_idx) []
      compileStms mempty (kernelBodyStms kbody) $ do
        dLParams $ lambdaParams lam
        let (x_params, y_params) =
              splitAt (length nes) $ lambdaParams lam

        forM_ (zip x_params acc_vs) $ \(x_param, acc_v) ->
          copyDWIMFix (paramName x_param) [] (Var acc_v) []

        forM_ (zip y_params (kernelBodyResult kbody)) $ \(y_param, Returns _ se) ->
          copyDWIMFix (paramName y_param) [] se []

        compileStms mempty (bodyStms (lambdaBody lam)) $
          forM_ (zip acc_vs (bodyResult (lambdaBody lam))) $ \(acc_v, se) ->
            copyDWIMFix acc_v [] se []
    emit $ Imp.Op $ Imp.DistributedLoop "segred" (tvVar stage_one_idx) Imp.Skip tmp mempty [] $ segFlat space
    
    -- Step B : Copy accumulator to array
    -- Recover infos about the world and the node
    nb_nodes <- dPrim "nb_nodes" int32
    emit $ Imp.Op $ Imp.LoadNbNode (tvVar nb_nodes)

    node_id <- dPrim "node_id" int32
    emit $ Imp.Op $ Imp.LoadNodeId (tvVar node_id)

    -- I may need to change the PrimeType int64 -> xx
    -- `pt <- lookupType $ head acc_vs` I should look up how to create an array with those types
    let array_type = int64 
    let array_size = typeSize $ Array array_type (Shape [Var $ tvVar nb_nodes]) NoUniqueness
    second_stage_mem <- sAlloc "second_stage_acc" array_size DefaultSpace 
    second_stage_array <- sArrayInMem "second_stage_arr" array_type (Shape [Var . tvVar $ nb_nodes]) second_stage_mem 
    copyDWIMFix second_stage_array [Imp.vi64 $ tvVar node_id] (Var $ head acc_vs) []
    
    -- Step C : Gather array on main node
    emit $ Imp.Op $ Imp.Gather second_stage_mem
    
    -- Step D : Stage 2 reduction
    stage_two <- collect $ do
      -- Reset acc 
      forM_ (zip acc_vs nes) $ \(acc_v, ne) -> do
        ne' <- toExp ne
        -- "<~~" Untyped assignment.
        acc_v <~~ ne'
      -- Here I need to change the input array of the loop body
      sFor "i" (Imp.vi64 . tvVar $ nb_nodes) $ \i -> do
        dPrimV_ gtid i
        compileStms mempty (kernelBodyStms kbody) $ do
          dLParams $ lambdaParams lam
          let (x_params, y_params) =
                splitAt (length nes) $ lambdaParams lam

          -- Test fix, rewrite the input array var
          let (Returns _ (Var result_vname)) = head $ kernelBodyResult kbody
          traceM $ "var : " ++ show result_vname
          -- Replace se with read from stage 1 result, namely `second_stage_array`
          copyDWIMFix result_vname [] (Var second_stage_array) [Imp.vi64 gtid]

          forM_ (zip x_params acc_vs) $ \(x_param, acc_v) ->
            copyDWIMFix (paramName x_param) [] (Var acc_v) []
          
          forM_ (zip y_params (kernelBodyResult kbody)) $ \(y_param, Returns _ se) ->
            copyDWIMFix (paramName y_param) [] se []

          compileStms mempty (bodyStms (lambdaBody lam)) $
            forM_ (zip acc_vs (bodyResult (lambdaBody lam))) $ \(acc_v, se) ->
              copyDWIMFix acc_v [] se []
    
    -- Node id 0 is the main node
    sIf ((Imp.vi64. tvVar  $ node_id) .==. 0) (emit stage_two) (pure ())
    
    forM_ (zip (patternNames pat) acc_vs) $ \(v, acc_v) ->
      copyDWIMFix v [] (Var acc_v) []
    
compileSegRed _pat _space _reds _kbody = collect $ do emit Imp.Skip

-- | Like 'compileSegRed', but where the body is a monadic action.
compileSegRed' ::
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  TV Int32 ->
  DoSegBody ->
  MPIGen Imp.Code
compileSegRed' _pat _space _reds _nsubtasks _kbody =
  undefined