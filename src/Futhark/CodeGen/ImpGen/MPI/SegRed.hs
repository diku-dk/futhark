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

type DoSegBody = (([(SubExp, [Imp.TExp Int64])] -> MPIGen ()) -> MPIGen ())

initAccumulators :: [VName] -> [SubExp] -> ImpM lore r op ()
initAccumulators acc_vs nes =
  -- Assign each acumulator value to the corresponding neutral element
  forM_ (zip acc_vs nes) $ \(acc_v, ne) -> do
    ne' <- toExp ne
    -- "<~~" Untyped assignment.
    acc_v <~~ ne'

-- Core stage reduction
stageReduction :: Lambda MCMem -> [SubExp] -> [VName] -> [SubExp] -> [Imp.TExp Int64] -> ImpM MCMem Env Imp.MPIOp ()
stageReduction lam nes acc_vs stage_arrays idxs = do
  -- Declare lambda params
  dLParams $ lambdaParams lam
  let (x_params, y_params) =
        splitAt (length nes) $ lambdaParams lam

  -- Load accumulators values
  forM_ (zip x_params acc_vs) $ \(x_param, acc_v) ->
    copyDWIMFix (paramName x_param) [] (Var acc_v) []

  -- Load input arrays values
  forM_ (zip y_params stage_arrays) $ \(y_param, array) ->
    copyDWIMFix (paramName y_param) [] array idxs

  -- Compile the reduction lambdas
  compileStms mempty (bodyStms (lambdaBody lam)) $
    forM_ (zip acc_vs (bodyResult (lambdaBody lam))) $ \(acc_v, se) ->
      copyDWIMFix acc_v [] se []

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
    -- We have multiple accumulators because ie. an array of tuples is represented as multiple arrays.
    acc_vs <- forM (lambdaReturnType lam) $ \(Prim pt) ->
      tvVar <$> dPrim "acc" pt

    initAccumulators acc_vs nes

    -- Step A : Parralel loop for stage 1 reduction
    stage_one_idx <- dPrim "iter" int64
    stage_one <- collect $ do
      -- gtid is the index used to read the input array
      dPrim_ gtid int64
      copyDWIM gtid [] (Var . tvVar $ stage_one_idx) []
      compileStms mempty (kernelBodyStms kbody) $ stageReduction lam nes acc_vs (map (\(Returns _ se) -> se) (kernelBodyResult kbody)) []

    emit $ Imp.Op $ Imp.DistributedLoop "segred" (tvVar stage_one_idx) Imp.Skip stage_one mempty [] $ segFlat space

    -- Step B : Copy accumulator to array
    -- Recover infos about the world and the executing node
    nb_nodes <- dPrim "nb_nodes" int32
    emit $ Imp.Op $ Imp.LoadNbNode (tvVar nb_nodes)

    node_id <- dPrim "node_id" int32
    emit $ Imp.Op $ Imp.LoadNodeId (tvVar node_id)

    -- Allocate memory for stage one arrays
    stage_one_mems <- forM (lambdaReturnType lam) $ \(Prim pt) ->
      sAlloc "second_stage_acc" (typeSize $ Array pt (Shape [tvSize nb_nodes]) NoUniqueness) DefaultSpace

    -- Declare stage one arrays
    stage_one_arrays <- forM (zip stage_one_mems (lambdaReturnType lam)) $ \(mem, Prim pt) ->
      sArrayInMem "second_stage_arr" pt (Shape [Var . tvVar $ nb_nodes]) mem

    -- Store the result of stage one reduction
    forM_ (zip stage_one_arrays acc_vs) $ \(array, acc) ->
      copyDWIMFix array [Imp.vi64 $ tvVar node_id] (Var acc) []

    -- Step C : Gather arrays on main node
    forM_ (zip stage_one_mems (lambdaReturnType lam)) $ \(mem, Prim pt) ->
      gather mem pt

    -- Step D : Stage 2 reduction
    stage_two <- collect $ do
      -- Reset acc
      initAccumulators acc_vs nes
      -- Here I need to change the input array of the loop body()
      sFor "i" (Imp.vi64 . tvVar $ nb_nodes) $ \i -> stageReduction lam nes acc_vs (map Var stage_one_arrays) [i]

    -- Node id 0 is the main node
    sIf ((Imp.vi64 . tvVar $ node_id) .==. 0) (emit stage_two) (pure ())

    forM_ (zip (patternNames pat) acc_vs) $ \(v, acc_v) ->
      copyDWIMFix v [] (Var acc_v) []
compileSegRed _pat _space _reds _kbody = collect $ do emit $ Imp.Op $ Imp.CrashWithThisMessage "Not implemented yet"

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
