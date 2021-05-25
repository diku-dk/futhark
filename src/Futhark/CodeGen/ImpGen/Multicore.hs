{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for ImpCode with multicore operations.
module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg,
    Warnings,
  )
where

import Control.Monad
import qualified Data.Map as M
import Data.List (inits, tails, zip4)
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegHist
import Futhark.CodeGen.ImpGen.Multicore.SegMap
import Futhark.CodeGen.ImpGen.Multicore.SegRed
import Futhark.CodeGen.ImpGen.Multicore.SegScan
import Futhark.CodeGen.ImpGen.Multicore.SegStencil
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
import Futhark.Util.IntegralExp (rem)
import Prelude hiding (quot, rem)

import Debug.Trace (trace)

-- GCC supported primitve atomic Operations
-- TODO: Add support for 1, 2, and 16 bytes too
gccAtomics :: AtomicBinOp
gccAtomics = flip lookup cpu
  where
    cpu =
      [ (Add Int32 OverflowUndef, Imp.AtomicAdd Int32),
        (Sub Int32 OverflowUndef, Imp.AtomicSub Int32),
        (And Int32, Imp.AtomicAnd Int32),
        (Xor Int32, Imp.AtomicXor Int32),
        (Or Int32, Imp.AtomicOr Int32),
        (Add Int64 OverflowUndef, Imp.AtomicAdd Int64),
        (Sub Int64 OverflowUndef, Imp.AtomicSub Int64),
        (And Int64, Imp.AtomicAnd Int64),
        (Xor Int64, Imp.AtomicXor Int64),
        (Or Int64, Imp.AtomicOr Int64)
      ]

compileProg ::
  MonadFreshNames m =>
  Prog MCMem ->
  m (Warnings, Imp.Definitions Imp.Multicore)
compileProg = Futhark.CodeGen.ImpGen.compileProg (HostEnv gccAtomics mempty) ops Imp.DefaultSpace
  where
    ops =
      (defaultOperations opCompiler)
        { opsExpCompiler = compileMCExp
        }
    opCompiler dest (Alloc e space) = compileAlloc dest e space
    opCompiler dest (Inner op) = compileMCOp dest op

updateAcc :: VName -> [SubExp] -> [SubExp] -> MulticoreGen ()
updateAcc acc is vs = sComment "UpdateAcc" $ do
  -- See the ImpGen implementation of UpdateAcc for general notes.
  let is' = map toInt64Exp is
  (c, _space, arrs, dims, op) <- lookupAcc acc is'
  sWhen (inBounds (map DimFix is') dims) $
    case op of
      Nothing ->
        forM_ (zip arrs vs) $ \(arr, v) -> copyDWIMFix arr is' v []
      Just lam -> do
        dLParams $ lambdaParams lam
        let (_x_params, y_params) =
              splitAt (length vs) $ map paramName $ lambdaParams lam
        forM_ (zip y_params vs) $ \(yp, v) -> copyDWIM yp [] v []
        atomics <- hostAtomics <$> askEnv
        case atomicUpdateLocking atomics lam of
          AtomicPrim f -> f arrs is'
          AtomicCAS f -> f arrs is'
          AtomicLocking f -> do
            c_locks <- M.lookup c . hostLocks <$> askEnv
            case c_locks of
              Just (Locks locks num_locks) -> do
                let locking =
                      Locking locks 0 1 0 $
                        pure . (`rem` fromIntegral num_locks) . flattenIndex dims
                f locking arrs is'
              Nothing ->
                error $ "Missing locks for " ++ pretty acc

withAcc ::
  Pattern MCMem ->
  [(Shape, [VName], Maybe (Lambda MCMem, [SubExp]))] ->
  Lambda MCMem ->
  MulticoreGen ()
withAcc pat inputs lam = do
  atomics <- hostAtomics <$> askEnv
  locksForInputs atomics $ zip accs inputs
  where
    accs = map paramName $ lambdaParams lam
    locksForInputs _ [] =
      defCompileExp pat $ WithAcc inputs lam
    locksForInputs atomics ((c, (_, _, op)) : inputs')
      | Just (op_lam, _) <- op,
        AtomicLocking _ <- atomicUpdateLocking atomics op_lam = do
        let num_locks = 100151
        locks_arr <-
          sStaticArray "withacc_locks" DefaultSpace int32 $
            Imp.ArrayZeros num_locks
        let locks = Locks locks_arr num_locks
            extend env = env {hostLocks = M.insert c locks $ hostLocks env}
        localEnv extend $ locksForInputs atomics inputs'
      | otherwise =
        locksForInputs atomics inputs'

compileMCExp :: ExpCompiler MCMem HostEnv Imp.Multicore
compileMCExp _ (BasicOp (UpdateAcc acc is vs)) =
  updateAcc acc is vs
compileMCExp pat (WithAcc inputs lam) =
  withAcc pat inputs lam
compileMCExp dest e =
  defCompileExp dest e

compileMCOp ::
  Pattern MCMem ->
  MCOp MCMem () ->
  ImpM MCMem HostEnv Imp.Multicore ()
compileMCOp _ (OtherOp ()) = pure ()
compileMCOp pat (ParOp par_op op@(SegStencil _ _ sten _ kbody)) = do
  let space = getSpace op
      ns = map (toInt64Exp . snd) $ unSegSpace space
      numDims = length ns
      tds = map toInt64Exp $ tileDims (length ns)
      idxs = stencilIndexes sten
      bounds = map (\xs -> (fromInteger (minimum xs), fromInteger (maximum xs))) idxs
      nsInner = zipWith (\(mi, ma) n -> n + mi - ma) bounds ns
      nsLower = zipWith (++) (map init $ tail $ inits nsInner)
                             (zipWith (:) (map (negate . fst) bounds) $ tail $ tails ns)
      nsUpper = zipWith (++) (map init $ tail $ inits nsInner)
                                    (zipWith (:) (map snd bounds) $ tail $ tails ns)
      offsetsLower = init $ zipWith (++) (inits (map (negate . fst) bounds))
                                         (tails (replicate numDims 0))
      offsetsUpper = zipWith (++) (inits (map (negate . fst) bounds))
                                  (zipWith (:) (zipWith (+) nsInner (map (negate . fst) bounds)) $ tail $ tails (replicate numDims 0))
--      offsetsUpper = init $ zipWith (++) (inits (map (negate . fst) bounds))
--                                         (tails $ init $ 0 : zipWith (+) nsInner (map (negate . fst) bounds))
--      offsetsUpper = tail $ zipWith (++) (inits $ zipWith (+) nsInner (map (negate . fst) bounds))
--                                         (tails (replicate numDims 0))
      -- dims = zipWith (\(mi, ma) n -> (negate mi, n, ma)) bounds ns'
      innerLoopOffsets = trace (show offsetsUpper) $ map (negate . fst) bounds
      toIters n td = TPrimExp $ BinOpExp (SDiv Int64 Unsafe) (untyped $ n + td - 1)
                                                             (untyped td)
      innerIterations = product $ zipWith toIters nsInner tds :: Imp.TExp Int64

      --boundaryLoopDims = flip map [0..numDims-1] $ \i ->
      --  (zipWith (\(l, m, _) j -> if i == j then l else m) dims [0..],
      --   zipWith (\(_, m, r) j -> if i == j then r else m) dims [0..],
      --   zipWith (\(l, m, _) j -> if i == j then l + m else 0) dims [0..])

  nsubtasks <- dPrim "num_tasks" $ IntType Int32
  retvals <- getReturnParams pat op
  s <- segOpString op

  -- For each dimension, we need 2 loops (one at each boundary of the array)
  forM_ (zip4 nsLower offsetsLower nsUpper offsetsUpper) $
    \(loopStart, offsetsStart, loopEnd, offsetsEnd) -> do
      -- Loop for the "lower" boundary
      loop_lower_ns <- replicateM numDims (newVName "idx_lower")
      zipWithM_ dPrimV_ loop_lower_ns loopStart

      let loopStartSpace = SegSpace (segFlat space) $ zip (map fst $ unSegSpace space)
                                                    $ map Var loop_lower_ns
      codeStart <- compileStencilBoundaryLoop pat loopStartSpace offsetsStart ns sten kbody
      let iterationsStart = product loopStart
          schedulingInfoStart = Imp.SchedulerInfo (tvVar nsubtasks) (untyped iterationsStart)
          nonFreeStart = segFlat loopStartSpace :
                         tvVar nsubtasks :
                         map Imp.paramName retvals
      freeParamsStart <- freeParams codeStart nonFreeStart
      let taskStart = Imp.ParallelTask codeStart (segFlat loopStartSpace)
      emit $ Imp.Op
           $ Imp.Segop s freeParamsStart taskStart Nothing retvals
           $ schedulingInfoStart (decideScheduling' op codeStart)

      -- loop for the "upper" boundary
      -- Loop for the "lower" boundary
      loop_upper_ns <- replicateM numDims (newVName "idx_upper")
      zipWithM_ dPrimV_ loop_upper_ns loopEnd

      let loopEndSpace = SegSpace (segFlat space) $ zip (map fst $ unSegSpace space)
                                                  $ map Var loop_upper_ns
      codeEnd <- compileStencilBoundaryLoop pat loopStartSpace offsetsEnd ns sten kbody
      let iterationsEnd = product loopEnd
          schedulingInfoEnd = Imp.SchedulerInfo (tvVar nsubtasks) (untyped iterationsEnd)
          nonFreeEnd = segFlat loopEndSpace :
                         tvVar nsubtasks :
                         map Imp.paramName retvals
      freeParamsEnd <- freeParams codeEnd nonFreeEnd
      let taskEnd = Imp.ParallelTask codeEnd (segFlat loopEndSpace)
      emit $ Imp.Op
           $ Imp.Segop s freeParamsEnd taskEnd Nothing retvals
           $ schedulingInfoEnd (decideScheduling' op codeEnd)

  inner_ns <- replicateM numDims (newVName "idx_inner")
  zipWithM_ dPrimV_ inner_ns $ zipWith (\o n -> n + o) innerLoopOffsets nsInner

  let innerSpace = SegSpace (segFlat space) $ zip (map fst $ unSegSpace space)
                                            $ map Var inner_ns
  -- This is the loop that processes the inner part of the array, and thus doesn't need
  -- to do any boundary checking. This loop is the only one that's tiled.
  code <- compileSegStencilInner pat innerSpace innerLoopOffsets sten kbody

  let scheduling_info_inner = Imp.SchedulerInfo (tvVar nsubtasks) (untyped innerIterations)

  let non_free = segFlat innerSpace :
                 tvVar nsubtasks :
                 map Imp.paramName retvals

  free_params <- freeParams code non_free
  let inner_task = Imp.ParallelTask code (segFlat innerSpace)
  emit $ Imp.Op
       $ Imp.Segop s free_params inner_task Nothing retvals
       $ scheduling_info_inner (decideScheduling' op code)

compileMCOp pat (ParOp par_op op) = do
  let space = getSpace op
  dPrimV_ (segFlat space) (0 :: Imp.TExp Int64)
  iterations <- getIterationDomain op space
  nsubtasks <- dPrim "num_tasks" $ IntType Int32
  seq_code <- compileSegOp pat op nsubtasks
  retvals <- getReturnParams pat op

  let scheduling_info = Imp.SchedulerInfo (tvVar nsubtasks) (untyped iterations)

  par_code <- case par_op of
    Just nested_op -> do
      let space' = getSpace nested_op
      dPrimV_ (segFlat space') (0 :: Imp.TExp Int64)
      compileSegOp pat nested_op nsubtasks
    Nothing -> return mempty

  let par_task = case par_op of
        Just nested_op -> Just $ Imp.ParallelTask par_code $ segFlat $ getSpace nested_op
        Nothing -> Nothing

  let non_free =
        ( [segFlat space, tvVar nsubtasks]
            ++ map Imp.paramName retvals
        )
          ++ case par_op of
            Just nested_op ->
              [segFlat $ getSpace nested_op]
            Nothing -> []

  s <- segOpString op
  free_params <- freeParams (par_code <> seq_code) non_free
  let seq_task = Imp.ParallelTask seq_code (segFlat space)
  emit $ Imp.Op $ Imp.Segop s free_params seq_task par_task retvals $ scheduling_info (decideScheduling' op seq_code)

compileSegOp ::
  Pattern MCMem ->
  SegOp () MCMem ->
  TV Int32 ->
  ImpM MCMem HostEnv Imp.Multicore Imp.Code
compileSegOp pat (SegHist _ space histops _ kbody) ntasks =
  compileSegHist pat space histops kbody ntasks
compileSegOp pat (SegScan _ space scans _ kbody) ntasks =
  compileSegScan pat space scans kbody ntasks
compileSegOp pat (SegRed _ space reds _ kbody) ntasks =
  compileSegRed pat space reds kbody ntasks
compileSegOp pat (SegMap _ space _ kbody) _ =
  compileSegMap pat space kbody
compileSegOp _ (SegStencil _ _ _ _ _) _ =
  pure $ Imp.DebugPrint "Impossible: compileSegOp got a SegStencil" Nothing
