{-# LANGUAGE TypeFamilies #-}

-- | Code generation for ImpCode with multicore operations.
module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg,
    Warnings,
  )
where

import Control.Monad
import Data.Map qualified as M
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegHist
import Futhark.CodeGen.ImpGen.Multicore.SegMap
import Futhark.CodeGen.ImpGen.Multicore.SegRed
import Futhark.CodeGen.ImpGen.Multicore.SegScan
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
import Futhark.Util.IntegralExp (rem)
import Prelude hiding (quot, rem)

opCompiler :: OpCompiler MCMem HostEnv Imp.Multicore
opCompiler dest (Alloc e space) = compileAlloc dest e space
opCompiler dest (Inner op) = compileMCOp dest op

parallelCopy :: CopyCompiler MCMem HostEnv Imp.Multicore
parallelCopy pt destloc srcloc = do
  seq_code <- collect $ localOps inThreadOps $ do
    body <- genCopy
    free_params <- freeParams body
    emit $ Imp.Op $ Imp.ParLoop "copy" body free_params
  free_params <- freeParams seq_code
  s <- prettyString <$> newVName "copy"
  iterations <- dPrimVE "iterations" $ product $ map pe64 srcshape
  let scheduling = Imp.SchedulerInfo (untyped iterations) Imp.Static
  emit . Imp.Op $
    Imp.SegOp s free_params (Imp.ParallelTask seq_code) Nothing [] scheduling
  where
    MemLoc destmem _ _ = destloc
    MemLoc srcmem srcshape _ = srcloc
    genCopy = collect . inISPC . generateChunkLoop "copy" Vectorized $ \i -> do
      is <- dIndexSpace' "i" (map pe64 srcshape) i
      (_, destspace, destidx) <- fullyIndexArray' destloc is
      (_, srcspace, srcidx) <- fullyIndexArray' srcloc is
      tmp <- dPrimS "tmp" pt
      emit $ Imp.Read tmp srcmem srcidx pt srcspace Imp.Nonvolatile
      emit $ Imp.Write destmem destidx pt destspace Imp.Nonvolatile $ Imp.var tmp pt

topLevelOps, inThreadOps :: Operations MCMem HostEnv Imp.Multicore
inThreadOps =
  (defaultOperations opCompiler)
    { opsExpCompiler = compileMCExp
    }
topLevelOps =
  (defaultOperations opCompiler)
    { opsExpCompiler = compileMCExp,
      opsCopyCompiler = parallelCopy
    }

updateAcc :: Safety -> VName -> [SubExp] -> [SubExp] -> MulticoreGen ()
updateAcc safety acc is vs = sComment "UpdateAcc" $ do
  -- See the ImpGen implementation of UpdateAcc for general notes.
  let is' = map pe64 is
  (c, _space, arrs, dims, op) <- lookupAcc acc is'
  let boundsCheck =
        case safety of
          Safe -> sWhen (inBounds (Slice (map DimFix is')) dims)
          _ -> id
  boundsCheck $
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
                error $ "Missing locks for " ++ prettyString acc

withAcc ::
  Pat LetDecMem ->
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
            sStaticArray "withacc_locks" int32 $ Imp.ArrayZeros num_locks
          let locks = Locks locks_arr num_locks
              extend env = env {hostLocks = M.insert c locks $ hostLocks env}
          localEnv extend $ locksForInputs atomics inputs'
      | otherwise =
          locksForInputs atomics inputs'

compileMCExp :: ExpCompiler MCMem HostEnv Imp.Multicore
compileMCExp _ (BasicOp (UpdateAcc safety acc is vs)) =
  updateAcc safety acc is vs
compileMCExp pat (WithAcc inputs lam) =
  withAcc pat inputs lam
compileMCExp dest e =
  defCompileExp dest e

compileMCOp ::
  Pat LetDecMem ->
  MCOp NoOp MCMem ->
  ImpM MCMem HostEnv Imp.Multicore ()
compileMCOp _ (OtherOp NoOp) = pure ()
compileMCOp pat (ParOp par_op op) = do
  let space = getSpace op
  dPrimV_ (segFlat space) (0 :: Imp.TExp Int64)
  iterations <- getIterationDomain op space
  seq_code <- collect $ localOps inThreadOps $ do
    nsubtasks <- dPrim "nsubtasks"
    sOp $ Imp.GetNumTasks $ tvVar nsubtasks
    emit =<< compileSegOp pat op nsubtasks
  retvals <- getReturnParams pat op

  let scheduling_info = Imp.SchedulerInfo (untyped iterations)

  par_task <- case par_op of
    Just nested_op -> do
      let space' = getSpace nested_op
      dPrimV_ (segFlat space') (0 :: Imp.TExp Int64)
      par_code <- collect $ do
        nsubtasks <- dPrim "nsubtasks"
        sOp $ Imp.GetNumTasks $ tvVar nsubtasks
        emit =<< compileSegOp pat nested_op nsubtasks
      pure $ Just $ Imp.ParallelTask par_code
    Nothing -> pure Nothing

  s <- segOpString op
  let seq_task = Imp.ParallelTask seq_code
  free_params <- filter (`notElem` retvals) <$> freeParams (par_task, seq_task)
  emit . Imp.Op $
    Imp.SegOp s free_params seq_task par_task retvals $
      scheduling_info (decideScheduling' op seq_code)

compileSegOp ::
  Pat LetDecMem ->
  SegOp () MCMem ->
  TV Int32 ->
  ImpM MCMem HostEnv Imp.Multicore Imp.MCCode
compileSegOp pat (SegHist _ space _ kbody histops) ntasks =
  compileSegHist pat space histops kbody ntasks
compileSegOp pat (SegScan _ space _ kbody scans post_op) ntasks =
  compileSegScan pat space kbody scans post_op ntasks
compileSegOp pat (SegRed _ space _ kbody reds) ntasks =
  compileSegRed pat space reds kbody ntasks
compileSegOp pat (SegMap _ space _ kbody) _ =
  compileSegMap pat space kbody

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

-- | Compile the program.
compileProg ::
  (MonadFreshNames m) =>
  Prog MCMem ->
  m (Warnings, Imp.Definitions Imp.Multicore)
compileProg =
  Futhark.CodeGen.ImpGen.compileProg
    (HostEnv gccAtomics mempty)
    topLevelOps
    Imp.DefaultSpace
