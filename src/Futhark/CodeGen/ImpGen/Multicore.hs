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
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
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
  sWhen (inBounds (Slice (map DimFix is')) dims) $
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
  Pat MCMem ->
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
  Pat MCMem ->
  MCOp MCMem () ->
  ImpM MCMem HostEnv Imp.Multicore ()
compileMCOp _ (OtherOp ()) = pure ()
compileMCOp pat (ParOp par_op op) = do
  let space = getSpace op
  dPrimV_ (segFlat space) (0 :: Imp.TExp Int64)
  iterations <- getIterationDomain op space
  nsubtasks <- dPrim "num_tasks" $ IntType Int32
  seq_code <- compileSegOp pat op nsubtasks
  retvals <- getReturnParams pat op

  let scheduling_info = Imp.SchedulerInfo (tvVar nsubtasks) (untyped iterations)

  par_task <- case par_op of
    Just nested_op -> do
      let space' = getSpace nested_op
      dPrimV_ (segFlat space') (0 :: Imp.TExp Int64)
      par_code <- compileSegOp pat nested_op nsubtasks
      pure $ Just $ Imp.ParallelTask par_code $ segFlat $ getSpace nested_op
    Nothing -> pure Nothing

  let non_free =
        ( [segFlat space, tvVar nsubtasks]
            ++ map Imp.paramName retvals
        )
          ++ case par_op of
            Just nested_op ->
              [segFlat $ getSpace nested_op]
            Nothing -> []

  s <- segOpString op
  let seq_task = Imp.ParallelTask seq_code (segFlat space)
  free_params <- freeParams (par_task, seq_task) non_free
  emit . Imp.Op $
    Imp.SegOp s free_params seq_task par_task retvals $
      scheduling_info (decideScheduling' op seq_code)

compileSegOp ::
  Pat MCMem ->
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
