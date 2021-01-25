{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for ImpCode with multicore operations.
module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg,
    Warnings,
  )
where

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegHist
import Futhark.CodeGen.ImpGen.Multicore.SegMap
import Futhark.CodeGen.ImpGen.Multicore.SegRed
import Futhark.CodeGen.ImpGen.Multicore.SegScan
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
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
compileProg = Futhark.CodeGen.ImpGen.compileProg (HostEnv gccAtomics) ops Imp.DefaultSpace
  where
    ops = defaultOperations opCompiler
    opCompiler dest (Alloc e space) = compileAlloc dest e space
    opCompiler dest (Inner op) = compileMCOp dest op

compileMCOp ::
  Pattern MCMem ->
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
