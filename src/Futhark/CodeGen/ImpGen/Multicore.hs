{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg
  , Warnings
  )
  where

import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp

import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegMap
import Futhark.CodeGen.ImpGen.Multicore.SegRed
import Futhark.CodeGen.ImpGen.Multicore.SegScan
import Futhark.CodeGen.ImpGen.Multicore.SegHist

import Futhark.CodeGen.ImpGen
import Futhark.IR.MCMem
import Futhark.MonadFreshNames

gccAtomics :: AtomicBinOp
gccAtomics = flip lookup cpu
    where
        cpu = [ (Add Int32 OverflowUndef , Imp.AtomicAdd Int32)
              , (Sub Int32 OverflowUndef , Imp.AtomicSub Int32)
              , (And Int32               , Imp.AtomicAnd Int32)
              , (Xor Int32               , Imp.AtomicXor Int32)
              , (Or  Int32               , Imp.AtomicOr Int32)
              , (Add Int64 OverflowUndef , Imp.AtomicAdd Int64)
              , (Sub Int64 OverflowUndef , Imp.AtomicSub Int64)
              , (And Int64               , Imp.AtomicAnd Int64)
              , (Xor Int64               , Imp.AtomicXor Int64)
              , (Or  Int64               , Imp.AtomicOr Int64)
              ]


compileProg :: MonadFreshNames m => Prog MCMem
            -> m (Warnings, Imp.Definitions Imp.Multicore)
compileProg = Futhark.CodeGen.ImpGen.compileProg (HostEnv gccAtomics) ops Imp.DefaultSpace
  where ops = defaultOperations opCompiler
        opCompiler dest (Alloc e space) = compileAlloc dest e space
        opCompiler dest (Inner op) = compileMCOp dest op

compileMCOp :: Pattern MCMem -> MCOp MCMem ()
            -> ImpM MCMem HostEnv Imp.Multicore ()
compileMCOp _ (OtherOp ()) = pure ()
compileMCOp pat (ParOp _par_op op) = do
  let space = getSpace op
  dPrimV_ (segFlat space) 0
  iterations <- getIterationDomain op space
  nsubtasks  <- dPrim "num_tasks" $ IntType Int32
  seq_code   <- compileSegOp pat op nsubtasks
  retvals    <- getReturnParams pat op

  let scheduling_info = Imp.SchedulerInfo nsubtasks (segFlat space) iterations

  -- par_code <- case par_op of
  --   Just nested_op -> do
  --     let space' = getSpace nested_op
  --     dPrimV_ (segFlat space') 0
  --     compileSegOp pat nested_op
  --   Nothing -> return mempty

  -- let maybe_par_code = case par_op of
  --       Just _ -> Just par_code
  --       Nothing -> Nothing

  free_params <- freeParams seq_code ([segFlat space, nsubtasks] ++ map Imp.paramName retvals)
  emit $ Imp.Op $ Imp.Task free_params seq_code Nothing retvals $ scheduling_info (decideScheduling seq_code)


compileSegOp :: Pattern MCMem
             -> SegOp () MCMem
             -> VName
             -> ImpM MCMem HostEnv Imp.Multicore Imp.Code
compileSegOp pat (SegHist _ space histops _ kbody) ntasks =
  compileSegHist pat space histops kbody ntasks

compileSegOp pat (SegScan _ space scans _ kbody) ntasks =
  compileSegScan pat space scans kbody ntasks

compileSegOp pat (SegRed _ space reds _ kbody) ntasks =
  compileSegRed pat space reds kbody ntasks

compileSegOp pat (SegMap _ space _ kbody) _ =
  compileSegMap pat space kbody
