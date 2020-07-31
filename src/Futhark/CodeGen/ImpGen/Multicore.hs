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

import Control.Monad


gccAtomics :: AtomicBinOp
gccAtomics = flip lookup cpu
    where
        cpu =
                [ (Add Int32 OverflowUndef , Imp.AtomicAdd Int32)
                , (Sub Int32 OverflowUndef , Imp.AtomicSub Int32)
                , (And Int32               , Imp.AtomicAnd Int32)
                , (Xor Int32               , Imp.AtomicXor Int32)
                , (Or Int32                , Imp.AtomicOr Int32)
                ]


compileProg :: MonadFreshNames m => Prog MCMem
            -> m (Warnings, Imp.Definitions Imp.Multicore)
compileProg = Futhark.CodeGen.ImpGen.compileProg (HostEnv gccAtomics) ops Imp.DefaultSpace
  where ops = defaultOperations opCompiler
        opCompiler dest (Alloc e space) = compileAlloc dest e space
        opCompiler dest (Inner op) = compileMCOp dest op


getSpace :: SegOp () MCMem -> SegSpace
getSpace (SegHist _ space _ _ _ ) = space
getSpace (SegRed _ space _ _ _ ) = space
getSpace (SegScan _ space _ _ _ ) = space
getSpace (SegMap _ space _ _) = space

getIterationDomain :: SegOp () MCMem -> SegSpace -> ImpM MCMem HostEnv Imp.Multicore Imp.Exp
getIterationDomain SegMap{} space = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns
  return $ product ns'
getIterationDomain _ space = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns
  case unSegSpace space of
     [_] -> return $ product ns'
     _   -> return $ product $ init ns' -- Segmented reduction is over the inner most dimension

getReturnParams :: Pattern MCMem -> SegOp () MCMem -> ImpM MCMem HostEnv Imp.Multicore [Imp.Param]
getReturnParams pat SegRed{} = do
  let retvals = map patElemName $ patternElements pat
  retvals_ts <- mapM lookupType retvals
  zipWithM toParam retvals retvals_ts
getReturnParams _ _ = return mempty

compileMCOp :: Pattern MCMem -> MCOp MCMem ()
            -> ImpM MCMem HostEnv Imp.Multicore ()
compileMCOp _ (OtherOp ()) = pure ()
compileMCOp pat (ParOp _par_op op) = do
  let space = getSpace op
  dPrimV_ (segFlat space) 0
  iterations <- getIterationDomain op space
  ntasks <- dPrim "num_tasks" $ IntType Int32
  seq_code <- compileSegOp pat op ntasks
  retvals <- getReturnParams pat op


  -- par_code <- case par_op of
  --   Just nested_op -> do
  --     let space' = getSpace nested_op
  --     dPrimV_ (segFlat space') 0
  --     compileSegOp pat nested_op
  --   Nothing -> return mempty

  -- let maybe_par_code = case par_op of
  --       Just _ -> Just par_code
  --       Nothing -> Nothing

  free_params <- freeParams seq_code ([segFlat space, ntasks] ++ map Imp.paramName retvals)
  emit $ Imp.Op $ Imp.Task free_params iterations seq_code Nothing (segFlat space) ntasks retvals (decideScheduling seq_code)


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

compileSegOp pat (SegMap _ space _ kbody) ntasks =
  compileSegMap pat space kbody ntasks
