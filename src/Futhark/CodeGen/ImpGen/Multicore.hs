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

compileProg :: MonadFreshNames m => Prog MCMem
            -> m (Warnings, Imp.Definitions Imp.Multicore)
compileProg = Futhark.CodeGen.ImpGen.compileProg ModeParallel ops Imp.DefaultSpace
  where ops = defaultOperations opCompiler
        opCompiler dest (Alloc e space) = compileAlloc dest e space
        opCompiler dest (Inner op) = compileMCOp dest op


getSpace :: SegOp () MCMem -> SegSpace
getSpace (SegHist _ space _ _ _ ) = space
getSpace (SegRed _ space _ _ _ ) = space
getSpace (SegScan _ space _ _ _ ) = space
getSpace (SegMap _ space _ _) = space

getIterationDomain :: SegOp () MCMem -> SegSpace -> ImpM MCMem Mode Imp.Multicore Imp.Exp
getIterationDomain (SegMap {}) space = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns
  return $ product ns'
getIterationDomain _ space = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns
  case unSegSpace space of
     [_] -> return $ product ns'
     _   -> return $ product $ init ns' -- Segmented reduction is over the inner most dimension

getReturnParams :: Pattern MCMem -> SegOp () MCMem -> ImpM MCMem Mode Imp.Multicore [Imp.Param]
getReturnParams pat SegRed{} = do
  let retvals = map patElemName $ patternElements pat
  retvals_ts <- mapM lookupType retvals
  zipWithM toParam retvals retvals_ts
getReturnParams _ _ = return mempty

-- TODO: use the par_op version
compileMCOp :: Pattern MCMem -> MCOp MCMem ()
            -> ImpM MCMem Mode Imp.Multicore ()
compileMCOp _ (OtherOp ()) = pure ()
compileMCOp pat (ParOp _par_op op) = do
  let space = getSpace op
  dPrimV_ (segFlat space) 0
  iterations <- getIterationDomain op space
  seq_code <- compileSegOp pat op
  retvals <- getReturnParams pat op

  -- par_code <- case par_op of
  --   Just nested_op -> collect $ compileSegOp pat nested_op Nothing
  --   Nothing -> compileSegHist pat space histops kbody ModeParallel

  free_params <- freeParams seq_code (segFlat space : map Imp.paramName retvals)
  emit $ Imp.Op $ Imp.Task free_params iterations mempty seq_code (segFlat space) retvals


compileSegOp :: Pattern MCMem -> SegOp () MCMem
             -> ImpM MCMem Mode Imp.Multicore Imp.Code
compileSegOp pat (SegHist _ space histops _ kbody) =
  compileSegHist pat space histops kbody ModeParallel

compileSegOp pat (SegScan _ space scans _ kbody) =
  compileSegScan pat space scans kbody ModeParallel

compileSegOp pat (SegRed _ space reds _ kbody) =
  compileSegRed pat space reds kbody ModeParallel

compileSegOp pat (SegMap _ space _ kbody) =
  compileParallelSegMap pat space kbody
