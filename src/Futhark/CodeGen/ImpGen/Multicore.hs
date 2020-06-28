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

compileMCOp :: Pattern MCMem -> MCOp MCMem ()
            -> ImpM MCMem Mode Imp.Multicore ()
compileMCOp _ (OtherOp ()) = pure ()
compileMCOp pat (ParOp par_op op) =
  -- TODO: use the par_op version
  compileSegOp pat op par_op

compileSegOp :: Pattern MCMem -> SegOp () MCMem -> Maybe (SegOp () MCMem)
             -> ImpM MCMem Mode Imp.Multicore ()
compileSegOp pat (SegHist _ space histops _ kbody) par_op = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  dPrimV_ (segFlat space) 0
  let iterations = case unSegSpace space of
        [_] -> product ns'
        _   -> product $ init ns' -- Segmented reduction is over the inner most dimension

  seq_code <- compileSegHist pat space histops kbody ModeSequential
  par_code <- case par_op of
    Just nested_op -> collect $ compileSegOp pat nested_op Nothing
    Nothing -> compileSegHist pat space histops kbody ModeParallel

  free_params <- freeParams (par_code <> seq_code)  [segFlat space]
  emit $ Imp.Op $ Imp.Task free_params iterations par_code seq_code (segFlat space) []
  emit $ Imp.DebugPrint "Histogram end" Nothing

compileSegOp pat (SegScan _ space scans _ kbody) par_op = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  dPrimV_ (segFlat space) 0
  let iterations = case unSegSpace space of
        [_] -> product ns'
        _   -> product $ init ns' -- Segmented reduction is over the inner most dimension

  seq_code <- compileSegScan pat space scans kbody ModeSequential

  par_code <- case par_op of
    Just nested_op -> compileSegScan pat space scans kbody ModeParallel -- collect $ compileSegOp pat nested_op Nothing
    Nothing -> compileSegScan pat space scans kbody ModeParallel

  free_task_params <- freeParams (par_code <> seq_code) [segFlat space]
  emit $ Imp.Op $ Imp.Task free_task_params iterations par_code seq_code (segFlat space) []


compileSegOp pat (SegRed _ space reds _ kbody) par_op = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  sComment "neutral-initialise the output" $
   forM_ reds $ \red ->
     forM_ (zip (patternElements pat) $ segBinOpNeutral red) $ \(pe, ne) ->
       sLoopNest (segBinOpShape red) $ \vec_is ->
         copyDWIMFix (patElemName pe) vec_is ne []

  let iterations = case unSegSpace space of
        [_] -> product ns'
        _   -> product $ init ns' -- Segmented reduction is over the inner most dimension

  let retvals = map patElemName $ patternElements pat
  retvals_ts <- mapM lookupType retvals
  retval_params <- zipWithM toParam retvals retvals_ts
  let retval_names = map Imp.paramName retval_params

  dPrimV_ (segFlat space) 0
  seq_code <- compileSegRed pat space reds kbody ModeSequential

  par_code <- case par_op of
    Just _nested_op -> compileSegRed pat space reds kbody ModeParallel
      -- collect $ compileSegOp pat nested_op Nothing
    Nothing -> compileSegRed pat space reds kbody ModeParallel

  free_params <- freeParams (par_code <> seq_code) (segFlat space : retval_names)
  emit $ Imp.Op $ Imp.Task free_params iterations par_code seq_code (segFlat space) retval_params


compileSegOp pat (SegMap _ space _ kbody) par_op = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  seq_code <- compileSequentialSegMap pat space kbody
  par_code <- case par_op of
    Just _nested_op -> compileParallelSegMap pat space kbody -- collect $ compileSegOp pat nested_op Nothing
    Nothing -> compileParallelSegMap pat space kbody

  free_params_task <- freeParams (par_code <> seq_code) mempty
  emit $ Imp.Op $ Imp.Task free_params_task (product ns') par_code seq_code (segFlat space) []
