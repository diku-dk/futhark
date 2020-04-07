{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg
  )
  where

import Control.Monad
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp

import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegMap
import Futhark.CodeGen.ImpGen.Multicore.SegRed
import Futhark.CodeGen.ImpGen.Multicore.SegScan
import Futhark.CodeGen.ImpGen.Multicore.SegHist

import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.MonadFreshNames
import Futhark.Util.IntegralExp (quotRoundingUp)


computeThreadChunkSize :: SplitOrdering
                       -> Imp.Exp
                       -> Imp.Count Imp.Elements Imp.Exp
                       -> Imp.Count Imp.Elements Imp.Exp
                       -> VName
                       -> ImpM lore op ()
computeThreadChunkSize (SplitStrided stride) thread_index elements_per_thread num_elements chunk_var = do
  stride' <- toExp stride
  chunk_var <--
    Imp.BinOpExp (SMin Int32)
    (Imp.unCount elements_per_thread)
    ((Imp.unCount num_elements - thread_index) `quotRoundingUp` stride')


computeThreadChunkSize SplitContiguous thread_index elements_per_thread num_elements chunk_var = do
  starting_point <- dPrimV "starting_point" $
    thread_index * Imp.unCount elements_per_thread
  remaining_elements <- dPrimV "remaining_elements" $
    Imp.unCount num_elements - Imp.var starting_point int32

  let no_remaining_elements = Imp.var remaining_elements int32 .<=. 0
      beyond_bounds = Imp.unCount num_elements .<=. Imp.var starting_point int32

  sIf (no_remaining_elements .||. beyond_bounds)
    (chunk_var <-- 0)
    (sIf is_last_thread
       (chunk_var <-- Imp.unCount last_thread_elements)
       (chunk_var <-- Imp.unCount elements_per_thread))
  where last_thread_elements =
          num_elements - Imp.elements thread_index * elements_per_thread
        is_last_thread =
          Imp.unCount num_elements .<.
          (thread_index + 1) * Imp.unCount elements_per_thread


compileProg :: MonadFreshNames m => Prog ExplicitMemory
            -> m Imp.Program
compileProg = Futhark.CodeGen.ImpGen.compileProg ops Imp.DefaultSpace
  where ops = defaultOperations opCompiler
        opCompiler :: OpCompiler ExplicitMemory Imp.Multicore
        opCompiler dest (Alloc e space) =
          compileAlloc dest e space
        opCompiler dest (Inner (SegOp op)) =
          compileSegOp dest op
        opCompiler  (Pattern _ [pe]) (Inner (SizeOp GetSize{})) =
          getNumThreads' $ patElemName pe
        opCompiler (Pattern _ [pe]) (Inner (SizeOp CalcNumGroups{})) = do
          let dest = patElemName pe
          dest <-- 1
          return ()
        opCompiler (Pattern _ [size])(Inner (SizeOp (SplitSpace o w i elems_per_thread))) = do
          num_elements <- Imp.elements <$> toExp w
          i' <- toExp i
          elems_per_thread' <- Imp.elements <$> toExp elems_per_thread
          computeThreadChunkSize o i' elems_per_thread' num_elements (patElemName size)
        opCompiler _ (Inner SizeOp{}) =
          return ()
        opCompiler _ (Inner (OtherOp ())) =
          return ()


compileSegOp :: Pattern ExplicitMemory -> SegOp ExplicitMemory
             -> ImpM ExplicitMemory Imp.Multicore ()
compileSegOp pat  (SegHist _ space histops _ kbody) =
  compileSegHist pat space histops kbody

compileSegOp pat (SegScan _ space op nes _ kbody) =
  compileSegScan pat space op nes kbody

compileSegOp pat (SegRed _ space reds _ kbody) =
  compileSegRed pat space reds kbody

compileSegOp pat (SegMap _ space _ kbody) =
  compileSegMap pat space kbody
