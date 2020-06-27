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
  compileSegOp pat op

compileSegOp :: Pattern MCMem -> SegOp () MCMem
             -> ImpM MCMem Mode Imp.Multicore ()
compileSegOp pat (SegHist _ space histops _ kbody) =
  compileSegHist pat space histops kbody

compileSegOp pat (SegScan _ space scans _ kbody) =
  compileSegScan pat space scans kbody

compileSegOp pat (SegRed _ space reds _ kbody) =
  compileSegRed pat space reds kbody

compileSegOp pat (SegMap _ space _ kbody) =
  compileSegMap pat space kbody
