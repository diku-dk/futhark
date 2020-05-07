{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg
  )
  where

import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp

import Futhark.CodeGen.ImpGen.Multicore.SegMap
import Futhark.CodeGen.ImpGen.Multicore.SegRed
import Futhark.CodeGen.ImpGen.Multicore.SegScan
import Futhark.CodeGen.ImpGen.Multicore.SegHist

import Futhark.CodeGen.ImpGen
import Futhark.Representation.MCMem
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog MCMem
            -> m (Imp.Definitions Imp.Multicore)
compileProg = Futhark.CodeGen.ImpGen.compileProg () ops Imp.DefaultSpace
  where ops = defaultOperations opCompiler
        opCompiler dest (Alloc e space) = compileAlloc dest e space
        opCompiler dest (Inner op) = compileSegOp dest op

compileSegOp :: Pattern MCMem -> SegOp () MCMem
             -> ImpM MCMem () Imp.Multicore ()
compileSegOp pat  (SegHist _ space histops _ kbody) =
  compileSegHist pat space histops kbody

compileSegOp pat (SegScan _ space scans _ kbody) =
  compileSegScan pat space scans kbody

compileSegOp pat (SegRed _ space reds _ kbody) =
  compileSegRed pat space reds kbody

compileSegOp pat (SegMap _ space _ kbody) =
  compileSegMap pat space kbody
