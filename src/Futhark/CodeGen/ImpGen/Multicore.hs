{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg
  )
  where

import Control.Monad
import Prelude hiding (quot, rem)

import Futhark.Error
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp

import Futhark.CodeGen.ImpGen.Multicore.SegMap
import Futhark.CodeGen.ImpGen.Multicore.SegRed
import Futhark.CodeGen.ImpGen.Multicore.SegScan
import Futhark.CodeGen.ImpGen.Multicore.SegHist

import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.MonadFreshNames


compileProg :: MonadFreshNames m => Prog ExplicitMemory
            -> m (Either InternalError Imp.Program)
compileProg = Futhark.CodeGen.ImpGen.compileProg ops Imp.DefaultSpace
  where ops = defaultOperations opCompiler
        opCompiler :: OpCompiler ExplicitMemory Imp.Multicore
        opCompiler dest (Alloc e space) =
          compileAlloc dest e space
        opCompiler dest (Inner (SegOp op)) =
          compileSegOp dest op
        opCompiler _ (Inner SizeOp{}) =
          -- FIXME: we will have to do something here at some point.
          return ()
        opCompiler _ (Inner (OtherOp ())) =
          return ()


compileSegOp :: Pattern ExplicitMemory -> SegOp ExplicitMemory
             -> ImpM ExplicitMemory Imp.Multicore ()
compileSegOp pat  (SegHist _ space histops _ kbody) =
  compileSegHist pat space histops kbody

compileSegOp pat (SegScan _ space op nes _ kbody) =
  compileSegScan pat space op nes kbody

compileSegOp pat (SegRed lvl space reds _ kbody) =
  compileSegRed pat lvl space reds kbody

compileSegOp pat (SegMap _ space _ kbody) =
  compileSegMap pat space kbody
