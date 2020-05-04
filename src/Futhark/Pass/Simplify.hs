{-# LANGUAGE FlexibleContexts #-}
module Futhark.Pass.Simplify
  ( simplify
  , simplifySOACS
  , simplifySeq
  , simplifyKernels
  , simplifyKernelsMem
  , simplifySeqMem
  )
  where

import qualified Futhark.Representation.SOACS as R
import qualified Futhark.Representation.SOACS.Simplify as R
import qualified Futhark.Representation.Kernels as R
import qualified Futhark.Representation.Kernels.Simplify as R
import qualified Futhark.Representation.Seq as R
import qualified Futhark.Representation.KernelsMem as KernelsMem
import qualified Futhark.Representation.SeqMem as SeqMem

import Futhark.Pass
import Futhark.Representation.AST.Syntax

simplify :: (Prog lore -> PassM (Prog lore))
         -> Pass lore lore
simplify = Pass "simplify" "Perform simple enabling optimisations."

simplifySOACS :: Pass R.SOACS R.SOACS
simplifySOACS = simplify R.simplifySOACS

simplifyKernels :: Pass R.Kernels R.Kernels
simplifyKernels = simplify R.simplifyKernels

simplifySeq :: Pass R.Seq R.Seq
simplifySeq = simplify R.simplifyProg

simplifyKernelsMem :: Pass KernelsMem.KernelsMem KernelsMem.KernelsMem
simplifyKernelsMem = simplify KernelsMem.simplifyProg

simplifySeqMem :: Pass SeqMem.SeqMem SeqMem.SeqMem
simplifySeqMem = simplify SeqMem.simplifyProg
