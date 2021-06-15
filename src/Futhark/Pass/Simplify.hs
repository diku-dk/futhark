{-# LANGUAGE FlexibleContexts #-}

module Futhark.Pass.Simplify
  ( simplify,
    simplifySOACS,
    simplifySeq,
    simplifyMC,
    simplifyGPU,
    simplifyGPUMem,
    simplifySeqMem,
    simplifyMCMem,
  )
where

import qualified Futhark.IR.GPU.Simplify as GPU
import qualified Futhark.IR.GPUMem as GPUMem
import qualified Futhark.IR.MC as MC
import qualified Futhark.IR.MCMem as MCMem
import qualified Futhark.IR.SOACS.Simplify as SOACS
import qualified Futhark.IR.Seq as Seq
import qualified Futhark.IR.SeqMem as SeqMem
import Futhark.IR.Syntax
import Futhark.Pass

simplify ::
  (Prog rep -> PassM (Prog rep)) ->
  Pass rep rep
simplify = Pass "simplify" "Perform simple enabling optimisations."

simplifySOACS :: Pass SOACS.SOACS SOACS.SOACS
simplifySOACS = simplify SOACS.simplifySOACS

simplifyGPU :: Pass GPU.GPU GPU.GPU
simplifyGPU = simplify GPU.simplifyGPU

simplifySeq :: Pass Seq.Seq Seq.Seq
simplifySeq = simplify Seq.simplifyProg

simplifyMC :: Pass MC.MC MC.MC
simplifyMC = simplify MC.simplifyProg

simplifyGPUMem :: Pass GPUMem.GPUMem GPUMem.GPUMem
simplifyGPUMem = simplify GPUMem.simplifyProg

simplifySeqMem :: Pass SeqMem.SeqMem SeqMem.SeqMem
simplifySeqMem = simplify SeqMem.simplifyProg

simplifyMCMem :: Pass MCMem.MCMem MCMem.MCMem
simplifyMCMem = simplify MCMem.simplifyProg
