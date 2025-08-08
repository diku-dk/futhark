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

import Futhark.IR.GPU.Simplify qualified as GPU
import Futhark.IR.GPUMem qualified as GPUMem
import Futhark.IR.MC qualified as MC
import Futhark.IR.MCMem qualified as MCMem
import Futhark.IR.SOACS.Simplify qualified as SOACS
import Futhark.IR.Seq qualified as Seq
import Futhark.IR.SeqMem qualified as SeqMem
import Futhark.IR.Syntax
import Futhark.Pass

simplify ::
  (Prog rep -> PassM (Prog rep)) ->
  Pass rep rep
simplify = Pass "simplify" "Perform simple enabling optimisations."

simplifySOACS :: SOACS.Aggressiveness -> Pass SOACS.SOACS SOACS.SOACS
simplifySOACS agg = simplify (SOACS.simplifySOACS agg)

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
