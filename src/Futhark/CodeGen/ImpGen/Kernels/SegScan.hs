module Futhark.CodeGen.ImpGen.Kernels.SegScan (compileSegScan) where

import Futhark.CodeGen.ImpGen hiding (compileProg)
import Futhark.CodeGen.ImpGen.Kernels.Base
import qualified Futhark.CodeGen.ImpGen.Kernels.SegScan.SinglePass as SinglePass
import qualified Futhark.CodeGen.ImpGen.Kernels.SegScan.TwoPass as TwoPass
import Futhark.IR.KernelsMem

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  [SegBinOp KernelsMem] ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegScan pat lvl space scans kbody = do
  target <- hostTarget <$> askEnv
  case target of
    OpenCL -> TwoPass.compileSegScan pat lvl space scans kbody
    CUDA -> SinglePass.compileSegScan pat lvl space scans kbody
