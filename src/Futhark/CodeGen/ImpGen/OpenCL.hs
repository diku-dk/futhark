module Futhark.CodeGen.ImpGen.OpenCL
  ( compileProg
  ) where

import Control.Monad

import Prelude

import Futhark.Representation.ExplicitMemory (Prog)
import qualified Futhark.CodeGen.ImpCode.OpenCL as OpenCL
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import Futhark.CodeGen.ImpGen.Kernels.ToOpenCL

compileProg :: Prog -> Either String OpenCL.Program
compileProg = kernelsToOpenCL <=< ImpGenKernels.compileProg
