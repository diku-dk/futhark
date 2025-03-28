-- | Imperative code with an OpenCL component.
--
-- Apart from ordinary imperative code, this also carries around an
-- OpenCL program as a string, as well as a list of kernels defined by
-- the OpenCL program.
--
-- The imperative code has been augmented with a 'LaunchKernel'
-- operation that allows one to execute an OpenCL kernel.
module Futhark.CodeGen.ImpCode.OpenCL
  ( Program (..),
    KernelTarget (..),
    module Futhark.CodeGen.ImpCode.Kernels,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode.Kernels

-- | An program calling OpenCL kernels.
data Program = Program
  { openClProgram :: T.Text,
    -- | Must be prepended to the program.
    openClPrelude :: T.Text,
    -- | Definitions to be passed as macro definitions to the kernel
    -- compiler.
    openClMacroDefs :: [(Name, KernelConstExp)],
    openClKernelNames :: M.Map KernelName KernelSafety,
    -- | So we can detect whether the device is capable.
    openClUsedTypes :: [PrimType],
    -- | Runtime-configurable constants.
    openClParams :: ParamMap,
    -- | Assertion failure error messages.
    openClFailures :: [FailureMsg],
    hostDefinitions :: Definitions HostOp
  }

-- | The target platform when compiling imperative code to a 'Program'
data KernelTarget
  = TargetOpenCL
  | TargetCUDA
  | TargetHIP
  deriving (Eq)
