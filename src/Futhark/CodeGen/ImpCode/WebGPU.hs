-- | Imperative code with a WebGPU component.
--
-- Apart from ordinary imperative code, this also carries around a
-- WebGPU program as a string, as well as a list of kernels defined by
-- the program.
--
-- The imperative code has been augmented with a 'LaunchKernel'
-- operation that allows one to execute a WebGPU kernel.
module Futhark.CodeGen.ImpCode.WebGPU
  ( KernelInterface (..),
    Program (..),
    module Futhark.CodeGen.ImpCode.Kernels,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode.Kernels
import Futhark.Util.Pretty

-- | The interface to a WebGPU/WGSL kernel.
--
-- Arguments are assumed to be passed as shared memory sizes first, then
-- scalars, and then memory bindings.
data KernelInterface = KernelInterface
  { safety :: KernelSafety,
    -- | Offsets of all fields in the corresponding scalars struct.
    scalarsOffsets :: [Int],
    -- | Total size in bytes of the scalars uniform buffer.
    scalarsSize :: Int,
    -- | Bind slot index for the scalars uniform buffer.
    scalarsBindSlot :: Int,
    -- | Bind slot indices for all memory arguments.
    memBindSlots :: [Int],
    -- | Names of all the override declarations used by the kernel. Should only
    -- be required for the ad-hoc WGSL testing setup, in normal code generation
    -- these get passed through 'webgpuMacroDefs'.
    -- Currently also used to work around a Chrome/Dawn bug, see
    -- `gpu_create_kernel` in rts/c/backends/webgpu.h.
    overrideNames :: [T.Text],
    -- | Dynamic block dimensions, with the corresponding override name. They
    -- are also included in `overrideNames`.
    dynamicBlockDims :: [(Int, T.Text)],
    -- | Override names for shared memory sizes. They are also included in
    -- `overrideNames`.
    sharedMemoryOverrides :: [T.Text]
  }

-- | A program calling WebGPU kernels.
data Program = Program
  { webgpuProgram :: T.Text,
    -- | Must be prepended to the program.
    webgpuPrelude :: T.Text,
    -- | Definitions to be passed as macro definitions to the kernel
    -- compiler.
    webgpuMacroDefs :: [(Name, KernelConstExp)],
    webgpuKernels :: M.Map KernelName KernelInterface,
    -- | Runtime-configurable constants.
    webgpuParams :: ParamMap,
    -- | Assertion failure error messages.
    webgpuFailures :: [FailureMsg],
    hostDefinitions :: Definitions HostOp
  }

instance Pretty Program where
  pretty prog =
    -- TODO: print everything
    "webgpu {"
      </> indent 2 (stack $ map pretty $ T.lines $ webgpuPrelude prog)
      </> indent 2 (stack $ map pretty $ T.lines $ webgpuProgram prog)
      </> "}"
      </> ""
        <> pretty (hostDefinitions prog)
