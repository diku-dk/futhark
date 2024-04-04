-- | Imperative code with a WebGPU component.
--
-- Apart from ordinary imperative code, this also carries around a
-- WebGPU program as a string, as well as a list of kernels defined by
-- the program.
--
-- The imperative code has been augmented with a 'LaunchKernel'
-- operation that allows one to execute a WebGPU kernel.
module Futhark.CodeGen.ImpCode.WebGPU
  ( Program (..),
    module Futhark.CodeGen.ImpCode.Kernels,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode.Kernels
import Futhark.Util.Pretty

-- | An program calling WebGPU kernels.
data Program = Program
  { webgpuProgram :: T.Text,
    -- | Must be prepended to the program.
    webgpuPrelude :: T.Text,
    -- | Definitions to be passed as macro definitions to the kernel
    -- compiler.
    webgpuMacroDefs :: [(Name, KernelConstExp)],
    webgpuKernelNames :: M.Map KernelName KernelSafety,
    -- | Runtime-configurable constants.
    webgpuParams :: ParamMap,
    -- | Assertion failure error messages.
    webgpuFailures :: [FailureMsg],
    -- | Information about arguments passed to a kernel. List of override
    -- variable names and the bind slots used by the kernel.
    --
    -- Mostly to support the temporary WGSL kernel testing setup, should not be
    -- required in this form when proper host-side code generation is done.
    webgpuKernelInfo :: M.Map KernelName ([T.Text], [Int]),
    hostDefinitions :: Definitions HostOp
  }

instance Pretty Program where
  pretty prog =
    -- TODO: print everything
    "webgpu {"
      </> "== SHADER START =="
      </> indent 2 (stack $ map pretty $ T.lines $ webgpuPrelude prog)
      </> indent 2 (stack $ map pretty $ T.lines $ webgpuProgram prog)
      </> "== SHADER END =="
      </> "}"
      </> ""
      <> pretty (hostDefinitions prog)
