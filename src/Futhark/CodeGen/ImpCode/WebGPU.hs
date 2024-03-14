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
    KernelName,
    KernelArg (..),
    HostCode,
    HostOp (..),
    KernelSafety (..),
    numFailureParams,
    FailureMsg (..),
    BlockDim,
    KernelConst (..),
    KernelConstExp,
    module Futhark.CodeGen.ImpCode,
    module Futhark.IR.GPU.Sizes,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.ImpCode.GPU (BlockDim, KernelConst (..), KernelConstExp)
import Futhark.IR.GPU.Sizes
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

-- | Something that can go wrong in a kernel.  Part of the machinery
-- for reporting error messages from within kernels.
data FailureMsg = FailureMsg
  { failureError :: ErrorMsg Exp,
    failureBacktrace :: String
  }

-- | A piece of code calling WebGPU.
type HostCode = Code HostOp

-- | The name of a kernel.
type KernelName = Name

-- | An argument to be passed to a kernel.
data KernelArg
  = -- | Pass the value of this scalar expression as argument.
    ValueKArg Exp PrimType
  | -- | Pass this pointer as argument.
    MemKArg VName
  deriving (Show)

-- | Whether a kernel can potentially fail (because it contains bounds
-- checks and such).
data MayFail = MayFail | CannotFail
  deriving (Show)

-- | Information about bounds checks and how sensitive it is to
-- errors.  Ordered by least demanding to most.
data KernelSafety
  = -- | Does not need to know if we are in a failing state, and also
    -- cannot fail.
    SafetyNone
  | -- | Needs to be told if there's a global failure, and that's it,
    -- and cannot fail.
    SafetyCheap
  | -- | Needs all parameters, may fail itself.
    SafetyFull
  deriving (Eq, Ord, Show)

-- | How many leading failure arguments we must pass when launching a
-- kernel with these safety characteristics.
numFailureParams :: KernelSafety -> Int
numFailureParams SafetyNone = 0
numFailureParams SafetyCheap = 1
numFailureParams SafetyFull = 3

-- | Host-level WebGPU operation.
data HostOp
  = LaunchKernel KernelSafety KernelName (Count Bytes (TExp Int64)) [KernelArg] [Exp] [BlockDim]
  | GetSize VName Name
  | CmpSizeLe VName Name Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

instance Pretty HostOp where
  pretty = pretty . show

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
