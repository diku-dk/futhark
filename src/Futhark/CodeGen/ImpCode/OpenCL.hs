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
    KernelName,
    KernelArg (..),
    CLCode,
    OpenCL (..),
    KernelSafety (..),
    numFailureParams,
    KernelTarget (..),
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
    hostDefinitions :: Definitions OpenCL
  }

-- | Something that can go wrong in a kernel.  Part of the machinery
-- for reporting error messages from within kernels.
data FailureMsg = FailureMsg
  { failureError :: ErrorMsg Exp,
    failureBacktrace :: String
  }

-- | A piece of code calling OpenCL.
type CLCode = Code OpenCL

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

-- | Host-level OpenCL operation.
data OpenCL
  = LaunchKernel KernelSafety KernelName (Count Bytes (TExp Int64)) [KernelArg] [Exp] [BlockDim]
  | GetSize VName Name
  | CmpSizeLe VName Name Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

-- | The target platform when compiling imperative code to a 'Program'
data KernelTarget
  = TargetOpenCL
  | TargetCUDA
  | TargetHIP
  deriving (Eq)

instance Pretty OpenCL where
  pretty = pretty . show
