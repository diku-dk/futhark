-- | Common definitions for imperative code augmented with the ability to launch
-- kernels.
module Futhark.CodeGen.ImpCode.Kernels
  ( KernelName,
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

import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.ImpCode.GPU (BlockDim, KernelConst (..), KernelConstExp)
import Futhark.IR.GPU.Sizes
import Futhark.Util.Pretty

-- | Something that can go wrong in a kernel.  Part of the machinery
-- for reporting error messages from within kernels.
data FailureMsg = FailureMsg
  { failureError :: ErrorMsg Exp,
    failureBacktrace :: String
  }

-- | A piece of code calling kernels.
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

-- | Host-level kernel operation.
data HostOp
  = LaunchKernel KernelSafety KernelName (Count Bytes (TExp Int64)) [KernelArg] [Exp] [BlockDim]
  | GetSize VName Name
  | CmpSizeLe VName Name Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

instance Pretty HostOp where
  pretty = pretty . show
