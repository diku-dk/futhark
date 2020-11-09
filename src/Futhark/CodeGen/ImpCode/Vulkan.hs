-- | Imperative code with an Vulkan and SPIR-V component.
--
-- Apart from ordinary imperative code, this also carries around an
-- SPIR-V program as a Word32-array, as well as a list of entry points
-- defined by the SPIR-V program.
--
-- The imperative code has been augmented with a 'LaunchEntryPoint'
-- operation that allows one to execute an SPIR-V entry point.
module Futhark.CodeGen.ImpCode.Vulkan
  ( Program (..),
    Function,
    FunctionT (Function),
    Code,
    EntryPointArg (..),
    SpecConstExp (..),
    WorkGroups,
    Vulkan (..),
    Kernel.KernelConstExp,
    SPIRV.EntryPointName,
    module Futhark.CodeGen.ImpCode,
  )
where

import qualified Data.Map.Strict as M
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV
import Futhark.CodeGen.ImpCode hiding (Code, Function)
import qualified Futhark.CodeGen.ImpCode as Imp
import qualified Futhark.CodeGen.ImpCode.Kernels as Kernel
import Futhark.IR.Kernels.Sizes
import Futhark.Util.Pretty

-- | An program calling SPIR-V entry-points using the Vulkan API.
data Program = Program
  { spirvShaders :: [SPIRV.SingleEntryShader],
    hostSizes :: M.Map Name (SizeClass, Name),
    hostDefinitions :: Definitions Vulkan
  }

-- | A function calling SPIR-V entry points.
type Function = Imp.Function Vulkan

-- | A piece of code using the Vulkan API.
type Code = Imp.Code Vulkan

-- | An argument to be passed to a a SPIR-V entry point.
data EntryPointArg
  = -- | Pass the value of this scalar expression as argument.
    ValueKArg Exp PrimType
  | -- | Pass this pointer as argument.
    MemKArg VName
  deriving (Show)

-- | The expressions of specialization constants of a shader
data SpecConstExp
  = SpecConstKernelExp VName Kernel.KernelConstExp
  | SpecConstSizeExp DimSize
  | SpecConstExp Exp
  | SpecConstLocalMemExp Kernel.KernelConstExp
  | SpecConstLockstepWidth
  deriving (Show)

-- | Work group dimensions of an entry point
type WorkGroups = (Exp, Exp, Exp)

-- | Host-level OpenCL operation.
data Vulkan
  = LaunchEntryPoint SPIRV.EntryPointName [EntryPointArg] [SpecConstExp] WorkGroups
  | HostCode Code
  | GetSize VName Name
  | CmpSizeLe VName Name Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

instance Pretty Vulkan where
  ppr = text . show
