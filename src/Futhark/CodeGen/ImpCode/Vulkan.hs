-- | Imperative code with an Vulkan and SPIR-V component.
--
-- Apart from ordinary imperative code, this also carries around an
-- SPIR-V program as a Word32-array, as well as a list of entry points
-- defined by the SPIR-V program.
--
-- The imperative code has been augmented with a 'LaunchEntryPoint'
-- operation that allows one to execute an SPIR-V entry point.
module Futhark.CodeGen.ImpCode.Vulkan
  ( SingleEntryShader(..),
    Program (..),
    Function,
    FunctionT (Function),
    Code,
    EntryPointArg (..),
    SpecConstExp (..),
    Vulkan (..),
    Kernel.KernelConstExp,
    module Futhark.CodeGen.ImpCode,
  )
where

import qualified Data.Map.Strict as M
import Futhark.CodeGen.ImpCode hiding (Code, Function)
import qualified Futhark.CodeGen.ImpCode as Imp
import qualified Futhark.CodeGen.ImpCode.Kernels as Kernel
import Futhark.IR.Kernels.Sizes
import Futhark.Util.Pretty

data SingleEntryShader = SEShader
  { shaderEntryPoint :: String,
    shaderDescriptorSetSize :: Int,
    shaderCode :: [Word32]
  }

-- | An program calling SPIR-V entry-points using the Vulkan API.
data Program = Program
  { spirvShaders :: [SingleEntryShader],
    hostSizes :: M.Map Name SizeClass,
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
  = -- For constants
    SpecConstKernelExp VName Kernel.KernelConstExp
  | SpecConstExp Exp
  | -- For workgroup size: spec ids (0, 1, 2)
    SpecConstSizeExp DimSize
  | -- LocalAlloc. NB: Count is elements, not bytes
    SpecConstLocalMemExp (Count Elements Exp)
  deriving (Show)

-- | Host-level OpenCL operation.
data Vulkan
  = LaunchEntryPoint String [EntryPointArg] [SpecConstExp] (Exp, Exp, Exp)
  | GetSize VName Name
  | CmpSizeLe VName Name Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

instance Pretty Vulkan where
  ppr = text . show
