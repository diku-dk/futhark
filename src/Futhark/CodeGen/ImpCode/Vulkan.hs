-- | Imperative code with an Vulkan and SPIR-V component.
--
-- Apart from ordinary imperative code, this also carries around an
-- SPIR-V program as a Word32-array, as well as a list of entry points
-- defined by the SPIR-V program.
--
-- The imperative code has been augmented with a 'LaunchEntryPoint'
-- operation that allows one to execute an SPIR-V entry point.
module Futhark.CodeGen.ImpCode.Vulkan
       ( Program (..)
       , Function
       , FunctionT (Function)
       , Code
       , EntryPointArg (..)
       , SpecConstExp (..)
       , Vulkan (..)
       , Kernel.KernelConstExp
       , transposeGroupSize
       , SPIRV.EntryPointName
       , SPIRV.transposeEntryPointName
       , module Futhark.CodeGen.ImpCode
       )
       where
        
import qualified Data.Map.Strict as M

import Futhark.CodeGen.ImpCode hiding (Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.Kernels.Sizes
import qualified Futhark.CodeGen.ImpCode.Kernels as Kernel
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV

import Futhark.Util.Pretty

-- | An program calling SPIR-V entry-points using the Vulkan API.
data Program = Program { spirvShaders :: [SPIRV.SingleEntryShader]
                       , hostSizes :: M.Map VName (SizeClass, Name)
                       , hostFunctions :: Functions Vulkan
                       }

-- | A function calling SPIR-V entry points.
type Function = Imp.Function Vulkan

-- | A piece of code using the Vulkan API.
type Code = Imp.Code Vulkan

-- | An argument to be passed to a a SPIR-V entry point.
data EntryPointArg = ValueKArg Exp PrimType
                    -- ^ Pass the value of this scalar expression as argument.
                   | MemKArg VName
                    -- ^ Pass this pointer as argument.
                deriving (Show)

data SpecConstExp = SpecConstKernelExp VName Kernel.KernelConstExp
                  | SpecConstSizeExp DimSize
                  | SpecConstLocalMemExp (Either MemSize Kernel.KernelConstExp)
                  | SpecConstLockstepWidth
                deriving (Show)

-- | Host-level OpenCL operation.
data Vulkan = LaunchEntryPoint SPIRV.EntryPointName [EntryPointArg] [SpecConstExp] Exp
            | HostCode Code
            | GetSize VName VName
            | CmpSizeLe VName VName Exp
            | GetSizeMax VName SizeClass
            deriving (Show)

transposeGroupSize :: Num a => a
transposeGroupSize = 256

instance Pretty Vulkan where
  ppr = text . show
