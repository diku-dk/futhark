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
       , EntryPointName
       , EntryPointArg (..)
       , Vulkan (..)
       , module Futhark.CodeGen.ImpCode
       )
       where

import Futhark.CodeGen.ImpCode hiding (Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp

import Futhark.Util.Pretty

-- | An program calling SPIR-V entry-points using the Vulkan API.
data Program = Program { spirvProgram :: [Word32]
                       , spirvEntryPoints :: [EntryPointName]
                       , hostFunctions :: Functions Vulkan
                       }

-- | A function calling SPIR-V entry points.
type Function = Imp.Function Vulkan

-- | A piece of code using the Vulkan API.
type Code = Imp.Code Vulkan

-- | The name of an entry point.
type EntryPointName = String

-- | An argument to be passed to a a SPIR-V entry point.
data EntryPointArg = ValueKArg Exp PrimType
                      -- ^ Pass the value of this scalar expression as argument.
                   | MemKArg VName
                    -- ^ Pass this pointer as argument.
                   | SharedMemoryKArg (Count Bytes)
                    -- ^ Create this much local memory per workgroup.
                deriving (Show)

-- | Host-level OpenCL operation.
data Vulkan = LaunchEntryPoint EntryPointName [EntryPointArg] [Exp] [Exp]
            | HostCode Code
            deriving (Show)

instance Pretty Vulkan where
  ppr = text . show
