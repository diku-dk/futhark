-- | Imperative code with an OpenGL component.
module Futhark.CodeGen.ImpCode.OpenGL
       ( Program (..)
       , Function
       , FunctionT (Function)
       , Code
       , ShaderName
       , ShaderArg (..)
       , OpenGL (..)
       , Safety (..)
       , module Futhark.CodeGen.ImpCode
       , module Futhark.Representation.Kernels.Sizes
       )
       where

import qualified Data.Map as M

import Futhark.CodeGen.ImpCode hiding (Function, Code)
import Futhark.Representation.Kernels.Sizes
import qualified Futhark.CodeGen.ImpCode as Imp

import Futhark.Util.Pretty

-- | A program calling OpenGL shaders.
data Program = Program { openGlProgram :: String
                       , openGlPrelude :: String
                         -- ^ Must be prepended to the program.
                       , openGlShaderNames :: M.Map ShaderName Safety
                       , openGlUsedTypes   :: [PrimType]
                       , openGlSizes       :: M.Map Name SizeClass
                         -- ^ Runtime-configurable constants.
                       , hostFunctions :: Functions OpenGL
                       }

-- | A function calling OpenGL kernels.
type Function = Imp.Function OpenGL

-- | A piece of code calling OpenGL.
type Code = Imp.Code OpenGL

-- | The name of a kernel.
type ShaderName = String

-- | An argument to be passed to a kernel.
data ShaderArg = ValueKArg Exp PrimType
                 -- ^ Pass the value of this scalar expression as argument.
               | MemKArg VName
                 -- ^ Pass this pointer as argument.
               | SharedMemoryKArg (Count Bytes Exp)
                 -- ^ Create this much local memory per workgroup.
               deriving (Show)

-- | Information about bounds checks and how sensitive it is to
-- errors.  Ordered by least demanding to most.
data Safety
  = SafetyNone
    -- ^ Does not need to know if we are in a failing state, and also
    -- cannot fail.
  | SafetyCheap
    -- ^ Needs to be told if there's a global failure, and that's it,
    -- and cannot fail.
  | SafetyFull
    -- ^ Needs all parameters, may fail itself.
    deriving (Eq, Ord, Show)

-- | Host-level OpenGL operation.
data OpenGL = LaunchShader Safety ShaderName [ShaderArg] [Exp] [Exp]
            | GetSize VName Name
            | CmpSizeLe VName Name Exp
            | GetSizeMax VName SizeClass
            deriving (Show)

instance Pretty OpenGL where
  ppr = text . show
