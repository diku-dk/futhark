{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.CVulkan
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import Control.Monad
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.CodeGen.Backends.CVulkan.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.Vulkan
import qualified Futhark.CodeGen.ImpGen.Vulkan as ImpGen
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program spirv_code entry_points prog') ->
      Right <$> GC.compileProg operations (generateBoilerplate spirv_code entry_points)
                "#include <vulkan/vulkan.h>" [Space "device", Space "local", DefaultSpace]
                cliOptions prog'
  where operations :: GC.Operations Vulkan ()
        operations = GC.Operations
                     { GC.opsCompiler = callKernel
                     , GC.opsWriteScalar = writeVulkanScalar
                     , GC.opsReadScalar = readVulkanScalar
                     , GC.opsAllocate = allocateVulkanBuffer
                     , GC.opsDeallocate = deallocateVulkanBuffer
                     , GC.opsCopy = copyVulkanMemory
                     , GC.opsStaticArray = staticVulkanArray
                     , GC.opsMemoryType = vulkanMemoryType
                     , GC.opsFatMemory = True
                     }

cliOptions :: [Option]
cliOptions = [ Option { optionLongName = "dump-spirv"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_dump_program_to(cfg, optarg);|]
                      }
             , Option { optionLongName = "load-spirv"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
                      }
             ]

writeVulkanScalar :: GC.WriteScalar Vulkan ()
writeVulkanScalar mem i t "device" _ val = return ()
writeVulkanScalar _ _ _ space _ _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readVulkanScalar :: GC.ReadScalar Vulkan ()
readVulkanScalar mem i t "device" _ = pure [C.cexp|nullptr|] -- dummy type
readVulkanScalar _ _ _ space _ =
  fail $ "Cannot read from '" ++ space ++ "' memory space."

allocateVulkanBuffer :: GC.Allocate Vulkan ()
allocateVulkanBuffer mem size tag "device" = return ()
allocateVulkanBuffer _ _ _ "local" = return ()
allocateVulkanBuffer _ _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' space"

deallocateVulkanBuffer :: GC.Deallocate Vulkan ()
deallocateVulkanBuffer mem tag "device" = return ()
deallocateVulkanBuffer _ _ "local" = return ()
deallocateVulkanBuffer _ _ space =
  fail $ "Cannot deallocate in '" ++ space ++ "' space"

copyVulkanMemory :: GC.Copy Vulkan ()
copyVulkanMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes = return ()
copyVulkanMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes = return ()
copyVulkanMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes = return ()
copyVulkanMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes = return ()
copyVulkanMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

vulkanMemoryType :: GC.MemoryType Vulkan ()
vulkanMemoryType "device" = pure [C.cty|unsigned char|] -- dummy type
vulkanMemoryType "local" = pure [C.cty|unsigned char|] -- dummy type
vulkanMemoryType space =
  fail $ "Vulkan backend does not support '" ++ space ++ "' memory space."

staticVulkanArray :: GC.StaticArray Vulkan ()
staticVulkanArray name "device" t vs = return ()
staticVulkanArray _ space _ _ =
  fail $ "Vulkan backend cannot create static array in memory space '" ++ space ++ "'"

callKernel :: GC.OpCompiler Vulkan ()
callKernel (HostCode c) = GC.compileCode c
callKernel (LaunchEntryPoint name args kernel_size workgroup_size) = return ()
