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
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right vk_prog@(Program _ _ _ _ prog') ->
      Right <$> GC.compileProg operations (generateBoilerplate vk_prog)
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
writeVulkanScalar mem i t "device" _ val = fail "Write scalar not implemented."
writeVulkanScalar _ _ _ space _ _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readVulkanScalar :: GC.ReadScalar Vulkan ()
readVulkanScalar mem i t "device" _ = fail "Read scalar not implemented."
readVulkanScalar _ _ _ space _ =
  fail $ "Cannot read from '" ++ space ++ "' memory space."

allocateVulkanBuffer :: GC.Allocate Vulkan ()
allocateVulkanBuffer mem size tag "device" = 
  GC.stm [C.cstm|VULKAN_SUCCEED(vulkan_alloc(&ctx->vulkan, $exp:size, $exp:tag, &$exp:mem));|]
allocateVulkanBuffer _ _ _ "local" = return ()
allocateVulkanBuffer _ _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' space"

deallocateVulkanBuffer :: GC.Deallocate Vulkan ()
deallocateVulkanBuffer mem tag "device" = 
  GC.stm [C.cstm|vulkan_free(&ctx->vulkan, $exp:mem, $exp:tag);|]
deallocateVulkanBuffer _ _ "local" = return ()
deallocateVulkanBuffer _ _ space =
  fail $ "Cannot deallocate in '" ++ space ++ "' space"

copyVulkanMemory :: GC.Copy Vulkan ()
copyVulkanMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes = do
  mapped_mem <- newVName "mapped_src_memory"
  mem_t <- vulkanMemoryType "local"
  GC.decl [C.cdecl|$ty:mem_t *$id:mapped_mem;|]
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      VULKAN_SUCCEED(vkMapMemory(ctx->vulkan.device,
                                 $exp:srcmem.memory,
                                 0,
                                 VK_WHOLE_SIZE,
                                 0,
                                 (void *)&$id:mapped_mem));
    }|]
  GC.copyMemoryDefaultSpace destmem destidx [C.cexp|$id:mapped_mem|] srcidx nbytes
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      vkUnmapMemory(ctx->vulkan.device, $exp:srcmem.memory);
    }|]
copyVulkanMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes = do
  mapped_mem <- newVName "mapped_dest_memory"
  mem_t <- vulkanMemoryType "local"
  GC.decl [C.cdecl|$ty:mem_t *$id:mapped_mem;|]
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      VULKAN_SUCCEED(vkMapMemory(ctx->vulkan.device,
                                 $exp:destmem.memory,
                                 0,
                                 VK_WHOLE_SIZE,
                                 0,
                                 (void *)&$id:mapped_mem));
    }|]
  GC.copyMemoryDefaultSpace [C.cexp|$id:mapped_mem|] destidx srcmem srcidx nbytes
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      vkUnmapMemory(ctx->vulkan.device, $exp:destmem.memory);
    }|]
copyVulkanMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      typename VkBufferCopy buffer_copy;
      buffer_copy.srcOffset = $exp:srcidx;
      buffer_copy.dstOffset = $exp:destidx;
      buffer_copy.size      = $exp:nbytes;

      vkCmdCopyBuffer(ctx->vulkan.command_buffer,
                      $exp:srcmem.buffer,
                      $exp:destmem.buffer,
                      1,
                      &buffer_copy);
    }|]
copyVulkanMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyVulkanMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

vulkanMemoryType :: GC.MemoryType Vulkan ()
vulkanMemoryType "device" = pure [C.cty|struct vk_buffer_mem_pair|]
vulkanMemoryType "local" = pure [C.cty|unsigned char|] -- dummy type
vulkanMemoryType space =
  fail $ "Vulkan backend does not support '" ++ space ++ "' memory space."

staticVulkanArray :: GC.StaticArray Vulkan ()
staticVulkanArray name "device" t vs = fail "Static array not implemented."
staticVulkanArray _ space _ _ =
  fail $ "Vulkan backend cannot create static array in memory space '" ++ space ++ "'"

callKernel :: GC.OpCompiler Vulkan ()
callKernel (GetSize v key) = 
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key;|]
callKernel (CmpSizeLe v key x) = do
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key <= $exp:x';|]
  GC.stm [C.cstm|if (ctx->logging) {
    fprintf(stderr, "Compared %s <= %d.\n", $string:(pretty key), $exp:x');
    }|]
callKernel (GetSizeMax v size_class) =
  GC.stm [C.cstm|$id:v = ctx->opencl.$id:("max_" ++ pretty size_class);|]
callKernel (HostCode c) = GC.compileCode c
callKernel (LaunchEntryPoint name desc_bind args kernel_size workgroup_size) = do
  pipeline <- createPipeline name desc_bind
  -- Use pipeline here
  destroyPipeline pipeline

createPipeline :: VName -> Int -> GC.CompilerM op s VName
createPipeline entry_point entry_i = do
  pipeline                           <- newVName "vulkan_pipeline"
  compute_pipeline_create_info       <- newVName "vulkan_compute_pipeline_create_info"
  GC.decl [C.cdecl|typename VkPipeline $id:pipeline;|]
  GC.stm [C.cstm|{
      typename VkComputePipelineCreateInfo $id:compute_pipeline_create_info;
      $id:compute_pipeline_create_info.sType = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO;
      $id:compute_pipeline_create_info.pNext = 0;
      $id:compute_pipeline_create_info.flags = 0;
      $id:compute_pipeline_create_info.layout = ctx->vulkan.pipeline_layouts[$int:entry_i];
      $id:compute_pipeline_create_info.basePipelineHandle = 0;
      $id:compute_pipeline_create_info.basePipelineIndex = 0;
      $id:compute_pipeline_create_info.stage.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
      $id:compute_pipeline_create_info.stage.pNext = 0;
      $id:compute_pipeline_create_info.stage.flags = 0;
      $id:compute_pipeline_create_info.stage.stage = VK_SHADER_STAGE_COMPUTE_BIT;
      $id:compute_pipeline_create_info.stage.$id:("module") = ctx->vulkan.shader_module;
      $id:compute_pipeline_create_info.stage.pName = $string:(SPIRV.entryPointName entry_point);
      $id:compute_pipeline_create_info.stage.pSpecializationInfo = 0; // TODO Set spec consts here

      VULKAN_SUCCEED(vkCreateComputePipelines(ctx->vulkan.device,
                                              0,
                                              1,
                                              &$id:compute_pipeline_create_info,
                                              0,
                                              &$id:pipeline));
    }|]
  return pipeline

destroyPipeline :: VName -> GC.CompilerM op s ()
destroyPipeline pipeline =
  GC.stm [C.cstm|vkDestroyPipeline(ctx->vulkan.device, $id:pipeline, 0);|]

