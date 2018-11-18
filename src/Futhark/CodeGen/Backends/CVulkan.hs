{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.CVulkan
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Control.Monad.Identity

import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.CodeGen.Backends.CVulkan.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.ImpCode.Kernels as Kernel
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.Vulkan
import qualified Futhark.CodeGen.ImpGen.Vulkan as ImpGen
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right vk_prog@(Program _ _ prog') ->
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
writeVulkanScalar mem i t "device" _ val = do
  mapped_val <- newVName "mapped_val"
  GC.decl [C.cdecl|$ty:t *$id:mapped_val;|]
  GC.stm [C.cstm|vulkan_sync_mem(&ctx->vulkan, &$exp:mem);|]
  GC.stm [C.cstm|VULKAN_SUCCEED(vkMapMemory(ctx->vulkan.device,
                                            $exp:mem.memory,
                                            $exp:i,
                                            sizeof($ty:t),
                                            0,
                                            (void *)&$id:mapped_val));
                |]
  GC.stm [C.cstm|*$id:mapped_val = $exp:val;|]
  GC.stm [C.cstm|vkUnmapMemory(ctx->vulkan.device, $exp:mem.memory);|]
writeVulkanScalar _ _ _ space _ _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readVulkanScalar :: GC.ReadScalar Vulkan ()
readVulkanScalar mem i t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t *$id:val;|]
  GC.stm [C.cstm|vulkan_sync_mem(&ctx->vulkan, &$exp:mem);|]
  GC.stm [C.cstm|VULKAN_SUCCEED(vkMapMemory(ctx->vulkan.device,
                                            $exp:mem.memory,
                                            $exp:i,
                                            sizeof($ty:t),
                                            0,
                                            (void *)&$id:val));
                |]
  GC.stm [C.cstm|vkUnmapMemory(ctx->vulkan.device, $exp:mem.memory);|]
  return [C.cexp|*$id:val|]
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
      vulkan_sync_mem(&ctx->vulkan, &$exp:srcmem);
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
      vulkan_sync_mem(&ctx->vulkan, &$exp:destmem);
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
      
      vulkan_copy_buffers(&ctx->vulkan, buffer_copy, &$exp:srcmem, &$exp:destmem);
      
      if (ctx->debugging) {
        VULKAN_SUCCEED(vkQueueWaitIdle(ctx->vulkan.queue));
      }
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
staticVulkanArray name "device" t vs = do
  let ct = GC.primTypeToCType t
      vs' = [[C.cinit|$exp:(GC.compilePrimValue v)|] | v <- vs]
      num_elems = length vs
  mapped_mem <- newVName "mapped_src_memory"
  mem_t <- vulkanMemoryType "local"
  name_realtype <- newVName $ baseString name ++ "_realtype"
  GC.libDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:num_elems] = {$inits:vs'};|]
  -- Fake a memory block.
  GC.contextField (pretty name) [C.cty|struct memblock_device|] Nothing
  -- During startup, copy the data to where we need it.
  GC.atInit [C.cstm|{
    ctx->$id:name.references = NULL;
    ctx->$id:name.size = 0;
    VULKAN_SUCCEED(vulkan_alloc(&ctx->vulkan,
                                ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct),
                                $string:(pretty name),
                                &ctx->$id:name.mem));
    if ($int:num_elems > 0) {
      $ty:mem_t *$id:mapped_mem;
      VULKAN_SUCCEED(vkMapMemory(ctx->vulkan.device,
                                 ctx->$id:name.mem.memory,
                                 0,
                                 VK_WHOLE_SIZE,
                                 0,
                                 (void *)&$id:mapped_mem));
      memcpy($id:mapped_mem, &$id:name_realtype, $int:num_elems*sizeof($ty:ct));
      vkUnmapMemory(ctx->vulkan.device, ctx->$id:name.mem.memory);
    }
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]
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
callKernel (LaunchEntryPoint name args spec_consts workgroup_count) = do
  cmd_buffer_id <- newVName "cmd_buffer_id"
  scalar_buffers <- newVName "scalar_buffers"
  scalar_sizes <- newVName "scalar_sizes"
  write_desc_set <- newVName "desc_write_set"
  pipeline <- newVName "pipeline"
  spec_data_struct <- newVName "spec_data_struct"
  spec_data <- newVName "spec_data"
  spec_entries <- newVName "spec_entries"
  spec_info <- newVName "spec_info"
  
  GC.decl [C.cdecl|typename uint32_t $id:cmd_buffer_id;|]
  GC.stm [C.cstm|VULKAN_SUCCEED(vulkan_available_command_buffer(&ctx->vulkan,
                                                                &$id:cmd_buffer_id));
         |]
  let scalar_sizes_inits = [ [C.cinit|sizeof($ty:t)|] | t <- filterScalarTypes args ]
  GC.decl [C.cdecl|size_t $id:scalar_sizes[] = { $inits:scalar_sizes_inits };|]
  GC.decl [C.cdecl|struct vk_buffer_mem_pair *$id:scalar_buffers =
                      vulkan_alloc_scalars(&ctx->vulkan,
                                           $id:cmd_buffer_id,
                                           $int:(length scalar_sizes_inits),
                                           $id:scalar_sizes);
         |]
  buffers <- getArgBuffers args scalar_buffers 0
  buffer_infos <- mapM initBufferInfo buffers
  workgroup_count' <- GC.compileExp workgroup_count
  let buffer_count = length buffer_infos
      wd_inits = zipWith (writeDescriptorInits name) buffer_infos [0..buffer_count - 1]
      spec_const_exps = map specConstToExp spec_consts
      spec_const_types = map specConstToType spec_consts
      spec_const_s_names = [ "data" ++ show i | i <- [0..length spec_consts - 1] ]
      spec_const_s_mems = zipWith (\t n -> [C.csdecl|$ty:t $id:n;|]) spec_const_types spec_const_s_names
      spec_const_exp_inits = [ [C.cinit|$exp:e|] | e <- spec_const_exps ]
      spec_map_entry_inits = specMapEntry spec_data_struct spec_data spec_const_s_names
      total_size = [C.cexp|sizeof($id:spec_data)|]
  mapM_ (memArgAcquireOwnership cmd_buffer_id) args
  GC.decl [C.cdecl|struct $id:spec_data_struct { $sdecls:spec_const_s_mems }
                          $id:spec_data = { $inits:spec_const_exp_inits };
          |]
  GC.decl [C.cdecl|const typename VkSpecializationMapEntry $id:spec_entries[]
                      = { $inits:spec_map_entry_inits };
          |]
  GC.decl [C.cdecl|const typename VkSpecializationInfo $id:spec_info = {
                      $int:(length spec_consts),
                      $id:spec_entries,
                      $exp:total_size,
                      &$id:spec_data
                    };
          |]
  GC.decl [C.cdecl|typename VkPipeline $id:pipeline;|]
  GC.stm [C.cstm|vulkan_create_pipeline(&ctx->vulkan,
                                        &ctx->$id:(shaderCtx name),
                                        $id:spec_info,
                                        &$id:pipeline);|]
  GC.decl [C.cdecl|const typename VkWriteDescriptorSet $id:write_desc_set[]
                      = {$inits:wd_inits};
          |]
  GC.stm [C.cstm|vkUpdateDescriptorSets(ctx->vulkan.device,
                                        $int:buffer_count,
                                        $id:write_desc_set,
                                        0,
                                        0);
         |]
  GC.stm [C.cstm|vulkan_dispatch(&ctx->vulkan,
                                 &ctx->$id:(shaderCtx name),
                                 $id:cmd_buffer_id,
                                 $id:pipeline,
                                 $exp:workgroup_count');|]
  --GC.stm [C.cstm|vulkan_destroy_pipeline(&ctx->vulkan, $id:pipeline);|] TODO: Destroy on sync (Remember to sync and destroy on clean)

specConstToExp :: SpecConstExp -> C.Exp
specConstToExp (SpecConstSizeExp ds)             = GC.dimSizeToExp ds
specConstToExp SpecConstLockstepWidth            = [C.cexp|ctx->vulkan.lockstep_width|]
specConstToExp (SpecConstKernelExp vn _)         = [C.cexp|$id:vn|]
specConstToExp (SpecConstLocalMemExp (Left ms))  = GC.dimSizeToExp ms
specConstToExp (SpecConstLocalMemExp (Right kc)) = 
  runIdentity $ GC.compilePrimExp compileKernelConst kc
  where compileKernelConst (Kernel.SizeConst key) = return [C.cexp|$id:(pretty key)|]

specConstToType :: SpecConstExp -> C.Type
specConstToType SpecConstSizeExp{}       = [C.cty|typename int32_t|]
specConstToType SpecConstLocalMemExp{}   = [C.cty|typename int32_t|]
specConstToType SpecConstLockstepWidth   = [C.cty|typename int32_t|]
specConstToType (SpecConstKernelExp _ kce) =
  [C.cty|$ty:(GC.primTypeToCType $ primExpType kce)|]

specMapEntry :: VName -> VName -> [String] -> [C.Initializer]
specMapEntry s d ns = zipWith mapEntry [0..length ns - 1] ns
  where mapEntry i n = [C.cinit|{$int:i, offsetof($esc:("struct " ++ pretty s), $id:n), sizeof($id:d.$id:n)}|]

filterScalarTypes :: [EntryPointArg] -> [C.Type]
filterScalarTypes []                     = []
filterScalarTypes (ValueKArg _ t : args) = GC.primTypeToCType t : filterScalarTypes args
filterScalarTypes (_:args)               = filterScalarTypes args

getArgBuffers :: [EntryPointArg] -> VName -> Int -> GC.CompilerM op s [C.Exp]
getArgBuffers [] _ _                    = return []
getArgBuffers (a:args) scalar_buffers i = do
  (i', a_e) <- getArgBuffer a scalar_buffers i
  arg_es <- getArgBuffers args scalar_buffers i'
  return $ a_e : arg_es

getArgBuffer :: EntryPointArg -> VName -> Int -> GC.CompilerM op s (Int, C.Exp)
getArgBuffer (MemKArg bname) _ i               = return (i, [C.cexp|$id:bname.mem.buffer|])
getArgBuffer (ValueKArg e bt) scalar_buffers i = do
  v <- GC.compileExpToName "value_arg" bt e
  mapped_mem <- newVName "mapped_memory"
  GC.decl [C.cdecl|char *$id:mapped_mem;|]
  GC.stm [C.cstm|VULKAN_SUCCEED(vkMapMemory(ctx->vulkan.device,
                                            $id:scalar_buffers[$int:i].memory,
                                            0,
                                            VK_WHOLE_SIZE,
                                            0,
                                            (void *)&$id:mapped_mem));
          |]
  GC.copyMemoryDefaultSpace [C.cexp|$id:mapped_mem|] [C.cexp|0|] [C.cexp|&$id:v|] [C.cexp|0|] [C.cexp|sizeof($id:v)|]
  GC.stm [C.cstm|vkUnmapMemory(ctx->vulkan.device, $id:scalar_buffers[$int:i].memory);|]
  return (i + 1, [C.cexp|$id:scalar_buffers[$int:i].buffer|])

initBufferInfo :: C.Exp -> GC.CompilerM op s VName
initBufferInfo bexp = do
  buffer_info <- newVName "buffer_info"
  GC.decl [C.cdecl|typename VkDescriptorBufferInfo $id:buffer_info;|]
  GC.stm [C.cstm|$id:buffer_info.buffer = $exp:bexp;|]
  GC.stm [C.cstm|$id:buffer_info.offset = 0;|]
  GC.stm [C.cstm|$id:buffer_info.range = VK_WHOLE_SIZE;|]
  return buffer_info

writeDescriptorInits :: VName -> VName -> Int -> C.Initializer
writeDescriptorInits fname biname i = [C.cinit|
    {
      VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
      0,
      ctx->$id:(shaderCtx fname).descriptor_set,
      $int:i,
      0,
      1,
      VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
      0,
      &$id:biname,
      0
    }
  |]

memArgAcquireOwnership :: VName -> EntryPointArg -> GC.CompilerM op s ()
memArgAcquireOwnership cmd_buffer_id (MemKArg bname) = GC.stm
  [C.cstm|vulkan_acquire_ownership(&ctx->vulkan, &$id:bname.mem, $id:cmd_buffer_id);|]
memArgAcquireOwnership _ _                           = return ()
