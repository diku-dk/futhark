{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.CVulkan.Boilerplate
  ( generateBoilerplate
  ) where

import Data.Word
import Data.FileEmbed
import Data.List
import qualified Data.Map as M
import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.CodeGen.ImpCode.Vulkan
import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV

generateBoilerplate :: Program -> GC.CompilerM Vulkan () ()
generateBoilerplate (Program spirv_code entry_points desc_sets sizes _) = do
  final_inits <- GC.contextFinalInits
  
  -- Define buffer structure in header
  GC.headerDecl GC.InitDecl [C.cedecl|struct vk_buffer_mem_pair {
                                          typename VkBuffer buffer;
                                          typename VkDeviceMemory memory;
                                          size_t size;
                                        };|]

  let entry_point_names  = map SPIRV.entryPointName entry_points
      vulkan_boilerplate = vulkanBoilerplate spirv_code
      vulkan_ctx_fields  = vulkanCtxFields entry_point_names
      vulkan_ctx_inits   = vulkanCtxInits entry_point_names
      desc_layout_create = vulkanCreateDescriptorSetLayouts desc_sets
      pipe_layout_create = vulkanCreatePipelineLayouts entry_points desc_sets
      desc_pool_init     = vulkanCreateDescriptorPool desc_sets
      size_name_inits = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_class_inits = map (\(c,_) -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes
      size_entry_points_inits = map (\(_,e) -> [C.cinit|$string:(pretty e)|]) $ M.elems sizes
  GC.earlyDecls vulkan_boilerplate

  GC.libDecl [C.cedecl|static const char *size_names[] = { $inits:size_name_inits };|]
  GC.libDecl [C.cedecl|static const char *size_classes[] = { $inits:size_class_inits };|]
  GC.libDecl [C.cedecl|static const char *size_entry_points[] = { $inits:size_entry_points_inits };|]

  GC.publicDef_ "get_num_sizes" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(void);|],
     [C.cedecl|int $id:s(void) {
                return $int:(M.size sizes);
              }|])

  GC.publicDef_ "get_size_name" GC.InitDecl $ \s ->
    ([C.cedecl|const char* $id:s(int);|],
     [C.cedecl|const char* $id:s(int i) {
                return size_names[i];
              }|])

  GC.publicDef_ "get_size_class" GC.InitDecl $ \s ->
    ([C.cedecl|const char* $id:s(int);|],
     [C.cedecl|const char* $id:s(int i) {
                return size_classes[i];
              }|])

  GC.publicDef_ "get_size_entry" GC.InitDecl $ \s ->
    ([C.cedecl|const char* $id:s(int);|],
     [C.cedecl|const char* $id:s(int i) {
                return size_entry_points[i];
              }|])
            
  let size_decls = map (\k -> [C.csdecl|size_t $id:k;|]) $ M.keys sizes
  GC.libDecl [C.cedecl|struct sizes { $sdecls:size_decls };|]
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s { struct vulkan_config vulkan;
                              size_t sizes[$int:(M.size sizes)];
                            };|])
     
  let size_value_inits = map (\i -> [C.cstm|cfg->sizes[$int:i] = 0;|]) [0..M.size sizes-1]
  
  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:cfg* $id:s(void);|],
     [C.cedecl|struct $id:cfg* $id:s(void) {
                         struct $id:cfg *cfg = malloc(sizeof(struct $id:cfg));
                         if (cfg == NULL) {
                           return NULL;
                         }
                         
                         vulkan_config_init(&cfg->vulkan, $int:(M.size sizes),
                                            size_names, cfg->sizes, size_classes);
                         $stms:size_value_inits

                         return cfg;
                       }|])

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         free(cfg);
                       }|])

  GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->vulkan.logging = cfg->vulkan.debugging = flag;
                       }|])

  GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->vulkan.logging = flag;
                       }|])

  GC.publicDef_ "context_config_dump_program_to" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->vulkan.dump_program_to = path;
                       }|])

  GC.publicDef_ "context_config_load_program_from" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->vulkan.load_program_from = path;
                       }|])

  GC.publicDef_ "context_config_set_size" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value);|],
     [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value) {

                         for (int i = 0; i < $int:(M.size sizes); i++) {
                           if (strcmp(size_name, size_names[i]) == 0) {
                             cfg->sizes[i] = size_value;
                             return 0;
                           }
                         }
                         return 1;
                       }|])

  (fields, init_fields) <- GC.contextContents
  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s {
                         int detail_memory;
                         int debugging;
                         int logging;
                         typename lock_t lock;
                         char *error;
                         $sdecls:fields
                         $sdecls:vulkan_ctx_fields
                         struct vulkan_context vulkan;
                         struct sizes sizes;
                       };|])

  GC.libDecl [C.cedecl|static void init_context_early(struct $id:cfg *cfg, struct $id:ctx* ctx) {
                     ctx->vulkan.cfg = cfg->vulkan;
                     ctx->detail_memory = cfg->vulkan.debugging;
                     ctx->debugging = cfg->vulkan.debugging;
                     ctx->logging = cfg->vulkan.logging;
                     ctx->error = NULL;
                     create_lock(&ctx->lock);

                     $stms:init_fields
                     $stms:vulkan_ctx_inits
                  }|]

                  
  let set_sizes = zipWith (\i k -> [C.cstm|ctx->sizes.$id:k = cfg->sizes[$int:i];|])
                          [(0::Int)..] $ M.keys sizes

  GC.libDecl [C.cedecl|static void init_context_late(struct $id:cfg *cfg, struct $id:ctx* ctx) {
                     $stms:desc_layout_create
                     $stms:pipe_layout_create
                     $stm:desc_pool_init

                     $stms:final_inits

                     $stms:set_sizes
                  }|]

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                          struct $id:ctx* ctx = malloc(sizeof(struct $id:ctx));
                          if (ctx == NULL) {
                            return NULL;
                          }

                          init_context_early(cfg, ctx);
                          setup_vulkan(&ctx->vulkan,
                                       $int:(M.size desc_sets),
                                       spirv_shader,
                                       sizeof(spirv_shader));
                          init_context_late(cfg, ctx);
                          return ctx;
                       }|])

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 free_lock(&ctx->lock);
                                 vulkan_cleanup(&ctx->vulkan);
                                 free(ctx);
                               }|])

  GC.publicDef_ "context_sync" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                         VULKAN_SUCCEED(vkQueueWaitIdle(ctx->vulkan.queue));
                         return 0;
                       }|])

  GC.publicDef_ "context_get_error" GC.InitDecl $ \s ->
    ([C.cedecl|char* $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|char* $id:s(struct $id:ctx* ctx) {
                         char* error = ctx->error;
                         ctx->error = NULL;
                         return error;
                       }|])

  GC.publicDef_ "context_clear_caches" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                         vulkan_free_all(&ctx->vulkan);
                         return 0;
                       }|])

  GC.publicDef_ "context_get_device_queue" GC.InitDecl $ \s ->
    ([C.cedecl|typename VkQueue $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|typename VkQueue $id:s(struct $id:ctx* ctx) {
                 return ctx->vulkan.queue;
               }|])

  return () -- Report

vulkanBoilerplate :: [Word32] -> [C.Definition]
vulkanBoilerplate spirv_code = [C.cunit|
    $esc:("typedef struct vk_buffer_mem_pair fl_entry_mem;")
    $esc:freelist_h
    $esc:vulkan_h
    $esc:("const uint32_t spirv_shader[] = " ++ shader ++ ";")
  |]
  where freelist_h = $(embedStringFile "rts/c/free_list.h")
        vulkan_h = $(embedStringFile "rts/c/vulkan.h")
        shader = "{" ++ intercalate "," (map show spirv_code) ++ "}"


vulkanCtxFields :: [String] -> [C.FieldGroup]
vulkanCtxFields entry_point_names =
  [ [C.csdecl|int total_runs;|],
    [C.csdecl|long int total_runtime;|] ] ++
  concat
  [ [ [C.csdecl|int $id:(entryPointRuntime name);|]
    , [C.csdecl|int $id:(entryPointRuns name);|]
    ]
  | name <- entry_point_names ]

vulkanCtxInits :: [String] -> [C.Stm]
vulkanCtxInits entry_point_names =
  [ [C.cstm|ctx->total_runs = 0;|],
    [C.cstm|ctx->total_runtime = 0;|] ] ++
  concat
  [ [ [C.cstm|ctx->$id:(entryPointRuntime name) = 0;|]
    , [C.cstm|ctx->$id:(entryPointRuns name) = 0;|]
    ]
  | name <- entry_point_names ]

vulkanCreateDescriptorSetLayout :: Int -> SPIRV.DescriptorSet -> C.Stm
vulkanCreateDescriptorSetLayout map_i descs = [C.cstm|{
    typename VkDescriptorSetLayoutBinding desc_set_layout_bindings[$int:(length descs)];
    $stms:binding_inits

    typename VkDescriptorSetLayoutCreateInfo desc_set_layout_create_info;
    desc_set_layout_create_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    desc_set_layout_create_info.pNext = 0;
    desc_set_layout_create_info.flags = 0;
    desc_set_layout_create_info.bindingCount = $int:(length descs);
    desc_set_layout_create_info.pBindings = desc_set_layout_bindings;

    VULKAN_SUCCEED(vkCreateDescriptorSetLayout(ctx->vulkan.device,
                                               &desc_set_layout_create_info,
                                               0,
                                               &ctx->vulkan.descriptor_set_layouts[$int:map_i]));
  }|]
  where binding_inits = concat
          [ [ [C.cstm|desc_set_layout_bindings[$int:i].binding = $int:i;|]
            , [C.cstm|desc_set_layout_bindings[$int:i].descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;|]
            , [C.cstm|desc_set_layout_bindings[$int:i].descriptorCount = 1;|]
            , [C.cstm|desc_set_layout_bindings[$int:i].stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;|]
            , [C.cstm|desc_set_layout_bindings[$int:i].pImmutableSamplers = 0;|]
            ]
          | i <- [0..length descs - 1]]

vulkanCreateDescriptorSetLayouts :: SPIRV.DescriptorSetMap -> [C.Stm]
vulkanCreateDescriptorSetLayouts desc_map =
  let descs = M.elems desc_map
  in zipWith vulkanCreateDescriptorSetLayout [0..M.size desc_map - 1] descs

vulkanCreateDescriptorPool :: SPIRV.DescriptorSetMap -> C.Stm
vulkanCreateDescriptorPool desc_map =
  [C.cstm|VULKAN_SUCCEED(vulkan_init_descriptor_pool(&ctx->vulkan, $int:total_desc_count));|]
  where total_desc_count = sum $ map length $ M.elems desc_map

vulkanCreatePipelineLayout :: Int -> Int -> C.Stm
vulkanCreatePipelineLayout entry_i desc_set_i = [C.cstm|{
    typename VkPipelineLayoutCreateInfo pipeline_layout_create_info;
    pipeline_layout_create_info.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pipeline_layout_create_info.pNext = 0;
    pipeline_layout_create_info.flags = 0;
    pipeline_layout_create_info.setLayoutCount = 1;
    pipeline_layout_create_info.pSetLayouts = &ctx->vulkan.descriptor_set_layouts[$int:desc_set_i];
    pipeline_layout_create_info.pushConstantRangeCount = 0;
    pipeline_layout_create_info.pPushConstantRanges = 0;

    VULKAN_SUCCEED(vkCreatePipelineLayout(ctx->vulkan.device,
                                          &pipeline_layout_create_info,
                                          0,
                                          &ctx->vulkan.pipeline_layouts[$int:entry_i]));
  }|]

vulkanCreatePipelineLayouts :: [VName] -> SPIRV.DescriptorSetMap -> [C.Stm]
vulkanCreatePipelineLayouts entry_points desc_map =
  let indexed_create entry_i entry = vulkanCreatePipelineLayout entry_i $ M.findIndex entry desc_map
  in zipWith indexed_create [0..length entry_points - 1] entry_points

entryPointRuntime :: String -> String
entryPointRuntime = (++"_total_runtime")

entryPointRuns :: String -> String
entryPointRuns = (++"_runs")