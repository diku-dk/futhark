{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.CVulkan.Boilerplate
  ( generateBoilerplate
  , entryPointRuntime
  , entryPointRuns
  , entryPointPipeCacheMiss
  , shaderCodeName
  , shaderCtx
  ) where

import Data.FileEmbed
import qualified Data.Map as M
import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.CodeGen.ImpCode.Vulkan
import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV

generateBoilerplate :: Program -> GC.CompilerM Vulkan () ()
generateBoilerplate (Program shaders sizes _) = do
  final_inits <- GC.contextFinalInits
  ctx_cleanup <- GC.contextCleanup
  
  -- Define buffer structure in header
  GC.headerDecl GC.InitDecl [C.cedecl|struct vk_buffer_mem_pair {
                                          typename VkBuffer buffer;
                                          typename VkDeviceMemory memory;
                                          size_t size;
                                          typename uint32_t owner_index;
                                          typename uint8_t owned;
                                        };|]

  let entry_point_names       = map SPIRV.shaderEntryPoint shaders
      vulkan_boilerplate      = vulkanBoilerplate shaders
      vulkan_ctx_fields       = vulkanCtxFields entry_point_names
      vulkan_ctx_inits        = vulkanCtxInits entry_point_names
      vulkan_shader_ctx_inits = map vulkanShaderCtxInit shaders
      vulkan_shader_ctx_clrs  = map vulkanShaderCtxCleanup shaders
      max_desc_count          = maximum $ 0 : map SPIRV.shaderDescriptorSetSize shaders
      size_name_inits         = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_class_inits        = map (\(c,_) -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes
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

  GC.publicDef_ "context_config_set_lunarg_debugging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         cfg->vulkan.lunarg_debugging = 1;
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

  GC.publicDef_ "context_config_set_default_group_size" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int size);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->vulkan.default_group_size = size;
                         cfg->vulkan.default_group_size_changed = 1;
                       }|])

  GC.publicDef_ "context_config_set_default_num_groups" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int num) {
                         cfg->vulkan.default_num_groups = num;
                       }|])

  GC.publicDef_ "context_config_set_default_tile_size" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->vulkan.default_tile_size = size;
                         cfg->vulkan.default_tile_size_changed = 1;
                       }|])

  GC.publicDef_ "context_config_set_default_threshold" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->vulkan.default_threshold = size;
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
                     $stms:set_sizes

                     $stms:vulkan_shader_ctx_inits

                     $stms:final_inits
                  }|]

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                          struct $id:ctx* ctx = malloc(sizeof(struct $id:ctx));
                          if (ctx == NULL) {
                            return NULL;
                          }

                          init_context_early(cfg, ctx);
                          setup_vulkan(&ctx->vulkan, $int:max_desc_count);
                          init_context_late(cfg, ctx);
                          return ctx;
                       }|])

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 free_lock(&ctx->lock);
                                 $stms:vulkan_shader_ctx_clrs
                                 $stms:ctx_cleanup
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

  mapM_ GC.debugReport $ vulkanReport entry_point_names

vulkanBoilerplate :: [SPIRV.SingleEntryShader] -> [C.Definition]
vulkanBoilerplate shaders = [C.cunit|
    $esc:("typedef struct vk_buffer_mem_pair fl_entry_mem;")
    $esc:freelist_h
    $esc:vulkan_h
    $edecls:shader_inits
  |]
  where freelist_h = $(embedStringFile "rts/c/free_list.h")
        vulkan_h = $(embedStringFile "rts/c/vulkan.h")
        shader_inits = map vulkanShaderInit shaders

vulkanShaderInit :: SPIRV.SingleEntryShader -> C.Definition
vulkanShaderInit (SPIRV.SEShader name _ code) = [C.cedecl|
    const typename uint32_t $id:(shaderCodeName name)[] = { $inits:code_words };
  |]
  where code_words = [ [C.cinit|$int:w|] | w <- code ]

vulkanShaderCtxInit :: SPIRV.SingleEntryShader -> C.Stm
vulkanShaderCtxInit (SPIRV.SEShader name desc_set_size _) = [C.cstm|
    vulkan_setup_shader(&ctx->vulkan,
                        &ctx->$id:(shaderCtx name),
                        $string:name,
                        $int:desc_set_size,
                        $id:(shaderCodeName name),
                        sizeof($id:(shaderCodeName name)));
  |]

vulkanShaderCtxCleanup :: SPIRV.SingleEntryShader -> C.Stm
vulkanShaderCtxCleanup (SPIRV.SEShader name _ _) = [C.cstm|
    vulkan_shader_cleanup(&ctx->vulkan, &ctx->$id:(shaderCtx name));
  |]

vulkanCtxFields :: [String] -> [C.FieldGroup]
vulkanCtxFields entry_point_names =
  [ [C.csdecl|int total_runs;|],
    [C.csdecl|long int total_runtime;|] ] ++
  concat
  [ [ [C.csdecl|int $id:(entryPointRuntime name);|]
    , [C.csdecl|int $id:(entryPointPipeCacheMiss name);|]
    , [C.csdecl|int $id:(entryPointRuns name);|]
    , [C.csdecl|struct vulkan_shader_context $id:(shaderCtx name);|]
    ]
  | name <- entry_point_names ]

vulkanCtxInits :: [String] -> [C.Stm]
vulkanCtxInits entry_point_names =
  [ [C.cstm|ctx->total_runs = 0;|],
    [C.cstm|ctx->total_runtime = 0;|] ] ++
  concat
  [ [ [C.cstm|ctx->$id:(entryPointRuntime name) = 0;|]
    , [C.cstm|ctx->$id:(entryPointPipeCacheMiss name) = 0;|]
    , [C.cstm|ctx->$id:(entryPointRuns name) = 0;|]
    ]
  | name <- entry_point_names ]

vulkanReport :: [String] -> [C.BlockItem]
vulkanReport names = report_kernels ++ [report_total]
  where longest_name = foldl max 0 $ map length names
        report_kernels = concatMap reportKernel names
        format_string name =
          let padding = replicate (longest_name - length name) ' '
          in unwords ["Kernel",
                      name ++ padding,
                      "executed %6d times, with average runtime: %6ldus\t total runtime: %6ldus\t pipeline cache misses: %6ld\n"]
        reportKernel name =
          let runs = entryPointRuns name
              total_runtime = entryPointRuntime name
              pipeline_cache_misses = entryPointPipeCacheMiss name
          in [[C.citem|
               fprintf(stderr,
                       $string:(format_string name),
                       ctx->$id:runs,
                       (long int) ctx->$id:total_runtime / (ctx->$id:runs != 0 ? ctx->$id:runs : 1),
                       (long int) ctx->$id:total_runtime,
                       (long int) ctx->$id:pipeline_cache_misses);
              |],
              [C.citem|ctx->total_runtime += ctx->$id:total_runtime;|],
              [C.citem|ctx->total_runs += ctx->$id:runs;|]]

        report_total = [C.citem|
                          if (ctx->debugging) {
                            fprintf(stderr, "Ran %d kernels with cumulative runtime: %6ldus\n",
                                    ctx->total_runs, ctx->total_runtime);
                          }
                        |]

entryPointRuntime :: String -> String
entryPointRuntime name = name ++ "_total_runtime"

entryPointRuns :: String -> String
entryPointRuns name = name ++ "_runs"

entryPointPipeCacheMiss :: String -> String
entryPointPipeCacheMiss name = name ++ "_pipeline_cache_misses"

shaderCodeName :: String -> String
shaderCodeName name = name ++ "_shader"

shaderCtx :: String -> String
shaderCtx name = name ++ "_shader_ctx"
