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

generateBoilerplate :: [Word32] -> [String] -> GC.CompilerM Vulkan () ()
generateBoilerplate spirv_code entry_point_names = do
  final_inits <- GC.contextFinalInits

  let vulkan_boilerplate = vulkanBoilerplate spirv_code
      vulkan_ctx_fields = vulkanCtxFields entry_point_names
      vulkan_ctx_inits = vulkanCtxInits entry_point_names
  GC.earlyDecls vulkan_boilerplate
            
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s { struct vulkan_config vulkan; };|])
  
  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:cfg* $id:s(void);|],
     [C.cedecl|struct $id:cfg* $id:s(void) {
                         struct $id:cfg *cfg = malloc(sizeof(struct $id:cfg));
                         if (cfg == NULL) {
                           return NULL;
                         }

                         vulkan_config_init(&cfg->vulkan);

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
                          // Todo
                          return 0;
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

  GC.libDecl [C.cedecl|static void init_context_late(struct $id:cfg *cfg, struct $id:ctx* ctx) {
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
                          setup_vulkan(&ctx->vulkan, spirv_shader, sizeof(spirv_shader));
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

  GC.publicDef_ "context_get_device_queue" GC.InitDecl $ \s ->
    ([C.cedecl|typename VkQueue $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|typename VkQueue $id:s(struct $id:ctx* ctx) {
                 return ctx->vulkan.queue;
               }|])

  return ()

vulkanBoilerplate :: [Word32] -> [C.Definition]
vulkanBoilerplate spirv_code = [C.cunit|
    $esc:vulkan_h
    $esc:("const uint32_t spirv_shader[] = " ++ shader ++ ";")|]
  where vulkan_h = $(embedStringFile "rts/c/vulkan.h")
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

entryPointRuntime :: String -> String
entryPointRuntime = (++"_total_runtime")

entryPointRuns :: String -> String
entryPointRuns = (++"_runs")