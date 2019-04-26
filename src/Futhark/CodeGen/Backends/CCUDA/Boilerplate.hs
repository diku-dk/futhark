{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.CCUDA.Boilerplate
  (
    generateBoilerplate
  ) where

import qualified Language.C.Quote.OpenCL as C

import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.Util (chunk, zEncodeString)

import qualified Data.Map as M
import Data.FileEmbed (embedStringFile)



generateBoilerplate :: String -> String -> [String]
                    -> M.Map Name SizeClass
                    -> GC.CompilerM OpenCL () ()
generateBoilerplate cuda_program cuda_prelude kernel_names sizes = do
  GC.earlyDecls [C.cunit|
      $esc:("#include <cuda.h>")
      $esc:("#include <nvrtc.h>")
      $esc:("#include <pthread.h>")
      $esc:("#include <semaphore.h>")
      $esc:("typedef CUdeviceptr fl_mem_t;")
      $esc:free_list_h
      $esc:cuda_h
      const char *cuda_program[] = {$inits:fragments, NULL};
      |]

  generateSizeFuns sizes
  cfg <- generateConfigFuns sizes
  generateContextFuns cfg kernel_names sizes
  where
    cuda_h = $(embedStringFile "rts/c/cuda.h")
    free_list_h = $(embedStringFile "rts/c/free_list.h")
    fragments = map (\s -> [C.cinit|$string:s|])
                  $ chunk 2000 (cuda_prelude ++ cuda_program)

generateSizeFuns :: M.Map Name SizeClass -> GC.CompilerM OpenCL () ()
generateSizeFuns sizes = do
  let size_name_inits = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_var_inits = map (\k -> [C.cinit|$string:(zEncodeString (pretty k))|]) $ M.keys sizes
      size_class_inits = map (\c -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes
      num_sizes = M.size sizes

  GC.libDecl [C.cedecl|static const char *size_names[] = { $inits:size_name_inits };|]
  GC.libDecl [C.cedecl|static const char *size_vars[] = { $inits:size_var_inits };|]
  GC.libDecl [C.cedecl|static const char *size_classes[] = { $inits:size_class_inits };|]

  GC.publicDef_ "get_num_sizes" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(void);|],
     [C.cedecl|int $id:s(void) {
                return $int:num_sizes;
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

generateConfigFuns :: M.Map Name SizeClass -> GC.CompilerM OpenCL () String
generateConfigFuns sizes = do
  let size_decls = map (\k -> [C.csdecl|size_t $id:k;|]) $ M.keys sizes
      num_sizes = M.size sizes
  GC.libDecl [C.cedecl|struct sizes { $sdecls:size_decls };|]
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s { struct cuda_config cu_cfg;
                              size_t sizes[$int:num_sizes];
                              int num_nvrtc_opts;
                              const char **nvrtc_opts;
                            };|])

  let size_value_inits = map (\i -> [C.cstm|cfg->sizes[$int:i] = 0;|])
                           [0..M.size sizes-1]
  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:cfg* $id:s(void);|],
     [C.cedecl|struct $id:cfg* $id:s(void) {
                         struct $id:cfg *cfg = malloc(sizeof(struct $id:cfg));
                         if (cfg == NULL) {
                           return NULL;
                         }

                         cfg->num_nvrtc_opts = 0;
                         cfg->nvrtc_opts = malloc(sizeof(const char*));
                         cfg->nvrtc_opts[0] = NULL;
                         $stms:size_value_inits
                         cuda_config_init(&cfg->cu_cfg, $int:num_sizes,
                                          size_names, size_vars,
                                          cfg->sizes, size_classes);
                         return cfg;
                       }|])

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         free(cfg->nvrtc_opts);
                         free(cfg);
                       }|])

  GC.publicDef_ "context_config_add_nvrtc_option" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt) {
                         cfg->nvrtc_opts[cfg->num_nvrtc_opts] = opt;
                         cfg->num_nvrtc_opts++;
                         cfg->nvrtc_opts = realloc(cfg->nvrtc_opts, (cfg->num_nvrtc_opts+1) * sizeof(const char*));
                         cfg->nvrtc_opts[cfg->num_nvrtc_opts] = NULL;
                       }|])

  GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->cu_cfg.logging = cfg->cu_cfg.debugging = flag;
                       }|])

  GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->cu_cfg.logging = flag;
                       }|])

  GC.publicDef_ "context_config_set_device" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *s);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *s) {
                         set_preferred_device(&cfg->cu_cfg, s);
                       }|])

  GC.publicDef_ "context_config_dump_program_to" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->cu_cfg.dump_program_to = path;
                       }|])

  GC.publicDef_ "context_config_load_program_from" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->cu_cfg.load_program_from = path;
                       }|])

  GC.publicDef_ "context_config_dump_ptx_to" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                          cfg->cu_cfg.dump_ptx_to = path;
                      }|])

  GC.publicDef_ "context_config_load_ptx_from" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                          cfg->cu_cfg.load_ptx_from = path;
                      }|])

  GC.publicDef_ "context_config_set_default_block_size" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int size);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->cu_cfg.default_block_size = size;
                         cfg->cu_cfg.default_block_size_changed = 1;
                       }|])

  GC.publicDef_ "context_config_set_default_grid_size" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int num) {
                         cfg->cu_cfg.default_grid_size = num;
                       }|])

  GC.publicDef_ "context_config_set_default_tile_size" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->cu_cfg.default_tile_size = size;
                         cfg->cu_cfg.default_tile_size_changed = 1;
                       }|])

  GC.publicDef_ "context_config_set_default_threshold" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->cu_cfg.default_threshold = size;
                       }|])

  GC.publicDef_ "context_config_set_num_nodes" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int num) {
                         cfg->cu_cfg.num_nodes = num;
                       }|])

  GC.publicDef_ "context_config_set_size" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value);|],
     [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value) {

                         for (int i = 0; i < $int:num_sizes; i++) {
                           if (strcmp(size_name, size_names[i]) == 0) {
                             cfg->sizes[i] = size_value;
                             return 0;
                           }
                         }
                         return 1;
                       }|])
  return cfg

generateContextFuns :: String -> [String]
                    -> M.Map Name SizeClass
                    -> GC.CompilerM OpenCL () ()
generateContextFuns cfg kernel_names sizes = do
  final_inits <- GC.contextFinalInits
  (fields, init_fields) <- GC.contextContents
  node_field_names <- GC.nodeContextFields

  let kernel_fields = map (\k -> [C.csdecl|typename CUfunction $id:k;|])
                        kernel_names
      node_fields = map (\n -> [C.csdecl|struct memblock_device $id:n;|])
                      node_field_names

  node_ctx <- GC.publicDef "node_context" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s {
                           $sdecls:kernel_fields
                           $sdecls:node_fields 
                        };|])

  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s {
                         int detail_memory;
                         int debugging;
                         typename lock_t lock;
                         char *error;
                         $sdecls:fields
                         struct $id:node_ctx *node_ctx;
                         struct cuda_context cuda;
                         struct sizes sizes;
                       };|])

  let set_sizes = zipWith (\i k -> [C.cstm|ctx->sizes.$id:k = cfg->sizes[$int:i];|])
                          [(0::Int)..] $ M.keys sizes

  node_launch_params <- GC.publicDef "node_launch_params" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s {
                         typename int32_t node_id;
                         struct $id:ctx *ctx;
                         char *ptx;
                       };|])

  run_node <- GC.publicDef "run_node_thread" GC.InitDecl $ \s ->
    ([C.cedecl|void *$id:s(void *p);|],
     [C.cedecl|void *$id:s(void *p) {
      struct $id:node_launch_params *params = (struct $id:node_launch_params*)p;
      typename int32_t node_id = params->node_id;
      struct $id:ctx *ctx = params->ctx;

      cuda_node_setup(&ctx->cuda.nodes[node_id], params->ptx);
      
      $stms:(map loadKernelByName kernel_names)

      cuda_thread_sync(&ctx->cuda.node_sync_point);

      free(p);
     }|])

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                          struct $id:ctx* ctx = malloc(sizeof(struct $id:ctx));
                          if (ctx == NULL) {
                            return NULL;
                          }
                          ctx->debugging = ctx->detail_memory = cfg->cu_cfg.debugging;

                          ctx->cuda.cfg = cfg->cu_cfg;
                          create_lock(&ctx->lock);
                          $stms:init_fields

                          cuda_setup(&ctx->cuda);

                          ctx->node_ctx = malloc(sizeof(struct $id:node_ctx) * ctx->cuda.cfg.num_nodes);

                          char *ptx = cuda_get_ptx(&ctx->cuda, cuda_program, cfg->nvrtc_opts);

                          for (int i = 0; i < ctx->cuda.cfg.num_nodes; ++i) {
                            struct $id:node_launch_params *node_params = malloc(sizeof(struct $id:node_launch_params));
                            node_params->ctx = ctx;
                            node_params->node_id = i;
                            node_params->ptx = ptx;

                            if(pthread_create(&ctx->cuda.nodes[i].thread, NULL, $id:run_node, node_params))
                              panic(-1, "Error creating thread.");
                          }

                          cuda_thread_sync(&ctx->cuda.node_sync_point);
                          
                          free(ptx);

                          cuCtxSetCurrent(ctx->cuda.nodes[0].cu_ctx);
                          cuda_enable_peer_access(&ctx->cuda);

                          $stms:final_inits
                          $stms:set_sizes
                          return ctx;
                       }|])

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 cuda_cleanup(&ctx->cuda);
                                 free_lock(&ctx->lock);
                                 free(ctx->node_ctx);
                                 free(ctx);
                               }|])

  GC.publicDef_ "context_sync" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                         CUDA_SUCCEED(cuCtxSynchronize());
                         return 0;
                       }|])

  GC.publicDef_ "context_get_error" GC.InitDecl $ \s ->
    ([C.cedecl|char* $id:s(struct $id:ctx* ctx);|],
     [C.cedecl|char* $id:s(struct $id:ctx* ctx) {
                         return ctx->error;
                       }|])
  where
    loadKernelByName name =
      [C.cstm|CUDA_SUCCEED(cuModuleGetFunction(
                &ctx->node_ctx[node_id].$id:name,
                ctx->cuda.nodes[node_id].module,
                $string:name));|]
