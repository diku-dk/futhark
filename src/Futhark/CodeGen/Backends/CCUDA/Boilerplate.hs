{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various boilerplate definitions for the CUDA backend.
module Futhark.CodeGen.Backends.CCUDA.Boilerplate
  ( generateBoilerplate,
    profilingEnclosure,
    module Futhark.CodeGen.Backends.COpenCL.Boilerplate,
  )
where

import Data.FileEmbed (embedStringFile)
import qualified Data.Map as M
import Data.Maybe
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
  ( copyDevToDev,
    copyDevToHost,
    copyHostToDev,
    copyScalarFromDev,
    copyScalarToDev,
    costCentreReport,
    failureSwitch,
    kernelRuns,
    kernelRuntime,
  )
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.Util (chunk, zEncodeString)
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

-- | Block items to put before and after a thing to be profiled.
profilingEnclosure :: Name -> ([C.BlockItem], [C.BlockItem])
profilingEnclosure name =
  ( [C.citems|
      typename cudaEvent_t *pevents = NULL;
      if (ctx->profiling && !ctx->profiling_paused) {
        pevents = cuda_get_events(&ctx->cuda,
                                  &ctx->$id:(kernelRuns name),
                                  &ctx->$id:(kernelRuntime name));
        CUDA_SUCCEED(cudaEventRecord(pevents[0], 0));
      }
      |],
    [C.citems|
      if (pevents != NULL) {
        CUDA_SUCCEED(cudaEventRecord(pevents[1], 0));
      }
      |]
  )

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  String ->
  String ->
  [Name] ->
  M.Map KernelName KernelSafety ->
  M.Map Name SizeClass ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateBoilerplate cuda_program cuda_prelude cost_centres kernels sizes failures = do
  mapM_
    GC.earlyDecl
    [C.cunit|
      $esc:("#include <cuda.h>")
      $esc:("#include <nvrtc.h>")
      $esc:("typedef CUdeviceptr fl_mem_t;")
      $esc:free_list_h
      $esc:cuda_h
      const char *cuda_program[] = {$inits:fragments, NULL};
      |]

  generateSizeFuns sizes
  cfg <- generateConfigFuns sizes
  generateContextFuns cfg cost_centres kernels sizes failures

  GC.profileReport [C.citem|CUDA_SUCCEED(cuda_tally_profiling_records(&ctx->cuda));|]
  mapM_ GC.profileReport $ costCentreReport $ cost_centres ++ M.keys kernels
  where
    cuda_h = $(embedStringFile "rts/c/cuda.h")
    free_list_h = $(embedStringFile "rts/c/free_list.h")
    fragments =
      map (\s -> [C.cinit|$string:s|]) $
        chunk 2000 (cuda_prelude ++ cuda_program)

generateSizeFuns :: M.Map Name SizeClass -> GC.CompilerM OpenCL () ()
generateSizeFuns sizes = do
  let size_name_inits = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_var_inits = map (\k -> [C.cinit|$string:(zEncodeString (pretty k))|]) $ M.keys sizes
      size_class_inits = map (\c -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes
      num_sizes = M.size sizes

  GC.earlyDecl [C.cedecl|static const char *size_names[] = { $inits:size_name_inits };|]
  GC.earlyDecl [C.cedecl|static const char *size_vars[] = { $inits:size_var_inits };|]
  GC.earlyDecl [C.cedecl|static const char *size_classes[] = { $inits:size_class_inits };|]

  GC.publicDef_ "get_num_sizes" GC.InitDecl $ \s ->
    ( [C.cedecl|int $id:s(void);|],
      [C.cedecl|int $id:s(void) {
                return $int:num_sizes;
              }|]
    )

  GC.publicDef_ "get_size_name" GC.InitDecl $ \s ->
    ( [C.cedecl|const char* $id:s(int);|],
      [C.cedecl|const char* $id:s(int i) {
                return size_names[i];
              }|]
    )

  GC.publicDef_ "get_size_class" GC.InitDecl $ \s ->
    ( [C.cedecl|const char* $id:s(int);|],
      [C.cedecl|const char* $id:s(int i) {
                return size_classes[i];
              }|]
    )

generateConfigFuns :: M.Map Name SizeClass -> GC.CompilerM OpenCL () String
generateConfigFuns sizes = do
  let size_decls = map (\k -> [C.csdecl|typename int64_t $id:k;|]) $ M.keys sizes
      num_sizes = M.size sizes
  GC.earlyDecl [C.cedecl|struct sizes { $sdecls:size_decls };|]
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s { struct cuda_config cu_cfg;
                              int profiling;
                              typename int64_t sizes[$int:num_sizes];
                              int num_nvrtc_opts;
                              const char **nvrtc_opts;
                            };|]
    )

  let size_value_inits = zipWith sizeInit [0 .. M.size sizes -1] (M.elems sizes)
      sizeInit i size = [C.cstm|cfg->sizes[$int:i] = $int:val;|]
        where
          val = fromMaybe 0 $ sizeDefault size
  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:cfg* $id:s(void);|],
      [C.cedecl|struct $id:cfg* $id:s(void) {
                         struct $id:cfg *cfg = (struct $id:cfg*) malloc(sizeof(struct $id:cfg));
                         if (cfg == NULL) {
                           return NULL;
                         }

                         cfg->profiling = 0;
                         cfg->num_nvrtc_opts = 0;
                         cfg->nvrtc_opts = (const char**) malloc(sizeof(const char*));
                         cfg->nvrtc_opts[0] = NULL;
                         $stms:size_value_inits
                         cuda_config_init(&cfg->cu_cfg, $int:num_sizes,
                                          size_names, size_vars,
                                          cfg->sizes, size_classes);
                         return cfg;
                       }|]
    )

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         free(cfg->nvrtc_opts);
                         free(cfg);
                       }|]
    )

  GC.publicDef_ "context_config_add_nvrtc_option" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt) {
                         cfg->nvrtc_opts[cfg->num_nvrtc_opts] = opt;
                         cfg->num_nvrtc_opts++;
                         cfg->nvrtc_opts = (const char**) realloc(cfg->nvrtc_opts, (cfg->num_nvrtc_opts+1) * sizeof(const char*));
                         cfg->nvrtc_opts[cfg->num_nvrtc_opts] = NULL;
                       }|]
    )

  GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->cu_cfg.logging = cfg->cu_cfg.debugging = flag;
                       }|]
    )

  GC.publicDef_ "context_config_set_profiling" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->profiling = flag;
                       }|]
    )

  GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->cu_cfg.logging = flag;
                       }|]
    )

  GC.publicDef_ "context_config_set_device" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *s);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *s) {
                         set_preferred_device(&cfg->cu_cfg, s);
                       }|]
    )

  GC.publicDef_ "context_config_dump_program_to" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->cu_cfg.dump_program_to = path;
                       }|]
    )

  GC.publicDef_ "context_config_load_program_from" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->cu_cfg.load_program_from = path;
                       }|]
    )

  GC.publicDef_ "context_config_dump_ptx_to" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                          cfg->cu_cfg.dump_ptx_to = path;
                      }|]
    )

  GC.publicDef_ "context_config_load_ptx_from" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                          cfg->cu_cfg.load_ptx_from = path;
                      }|]
    )

  GC.publicDef_ "context_config_set_default_group_size" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int size);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->cu_cfg.default_block_size = size;
                         cfg->cu_cfg.default_block_size_changed = 1;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_num_groups" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int num) {
                         cfg->cu_cfg.default_grid_size = num;
                         cfg->cu_cfg.default_grid_size_changed = 1;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_tile_size" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->cu_cfg.default_tile_size = size;
                         cfg->cu_cfg.default_tile_size_changed = 1;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_threshold" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->cu_cfg.default_threshold = size;
                       }|]
    )

  GC.publicDef_ "context_config_set_size" GC.InitDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value);|],
      [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value) {

                         for (int i = 0; i < $int:num_sizes; i++) {
                           if (strcmp(size_name, size_names[i]) == 0) {
                             cfg->sizes[i] = size_value;
                             return 0;
                           }
                         }

                         if (strcmp(size_name, "default_group_size") == 0) {
                           cfg->cu_cfg.default_block_size = size_value;
                           return 0;
                         }

                         if (strcmp(size_name, "default_num_groups") == 0) {
                           cfg->cu_cfg.default_grid_size = size_value;
                           return 0;
                         }

                         if (strcmp(size_name, "default_threshold") == 0) {
                           cfg->cu_cfg.default_threshold = size_value;
                           return 0;
                         }

                         if (strcmp(size_name, "default_tile_size") == 0) {
                           cfg->cu_cfg.default_tile_size = size_value;
                           return 0;
                         }
                         return 1;
                       }|]
    )
  return cfg

generateContextFuns ::
  String ->
  [Name] ->
  M.Map KernelName KernelSafety ->
  M.Map Name SizeClass ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateContextFuns cfg cost_centres kernels sizes failures = do
  final_inits <- GC.contextFinalInits
  (fields, init_fields) <- GC.contextContents
  let forCostCentre name =
        [ ( [C.csdecl|typename int64_t $id:(kernelRuntime name);|],
            [C.cstm|ctx->$id:(kernelRuntime name) = 0;|]
          ),
          ( [C.csdecl|int $id:(kernelRuns name);|],
            [C.cstm|ctx->$id:(kernelRuns name) = 0;|]
          )
        ]

      forKernel name =
        ( [C.csdecl|typename CUfunction $id:name;|],
          [C.cstm|CUDA_SUCCEED(cuModuleGetFunction(
                                &ctx->$id:name,
                                ctx->cuda.module,
                                $string:(pretty (C.toIdent name mempty))));|]
        ) :
        forCostCentre name

      (kernel_fields, init_kernel_fields) =
        unzip $
          concatMap forKernel (M.keys kernels)
            ++ concatMap forCostCentre cost_centres

  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s {
                         int detail_memory;
                         int debugging;
                         int profiling;
                         int profiling_paused;
                         int logging;
                         typename lock_t lock;
                         char *error;
                         $sdecls:fields
                         $sdecls:kernel_fields
                         typename CUdeviceptr global_failure;
                         typename CUdeviceptr global_failure_args;
                         struct cuda_context cuda;
                         struct sizes sizes;
                         // True if a potentially failing kernel has been enqueued.
                         typename int32_t failure_is_an_option;

                         int total_runs;
                         long int total_runtime;
                       };|]
    )

  let set_sizes =
        zipWith
          (\i k -> [C.cstm|ctx->sizes.$id:k = cfg->sizes[$int:i];|])
          [(0 :: Int) ..]
          $ M.keys sizes
      max_failure_args =
        foldl max 0 $ map (errorMsgNumArgs . failureError) failures

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                 struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                 if (ctx == NULL) {
                   return NULL;
                 }
                 ctx->debugging = ctx->detail_memory = cfg->cu_cfg.debugging;
                 ctx->profiling = cfg->profiling;
                 ctx->profiling_paused = 0;
                 ctx->logging = cfg->cu_cfg.logging;
                 ctx->error = NULL;
                 ctx->cuda.profiling_records_capacity = 200;
                 ctx->cuda.profiling_records_used = 0;
                 ctx->cuda.profiling_records =
                   malloc(ctx->cuda.profiling_records_capacity *
                          sizeof(struct profiling_record));

                 ctx->cuda.cfg = cfg->cu_cfg;
                 create_lock(&ctx->lock);

                 ctx->failure_is_an_option = 0;
                 ctx->total_runs = 0;
                 ctx->total_runtime = 0;
                 $stms:init_fields

                 cuda_setup(&ctx->cuda, cuda_program, cfg->nvrtc_opts);

                 typename int32_t no_error = -1;
                 CUDA_SUCCEED(cuMemAlloc(&ctx->global_failure, sizeof(no_error)));
                 CUDA_SUCCEED(cuMemcpyHtoD(ctx->global_failure, &no_error, sizeof(no_error)));
                 // The +1 is to avoid zero-byte allocations.
                 CUDA_SUCCEED(cuMemAlloc(&ctx->global_failure_args, sizeof(int64_t)*($int:max_failure_args+1)));

                 $stms:init_kernel_fields

                 $stms:final_inits
                 $stms:set_sizes

                 init_constants(ctx);
                 // Clear the free list of any deallocations that occurred while initialising constants.
                 CUDA_SUCCEED(cuda_free_all(&ctx->cuda));

                 futhark_context_sync(ctx);

                 return ctx;
               }|]
    )

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 free_constants(ctx);
                                 cuda_cleanup(&ctx->cuda);
                                 free_lock(&ctx->lock);
                                 free(ctx);
                               }|]
    )

  GC.publicDef_ "context_sync" GC.MiscDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                 CUDA_SUCCEED(cuCtxPushCurrent(ctx->cuda.cu_ctx));
                 CUDA_SUCCEED(cuCtxSynchronize());
                 if (ctx->failure_is_an_option) {
                   // Check for any delayed error.
                   typename int32_t failure_idx;
                   CUDA_SUCCEED(
                     cuMemcpyDtoH(&failure_idx,
                                  ctx->global_failure,
                                  sizeof(int32_t)));
                   ctx->failure_is_an_option = 0;

                   if (failure_idx >= 0) {
                     // We have to clear global_failure so that the next entry point
                     // is not considered a failure from the start.
                     typename int32_t no_failure = -1;
                     CUDA_SUCCEED(
                       cuMemcpyHtoD(ctx->global_failure,
                                    &no_failure,
                                    sizeof(int32_t)));

                     typename int64_t args[$int:max_failure_args+1];
                     CUDA_SUCCEED(
                       cuMemcpyDtoH(&args,
                                    ctx->global_failure_args,
                                    sizeof(args)));

                     $stm:(failureSwitch failures)

                     return 1;
                   }
                 }
                 CUDA_SUCCEED(cuCtxPopCurrent(&ctx->cuda.cu_ctx));
                 return 0;
               }|]
    )

  GC.publicDef_ "context_clear_caches" GC.MiscDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                         lock_lock(&ctx->lock);
                         CUDA_SUCCEED(cuda_free_all(&ctx->cuda));
                         lock_unlock(&ctx->lock);
                         return 0;
                       }|]
    )
