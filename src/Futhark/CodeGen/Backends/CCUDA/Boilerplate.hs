{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various boilerplate definitions for the CUDA backend.
module Futhark.CodeGen.Backends.CCUDA.Boilerplate
  ( generateBoilerplate,
    profilingEnclosure,
    module Futhark.CodeGen.Backends.COpenCL.Boilerplate,
  )
where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
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
import Futhark.CodeGen.RTS.C (cudaH, freeListH)
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
        CUDA_SUCCEED_FATAL(cudaEventRecord(pevents[0], 0));
      }
      |],
    [C.citems|
      if (pevents != NULL) {
        CUDA_SUCCEED_FATAL(cudaEventRecord(pevents[1], 0));
      }
      |]
  )

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  T.Text ->
  T.Text ->
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
      $esc:(T.unpack freeListH)
      $esc:(T.unpack cudaH)
      const char *cuda_program[] = {$inits:fragments, NULL};
      |]

  generateSizeFuns sizes
  cfg <- generateConfigFuns sizes
  generateContextFuns cfg cost_centres kernels sizes failures

  GC.profileReport [C.citem|CUDA_SUCCEED_FATAL(cuda_tally_profiling_records(&ctx->cuda));|]
  mapM_ GC.profileReport $ costCentreReport $ cost_centres ++ M.keys kernels
  where
    fragments =
      [ [C.cinit|$string:s|]
        | s <- chunk 2000 $ T.unpack $ cuda_prelude <> cuda_program
      ]

generateSizeFuns :: M.Map Name SizeClass -> GC.CompilerM OpenCL () ()
generateSizeFuns sizes = do
  let size_name_inits = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_var_inits = map (\k -> [C.cinit|$string:(zEncodeString (pretty k))|]) $ M.keys sizes
      size_class_inits = map (\c -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes

  GC.earlyDecl [C.cedecl|static const char *tuning_param_names[] = { $inits:size_name_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_vars[] = { $inits:size_var_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_classes[] = { $inits:size_class_inits };|]

generateConfigFuns :: M.Map Name SizeClass -> GC.CompilerM OpenCL () String
generateConfigFuns sizes = do
  let size_decls = map (\k -> [C.csdecl|typename int64_t *$id:k;|]) $ M.keys sizes
      num_sizes = M.size sizes
  GC.earlyDecl [C.cedecl|struct tuning_params { $sdecls:size_decls };|]
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s {int in_use;
                              struct cuda_config cu_cfg;
                              int profiling;
                              typename int64_t tuning_params[$int:num_sizes];
                              int num_nvrtc_opts;
                              const char **nvrtc_opts;
                            };|]
    )

  let size_value_inits = zipWith sizeInit [0 .. M.size sizes -1] (M.elems sizes)
      sizeInit i size = [C.cstm|cfg->tuning_params[$int:i] = $int:val;|]
        where
          val = fromMaybe 0 $ sizeDefault size
  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:cfg* $id:s(void);|],
      [C.cedecl|struct $id:cfg* $id:s(void) {
                         struct $id:cfg *cfg = (struct $id:cfg*) malloc(sizeof(struct $id:cfg));
                         if (cfg == NULL) {
                           return NULL;
                         }
                         cfg->in_use = 0;

                         cfg->profiling = 0;
                         cfg->num_nvrtc_opts = 0;
                         cfg->nvrtc_opts = (const char**) malloc(sizeof(const char*));
                         cfg->nvrtc_opts[0] = NULL;
                         $stms:size_value_inits
                         cuda_config_init(&cfg->cu_cfg, $int:num_sizes,
                                          tuning_param_names, tuning_param_vars,
                                          cfg->tuning_params, tuning_param_classes);
                         return cfg;
                       }|]
    )

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         assert(!cfg->in_use);
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

  GC.publicDef_ "context_config_set_default_reg_tile_size" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->cu_cfg.default_reg_tile_size = size;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_threshold" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->cu_cfg.default_threshold = size;
                       }|]
    )

  GC.publicDef_ "context_config_set_tuning_param" GC.InitDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *param_name, size_t new_value);|],
      [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *param_name, size_t new_value) {

                         for (int i = 0; i < $int:num_sizes; i++) {
                           if (strcmp(param_name, tuning_param_names[i]) == 0) {
                             cfg->tuning_params[i] = new_value;
                             return 0;
                           }
                         }

                         if (strcmp(param_name, "default_group_size") == 0) {
                           cfg->cu_cfg.default_block_size = new_value;
                           return 0;
                         }

                         if (strcmp(param_name, "default_num_groups") == 0) {
                           cfg->cu_cfg.default_grid_size = new_value;
                           return 0;
                         }

                         if (strcmp(param_name, "default_threshold") == 0) {
                           cfg->cu_cfg.default_threshold = new_value;
                           return 0;
                         }

                         if (strcmp(param_name, "default_tile_size") == 0) {
                           cfg->cu_cfg.default_tile_size = new_value;
                           return 0;
                         }

                         if (strcmp(param_name, "default_reg_tile_size") == 0) {
                           cfg->cu_cfg.default_reg_tile_size = new_value;
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
  (fields, init_fields, free_fields) <- GC.contextContents
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
          [C.cstm|CUDA_SUCCEED_FATAL(cuModuleGetFunction(
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
                         struct $id:cfg* cfg;
                         int detail_memory;
                         int debugging;
                         int profiling;
                         int profiling_paused;
                         int logging;
                         typename lock_t lock;
                         char *error;
                         typename FILE *log;
                         $sdecls:fields
                         $sdecls:kernel_fields
                         typename CUdeviceptr global_failure;
                         typename CUdeviceptr global_failure_args;
                         struct cuda_context cuda;
                         struct tuning_params tuning_params;
                         // True if a potentially failing kernel has been enqueued.
                         typename int32_t failure_is_an_option;

                         int total_runs;
                         long int total_runtime;
                       };|]
    )

  let set_tuning_params =
        zipWith
          (\i k -> [C.cstm|ctx->tuning_params.$id:k = &cfg->tuning_params[$int:i];|])
          [(0 :: Int) ..]
          $ M.keys sizes
      max_failure_args =
        foldl max 0 $ map (errorMsgNumArgs . failureError) failures

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                 assert(!cfg->in_use);
                 struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                 if (ctx == NULL) {
                   return NULL;
                 }
                 ctx->cfg = cfg;
                 ctx->cfg->in_use = 1;
                 ctx->debugging = ctx->detail_memory = cfg->cu_cfg.debugging;
                 ctx->profiling = cfg->profiling;
                 ctx->profiling_paused = 0;
                 ctx->logging = cfg->cu_cfg.logging;
                 ctx->error = NULL;
                 ctx->log = stderr;
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

                 ctx->error = cuda_setup(&ctx->cuda, cuda_program, cfg->nvrtc_opts);

                 if (ctx->error != NULL) {
                   return NULL;
                 }

                 typename int32_t no_error = -1;
                 CUDA_SUCCEED_FATAL(cuMemAlloc(&ctx->global_failure, sizeof(no_error)));
                 CUDA_SUCCEED_FATAL(cuMemcpyHtoD(ctx->global_failure, &no_error, sizeof(no_error)));
                 // The +1 is to avoid zero-byte allocations.
                 CUDA_SUCCEED_FATAL(cuMemAlloc(&ctx->global_failure_args, sizeof(int64_t)*($int:max_failure_args+1)));

                 $stms:init_kernel_fields

                 $stms:final_inits
                 $stms:set_tuning_params

                 init_constants(ctx);
                 // Clear the free list of any deallocations that occurred while initialising constants.
                 CUDA_SUCCEED_FATAL(cuda_free_all(&ctx->cuda));

                 futhark_context_sync(ctx);

                 return ctx;
               }|]
    )

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 $stms:free_fields
                                 free_constants(ctx);
                                 cuMemFree(ctx->global_failure);
                                 cuMemFree(ctx->global_failure_args);
                                 cuda_cleanup(&ctx->cuda);
                                 free_lock(&ctx->lock);
                                 ctx->cfg->in_use = 0;
                                 free(ctx);
                               }|]
    )

  GC.publicDef_ "context_sync" GC.MiscDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                 CUDA_SUCCEED_OR_RETURN(cuCtxPushCurrent(ctx->cuda.cu_ctx));
                 CUDA_SUCCEED_OR_RETURN(cuCtxSynchronize());
                 if (ctx->failure_is_an_option) {
                   // Check for any delayed error.
                   typename int32_t failure_idx;
                   CUDA_SUCCEED_OR_RETURN(
                     cuMemcpyDtoH(&failure_idx,
                                  ctx->global_failure,
                                  sizeof(int32_t)));
                   ctx->failure_is_an_option = 0;

                   if (failure_idx >= 0) {
                     // We have to clear global_failure so that the next entry point
                     // is not considered a failure from the start.
                     typename int32_t no_failure = -1;
                     CUDA_SUCCEED_OR_RETURN(
                       cuMemcpyHtoD(ctx->global_failure,
                                    &no_failure,
                                    sizeof(int32_t)));

                     typename int64_t args[$int:max_failure_args+1];
                     CUDA_SUCCEED_OR_RETURN(
                       cuMemcpyDtoH(&args,
                                    ctx->global_failure_args,
                                    sizeof(args)));

                     $stm:(failureSwitch failures)

                     return FUTHARK_PROGRAM_ERROR;
                   }
                 }
                 CUDA_SUCCEED_OR_RETURN(cuCtxPopCurrent(&ctx->cuda.cu_ctx));
                 return 0;
               }|]
    )

  GC.onClear
    [C.citem|if (ctx->error == NULL) {
               CUDA_SUCCEED_NONFATAL(cuda_free_all(&ctx->cuda));
             }|]
