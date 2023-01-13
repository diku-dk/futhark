{-# LANGUAGE QuasiQuotes #-}

-- | Various boilerplate definitions for the CUDA backend.
module Futhark.CodeGen.Backends.CCUDA.Boilerplate
  ( generateBoilerplate,
    profilingEnclosure,
    module Futhark.CodeGen.Backends.COpenCL.Boilerplate,
  )
where

import Control.Monad
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
  ( copyDevToDev,
    copyDevToHost,
    copyHostToDev,
    copyScalarFromDev,
    copyScalarToDev,
    costCentreReport,
    failureMsgFunction,
    generateTuningParams,
    kernelRuns,
    kernelRuntime,
  )
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.RTS.C (backendsCudaH)
import Futhark.Util (chunk)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

-- | Block items to put before and after a thing to be profiled.
profilingEnclosure :: Name -> ([C.BlockItem], [C.BlockItem])
profilingEnclosure name =
  ( [C.citems|
      typename cudaEvent_t *pevents = NULL;
      if (ctx->profiling && !ctx->profiling_paused) {
        pevents = cuda_get_events(&ctx->cuda,
                                  &ctx->program->$id:(kernelRuns name),
                                  &ctx->program->$id:(kernelRuntime name));
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
      $esc:(T.unpack backendsCudaH)
      static const char *cuda_program[] = {$inits:fragments, NULL};
      |]
  GC.earlyDecl $ failureMsgFunction failures

  generateTuningParams sizes
  generateConfigFuns
  generateContextFuns cost_centres kernels sizes failures

  GC.profileReport [C.citem|CUDA_SUCCEED_FATAL(cuda_tally_profiling_records(&ctx->cuda));|]
  mapM_ GC.profileReport $ costCentreReport $ cost_centres ++ M.keys kernels
  where
    fragments =
      [ [C.cinit|$string:s|]
        | s <- chunk 2000 $ T.unpack $ cuda_prelude <> cuda_program
      ]

generateConfigFuns :: GC.CompilerM OpenCL () ()
generateConfigFuns = do
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_add_nvrtc_option(struct futhark_context_config *cfg, const char* opt);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_device(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_dump_program_to(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_load_program_from(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_dump_ptx_to(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_load_ptx_from(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_reg_tile_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_threshold(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|int futhark_context_config_set_tuning_param(struct futhark_context_config *cfg, const char *param_name, size_t new_value);|]

generateContextFuns ::
  [Name] ->
  M.Map KernelName KernelSafety ->
  M.Map Name SizeClass ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateContextFuns cost_centres kernels sizes failures = do
  let forCostCentre name = do
        GC.contextField
          (C.toIdent (kernelRuntime name) mempty)
          [C.cty|typename int64_t|]
          (Just [C.cexp|0|])
        GC.contextField
          (C.toIdent (kernelRuns name) mempty)
          [C.cty|int|]
          (Just [C.cexp|0|])

  forM_ (M.keys kernels) $ \name -> do
    GC.contextFieldDyn
      (C.toIdent name mempty)
      [C.cty|typename CUfunction|]
      [C.cstm|
             CUDA_SUCCEED_FATAL(cuModuleGetFunction(
                                     &ctx->program->$id:name,
                                     ctx->cuda.module,
                                     $string:(T.unpack (idText (C.toIdent name mempty)))));|]
      [C.cstm|{}|]
    forCostCentre name

  mapM_ forCostCentre cost_centres

  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s {
                         struct futhark_context_config* cfg;
                         int detail_memory;
                         int debugging;
                         int profiling;
                         int profiling_paused;
                         int logging;
                         typename lock_t lock;
                         char *error;
                         typename lock_t error_lock;
                         typename FILE *log;
                         struct constants *constants;
                         struct free_list free_list;
                         typename int64_t peak_mem_usage_default;
                         typename int64_t cur_mem_usage_default;

                         typename CUdeviceptr global_failure;
                         typename CUdeviceptr global_failure_args;
                         struct cuda_context cuda;
                         struct tuning_params tuning_params;
                         // True if a potentially failing kernel has been enqueued.
                         typename int32_t failure_is_an_option;
                         int total_runs;
                         long int total_runtime;
                         typename int64_t peak_mem_usage_device;
                         typename int64_t cur_mem_usage_device;
                         struct program* program;
                       };|]
    )

  let set_tuning_params =
        zipWith
          (\i k -> [C.cstm|ctx->tuning_params.$id:k = &cfg->tuning_params[$int:i];|])
          [(0 :: Int) ..]
          $ M.keys sizes
      max_failure_args =
        foldl max 0 $ map (errorMsgNumArgs . failureError) failures

  GC.earlyDecl
    [C.cedecl|static void set_tuning_params(struct futhark_context_config *cfg, struct $id:ctx* ctx) {
             $stms:set_tuning_params
       }|]

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:ctx* $id:s(struct futhark_context_config* cfg);|],
      [C.cedecl|struct $id:ctx* $id:s(struct futhark_context_config* cfg) {
                 struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                 if (ctx == NULL) {
                   return NULL;
                 }
                 context_setup(cfg, ctx);

                 ctx->cuda.profiling_records_capacity = 200;
                 ctx->cuda.profiling_records_used = 0;
                 ctx->cuda.profiling_records =
                   malloc(ctx->cuda.profiling_records_capacity *
                          sizeof(struct profiling_record));

                 ctx->failure_is_an_option = 0;
                 ctx->total_runs = 0;
                 ctx->total_runtime = 0;

                 ctx->error = cuda_setup(ctx->cfg, &ctx->cuda, cuda_program, cfg->nvrtc_opts, cfg->cache_fname);

                 if (ctx->error != NULL) {
                   futhark_panic(1, "%s\n", ctx->error);
                 }

                 typename int32_t no_error = -1;
                 CUDA_SUCCEED_FATAL(cuMemAlloc(&ctx->global_failure, sizeof(no_error)));
                 CUDA_SUCCEED_FATAL(cuMemcpyHtoD(ctx->global_failure, &no_error, sizeof(no_error)));
                 // The +1 is to avoid zero-byte allocations.
                 CUDA_SUCCEED_FATAL(cuMemAlloc(&ctx->global_failure_args, sizeof(int64_t)*($int:max_failure_args+1)));

                 set_tuning_params(cfg, ctx);
                 setup_program(cfg, ctx);
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
                  teardown_program(ctx);
                  context_teardown(ctx);
                  cuMemFree(ctx->global_failure);
                  cuMemFree(ctx->global_failure_args);
                  cuda_cleanup(&ctx->cuda);
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

                     typename int64_t args[ctx->max_failure_args+1];
                     CUDA_SUCCEED_OR_RETURN(
                       cuMemcpyDtoH(&args,
                                    ctx->global_failure_args,
                                    sizeof(args)));

                     ctx->error = get_failure_msg(failure_idx, args);

                     return FUTHARK_PROGRAM_ERROR;
                   }
                 }
                 CUDA_SUCCEED_OR_RETURN(cuCtxPopCurrent(&ctx->cuda.cu_ctx));
                 return 0;
               }|]
    )

  GC.generateProgramStruct

  GC.onClear
    [C.citem|if (ctx->error == NULL) {
               CUDA_SUCCEED_NONFATAL(cuda_free_all(&ctx->cuda));
             }|]

{-# NOINLINE generateBoilerplate #-}
