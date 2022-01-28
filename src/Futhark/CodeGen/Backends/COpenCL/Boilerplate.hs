{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Futhark.CodeGen.Backends.COpenCL.Boilerplate
  ( generateBoilerplate,
    profilingEvent,
    copyDevToDev,
    copyDevToHost,
    copyHostToDev,
    copyScalarToDev,
    copyScalarFromDev,
    commonOptions,
    failureSwitch,
    costCentreReport,
    kernelRuntime,
    kernelRuns,
    sizeLoggingCode,
  )
where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.OpenCL.Heuristics
import Futhark.CodeGen.RTS.C (freeListH, openclH)
import Futhark.Util (chunk, zEncodeString)
import Futhark.Util.Pretty (prettyOneLine)
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

failureSwitch :: [FailureMsg] -> C.Stm
failureSwitch failures =
  let printfEscape =
        let escapeChar '%' = "%%"
            escapeChar c = [c]
         in concatMap escapeChar
      onPart (ErrorString s) = printfEscape s
      -- FIXME: bogus for non-ints.
      onPart ErrorVal {} = "%lld"
      onFailure i (FailureMsg emsg@(ErrorMsg parts) backtrace) =
        let msg = concatMap onPart parts ++ "\n" ++ printfEscape backtrace
            msgargs = [[C.cexp|args[$int:j]|] | j <- [0 .. errorMsgNumArgs emsg -1]]
         in [C.cstm|case $int:i: {ctx->error = msgprintf($string:msg, $args:msgargs); break;}|]
      failure_cases =
        zipWith onFailure [(0 :: Int) ..] failures
   in [C.cstm|switch (failure_idx) { $stms:failure_cases }|]

copyDevToDev, copyDevToHost, copyHostToDev, copyScalarToDev, copyScalarFromDev :: Name
copyDevToDev = "copy_dev_to_dev"
copyDevToHost = "copy_dev_to_host"
copyHostToDev = "copy_host_to_dev"
copyScalarToDev = "copy_scalar_to_dev"
copyScalarFromDev = "copy_scalar_from_dev"

profilingEvent :: Name -> C.Exp
profilingEvent name =
  [C.cexp|(ctx->profiling_paused || !ctx->profiling) ? NULL
          : opencl_get_event(&ctx->opencl,
                             &ctx->$id:(kernelRuns name),
                             &ctx->$id:(kernelRuntime name))|]

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  T.Text ->
  T.Text ->
  [Name] ->
  M.Map KernelName KernelSafety ->
  [PrimType] ->
  M.Map Name SizeClass ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateBoilerplate opencl_code opencl_prelude cost_centres kernels types sizes failures = do
  final_inits <- GC.contextFinalInits

  let (ctx_opencl_fields, ctx_opencl_inits, top_decls, later_top_decls) =
        openClDecls cost_centres kernels (opencl_prelude <> opencl_code)

  mapM_ GC.earlyDecl top_decls

  let size_name_inits = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_var_inits = map (\k -> [C.cinit|$string:(zEncodeString (pretty k))|]) $ M.keys sizes
      size_class_inits = map (\c -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes
      num_sizes = M.size sizes

  GC.earlyDecl [C.cedecl|static const char *tuning_param_names[] = { $inits:size_name_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_vars[] = { $inits:size_var_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_classes[] = { $inits:size_class_inits };|]

  let size_decls = map (\k -> [C.csdecl|typename int64_t *$id:k;|]) $ M.keys sizes
  GC.earlyDecl [C.cedecl|struct tuning_params { $sdecls:size_decls };|]
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s { int in_use;
                               struct opencl_config opencl;
                               typename int64_t tuning_params[$int:num_sizes];
                               int num_build_opts;
                               const char **build_opts;
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
                         cfg->num_build_opts = 0;
                         cfg->build_opts = (const char**) malloc(sizeof(const char*));
                         cfg->build_opts[0] = NULL;
                         $stms:size_value_inits
                         opencl_config_init(&cfg->opencl, $int:num_sizes,
                                            tuning_param_names, tuning_param_vars,
                                            cfg->tuning_params, tuning_param_classes);
                         return cfg;
                       }|]
    )

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         assert(!cfg->in_use);
                         free(cfg->build_opts);
                         free(cfg);
                       }|]
    )

  GC.publicDef_ "context_config_add_build_option" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt) {
                         cfg->build_opts[cfg->num_build_opts] = opt;
                         cfg->num_build_opts++;
                         cfg->build_opts = (const char**) realloc(cfg->build_opts, (cfg->num_build_opts+1) * sizeof(const char*));
                         cfg->build_opts[cfg->num_build_opts] = NULL;
                       }|]
    )

  GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->opencl.profiling = cfg->opencl.logging = cfg->opencl.debugging = flag;
                       }|]
    )

  GC.publicDef_ "context_config_set_profiling" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->opencl.profiling = flag;
                       }|]
    )

  GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->opencl.logging = flag;
                       }|]
    )

  GC.publicDef_ "context_config_set_device" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *s);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *s) {
                         set_preferred_device(&cfg->opencl, s);
                       }|]
    )

  GC.publicDef_ "context_config_set_platform" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *s);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *s) {
                         set_preferred_platform(&cfg->opencl, s);
                       }|]
    )

  GC.publicDef_ "context_config_select_device_interactively" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         select_device_interactively(&cfg->opencl);
                       }|]
    )

  GC.publicDef_ "context_config_list_devices" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         (void)cfg;
                         list_devices();
                       }|]
    )

  GC.publicDef_ "context_config_dump_program_to" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->opencl.dump_program_to = path;
                       }|]
    )

  GC.publicDef_ "context_config_load_program_from" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->opencl.load_program_from = path;
                       }|]
    )

  GC.publicDef_ "context_config_dump_binary_to" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->opencl.dump_binary_to = path;
                       }|]
    )

  GC.publicDef_ "context_config_load_binary_from" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->opencl.load_binary_from = path;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_group_size" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int size);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->opencl.default_group_size = size;
                         cfg->opencl.default_group_size_changed = 1;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_num_groups" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int num) {
                         cfg->opencl.default_num_groups = num;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_tile_size" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->opencl.default_tile_size = size;
                         cfg->opencl.default_tile_size_changed = 1;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_reg_tile_size" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->opencl.default_reg_tile_size = size;
                       }|]
    )

  GC.publicDef_ "context_config_set_default_threshold" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->opencl.default_threshold = size;
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
                           cfg->opencl.default_group_size = new_value;
                           return 0;
                         }

                         if (strcmp(param_name, "default_num_groups") == 0) {
                           cfg->opencl.default_num_groups = new_value;
                           return 0;
                         }

                         if (strcmp(param_name, "default_threshold") == 0) {
                           cfg->opencl.default_threshold = new_value;
                           return 0;
                         }

                         if (strcmp(param_name, "default_tile_size") == 0) {
                           cfg->opencl.default_tile_size = new_value;
                           return 0;
                         }

                         if (strcmp(param_name, "default_reg_tile_size") == 0) {
                           cfg->opencl.default_reg_tile_size = new_value;
                           return 0;
                         }

                         return 1;
                       }|]
    )

  (fields, init_fields, free_fields) <- GC.contextContents
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
                         $sdecls:ctx_opencl_fields
                         typename cl_mem global_failure;
                         typename cl_mem global_failure_args;
                         struct opencl_context opencl;
                         struct tuning_params tuning_params;
                         // True if a potentially failing kernel has been enqueued.
                         typename cl_int failure_is_an_option;
                       };|]
    )

  mapM_ GC.earlyDecl later_top_decls

  GC.earlyDecl
    [C.cedecl|static void init_context_early(struct $id:cfg *cfg, struct $id:ctx* ctx) {
                     ctx->opencl.cfg = cfg->opencl;
                     ctx->detail_memory = cfg->opencl.debugging;
                     ctx->debugging = cfg->opencl.debugging;
                     ctx->profiling = cfg->opencl.profiling;
                     ctx->profiling_paused = 0;
                     ctx->logging = cfg->opencl.logging;
                     ctx->error = NULL;
                     ctx->log = stderr;
                     ctx->opencl.profiling_records_capacity = 200;
                     ctx->opencl.profiling_records_used = 0;
                     ctx->opencl.profiling_records =
                       malloc(ctx->opencl.profiling_records_capacity *
                              sizeof(struct profiling_record));
                     create_lock(&ctx->lock);

                     ctx->failure_is_an_option = 0;
                     $stms:init_fields
                     $stms:ctx_opencl_inits
  }|]

  let set_tuning_params =
        zipWith
          (\i k -> [C.cstm|ctx->tuning_params.$id:k = &cfg->tuning_params[$int:i];|])
          [(0 :: Int) ..]
          $ M.keys sizes
      max_failure_args =
        foldl max 0 $ map (errorMsgNumArgs . failureError) failures

  GC.earlyDecl
    [C.cedecl|static int init_context_late(struct $id:cfg *cfg, struct $id:ctx* ctx, typename cl_program prog) {
                     typename cl_int error;

                     typename cl_int no_error = -1;
                     ctx->global_failure =
                       clCreateBuffer(ctx->opencl.ctx,
                                      CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
                                      sizeof(cl_int), &no_error, &error);
                     OPENCL_SUCCEED_OR_RETURN(error);

                     // The +1 is to avoid zero-byte allocations.
                     ctx->global_failure_args =
                       clCreateBuffer(ctx->opencl.ctx,
                                      CL_MEM_READ_WRITE,
                                      sizeof(int64_t)*($int:max_failure_args+1), NULL, &error);
                     OPENCL_SUCCEED_OR_RETURN(error);

                     // Load all the kernels.
                     $stms:(map loadKernel (M.toList kernels))

                     $stms:final_inits
                     $stms:set_tuning_params

                     init_constants(ctx);
                     // Clear the free list of any deallocations that occurred while initialising constants.
                     OPENCL_SUCCEED_OR_RETURN(opencl_free_all(&ctx->opencl));

                     // The program will be properly freed after all the kernels have also been freed.
                     OPENCL_SUCCEED_OR_RETURN(clReleaseProgram(prog));

                     return futhark_context_sync(ctx);
  }|]

  let set_required_types =
        [ [C.cstm|required_types |= OPENCL_F64; |]
          | FloatType Float64 `elem` types
        ]

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

                          int required_types = 0;
                          $stms:set_required_types

                          init_context_early(cfg, ctx);
                          typename cl_program prog = setup_opencl(&ctx->opencl, opencl_program, required_types, cfg->build_opts);
                          init_context_late(cfg, ctx, prog);
                          return ctx;
                       }|]
    )

  GC.publicDef_ "context_new_with_command_queue" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg, typename cl_command_queue queue);|],
      [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg, typename cl_command_queue queue) {
                          assert(!cfg->in_use);
                          struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                          if (ctx == NULL) {
                            return NULL;
                          }
                          ctx->cfg = cfg;
                          ctx->cfg->in_use = 1;

                          int required_types = 0;
                          $stms:set_required_types

                          init_context_early(cfg, ctx);
                          typename cl_program prog = setup_opencl_with_command_queue(&ctx->opencl, queue, opencl_program, required_types, cfg->build_opts);
                          init_context_late(cfg, ctx, prog);
                          return ctx;
                       }|]
    )

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 $stms:free_fields
                                 free_constants(ctx);
                                 free_lock(&ctx->lock);
                                 $stms:(map releaseKernel (M.toList kernels))
                                 OPENCL_SUCCEED_FATAL(clReleaseMemObject(ctx->global_failure));
                                 OPENCL_SUCCEED_FATAL(clReleaseMemObject(ctx->global_failure_args));
                                 teardown_opencl(&ctx->opencl);
                                 ctx->cfg->in_use = 0;
                                 free(ctx);
                               }|]
    )

  GC.publicDef_ "context_sync" GC.MiscDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                 // Check for any delayed error.
                 typename cl_int failure_idx = -1;
                 if (ctx->failure_is_an_option) {
                   OPENCL_SUCCEED_OR_RETURN(
                     clEnqueueReadBuffer(ctx->opencl.queue,
                                         ctx->global_failure,
                                         CL_FALSE,
                                         0, sizeof(typename cl_int), &failure_idx,
                                         0, NULL, $exp:(profilingEvent copyScalarFromDev)));
                   ctx->failure_is_an_option = 0;
                 }

                 OPENCL_SUCCEED_OR_RETURN(clFinish(ctx->opencl.queue));

                 if (failure_idx >= 0) {
                   // We have to clear global_failure so that the next entry point
                   // is not considered a failure from the start.
                   typename cl_int no_failure = -1;
                   OPENCL_SUCCEED_OR_RETURN(
                    clEnqueueWriteBuffer(ctx->opencl.queue, ctx->global_failure, CL_TRUE,
                                         0, sizeof(cl_int), &no_failure,
                                         0, NULL, NULL));

                   typename int64_t args[$int:max_failure_args+1];
                   OPENCL_SUCCEED_OR_RETURN(
                     clEnqueueReadBuffer(ctx->opencl.queue,
                                         ctx->global_failure_args,
                                         CL_TRUE,
                                         0, sizeof(args), &args,
                                         0, NULL, $exp:(profilingEvent copyDevToHost)));

                   $stm:(failureSwitch failures)

                   return FUTHARK_PROGRAM_ERROR;
                 }
                 return 0;
               }|]
    )

  GC.publicDef_ "context_get_command_queue" GC.InitDecl $ \s ->
    ( [C.cedecl|typename cl_command_queue $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|typename cl_command_queue $id:s(struct $id:ctx* ctx) {
                 return ctx->opencl.queue;
               }|]
    )

  GC.onClear
    [C.citem|if (ctx->error == NULL) {
                        ctx->error = OPENCL_SUCCEED_NONFATAL(opencl_free_all(&ctx->opencl));
                      }|]

  GC.profileReport [C.citem|OPENCL_SUCCEED_FATAL(opencl_tally_profiling_records(&ctx->opencl));|]
  mapM_ GC.profileReport $
    costCentreReport $
      cost_centres ++ M.keys kernels

openClDecls ::
  [Name] ->
  M.Map KernelName KernelSafety ->
  T.Text ->
  ([C.FieldGroup], [C.Stm], [C.Definition], [C.Definition])
openClDecls cost_centres kernels opencl_program =
  (ctx_fields, ctx_inits, openCL_boilerplate, openCL_load)
  where
    opencl_program_fragments =
      -- Some C compilers limit the size of literal strings, so
      -- chunk the entire program into small bits here, and
      -- concatenate it again at runtime.
      [ [C.cinit|$string:s|]
        | s <- chunk 2000 $ T.unpack opencl_program
      ]

    ctx_fields =
      [ [C.csdecl|int total_runs;|],
        [C.csdecl|long int total_runtime;|]
      ]
        ++ [ [C.csdecl|typename cl_kernel $id:name;|]
             | name <- M.keys kernels
           ]
        ++ concat
          [ [ [C.csdecl|typename int64_t $id:(kernelRuntime name);|],
              [C.csdecl|int $id:(kernelRuns name);|]
            ]
            | name <- cost_centres ++ M.keys kernels
          ]

    ctx_inits =
      [ [C.cstm|ctx->total_runs = 0;|],
        [C.cstm|ctx->total_runtime = 0;|]
      ]
        ++ concat
          [ [ [C.cstm|ctx->$id:(kernelRuntime name) = 0;|],
              [C.cstm|ctx->$id:(kernelRuns name) = 0;|]
            ]
            | name <- cost_centres ++ M.keys kernels
          ]

    openCL_load =
      [ [C.cedecl|
void post_opencl_setup(struct opencl_context *ctx, struct opencl_device_option *option) {
  $stms:(map sizeHeuristicsCode sizeHeuristicsTable)
}|]
      ]

    program_fragments = opencl_program_fragments ++ [[C.cinit|NULL|]]
    openCL_boilerplate =
      [C.cunit|
          $esc:("typedef cl_mem fl_mem_t;")
          $esc:(T.unpack freeListH)
          $esc:(T.unpack openclH)
          static const char *opencl_program[] = {$inits:program_fragments};|]

loadKernel :: (KernelName, KernelSafety) -> C.Stm
loadKernel (name, safety) =
  [C.cstm|{
  ctx->$id:name = clCreateKernel(prog, $string:(pretty (C.toIdent name mempty)), &error);
  OPENCL_SUCCEED_FATAL(error);
  $items:set_args
  if (ctx->debugging) {
    fprintf(ctx->log, "Created kernel %s.\n", $string:(pretty name));
  }
  }|]
  where
    set_global_failure =
      [C.citem|OPENCL_SUCCEED_FATAL(
                     clSetKernelArg(ctx->$id:name, 0, sizeof(typename cl_mem),
                                    &ctx->global_failure));|]
    set_global_failure_args =
      [C.citem|OPENCL_SUCCEED_FATAL(
                     clSetKernelArg(ctx->$id:name, 2, sizeof(typename cl_mem),
                                    &ctx->global_failure_args));|]
    set_args = case safety of
      SafetyNone -> []
      SafetyCheap -> [set_global_failure]
      SafetyFull -> [set_global_failure, set_global_failure_args]

releaseKernel :: (KernelName, KernelSafety) -> C.Stm
releaseKernel (name, _) = [C.cstm|OPENCL_SUCCEED_FATAL(clReleaseKernel(ctx->$id:name));|]

kernelRuntime :: KernelName -> Name
kernelRuntime = (<> "_total_runtime")

kernelRuns :: KernelName -> Name
kernelRuns = (<> "_runs")

costCentreReport :: [Name] -> [C.BlockItem]
costCentreReport names = report_kernels ++ [report_total]
  where
    longest_name = foldl max 0 $ map (length . pretty) names
    report_kernels = concatMap reportKernel names
    format_string name =
      let padding = replicate (longest_name - length name) ' '
       in unwords
            [ name ++ padding,
              "ran %5d times; avg: %8ldus; total: %8ldus\n"
            ]
    reportKernel name =
      let runs = kernelRuns name
          total_runtime = kernelRuntime name
       in [ [C.citem|
               str_builder(&builder,
                           $string:(format_string (pretty name)),
                           ctx->$id:runs,
                           (long int) ctx->$id:total_runtime / (ctx->$id:runs != 0 ? ctx->$id:runs : 1),
                           (long int) ctx->$id:total_runtime);
              |],
            [C.citem|ctx->total_runtime += ctx->$id:total_runtime;|],
            [C.citem|ctx->total_runs += ctx->$id:runs;|]
          ]

    report_total =
      [C.citem|
                          str_builder(&builder, "%d operations with cumulative runtime: %6ldus\n",
                                      ctx->total_runs, ctx->total_runtime);
                        |]

sizeHeuristicsCode :: SizeHeuristic -> C.Stm
sizeHeuristicsCode (SizeHeuristic platform_name device_type which (TPrimExp what)) =
  [C.cstm|
   if ($exp:which' == 0 &&
       strstr(option->platform_name, $string:platform_name) != NULL &&
       (option->device_type & $exp:(clDeviceType device_type)) == $exp:(clDeviceType device_type)) {
     $items:get_size
   }|]
  where
    clDeviceType DeviceGPU = [C.cexp|CL_DEVICE_TYPE_GPU|]
    clDeviceType DeviceCPU = [C.cexp|CL_DEVICE_TYPE_CPU|]

    which' = case which of
      LockstepWidth -> [C.cexp|ctx->lockstep_width|]
      NumGroups -> [C.cexp|ctx->cfg.default_num_groups|]
      GroupSize -> [C.cexp|ctx->cfg.default_group_size|]
      TileSize -> [C.cexp|ctx->cfg.default_tile_size|]
      RegTileSize -> [C.cexp|ctx->cfg.default_reg_tile_size|]
      Threshold -> [C.cexp|ctx->cfg.default_threshold|]

    get_size =
      let (e, m) = runState (GC.compilePrimExp onLeaf what) mempty
       in concat (M.elems m) ++ [[C.citem|$exp:which' = $exp:e;|]]

    onLeaf (DeviceInfo s) = do
      let s' = "CL_DEVICE_" ++ s
          v = s ++ "_val"
      m <- get
      case M.lookup s m of
        Nothing ->
          -- XXX: Cheating with the type here; works for the infos we
          -- currently use because we zero-initialise and assume a
          -- little-endian platform, but should be made more
          -- size-aware in the future.
          modify $
            M.insert
              s'
              [C.citems|size_t $id:v = 0;
                        clGetDeviceInfo(ctx->device, $id:s',
                                        sizeof($id:v), &$id:v,
                                        NULL);|]
        Just _ -> return ()

      return [C.cexp|$id:v|]

-- Output size information if logging is enabled.
--
-- The autotuner depends on the format of this output, so use caution if
-- changing it.
sizeLoggingCode :: VName -> Name -> C.Exp -> GC.CompilerM op () ()
sizeLoggingCode v key x' = do
  GC.stm
    [C.cstm|if (ctx->logging) {
    fprintf(ctx->log, "Compared %s <= %ld: %s.\n", $string:(prettyOneLine key), (long)$exp:x', $id:v ? "true" : "false");
    }|]

-- Options that are common to multiple GPU-like backends.
commonOptions :: [Option]
commonOptions =
  [ Option
      { optionLongName = "device",
        optionShortName = Just 'd',
        optionArgument = RequiredArgument "NAME",
        optionDescription = "Use the first OpenCL device whose name contains the given string.",
        optionAction = [C.cstm|futhark_context_config_set_device(cfg, optarg);|]
      },
    Option
      { optionLongName = "default-group-size",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default size of OpenCL workgroups that are launched.",
        optionAction = [C.cstm|futhark_context_config_set_default_group_size(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-num-groups",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default number of OpenCL workgroups that are launched.",
        optionAction = [C.cstm|futhark_context_config_set_default_num_groups(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-tile-size",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default tile size used when performing two-dimensional tiling.",
        optionAction = [C.cstm|futhark_context_config_set_default_tile_size(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-reg-tile-size",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default register tile size used when performing two-dimensional tiling.",
        optionAction = [C.cstm|futhark_context_config_set_default_reg_tile_size(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-threshold",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default parallelism threshold.",
        optionAction = [C.cstm|futhark_context_config_set_default_threshold(cfg, atoi(optarg));|]
      }
  ]
