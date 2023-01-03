{-# LANGUAGE QuasiQuotes #-}

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
    generateTuningParams,
  )
where

import Control.Monad.State
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.OpenCL.Heuristics
import Futhark.CodeGen.RTS.C (backendsOpenclH)
import Futhark.Util (chunk, zEncodeText)
import Futhark.Util.Pretty (prettyTextOneLine)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

failureSwitch :: [FailureMsg] -> C.Stm
failureSwitch failures =
  let printfEscape =
        let escapeChar '%' = "%%"
            escapeChar c = [c]
         in concatMap escapeChar
      onPart (ErrorString s) = printfEscape $ T.unpack s
      -- FIXME: bogus for non-ints.
      onPart ErrorVal {} = "%lld"
      onFailure i (FailureMsg emsg@(ErrorMsg parts) backtrace) =
        let msg = concatMap onPart parts ++ "\n" ++ printfEscape backtrace
            msgargs = [[C.cexp|args[$int:j]|] | j <- [0 .. errorMsgNumArgs emsg - 1]]
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

generateTuningParams :: M.Map Name SizeClass -> GC.CompilerM op a ()
generateTuningParams sizes = do
  let strinit s = [C.cinit|$string:(T.unpack s)|]
      intinit x = [C.cinit|$int:x|]
      size_name_inits = map (strinit . prettyText) $ M.keys sizes
      size_var_inits = map (strinit . zEncodeText . prettyText) $ M.keys sizes
      size_class_inits = map (strinit . prettyText) $ M.elems sizes
      size_default_inits = map (intinit . fromMaybe 0 . sizeDefault) $ M.elems sizes
      size_decls = map (\k -> [C.csdecl|typename int64_t *$id:k;|]) $ M.keys sizes
      num_sizes = length sizes
  GC.earlyDecl [C.cedecl|struct tuning_params { $sdecls:size_decls };|]
  GC.earlyDecl [C.cedecl|static const int num_tuning_params = $int:num_sizes;|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_names[] = { $inits:size_name_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_vars[] = { $inits:size_var_inits };|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_classes[] = { $inits:size_class_inits };|]
  GC.earlyDecl [C.cedecl|static typename int64_t tuning_param_defaults[] = { $inits:size_default_inits };|]

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

  generateTuningParams sizes

  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_add_build_option(struct futhark_context_config *cfg, const char* opt);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_device(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_platform(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_select_device_interactively(struct futhark_context_config *cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_list_devices(struct futhark_context_config *cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_dump_program_to(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_load_program_from(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_dump_binary_to(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_load_binary_from(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_reg_tile_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_threshold(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|int futhark_context_config_set_tuning_param(struct futhark_context_config *cfg, const char *param_name, size_t new_value);|]

  (fields, init_fields, free_fields) <- GC.contextContents
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

                         typename cl_mem global_failure;
                         typename cl_mem global_failure_args;
                         struct opencl_context opencl;
                         struct tuning_params tuning_params;
                         // True if a potentially failing kernel has been enqueued.
                         typename cl_int failure_is_an_option;
                         int total_runs;
                         long int total_runtime;
                         typename int64_t peak_mem_usage_device;
                         typename int64_t cur_mem_usage_device;

                         $sdecls:fields
                         $sdecls:ctx_opencl_fields
                       };|]
    )

  mapM_ GC.earlyDecl later_top_decls

  GC.earlyDecl
    [C.cedecl|static void init_context_early(struct futhark_context_config *cfg, struct $id:ctx* ctx) {
                     context_setup(cfg, ctx);

                     ctx->opencl.profiling_records_capacity = 200;
                     ctx->opencl.profiling_records_used = 0;
                     ctx->opencl.profiling_records =
                       malloc(ctx->opencl.profiling_records_capacity *
                              sizeof(struct profiling_record));

                     ctx->failure_is_an_option = 0;
                     ctx->total_runs = 0;
                     ctx->total_runtime = 0;
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
    [C.cedecl|static int init_context_late(struct futhark_context_config *cfg, struct $id:ctx* ctx, typename cl_program prog) {
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
    ( [C.cedecl|struct $id:ctx* $id:s(struct futhark_context_config* cfg);|],
      [C.cedecl|struct $id:ctx* $id:s(struct futhark_context_config* cfg) {
                          struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                          if (ctx == NULL) {
                            return NULL;
                          }

                          int required_types = 0;
                          $stms:set_required_types

                          init_context_early(cfg, ctx);
                          typename cl_program prog =
                            setup_opencl(ctx->cfg, &ctx->opencl, opencl_program, required_types, cfg->build_opts,
                                         cfg->cache_fname);
                          init_context_late(cfg, ctx, prog);
                          return ctx;
                       }|]
    )

  GC.publicDef_ "context_new_with_command_queue" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:ctx* $id:s(struct futhark_context_config* cfg, typename cl_command_queue queue);|],
      [C.cedecl|struct $id:ctx* $id:s(struct futhark_context_config* cfg, typename cl_command_queue queue) {
                          struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                          if (ctx == NULL) {
                            return NULL;
                          }

                          int required_types = 0;
                          $stms:set_required_types

                          init_context_early(cfg, ctx);
                          typename cl_program prog =
                            setup_opencl_with_command_queue(ctx->cfg, &ctx->opencl, queue, opencl_program, required_types, cfg->build_opts,
                                                            cfg->cache_fname);
                          init_context_late(cfg, ctx, prog);
                          return ctx;
                       }|]
    )

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 $stms:free_fields
                                 context_teardown(ctx);
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
      [ [C.csdecl|typename cl_kernel $id:name;|]
        | name <- M.keys kernels
      ]
        ++ concat
          [ [ [C.csdecl|typename int64_t $id:(kernelRuntime name);|],
              [C.csdecl|int $id:(kernelRuns name);|]
            ]
            | name <- cost_centres ++ M.keys kernels
          ]

    ctx_inits =
      concat
        [ [ [C.cstm|ctx->$id:(kernelRuntime name) = 0;|],
            [C.cstm|ctx->$id:(kernelRuns name) = 0;|]
          ]
          | name <- cost_centres ++ M.keys kernels
        ]

    openCL_load =
      [ [C.cedecl|
void post_opencl_setup(struct futhark_context_config *cfg, struct opencl_context *ctx, struct opencl_device_option *option) {
  $stms:(map sizeHeuristicsCode sizeHeuristicsTable)
}|]
      ]

    program_fragments = opencl_program_fragments ++ [[C.cinit|NULL|]]
    openCL_boilerplate =
      [C.cunit|
          $esc:(T.unpack backendsOpenclH)
          static const char *opencl_program[] = {$inits:program_fragments};|]

loadKernel :: (KernelName, KernelSafety) -> C.Stm
loadKernel (name, safety) =
  [C.cstm|{
  ctx->$id:name = clCreateKernel(prog, $string:(T.unpack (idText (C.toIdent name mempty))), &error);
  OPENCL_SUCCEED_FATAL(error);
  $items:set_args
  if (ctx->debugging) {
    fprintf(ctx->log, "Created kernel %s.\n", $string:(prettyString name));
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
    longest_name = foldl max 0 $ map (length . prettyString) names
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
                           $string:(format_string (prettyString name)),
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
      NumGroups -> [C.cexp|cfg->default_num_groups|]
      GroupSize -> [C.cexp|cfg->default_group_size|]
      TileSize -> [C.cexp|cfg->default_tile_size|]
      RegTileSize -> [C.cexp|cfg->default_reg_tile_size|]
      Threshold -> [C.cexp|cfg->default_threshold|]

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
        Just _ -> pure ()

      pure [C.cexp|$id:v|]

-- Output size information if logging is enabled.
--
-- The autotuner depends on the format of this output, so use caution if
-- changing it.
sizeLoggingCode :: VName -> Name -> C.Exp -> GC.CompilerM op () ()
sizeLoggingCode v key x' = do
  GC.stm
    [C.cstm|if (ctx->logging) {
    fprintf(ctx->log, "Compared %s <= %ld: %s.\n", $string:(T.unpack (prettyTextOneLine key)), (long)$exp:x', $id:v ? "true" : "false");
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

{-# NOINLINE generateBoilerplate #-}
