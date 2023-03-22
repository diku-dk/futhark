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
    failureMsgFunction,
    costCentreReport,
    kernelRuntime,
    kernelRuns,
    sizeLoggingCode,
  )
where

import Control.Monad
import Control.Monad.State
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.OpenCL.Heuristics
import Futhark.CodeGen.RTS.C (backendsOpenclH)
import Futhark.Util (chunk)
import Futhark.Util.Pretty (prettyTextOneLine)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

failureMsgFunction :: [FailureMsg] -> C.Definition
failureMsgFunction failures =
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
         in [C.cstm|case $int:i: {return msgprintf($string:msg, $args:msgargs); break;}|]
      failure_cases =
        zipWith onFailure [(0 :: Int) ..] failures
   in [C.cedecl|static char* get_failure_msg(int failure_idx, typename int64_t args[]) {
                  switch (failure_idx) { $stms:failure_cases }
                  return strdup("Unknown error.  This is a compiler bug.");
                }|]

copyDevToDev, copyDevToHost, copyHostToDev, copyScalarToDev, copyScalarFromDev :: Name
copyDevToDev = "copy_dev_to_dev"
copyDevToHost = "copy_dev_to_host"
copyHostToDev = "copy_host_to_dev"
copyScalarToDev = "copy_scalar_to_dev"
copyScalarFromDev = "copy_scalar_from_dev"

profilingEvent :: Name -> C.Exp
profilingEvent name =
  [C.cexp|(ctx->profiling_paused || !ctx->profiling) ? NULL
          : opencl_get_event(ctx,
                             &ctx->program->$id:(kernelRuns name),
                             &ctx->program->$id:(kernelRuntime name))|]

releaseKernel :: (KernelName, KernelSafety) -> C.Stm
releaseKernel (name, _) = [C.cstm|OPENCL_SUCCEED_FATAL(clReleaseKernel(ctx->program->$id:name));|]

loadKernel :: (KernelName, KernelSafety) -> C.Stm
loadKernel (name, safety) =
  [C.cstm|{
  ctx->program->$id:name = clCreateKernel(ctx->clprogram, $string:(T.unpack (idText (C.toIdent name mempty))), &error);
  OPENCL_SUCCEED_FATAL(error);
  $items:set_args
  if (ctx->debugging) {
    fprintf(ctx->log, "Created kernel %s.\n", $string:(prettyString name));
  }
  }|]
  where
    set_global_failure =
      [C.citem|OPENCL_SUCCEED_FATAL(
                     clSetKernelArg(ctx->program->$id:name, 0, sizeof(typename cl_mem),
                                    &ctx->global_failure));|]
    set_global_failure_args =
      [C.citem|OPENCL_SUCCEED_FATAL(
                     clSetKernelArg(ctx->program->$id:name, 2, sizeof(typename cl_mem),
                                    &ctx->global_failure_args));|]
    set_args = case safety of
      SafetyNone -> []
      SafetyCheap -> [set_global_failure]
      SafetyFull -> [set_global_failure, set_global_failure_args]

generateOpenCLDecls ::
  [Name] ->
  M.Map KernelName KernelSafety ->
  GC.CompilerM op s ()
generateOpenCLDecls cost_centres kernels = do
  forM_ (M.toList kernels) $ \(name, safety) ->
    GC.contextFieldDyn
      (C.toIdent name mempty)
      [C.cty|typename cl_kernel|]
      (loadKernel (name, safety))
      (releaseKernel (name, safety))
  forM_ (cost_centres <> M.keys kernels) $ \name -> do
    GC.contextField
      (C.toIdent (kernelRuntime name) mempty)
      [C.cty|typename int64_t|]
      (Just [C.cexp|0|])
    GC.contextField
      (C.toIdent (kernelRuns name) mempty)
      [C.cty|int|]
      (Just [C.cexp|0|])
  GC.earlyDecl
    [C.cedecl|
void post_opencl_setup(struct futhark_context *ctx, struct opencl_device_option *option) {
  $stms:(map sizeHeuristicsCode sizeHeuristicsTable)
}|]

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  T.Text ->
  T.Text ->
  [Name] ->
  M.Map KernelName KernelSafety ->
  [PrimType] ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateBoilerplate opencl_program opencl_prelude cost_centres kernels types failures = do
  let opencl_program_fragments =
        -- Some C compilers limit the size of literal strings, so
        -- chunk the entire program into small bits here, and
        -- concatenate it again at runtime.
        [[C.cinit|$string:s|] | s <- chunk 2000 $ T.unpack $ opencl_prelude <> opencl_program]
      program_fragments = opencl_program_fragments ++ [[C.cinit|NULL|]]
      f64_required
        | FloatType Float64 `elem` types = [C.cexp|1|]
        | otherwise = [C.cexp|0|]
      max_failure_args = foldl max 0 $ map (errorMsgNumArgs . failureError) failures
  mapM_
    GC.earlyDecl
    [C.cunit|static const int max_failure_args = $int:max_failure_args;
             static const int f64_required = $exp:f64_required;
             static const char *opencl_program[] = {$inits:program_fragments};
             $esc:(T.unpack backendsOpenclH)
            |]
  GC.earlyDecl $ failureMsgFunction failures

  generateOpenCLDecls cost_centres kernels

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
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_command_queue(struct futhark_context_config *cfg, typename cl_command_queue);|]
  GC.headerDecl GC.MiscDecl [C.cedecl|typename cl_command_queue futhark_context_get_command_queue(struct futhark_context* ctx);|]

  GC.generateProgramStruct

  GC.onClear
    [C.citem|if (ctx->error == NULL) { ctx->error = OPENCL_SUCCEED_NONFATAL(opencl_free_all(ctx)); }|]

  GC.profileReport [C.citem|OPENCL_SUCCEED_FATAL(opencl_tally_profiling_records(ctx));|]
  mapM_ GC.profileReport $ costCentreReport $ cost_centres ++ M.keys kernels

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
                           ctx->program->$id:runs,
                           (long int) ctx->program->$id:total_runtime / (ctx->program->$id:runs != 0 ? ctx->program->$id:runs : 1),
                           (long int) ctx->program->$id:total_runtime);
              |],
            [C.citem|ctx->total_runtime += ctx->program->$id:total_runtime;|],
            [C.citem|ctx->total_runs += ctx->program->$id:runs;|]
          ]

    report_total =
      [C.citem|str_builder(&builder, "%d operations with cumulative runtime: %6ldus\n",
                           ctx->total_runs, ctx->total_runtime);|]

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
      NumGroups -> [C.cexp|ctx->cfg->default_num_groups|]
      GroupSize -> [C.cexp|ctx->cfg->default_group_size|]
      TileSize -> [C.cexp|ctx->cfg->default_tile_size|]
      RegTileSize -> [C.cexp|ctx->cfg->default_reg_tile_size|]
      Threshold -> [C.cexp|ctx->cfg->default_threshold|]

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
