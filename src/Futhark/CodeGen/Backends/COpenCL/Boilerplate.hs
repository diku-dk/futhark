{-# LANGUAGE QuasiQuotes #-}

module Futhark.CodeGen.Backends.COpenCL.Boilerplate
  ( generateBoilerplate,
    profilingEvent,
    copyDevToDev,
    copyDevToHost,
    copyHostToDev,
    copyScalarToDev,
    copyScalarFromDev,
    genProfileReport,
    commonOptions,
    failureMsgFunction,
  )
where

import Control.Monad.State
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.OpenCL.Heuristics
import Futhark.CodeGen.RTS.C (backendsOpenclH, gpuH)
import Futhark.Util (chunk)
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
                  (void)args;
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
          : opencl_get_event(ctx, $string:(nameToString name))|]

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  T.Text ->
  T.Text ->
  [Name] ->
  [PrimType] ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateBoilerplate gpu_program opencl_prelude cost_centres types failures = do
  let gpu_program_fragments =
        -- Some C compilers limit the size of literal strings, so
        -- chunk the entire program into small bits here, and
        -- concatenate it again at runtime.
        [[C.cinit|$string:s|] | s <- chunk 2000 $ T.unpack $ opencl_prelude <> gpu_program]
      program_fragments = gpu_program_fragments ++ [[C.cinit|NULL|]]
      f64_required
        | FloatType Float64 `elem` types = [C.cexp|1|]
        | otherwise = [C.cexp|0|]
      max_failure_args = foldl max 0 $ map (errorMsgNumArgs . failureError) failures
  mapM_
    GC.earlyDecl
    [C.cunit|static const int max_failure_args = $int:max_failure_args;
             static const int f64_required = $exp:f64_required;
             static const char *gpu_program[] = {$inits:program_fragments};
             $esc:(T.unpack backendsOpenclH)
             $esc:(T.unpack gpuH)
            |]
  GC.earlyDecl $ failureMsgFunction failures

  GC.earlyDecl
    [C.cedecl|void post_opencl_setup(struct futhark_context *ctx, struct opencl_device_option *option) {
             $stms:(map sizeHeuristicsCode sizeHeuristicsTable)
             }|]

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

  genProfileReport cost_centres

genProfileReport :: [Name] -> GC.CompilerM op s ()
genProfileReport cost_centres =
  GC.profileReport
    [C.citem|{struct cost_centres* ccs = cost_centres_new(sizeof(struct cost_centres));
              $stms:(map initCostCentre (def_cost_centres<>cost_centres))
              tally_profiling_records(ctx, ccs);
              cost_centre_report(ccs, &builder);
              cost_centres_free(ccs);
              }|]
  where
    def_cost_centres =
      [ copyDevToDev,
        copyDevToHost,
        copyHostToDev,
        copyScalarToDev,
        copyScalarFromDev
      ]
    initCostCentre v =
      [C.cstm|cost_centres_init(ccs, $string:(nameToString v));|]

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
