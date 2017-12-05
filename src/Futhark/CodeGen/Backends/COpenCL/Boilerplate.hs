{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.COpenCL.Boilerplate
  ( generateBoilerplate

  , kernelRuntime
  , kernelRuns
  ) where

import Data.FileEmbed
import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.CodeGen.ImpCode.OpenCL
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.OpenCL.Kernels
import Futhark.Util (chunk)

generateBoilerplate :: String -> String -> [String] -> [PrimType]
                    -> GC.CompilerM OpenCL () ()
generateBoilerplate opencl_code opencl_prelude kernel_names types = do
  ctx_ty <- GC.contextType
  final_inits <- GC.contextFinalInits

  let (ctx_opencl_fields, ctx_opencl_inits, top_decls, later_top_decls) =
        openClDecls ctx_ty final_inits kernel_names opencl_code opencl_prelude

  GC.earlyDecls top_decls

  cfg <- GC.publicName "context_config"
  new_cfg <- GC.publicName "context_config_new"
  free_cfg <- GC.publicName "context_config_free"
  cfg_set_debugging <- GC.publicName "context_config_set_debugging"
  cfg_set_device <- GC.publicName "context_config_set_device"
  cfg_set_platform <- GC.publicName "context_config_set_platform"
  cfg_dump_program_to <- GC.publicName "context_config_dump_program_to"
  cfg_load_program_from <- GC.publicName "context_config_load_program_from"
  cfg_set_group_size <- GC.publicName "context_config_set_group_size"
  cfg_set_num_groups <- GC.publicName "context_config_set_num_groups"

  GC.headerDecl GC.InitDecl [C.cedecl|struct $id:cfg;|]
  GC.headerDecl GC.InitDecl [C.cedecl|struct $id:cfg* $id:new_cfg();|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:free_cfg(struct $id:cfg* cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:cfg_set_debugging(struct $id:cfg* cfg, int flag);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:cfg_set_device(struct $id:cfg* cfg, const char *s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:cfg_set_platform(struct $id:cfg* cfg, const char *s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:cfg_dump_program_to(struct $id:cfg* cfg, const char *path);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:cfg_load_program_from(struct $id:cfg* cfg, const char *path);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:cfg_set_group_size(struct $id:cfg* cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:cfg_set_num_groups(struct $id:cfg* cfg, int num);|]

  GC.libDecl [C.cedecl|struct $id:cfg {
                         struct opencl_config opencl;
                       };|]
  GC.libDecl [C.cedecl|struct $id:cfg* $id:new_cfg() {
                         struct $id:cfg *cfg = malloc(sizeof(struct $id:cfg));
                         if (cfg == NULL) {
                           return NULL;
                         }
                         opencl_config_init(&cfg->opencl);
                         cfg->opencl.transpose_block_dim = $int:(transposeBlockDim::Int);
                         return cfg;
                       }|]
  GC.libDecl [C.cedecl|void $id:free_cfg(struct $id:cfg* cfg) {
                         free(cfg);
                       }|]
  GC.libDecl [C.cedecl|void $id:cfg_set_debugging(struct $id:cfg* cfg, int flag) {
                         cfg->opencl.debugging = flag;
                       }|]
  GC.libDecl [C.cedecl|void $id:cfg_set_device(struct $id:cfg* cfg, const char *s) {
                         set_preferred_device(&cfg->opencl, s);
                       }|]
  GC.libDecl [C.cedecl|void $id:cfg_set_platform(struct $id:cfg* cfg, const char *s) {
                         set_preferred_platform(&cfg->opencl, s);
                       }|]
  GC.libDecl [C.cedecl|void $id:cfg_dump_program_to(struct $id:cfg* cfg, const char *path) {
                         cfg->opencl.dump_program_to = path;
                       }|]
  GC.libDecl [C.cedecl|void $id:cfg_load_program_from(struct $id:cfg* cfg, const char *path) {
                         cfg->opencl.load_program_from = path;
                       }|]
  GC.libDecl [C.cedecl|void $id:cfg_set_group_size(struct $id:cfg* cfg, int size) {
                         cfg->opencl.group_size = size;
                       }|]
  GC.libDecl [C.cedecl|void $id:cfg_set_num_groups(struct $id:cfg* cfg, int num) {
                         cfg->opencl.num_groups = num;
                       }|]

  ctx <- GC.publicName "context"
  new_ctx <- GC.publicName "context_new"
  free_ctx <- GC.publicName "context_free"
  sync_ctx <- GC.publicName "context_sync"

  GC.headerDecl GC.InitDecl [C.cedecl|struct $id:ctx;|]
  GC.headerDecl GC.InitDecl [C.cedecl|struct $id:ctx* $id:new_ctx(struct $id:cfg* cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void $id:free_ctx(struct $id:ctx* ctx);|]
  GC.headerDecl GC.InitDecl [C.cedecl|int $id:sync_ctx(struct $id:ctx* ctx);|]

  (fields, init_fields) <- GC.contextContents

  GC.libDecl [C.cedecl|struct $id:ctx {
                         int detail_memory;
                         int debugging;
                         $sdecls:fields
                         $sdecls:ctx_opencl_fields
                         struct opencl_context opencl;
                       };|]

  mapM_ GC.libDecl later_top_decls

  let set_required_types = [ [C.cstm|required_types |= OPENCL_F64; |]
                           | FloatType Float64 `elem` types ]
  GC.libDecl [C.cedecl|struct $id:ctx* $id:new_ctx(struct $id:cfg* cfg) {
                          struct $id:ctx* ctx = malloc(sizeof(struct $id:ctx));
                          if (ctx == NULL) {
                            return NULL;
                          }
                          ctx->detail_memory = cfg->opencl.debugging;
                          ctx->debugging = cfg->opencl.debugging;
                          ctx->opencl.cfg = cfg->opencl;
                          $stms:init_fields
                          $stms:ctx_opencl_inits

                          int required_types = 0;
                          $stms:set_required_types

                          setup_opencl_and_load_kernels(ctx, required_types);

                          return ctx;
                       }|]
  GC.libDecl [C.cedecl|void $id:free_ctx(struct $id:ctx* ctx) {
                                 free(ctx);
                               }|]
  GC.libDecl [C.cedecl|int $id:sync_ctx(struct $id:ctx* ctx) {
                         OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
                         return 0;
                       }|]

  mapM_ GC.debugReport $ openClReport kernel_names

openClDecls :: C.Type -> [C.Stm] -> [String] -> String -> String
            -> ([C.FieldGroup], [C.Stm], [C.Definition], [C.Definition])
openClDecls ctx_ty final_inits kernel_names opencl_program opencl_prelude =
  (ctx_fields, ctx_inits, openCL_boilerplate, openCL_load)
  where opencl_program_fragments =
          -- Some C compilers limit the size of literal strings, so
          -- chunk the entire program into small bits here, and
          -- concatenate it again at runtime.
          [ [C.cinit|$string:s|] | s <- chunk 2000 (opencl_prelude++opencl_program) ]
        nullptr = [C.cinit|NULL|]

        ctx_fields =
          [ [C.csdecl|int total_runs;|],
            [C.csdecl|int total_runtime;|] ] ++
          concat
          [ [ [C.csdecl|typename cl_kernel $id:name;|]
            , [C.csdecl|int $id:(kernelRuntime name);|]
            , [C.csdecl|int $id:(kernelRuns name);|]
            ]
          | name <- kernel_names ]

        ctx_inits =
          [ [C.cstm|ctx->total_runs = 0;|],
            [C.cstm|ctx->total_runtime = 0;|] ] ++
          concat
          [ [ [C.cstm|ctx->$id:(kernelRuntime name) = 0;|]
            , [C.cstm|ctx->$id:(kernelRuns name) = 0;|]
            ]
          | name <- kernel_names ]

        openCL_load = [
          [C.cedecl|
void setup_opencl_and_load_kernels($ty:ctx_ty *ctx, int required_types) {
  typename cl_int error;
  typename cl_program prog = setup_opencl(&ctx->opencl, opencl_program, required_types);
  // Load all the kernels.
  $stms:(map (loadKernelByName) kernel_names)

  $stms:final_inits
}|],
          [C.cedecl|
void post_opencl_setup(struct opencl_context *ctx, struct opencl_device_option *option) {
  $stms:(map sizeHeuristicsCode sizeHeuristicsTable)
}|]]

        openCL_h = $(embedStringFile "rts/c/opencl.h")

        openCL_boilerplate = [C.cunit|
          $esc:openCL_h
          const char *opencl_program[] =
            {$inits:(opencl_program_fragments++[nullptr])};|]

loadKernelByName :: String -> C.Stm
loadKernelByName name = [C.cstm|{
  ctx->$id:name = clCreateKernel(prog, $string:name, &error);
  assert(error == 0);
  if (ctx->debugging) {
    fprintf(stderr, "Created kernel %s.\n", $string:name);
  }
  }|]

kernelRuntime :: String -> String
kernelRuntime = (++"_total_runtime")

kernelRuns :: String -> String
kernelRuns = (++"_runs")

openClReport :: [String] -> [C.BlockItem]
openClReport names = report_kernels ++ [report_total]
  where longest_name = foldl max 0 $ map length names
        report_kernels = concatMap reportKernel names
        format_string name =
          let padding = replicate (longest_name - length name) ' '
          in unwords ["Kernel",
                      name ++ padding,
                      "executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n"]
        reportKernel name =
          let runs = kernelRuns name
              total_runtime = kernelRuntime name
          in [[C.citem|
               fprintf(stderr,
                       $string:(format_string name),
                       ctx->$id:runs,
                       (long int) ctx->$id:total_runtime / (ctx->$id:runs != 0 ? ctx->$id:runs : 1),
                       (long int) ctx->$id:total_runtime);
              |],
              [C.citem|ctx->total_runtime += ctx->$id:total_runtime;|],
              [C.citem|ctx->total_runs += ctx->$id:runs;|]]

        report_total = [C.citem|
                          if (ctx->debugging) {
                            fprintf(stderr, "Ran %d kernels with cumulative runtime: %6ldus\n",
                                    ctx->total_runs, ctx->total_runtime);
                          }
                        |]

sizeHeuristicsCode :: SizeHeuristic -> C.Stm
sizeHeuristicsCode
  (SizeHeuristic platform_name device_type which what) =
  [C.cstm|
   if ($exp:which' == 0 &&
       strstr(option->platform_name, $string:platform_name) != NULL &&
       option->device_type == $exp:(clDeviceType device_type)) {
     $stm:get_size
   }|]
  where clDeviceType DeviceGPU = [C.cexp|CL_DEVICE_TYPE_GPU|]
        clDeviceType DeviceCPU = [C.cexp|CL_DEVICE_TYPE_CPU|]

        which' = case which of
                   LockstepWidth -> [C.cexp|ctx->lockstep_width|]
                   NumGroups -> [C.cexp|ctx->cfg.num_groups|]
                   GroupSize -> [C.cexp|ctx->cfg.group_size|]

        get_size = case what of
                     HeuristicConst x ->
                       [C.cstm|$exp:which' = $int:x;|]
                     HeuristicDeviceInfo s ->
                       -- This only works for device info that fits.
                       let s' = "CL_DEVICE_" ++ s
                       in [C.cstm|clGetDeviceInfo(ctx->device,
                                                  $id:s',
                                                  sizeof($exp:which'),
                                                  &$exp:which',
                                                  NULL);|]
