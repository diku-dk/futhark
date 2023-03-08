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
        pevents = cuda_get_events(ctx,
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

generateCUDADecls ::
  [Name] ->
  M.Map KernelName KernelSafety ->
  GC.CompilerM op s ()
generateCUDADecls cost_centres kernels = do
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
                                     ctx->module,
                                     $string:(T.unpack (idText (C.toIdent name mempty)))));|]
      [C.cstm|{}|]
    forCostCentre name

  mapM_ forCostCentre cost_centres

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  T.Text ->
  T.Text ->
  [Name] ->
  M.Map KernelName KernelSafety ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateBoilerplate cuda_program cuda_prelude cost_centres kernels failures = do
  let cuda_program_fragments =
        -- Some C compilers limit the size of literal strings, so
        -- chunk the entire program into small bits here, and
        -- concatenate it again at runtime.
        [[C.cinit|$string:s|] | s <- chunk 2000 $ T.unpack $ cuda_prelude <> cuda_program]
      program_fragments = cuda_program_fragments ++ [[C.cinit|NULL|]]
  let max_failure_args = foldl max 0 $ map (errorMsgNumArgs . failureError) failures
  mapM_
    GC.earlyDecl
    [C.cunit|static const int max_failure_args = $int:max_failure_args;
             static const char *cuda_program[] = {$inits:program_fragments, NULL};
             $esc:(T.unpack backendsCudaH)
            |]
  GC.earlyDecl $ failureMsgFunction failures

  generateCUDADecls cost_centres kernels

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

  GC.generateProgramStruct

  GC.onClear
    [C.citem|if (ctx->error == NULL) {
               CUDA_SUCCEED_NONFATAL(cuda_free_all(ctx));
             }|]

  GC.profileReport [C.citem|CUDA_SUCCEED_FATAL(cuda_tally_profiling_records(ctx));|]
  mapM_ GC.profileReport $ costCentreReport $ cost_centres ++ M.keys kernels
{-# NOINLINE generateBoilerplate #-}
