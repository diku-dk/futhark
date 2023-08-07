{-# LANGUAGE QuasiQuotes #-}

-- | Various boilerplate definitions for the CUDA backend.
module Futhark.CodeGen.Backends.CCUDA.Boilerplate
  ( generateBoilerplate,
    profilingEnclosure,
    module Futhark.CodeGen.Backends.COpenCL.Boilerplate,
  )
where

import Data.Text qualified as T
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
  ( failureMsgFunction,
    genProfileReport,
  )
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.RTS.C (backendsCudaH, gpuH, gpuPrototypesH)
import Futhark.Util (chunk)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

-- | Block items to put before and after a thing to be profiled.
profilingEnclosure :: Name -> ([C.BlockItem], [C.BlockItem])
profilingEnclosure name =
  ( [C.citems|
      typename CUevent *pevents = NULL;
      if (ctx->profiling && !ctx->profiling_paused) {
        pevents = cuda_get_events(ctx, $string:(nameToString name));
        CUDA_SUCCEED_FATAL(cuEventRecord(pevents[0], ctx->stream));
      }
      |],
    [C.citems|
      if (pevents != NULL) {
        CUDA_SUCCEED_FATAL(cuEventRecord(pevents[1], ctx->stream));
      }
      |]
  )

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateBoilerplate ::
  T.Text ->
  T.Text ->
  [Name] ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateBoilerplate gpu_program cuda_prelude cost_centres failures = do
  let gpu_program_fragments =
        -- Some C compilers limit the size of literal strings, so
        -- chunk the entire program into small bits here, and
        -- concatenate it again at runtime.
        [[C.cinit|$string:s|] | s <- chunk 2000 $ T.unpack $ cuda_prelude <> gpu_program]
      program_fragments = gpu_program_fragments ++ [[C.cinit|NULL|]]
  let max_failure_args = foldl max 0 $ map (errorMsgNumArgs . failureError) failures
  mapM_
    GC.earlyDecl
    [C.cunit|static const int max_failure_args = $int:max_failure_args;
             static const char *gpu_program[] = {$inits:program_fragments, NULL};
             $esc:(T.unpack gpuPrototypesH)
             $esc:(T.unpack backendsCudaH)
             $esc:(T.unpack gpuH)
            |]
  GC.earlyDecl $ failureMsgFunction failures

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
               CUDA_SUCCEED_NONFATAL(gpu_free_all(ctx));
             }|]

  genProfileReport cost_centres
{-# NOINLINE generateBoilerplate #-}
