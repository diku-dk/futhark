{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for HIP.
module Futhark.CodeGen.Backends.HIP
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GPU
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.ImpGen.HIP qualified as ImpGen
import Futhark.CodeGen.RTS.C (backendsHipH)
import Futhark.IR.GPUMem hiding
  ( CmpSizeLe,
    GetSize,
    GetSizeMax,
  )
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import NeatInterpolation (untrimming)

mkBoilerplate ::
  T.Text ->
  M.Map Name KernelSafety ->
  [PrimType] ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
mkBoilerplate hip_program kernels types failures = do
  generateGPUBoilerplate
    hip_program
    backendsHipH
    (M.keys kernels)
    types
    failures

  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_add_build_option(struct futhark_context_config *cfg, const char* opt);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_device(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_dump_program_to(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_load_program_from(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_reg_tile_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_threshold(struct futhark_context_config *cfg, int size);|]

cliOptions :: [Option]
cliOptions =
  gpuOptions
    ++ [ Option
           { optionLongName = "dump-hip",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Dump the embedded HIP kernels to the indicated file.",
             optionAction =
               [C.cstm|{futhark_context_config_dump_program_to(cfg, optarg);
                                     entry_point = NULL;}|]
           },
         Option
           { optionLongName = "load-hip",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Instead of using the embedded HIP kernels, load them from the indicated file.",
             optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
           },
         Option
           { optionLongName = "build-option",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "OPT",
             optionDescription = "Add an additional build option to the string passed to NVRTC.",
             optionAction = [C.cstm|futhark_context_config_add_build_option(cfg, optarg);|]
           },
         Option
           { optionLongName = "profile",
             optionShortName = Just 'P',
             optionArgument = NoArgument,
             optionDescription = "Gather profiling data while executing and print out a summary at the end.",
             optionAction = [C.cstm|futhark_context_config_set_profiling(cfg, 1);|]
           }
       ]

hipMemoryType :: GC.MemoryType OpenCL ()
hipMemoryType "device" = pure [C.cty|void*|]
hipMemoryType space = error $ "GPU backend does not support '" ++ space ++ "' memory space."

-- | Compile the program to C with calls to HIP.
compileProg :: (MonadFreshNames m) => T.Text -> Prog GPUMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version prog = do
  ( ws,
    Program hip_code hip_prelude kernels types params failures prog'
    ) <-
    ImpGen.compileProg prog
  (ws,)
    <$> GC.compileProg
      "hip"
      version
      params
      operations
      (mkBoilerplate (hip_prelude <> hip_code) kernels types failures)
      hip_includes
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  where
    operations :: GC.Operations OpenCL ()
    operations =
      gpuOperations
        { GC.opsMemoryType = hipMemoryType
        }
    hip_includes =
      [untrimming|
       #define __HIP_PLATFORM_AMD__
       #include <hip/hip_runtime.h>
       #include <hip/hiprtc.h>
      |]
