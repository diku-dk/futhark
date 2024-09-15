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
    HostOp,
  )
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import NeatInterpolation (untrimming)

mkBoilerplate ::
  T.Text ->
  [(Name, KernelConstExp)] ->
  M.Map Name KernelSafety ->
  [PrimType] ->
  [FailureMsg] ->
  GC.CompilerM HostOp () ()
mkBoilerplate hip_program macros kernels types failures = do
  generateGPUBoilerplate
    hip_program
    macros
    backendsHipH
    (M.keys kernels)
    types
    failures

  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_add_build_option(struct futhark_context_config *cfg, const char* opt);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_device(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|const char* futhark_context_config_get_program(struct futhark_context_config *cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_program(struct futhark_context_config *cfg, const char* s);|]

cliOptions :: [Option]
cliOptions =
  gpuOptions
    ++ [ Option
           { optionLongName = "dump-hip",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Dump the embedded HIP kernels to the indicated file.",
             optionAction =
               [C.cstm|{const char* prog = futhark_context_config_get_program(cfg);
                        if (dump_file(optarg, prog, strlen(prog)) != 0) {
                          fprintf(stderr, "%s: %s\n", optarg, strerror(errno));
                          exit(1);
                        }
                        exit(0);}|]
           },
         Option
           { optionLongName = "load-hip",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Instead of using the embedded HIP kernels, load them from the indicated file.",
             optionAction =
               [C.cstm|{ size_t n; const char *s = slurp_file(optarg, &n);
                         if (s == NULL) { fprintf(stderr, "%s: %s\n", optarg, strerror(errno)); exit(1); }
                         futhark_context_config_set_program(cfg, s);
                       }|]
           },
         Option
           { optionLongName = "build-option",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "OPT",
             optionDescription = "Add an additional build option to the string passed to HIPRTC.",
             optionAction = [C.cstm|futhark_context_config_add_build_option(cfg, optarg);|]
           }
       ]

hipMemoryType :: GC.MemoryType HostOp ()
hipMemoryType "device" = pure [C.cty|typename hipDeviceptr_t|]
hipMemoryType space = error $ "GPU backend does not support '" ++ space ++ "' memory space."

-- | Compile the program to C with calls to HIP.
compileProg :: (MonadFreshNames m) => T.Text -> Prog GPUMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version prog = do
  ( ws,
    Program hip_code hip_prelude macros kernels types params failures prog'
    ) <-
    ImpGen.compileProg prog
  (ws,)
    <$> GC.compileProg
      "hip"
      version
      params
      operations
      (mkBoilerplate (hip_prelude <> hip_code) macros kernels types failures)
      hip_includes
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  where
    operations :: GC.Operations HostOp ()
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
