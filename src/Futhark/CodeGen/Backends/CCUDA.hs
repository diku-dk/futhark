{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for CUDA.
module Futhark.CodeGen.Backends.CCUDA
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
import Futhark.CodeGen.ImpGen.CUDA qualified as ImpGen
import Futhark.CodeGen.RTS.C (backendsCudaH)
import Futhark.IR.GPUMem hiding
  ( HostOp,
    CmpSizeLe,
    GetSize,
    GetSizeMax,
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
mkBoilerplate cuda_program macros kernels types failures = do
  generateGPUBoilerplate
    cuda_program
    macros
    backendsCudaH
    (M.keys kernels)
    types
    failures

  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_add_nvrtc_option(struct futhark_context_config *cfg, const char* opt);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_device(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|const char* futhark_context_config_get_program(struct futhark_context_config *cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_program(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_dump_ptx_to(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_load_ptx_from(struct futhark_context_config *cfg, const char* s);|]

cliOptions :: [Option]
cliOptions =
  gpuOptions
    ++ [ Option
           { optionLongName = "dump-cuda",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Dump the embedded CUDA kernels to the indicated file.",
             optionAction =
               [C.cstm|{const char* prog = futhark_context_config_get_program(cfg);
                        if (dump_file(optarg, prog, strlen(prog)) != 0) {
                          fprintf(stderr, "%s: %s\n", optarg, strerror(errno));
                          exit(1);
                        }
                        exit(0);}|]
           },
         Option
           { optionLongName = "load-cuda",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Instead of using the embedded CUDA kernels, load them from the indicated file.",
             optionAction =
               [C.cstm|{ size_t n; const char *s = slurp_file(optarg, &n);
                         if (s == NULL) { fprintf(stderr, "%s: %s\n", optarg, strerror(errno)); exit(1); }
                         futhark_context_config_set_program(cfg, s);
                       }|]
           },
         Option
           { optionLongName = "dump-ptx",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Dump the PTX-compiled version of the embedded kernels to the indicated file.",
             optionAction =
               [C.cstm|{futhark_context_config_dump_ptx_to(cfg, optarg);
                                     entry_point = NULL;}|]
           },
         Option
           { optionLongName = "load-ptx",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Load PTX code from the indicated file.",
             optionAction = [C.cstm|futhark_context_config_load_ptx_from(cfg, optarg);|]
           },
         Option
           { optionLongName = "nvrtc-option",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "OPT",
             optionDescription = "Add an additional build option to the string passed to NVRTC.",
             optionAction = [C.cstm|futhark_context_config_add_nvrtc_option(cfg, optarg);|]
           }
       ]

cudaMemoryType :: GC.MemoryType HostOp ()
cudaMemoryType "device" = pure [C.cty|typename CUdeviceptr|]
cudaMemoryType space = error $ "GPU backend does not support '" ++ space ++ "' memory space."

-- | Compile the program to C with calls to CUDA.
compileProg :: (MonadFreshNames m) => T.Text -> Prog GPUMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version prog = do
  ( ws,
    Program cuda_code cuda_prelude macros kernels types params failures prog'
    ) <-
    ImpGen.compileProg prog
  (ws,)
    <$> GC.compileProg
      "cuda"
      version
      params
      operations
      (mkBoilerplate (cuda_prelude <> cuda_code) macros kernels types failures)
      cuda_includes
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  where
    operations :: GC.Operations HostOp ()
    operations =
      gpuOperations
        { GC.opsMemoryType = cudaMemoryType,
          GC.opsCritical =
            ( [C.citems|CUDA_SUCCEED_FATAL(cuCtxPushCurrent(ctx->cu_ctx));|],
              [C.citems|CUDA_SUCCEED_FATAL(cuCtxPopCurrent(&ctx->cu_ctx));|]
            )
        }
    cuda_includes =
      [untrimming|
       #include <cuda.h>
       #include <cuda_runtime.h>
       #include <nvrtc.h>
      |]
