{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for Metal.
module Futhark.CodeGen.Backends.Metal
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
import Futhark.CodeGen.ImpGen.Metal qualified as ImpGen
import Futhark.CodeGen.RTS.C (backendsMetalH)
import Futhark.IR.GPUMem hiding
  ( CmpSizeLe,
    GetSize,
    GetSizeMax,
  )
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C

mkBoilerplate ::
  T.Text ->
  [(Name, KernelConstExp)] ->
  M.Map Name KernelSafety ->
  [PrimType] ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
mkBoilerplate metal_program macros kernels types failures = do
  generateGPUBoilerplate
    metal_program
    macros
    backendsMetalH
    (M.keys kernels)
    types
    failures

  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_add_build_option(struct futhark_context_config *cfg, const char* opt);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_device(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_list_devices(struct futhark_context_config *cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|const char* futhark_context_config_get_program(struct futhark_context_config *cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_program(struct futhark_context_config *cfg, const char* s);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_command_queue(struct futhark_context_config *cfg, void* queue);|]
  GC.headerDecl GC.MiscDecl [C.cedecl|void* futhark_context_get_command_queue(struct futhark_context* ctx);|]

cliOptions :: [Option]
cliOptions =
  gpuOptions
    ++ [ Option
           { optionLongName = "dump-metal",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Dump the embedded Metal program to the indicated file.",
             optionAction =
               [C.cstm|{const char* prog = futhark_context_config_get_program(cfg);
                        if (dump_file(optarg, prog, strlen(prog)) != 0) {
                          fprintf(stderr, "%s: %s\n", optarg, strerror(errno));
                          exit(1);
                        }
                        exit(0);}|]
           },
         Option
           { optionLongName = "load-metal",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Instead of using the embedded Metal program, load it from the indicated file.",
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
             optionDescription = "Add a preprocessor definition (-DNAME or -DNAME=VALUE) when compiling the Metal program.",
             optionAction = [C.cstm|futhark_context_config_add_build_option(cfg, optarg);|]
           },
         Option
           { optionLongName = "list-devices",
             optionShortName = Nothing,
             optionArgument = NoArgument,
             optionDescription = "List all Metal devices available on the system.",
             optionAction =
               [C.cstm|{futhark_context_config_list_devices(cfg);
                        entry_point = NULL;}|]
           }
       ]

-- | The public header must be usable from C, so device memory is a
-- @void*@ (an @id<MTLBuffer>@ in the runtime system).
metalMemoryType :: GC.MemoryType OpenCL ()
metalMemoryType "device" = pure [C.cty|void*|]
metalMemoryType space = error $ "GPU backend does not support '" ++ space ++ "' memory space."

-- | Compile the program to Objective-C with calls to Metal.
compileProg :: (MonadFreshNames m) => T.Text -> Prog GPUMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version prog = do
  ( ws,
    Program metal_code metal_prelude macros kernels types failures prog'
    ) <-
    ImpGen.compileProg prog
  (ws,)
    <$> GC.compileProg
      "metal"
      version
      operations
      (mkBoilerplate (metal_prelude <> metal_code) macros kernels types failures)
      mempty
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  where
    operations :: GC.Operations OpenCL ()
    operations =
      gpuOperations
        { GC.opsMemoryType = metalMemoryType
        }
