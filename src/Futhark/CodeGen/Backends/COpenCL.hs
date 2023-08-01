{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for C with OpenCL.
module Futhark.CodeGen.Backends.COpenCL
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
import Futhark.CodeGen.Backends.GPU
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.ImpGen.OpenCL qualified as ImpGen
import Futhark.IR.GPUMem hiding
  ( CmpSizeLe,
    GetSize,
    GetSizeMax,
  )
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C
import NeatInterpolation (untrimming)

-- | Compile the program to C with calls to OpenCL.
compileProg :: MonadFreshNames m => T.Text -> Prog GPUMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version prog = do
  ( ws,
    Program
      opencl_code
      opencl_prelude
      kernels
      types
      params
      failures
      prog'
    ) <-
    ImpGen.compileProg prog
  let cost_centres =
        [ copyDevToDev,
          copyDevToHost,
          copyHostToDev,
          copyScalarToDev,
          copyScalarFromDev
        ]
          <> M.keys kernels
      extra = do
        createKernels (M.keys kernels)
        generateBoilerplate
          opencl_code
          opencl_prelude
          cost_centres
          types
          failures
  (ws,)
    <$> GC.compileProg
      "opencl"
      version
      params
      operations
      extra
      include_opencl_h
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  where
    operations :: GC.Operations OpenCL ()
    operations =
      GC.defaultOperations
        { GC.opsCompiler = callKernel,
          GC.opsWriteScalar = writeOpenCLScalar,
          GC.opsReadScalar = readOpenCLScalar,
          GC.opsAllocate = allocateOpenCLBuffer,
          GC.opsDeallocate = deallocateOpenCLBuffer,
          GC.opsCopy = copyOpenCLMemory,
          GC.opsCopies =
            M.insert (Space "device", Space "device") copygpu2gpu $
              GC.opsCopies GC.defaultOperations,
          GC.opsMemoryType = openclMemoryType,
          GC.opsFatMemory = True
        }
    include_opencl_h =
      [untrimming|
       #define CL_TARGET_OPENCL_VERSION 120
       #define CL_USE_DEPRECATED_OPENCL_1_2_APIS
       #ifdef __APPLE__
       #define CL_SILENCE_DEPRECATION
       #include <OpenCL/cl.h>
       #else
       #include <CL/cl.h>
       #endif
       |]

cliOptions :: [Option]
cliOptions =
  commonOptions
    ++ [ Option
           { optionLongName = "platform",
             optionShortName = Just 'p',
             optionArgument = RequiredArgument "NAME",
             optionDescription = "Use the first OpenCL platform whose name contains the given string.",
             optionAction = [C.cstm|futhark_context_config_set_platform(cfg, optarg);|]
           },
         Option
           { optionLongName = "dump-opencl",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Dump the embedded OpenCL program to the indicated file.",
             optionAction =
               [C.cstm|{futhark_context_config_dump_program_to(cfg, optarg);
                                     entry_point = NULL;}|]
           },
         Option
           { optionLongName = "load-opencl",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Instead of using the embedded OpenCL program, load it from the indicated file.",
             optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
           },
         Option
           { optionLongName = "dump-opencl-binary",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Dump the compiled version of the embedded OpenCL program to the indicated file.",
             optionAction =
               [C.cstm|{futhark_context_config_dump_binary_to(cfg, optarg);
                                     entry_point = NULL;}|]
           },
         Option
           { optionLongName = "load-opencl-binary",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Load an OpenCL binary from the indicated file.",
             optionAction = [C.cstm|futhark_context_config_load_binary_from(cfg, optarg);|]
           },
         Option
           { optionLongName = "build-option",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "OPT",
             optionDescription = "Add an additional build option to the string passed to clBuildProgram().",
             optionAction = [C.cstm|futhark_context_config_add_build_option(cfg, optarg);|]
           },
         Option
           { optionLongName = "profile",
             optionShortName = Just 'P',
             optionArgument = NoArgument,
             optionDescription = "Gather profiling data while executing and print out a summary at the end.",
             optionAction = [C.cstm|futhark_context_config_set_profiling(cfg, 1);|]
           },
         Option
           { optionLongName = "list-devices",
             optionShortName = Nothing,
             optionArgument = NoArgument,
             optionDescription = "List all OpenCL devices and platforms available on the system.",
             optionAction =
               [C.cstm|{futhark_context_config_list_devices(cfg);
                        entry_point = NULL;}|]
           }
       ]

-- We detect the special case of writing a constant and turn it into a
-- non-blocking write.  This may be slightly faster, as it prevents
-- unnecessary synchronisation of the OpenCL command queue, and
-- writing a constant is fairly common.  This is only possible because
-- we can give the constant infinite lifetime (with 'static'), which
-- is not the case for ordinary variables.
writeOpenCLScalar :: GC.WriteScalar OpenCL ()
writeOpenCLScalar mem i t "device" _ val = do
  val' <- newVName "write_tmp"
  GC.item [C.citem|$ty:t $id:val' = $exp:val;|]
  GC.stm
    [C.cstm|if ((err = gpu_scalar_to_device(ctx, $exp:mem, $exp:i * sizeof($ty:t), sizeof($ty:t), &$id:val')) != 0) { goto cleanup; }|]
writeOpenCLScalar _ _ _ space _ _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

-- It is often faster to do a blocking clEnqueueReadBuffer() than to
-- do an async clEnqueueReadBuffer() followed by a clFinish(), even
-- with an in-order command queue.  This is safe if and only if there
-- are no possible outstanding failures.
readOpenCLScalar :: GC.ReadScalar OpenCL ()
readOpenCLScalar mem i t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t $id:val;|]
  GC.stm
    [C.cstm|if ((err = gpu_scalar_from_device(ctx, &$id:val, $exp:mem, $exp:i * sizeof($ty:t), sizeof($ty:t))) != 0) { goto cleanup; }|]
  GC.stm
    [C.cstm|if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
            { err = 1; goto cleanup; }|]
  pure [C.cexp|$id:val|]
readOpenCLScalar _ _ _ space _ =
  error $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: GC.Allocate OpenCL ()
allocateOpenCLBuffer mem size tag "device" =
  GC.stm
    [C.cstm|ctx->error =
     OPENCL_SUCCEED_NONFATAL(opencl_alloc(ctx, ctx->log,
                                          (size_t)$exp:size, $exp:tag,
                                          &$exp:mem, (size_t*)&$exp:size));|]
allocateOpenCLBuffer _ _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' memory space."

deallocateOpenCLBuffer :: GC.Deallocate OpenCL ()
deallocateOpenCLBuffer mem size tag "device" =
  GC.stm [C.cstm|OPENCL_SUCCEED_OR_RETURN(opencl_free(ctx, $exp:mem, $exp:size, $exp:tag));|]
deallocateOpenCLBuffer _ _ _ space =
  error $ "Cannot deallocate in '" ++ space ++ "' space"

syncArg :: GC.CopyBarrier -> C.Exp
syncArg GC.CopyBarrier = [C.cexp|CL_TRUE|]
syncArg GC.CopyNoBarrier = [C.cexp|CL_FALSE|]

copyOpenCLMemory :: GC.Copy OpenCL ()
-- The read/write/copy-buffer functions fail if the given offset is
-- out of bounds, even if asked to read zero bytes.  We protect with a
-- branch to avoid this.
copyOpenCLMemory b destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes =
  GC.stm
    [C.cstm|
    if ($exp:nbytes > 0) {
      typename cl_bool sync_call = $exp:(syncArg b);
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueReadBuffer(ctx->queue, $exp:srcmem,
                            ctx->failure_is_an_option ? CL_FALSE : sync_call,
                            (size_t)$exp:srcidx, (size_t)$exp:nbytes,
                            $exp:destmem + $exp:destidx,
                            0, NULL, $exp:(profilingEvent copyHostToDev)));
      if (sync_call &&
          ctx->failure_is_an_option &&
          futhark_context_sync(ctx) != 0) { return 1; }
   }
  |]
copyOpenCLMemory b destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  GC.stm
    [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueWriteBuffer(ctx->queue, $exp:destmem, $exp:(syncArg b),
                             (size_t)$exp:destidx, (size_t)$exp:nbytes,
                             $exp:srcmem + $exp:srcidx,
                             0, NULL, $exp:(profilingEvent copyDevToHost)));
    }
  |]
copyOpenCLMemory _ dstmem dstidx (Space "device") srcmem srcidx (Space "device") nbytes =
  GC.stm
    [C.cstm|gpu_memcpy(ctx, $exp:dstmem, $exp:dstidx, $exp:srcmem, $exp:srcidx, $exp:nbytes);|]
copyOpenCLMemory _ destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyOpenCLMemory _ _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openclMemoryType :: GC.MemoryType OpenCL ()
openclMemoryType "device" = pure [C.cty|typename cl_mem|]
openclMemoryType space = error $ "GPU backend does not support '" ++ space ++ "' memory space."
