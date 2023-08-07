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
import Futhark.CodeGen.Backends.CCUDA.Boilerplate
import Futhark.CodeGen.Backends.COpenCL.Boilerplate (commonOptions)
import Futhark.CodeGen.Backends.GPU
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.ImpGen.CUDA qualified as ImpGen
import Futhark.IR.GPUMem hiding
  ( CmpSizeLe,
    GetSize,
    GetSizeMax,
  )
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import NeatInterpolation (untrimming)

-- | Compile the program to C with calls to CUDA.
compileProg :: MonadFreshNames m => T.Text -> Prog GPUMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version prog = do
  (ws, Program cuda_code cuda_prelude kernels _ params failures prog') <-
    ImpGen.compileProg prog
  let cost_centres = M.keys kernels
      extra = do
        createKernels (M.keys kernels)
        generateBoilerplate
          cuda_code
          cuda_prelude
          cost_centres
          failures
  (ws,)
    <$> GC.compileProg
      "cuda"
      version
      params
      operations
      extra
      cuda_includes
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  where
    operations :: GC.Operations OpenCL ()
    operations =
      GC.defaultOperations
        { GC.opsWriteScalar = writeScalarGPU,
          GC.opsReadScalar = readScalarGPU,
          GC.opsAllocate = allocateGPU,
          GC.opsDeallocate = deallocateGPU,
          GC.opsCopy = copyCUDAMemory,
          GC.opsCopies = gpuCopies <> GC.opsCopies GC.defaultOperations,
          GC.opsMemoryType = cudaMemoryType,
          GC.opsCompiler = callKernel,
          GC.opsFatMemory = True,
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

cliOptions :: [Option]
cliOptions =
  commonOptions
    ++ [ Option
           { optionLongName = "dump-cuda",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Dump the embedded CUDA kernels to the indicated file.",
             optionAction =
               [C.cstm|{futhark_context_config_dump_program_to(cfg, optarg);
                                     entry_point = NULL;}|]
           },
         Option
           { optionLongName = "load-cuda",
             optionShortName = Nothing,
             optionArgument = RequiredArgument "FILE",
             optionDescription = "Instead of using the embedded CUDA kernels, load them from the indicated file.",
             optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
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
           },
         Option
           { optionLongName = "profile",
             optionShortName = Just 'P',
             optionArgument = NoArgument,
             optionDescription = "Gather profiling data while executing and print out a summary at the end.",
             optionAction = [C.cstm|futhark_context_config_set_profiling(cfg, 1);|]
           }
       ]

copyCUDAMemory :: GC.Copy OpenCL ()
copyCUDAMemory _ dstmem dstidx (Space "device") srcmem srcidx (Space "device") nbytes = do
  GC.stm [C.cstm|gpu_memcpy(ctx, $exp:dstmem, $exp:dstidx, $exp:srcmem, $exp:srcidx, $exp:nbytes);|]
copyCUDAMemory b dstmem dstidx dstSpace srcmem srcidx srcSpace nbytes = do
  let (copy, prof) = memcpyFun b dstSpace srcSpace
      (bef, aft) = profilingEnclosure prof
  GC.item
    [C.citem|{$items:bef CUDA_SUCCEED_OR_RETURN($exp:copy); $items:aft}|]
  where
    dst = [C.cexp|$exp:dstmem + $exp:dstidx|]
    src = [C.cexp|$exp:srcmem + $exp:srcidx|]
    memcpyFun GC.CopyBarrier DefaultSpace (Space "device") =
      ([C.cexp|cuMemcpyDtoH($exp:dst, $exp:src, $exp:nbytes)|], copyDevToHost)
    memcpyFun GC.CopyBarrier (Space "device") DefaultSpace =
      ([C.cexp|cuMemcpyHtoD($exp:dst, $exp:src, $exp:nbytes)|], copyHostToDev)
    memcpyFun GC.CopyNoBarrier DefaultSpace (Space "device") =
      ([C.cexp|cuMemcpyDtoHAsync($exp:dst, $exp:src, $exp:nbytes, ctx->stream)|], copyDevToHost)
    memcpyFun GC.CopyNoBarrier (Space "device") DefaultSpace =
      ([C.cexp|cuMemcpyHtoDAsync($exp:dst, $exp:src, $exp:nbytes, ctx->stream)|], copyHostToDev)
    memcpyFun _ _ _ =
      error $
        "Cannot copy to '"
          ++ show dstSpace
          ++ "' from '"
          ++ show srcSpace
          ++ "'."

cudaMemoryType :: GC.MemoryType OpenCL ()
cudaMemoryType "device" = pure [C.cty|typename CUdeviceptr|]
cudaMemoryType space = error $ "GPU backend does not support '" ++ space ++ "' memory space."
