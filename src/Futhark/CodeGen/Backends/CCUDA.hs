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

import Control.Monad
import Data.List (unzip4)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Futhark.CodeGen.Backends.CCUDA.Boilerplate
import Futhark.CodeGen.Backends.COpenCL.Boilerplate (commonOptions, copygpu2gpu, sizeLoggingCode)
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep (primStorageType, toStorage)
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.ImpGen.CUDA qualified as ImpGen
import Futhark.IR.GPUMem hiding
  ( CmpSizeLe,
    GetSize,
    GetSizeMax,
  )
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C
import NeatInterpolation (untrimming)

-- | Compile the program to C with calls to CUDA.
compileProg :: MonadFreshNames m => T.Text -> Prog GPUMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version prog = do
  (ws, Program cuda_code cuda_prelude kernels _ params failures prog') <-
    ImpGen.compileProg prog
  let cost_centres =
        [ copyDevToDev,
          copyDevToHost,
          copyHostToDev,
          copyScalarToDev,
          copyScalarFromDev
        ]
      extra =
        generateBoilerplate
          cuda_code
          cuda_prelude
          cost_centres
          kernels
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
        { GC.opsWriteScalar = writeCUDAScalar,
          GC.opsReadScalar = readCUDAScalar,
          GC.opsAllocate = allocateCUDABuffer,
          GC.opsDeallocate = deallocateCUDABuffer,
          GC.opsCopy = copyCUDAMemory,
          GC.opsCopies =
            M.insert (Space "device", Space "device") copygpu2gpu $
              GC.opsCopies GC.defaultOperations,
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

-- We detect the special case of writing a constant and turn it into a
-- non-blocking write.  This may be slightly faster, as it prevents
-- unnecessary synchronisation of the context, and writing a constant
-- is fairly common.  This is only possible because we can give the
-- constant infinite lifetime (with 'static'), which is not the case
-- for ordinary variables.
writeCUDAScalar :: GC.WriteScalar OpenCL ()
writeCUDAScalar mem idx t "device" _ val@C.Const {} = do
  val' <- newVName "write_static"
  let (bef, aft) = profilingEnclosure copyScalarToDev
  GC.item
    [C.citem|{static $ty:t $id:val' = $exp:val;
              $items:bef
              CUDA_SUCCEED_OR_RETURN(
                cuMemcpyHtoDAsync($exp:mem + $exp:idx * sizeof($ty:t),
                                  &$id:val',
                                  sizeof($ty:t),
                                  ctx->stream));
              $items:aft
             }|]
writeCUDAScalar mem idx t "device" _ val = do
  val' <- newVName "write_tmp"
  let (bef, aft) = profilingEnclosure copyScalarToDev
  GC.item
    [C.citem|{$ty:t $id:val' = $exp:val;
                  $items:bef
                  CUDA_SUCCEED_OR_RETURN(
                    cuMemcpyHtoD($exp:mem + $exp:idx * sizeof($ty:t),
                                 &$id:val',
                                 sizeof($ty:t)));
                  $items:aft
                 }|]
writeCUDAScalar _ _ _ space _ _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

readCUDAScalar :: GC.ReadScalar OpenCL ()
readCUDAScalar mem idx t "device" _ = do
  val <- newVName "read_res"
  let (bef, aft) = profilingEnclosure copyScalarFromDev
  mapM_
    GC.item
    [C.citems|
       $ty:t $id:val;
       {
       $items:bef
       CUDA_SUCCEED_OR_RETURN(
          cuMemcpyDtoH(&$id:val,
                       $exp:mem + $exp:idx * sizeof($ty:t),
                       sizeof($ty:t)));
       $items:aft
       }
       |]
  GC.stm
    [C.cstm|if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
            { return 1; }|]
  pure [C.cexp|$id:val|]
readCUDAScalar _ _ _ space _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

allocateCUDABuffer :: GC.Allocate OpenCL ()
allocateCUDABuffer mem size tag "device" =
  GC.stm
    [C.cstm|ctx->error =
     CUDA_SUCCEED_NONFATAL(cuda_alloc(ctx, ctx->log,
                                      (size_t)$exp:size, $exp:tag,
                                      &$exp:mem, (size_t*)&$exp:size));|]
allocateCUDABuffer _ _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' memory space."

deallocateCUDABuffer :: GC.Deallocate OpenCL ()
deallocateCUDABuffer mem size tag "device" =
  GC.stm [C.cstm|CUDA_SUCCEED_OR_RETURN(cuda_free(ctx, $exp:mem, $exp:size, $exp:tag));|]
deallocateCUDABuffer _ _ _ space =
  error $ "Cannot deallocate in '" ++ space ++ "' memory space."

copyCUDAMemory :: GC.Copy OpenCL ()
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
    memcpyFun _ (Space "device") (Space "device") =
      ([C.cexp|cuMemcpy($exp:dst, $exp:src, $exp:nbytes)|], copyDevToDev)
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
cudaMemoryType space =
  error $ "CUDA backend does not support '" ++ space ++ "' memory space."

kernelConstToExp :: KernelConst -> C.Exp
kernelConstToExp (SizeConst key) =
  [C.cexp|*ctx->tuning_params.$id:key|]
kernelConstToExp (SizeMaxConst size_class) =
  [C.cexp|ctx->$id:field|]
  where
    field = "max_" <> cudaSizeClass size_class
    cudaSizeClass SizeThreshold {} = "threshold"
    cudaSizeClass SizeGroup = "block_size"
    cudaSizeClass SizeNumGroups = "grid_size"
    cudaSizeClass SizeTile = "tile_size"
    cudaSizeClass SizeRegTile = "reg_tile_size"
    cudaSizeClass SizeLocalMemory = "shared_memory"
    cudaSizeClass (SizeBespoke x _) = prettyString x

compileGroupDim :: GroupDim -> GC.CompilerM op s C.Exp
compileGroupDim (Left e) = GC.compileExp e
compileGroupDim (Right kc) = pure $ kernelConstToExp kc

callKernel :: GC.OpCompiler OpenCL ()
callKernel (GetSize v key) = do
  let e = kernelConstToExp $ SizeConst key
  GC.stm [C.cstm|$id:v = $exp:e;|]
callKernel (CmpSizeLe v key x) = do
  let e = kernelConstToExp $ SizeConst key
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = $exp:e <= $exp:x';|]
  sizeLoggingCode v key x'
callKernel (GetSizeMax v size_class) = do
  let e = kernelConstToExp $ SizeMaxConst size_class
  GC.stm [C.cstm|$id:v = $exp:e;|]
callKernel (LaunchKernel safety kernel_name args num_blocks block_size) = do
  (arg_params, arg_params_inits, call_args, shared_vars) <-
    unzip4 <$> zipWithM mkArgs [(0 :: Int) ..] args
  let (shared_sizes, shared_offsets) = unzip $ catMaybes shared_vars
      shared_offsets_sc = mkOffsets shared_sizes
      shared_args = zip shared_offsets shared_offsets_sc
      shared_bytes = last shared_offsets_sc
  forM_ shared_args $ \(arg, offset) ->
    GC.decl [C.cdecl|unsigned int $id:arg = $exp:offset;|]

  (grid_x, grid_y, grid_z) <- mkDims <$> mapM GC.compileExp num_blocks
  (block_x, block_y, block_z) <- mkDims <$> mapM compileGroupDim block_size

  let need_perm = length num_blocks == 3
  kernel_fname <- genKernelFunction kernel_name safety need_perm arg_params arg_params_inits

  GC.stm
    [C.cstm|{
           err = $id:kernel_fname(ctx,
                                  $exp:grid_x,$exp:grid_y,$exp:grid_z,
                                  $exp:block_x, $exp:block_y, $exp:block_z,
                                  $exp:shared_bytes,
                                  $args:call_args);
           if (err != FUTHARK_SUCCESS) { goto cleanup; }
           }|]

  when (safety >= SafetyFull) $
    GC.stm [C.cstm|ctx->failure_is_an_option = 1;|]
  where
    mkDims [] = ([C.cexp|0|], [C.cexp|0|], [C.cexp|0|])
    mkDims [x] = (x, [C.cexp|1|], [C.cexp|1|])
    mkDims [x, y] = (x, y, [C.cexp|1|])
    mkDims (x : y : z : _) = (x, y, z)
    addExp x y = [C.cexp|$exp:x + $exp:y|]
    alignExp e = [C.cexp|$exp:e + ((8 - ($exp:e % 8)) % 8)|]
    mkOffsets = scanl (\a b -> a `addExp` alignExp b) [C.cexp|0|]
    mkArgs i (ValueKArg e t) = do
      e' <- GC.compileExp e
      pure
        ( [C.cparam|$ty:(primStorageType t) $id:("arg" <> show i)|],
          [C.cinit|&$id:("arg" <> show i)|],
          toStorage t e',
          Nothing
        )
    mkArgs i (MemKArg v) = do
      v' <- GC.rawMem v
      pure
        ( [C.cparam|typename CUdeviceptr $id:("arg" <> show i)|],
          [C.cinit|&$id:("arg" <> show i)|],
          v',
          Nothing
        )
    mkArgs i (SharedMemoryKArg (Count c)) = do
      num_bytes <- GC.compileExp c
      size <- newVName "shared_size"
      offset <- newVName "shared_offset"
      GC.decl [C.cdecl|unsigned int $id:size = $exp:num_bytes;|]
      pure
        ( [C.cparam|unsigned int $id:("arg" <> show i)|],
          [C.cinit|&$id:("arg" <> show i)|],
          [C.cexp|$id:offset|],
          Just (size, offset)
        )

genKernelFunction ::
  KernelName ->
  KernelSafety ->
  Bool ->
  [C.Param] ->
  [C.Initializer] ->
  GC.CompilerM op s Name
genKernelFunction kernel_name safety need_perm arg_params arg_params_inits = do
  let kernel_fname = "gpu_kernel_" <> kernel_name
      (bef, aft) = profilingEnclosure kernel_name
      perm_args
        | need_perm = [[C.cinit|&perm[0]|], [C.cinit|&perm[1]|], [C.cinit|&perm[2]|]]
        | otherwise = []
      failure_args =
        take
          (numFailureParams safety)
          [ [C.cinit|&ctx->global_failure|],
            [C.cinit|&ctx->failure_is_an_option|],
            [C.cinit|&ctx->global_failure_args|]
          ]

  GC.libDecl
    [C.cedecl|static int $id:kernel_fname(
                struct futhark_context* ctx, unsigned int grid_x, unsigned int grid_y, unsigned int grid_z,
                unsigned int block_x, unsigned int block_y, unsigned int block_z,
                unsigned int shared_bytes, $params:arg_params) {
    if (grid_x * grid_y * grid_z * block_x * block_y * block_z != 0) {
      int perm[3] = { 0, 1, 2 };

      if (grid_y >= (1<<16)) {
        perm[1] = perm[0];
        perm[0] = 1;
      }

      if (grid_z >= (1<<16)) {
        perm[2] = perm[0];
        perm[0] = 2;
      }

      size_t grid[3];
      grid[perm[0]] = grid_x;
      grid[perm[1]] = grid_y;
      grid[perm[2]] = grid_z;

      void *all_args[] = { $inits:(perm_args ++ failure_args ++ arg_params_inits) };
      typename int64_t time_start = 0, time_end = 0;
      if (ctx->debugging) {
        fprintf(ctx->log, "Launching %s with grid size [%d, %d, %d] and block size [%d, %d, %d]; shared memory: %d bytes.\n",
                $string:(prettyString kernel_name),
                grid_x, grid_y, grid_z,
                block_x, block_y, block_z,
                shared_bytes);
        time_start = get_wall_time();
      }
      $items:bef
      CUDA_SUCCEED_OR_RETURN(
        cuLaunchKernel(ctx->program->$id:kernel_name,
                       grid[0], grid[1], grid[2],
                       block_x, block_y, block_z,
                       shared_bytes, ctx->stream,
                       all_args, NULL));
      $items:aft
      if (ctx->debugging) {
        CUDA_SUCCEED_FATAL(cuCtxSynchronize());
        time_end = get_wall_time();
        fprintf(ctx->log, "Kernel %s runtime: %ldus\n",
                $string:(prettyString kernel_name), time_end - time_start);
      }
    }
    return FUTHARK_SUCCESS;
  }|]

  pure kernel_fname
