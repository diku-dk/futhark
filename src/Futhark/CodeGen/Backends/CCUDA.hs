{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Futhark.CodeGen.Backends.CCUDA.Boilerplate
import Futhark.CodeGen.Backends.COpenCL.Boilerplate (commonOptions, sizeLoggingCode)
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep (primStorageType, toStorage)
import Futhark.CodeGen.ImpCode.OpenCL
import qualified Futhark.CodeGen.ImpGen.CUDA as ImpGen
import Futhark.IR.GPUMem hiding
  ( CmpSizeLe,
    GetSize,
    GetSizeMax,
  )
import Futhark.MonadFreshNames
import qualified Language.C.Quote.OpenCL as C
import NeatInterpolation (untrimming)

-- | Compile the program to C with calls to CUDA.
compileProg :: MonadFreshNames m => T.Text -> Prog GPUMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version prog = do
  (ws, Program cuda_code cuda_prelude kernels _ sizes failures prog') <-
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
          sizes
          failures
  (ws,)
    <$> GC.compileProg
      "cuda"
      version
      operations
      extra
      cuda_includes
      [Space "device", DefaultSpace]
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
          GC.opsStaticArray = staticCUDAArray,
          GC.opsMemoryType = cudaMemoryType,
          GC.opsCompiler = callKernel,
          GC.opsFatMemory = True,
          GC.opsCritical =
            ( [C.citems|CUDA_SUCCEED_FATAL(cuCtxPushCurrent(ctx->cuda.cu_ctx));|],
              [C.citems|CUDA_SUCCEED_FATAL(cuCtxPopCurrent(&ctx->cuda.cu_ctx));|]
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

writeCUDAScalar :: GC.WriteScalar OpenCL ()
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
  GC.stm [C.cstm|if (futhark_context_sync(ctx) != 0) { return 1; }|]
  return [C.cexp|$id:val|]
readCUDAScalar _ _ _ space _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

allocateCUDABuffer :: GC.Allocate OpenCL ()
allocateCUDABuffer mem size tag "device" =
  GC.stm [C.cstm|ctx->error = CUDA_SUCCEED_NONFATAL(cuda_alloc(&ctx->cuda, (size_t)$exp:size, $exp:tag, &$exp:mem));|]
allocateCUDABuffer _ _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' memory space."

deallocateCUDABuffer :: GC.Deallocate OpenCL ()
deallocateCUDABuffer mem tag "device" =
  GC.stm [C.cstm|CUDA_SUCCEED_OR_RETURN(cuda_free(&ctx->cuda, $exp:mem, $exp:tag));|]
deallocateCUDABuffer _ _ space =
  error $ "Cannot deallocate in '" ++ space ++ "' memory space."

copyCUDAMemory :: GC.Copy OpenCL ()
copyCUDAMemory dstmem dstidx dstSpace srcmem srcidx srcSpace nbytes = do
  let (fn, prof) = memcpyFun dstSpace srcSpace
      (bef, aft) = profilingEnclosure prof
  GC.item
    [C.citem|{
                $items:bef
                CUDA_SUCCEED_OR_RETURN(
                  $id:fn($exp:dstmem + $exp:dstidx,
                         $exp:srcmem + $exp:srcidx,
                         $exp:nbytes));
                $items:aft
                }
                |]
  where
    memcpyFun DefaultSpace (Space "device") = ("cuMemcpyDtoH" :: String, copyDevToHost)
    memcpyFun (Space "device") DefaultSpace = ("cuMemcpyHtoD", copyHostToDev)
    memcpyFun (Space "device") (Space "device") = ("cuMemcpy", copyDevToDev)
    memcpyFun _ _ =
      error $
        "Cannot copy to '" ++ show dstSpace
          ++ "' from '"
          ++ show srcSpace
          ++ "'."

staticCUDAArray :: GC.StaticArray OpenCL ()
staticCUDAArray name "device" t vs = do
  let ct = GC.primTypeToCType t
  name_realtype <- newVName $ baseString name ++ "_realtype"
  num_elems <- case vs of
    ArrayValues vs' -> do
      let vs'' = [[C.cinit|$exp:v|] | v <- vs']
      GC.earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:(length vs'')] = {$inits:vs''};|]
      return $ length vs''
    ArrayZeros n -> do
      GC.earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:n];|]
      return n
  -- Fake a memory block.
  GC.contextField (C.toIdent name mempty) [C.cty|struct memblock_device|] Nothing
  -- During startup, copy the data to where we need it.
  GC.atInit
    [C.cstm|{
    ctx->$id:name.references = NULL;
    ctx->$id:name.size = 0;
    CUDA_SUCCEED_FATAL(cuMemAlloc(&ctx->$id:name.mem,
                            ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct)));
    if ($int:num_elems > 0) {
      CUDA_SUCCEED_FATAL(cuMemcpyHtoD(ctx->$id:name.mem, $id:name_realtype,
                                $int:num_elems*sizeof($ty:ct)));
    }
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]
staticCUDAArray _ space _ _ =
  error $
    "CUDA backend cannot create static array in '" ++ space
      ++ "' memory space"

cudaMemoryType :: GC.MemoryType OpenCL ()
cudaMemoryType "device" = return [C.cty|typename CUdeviceptr|]
cudaMemoryType space =
  error $ "CUDA backend does not support '" ++ space ++ "' memory space."

callKernel :: GC.OpCompiler OpenCL ()
callKernel (GetSize v key) =
  GC.stm [C.cstm|$id:v = *ctx->tuning_params.$id:key;|]
callKernel (CmpSizeLe v key x) = do
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = *ctx->tuning_params.$id:key <= $exp:x';|]
  sizeLoggingCode v key x'
callKernel (GetSizeMax v size_class) =
  let field = "max_" ++ cudaSizeClass size_class
   in GC.stm [C.cstm|$id:v = ctx->cuda.$id:field;|]
  where
    cudaSizeClass SizeThreshold {} = "threshold"
    cudaSizeClass SizeGroup = "block_size"
    cudaSizeClass SizeNumGroups = "grid_size"
    cudaSizeClass SizeTile = "tile_size"
    cudaSizeClass SizeRegTile = "reg_tile_size"
    cudaSizeClass SizeLocalMemory = "shared_memory"
    cudaSizeClass (SizeBespoke x _) = pretty x
callKernel (LaunchKernel safety kernel_name args num_blocks block_size) = do
  args_arr <- newVName "kernel_args"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  (args', shared_vars) <- unzip <$> mapM mkArgs args
  let (shared_sizes, shared_offsets) = unzip $ catMaybes shared_vars
      shared_offsets_sc = mkOffsets shared_sizes
      shared_args = zip shared_offsets shared_offsets_sc
      shared_tot = last shared_offsets_sc
  forM_ shared_args $ \(arg, offset) ->
    GC.decl [C.cdecl|unsigned int $id:arg = $exp:offset;|]

  (grid_x, grid_y, grid_z) <- mkDims <$> mapM GC.compileExp num_blocks
  (block_x, block_y, block_z) <- mkDims <$> mapM GC.compileExp block_size
  let perm_args
        | length num_blocks == 3 = [[C.cinit|&perm[0]|], [C.cinit|&perm[1]|], [C.cinit|&perm[2]|]]
        | otherwise = []
      failure_args =
        take
          (numFailureParams safety)
          [ [C.cinit|&ctx->global_failure|],
            [C.cinit|&ctx->failure_is_an_option|],
            [C.cinit|&ctx->global_failure_args|]
          ]
      args'' = perm_args ++ failure_args ++ [[C.cinit|&$id:a|] | a <- args']
      sizes_nonzero =
        expsNotZero
          [ grid_x,
            grid_y,
            grid_z,
            block_x,
            block_y,
            block_z
          ]
      (bef, aft) = profilingEnclosure kernel_name

  GC.stm
    [C.cstm|
    if ($exp:sizes_nonzero) {
      int perm[3] = { 0, 1, 2 };

      if ($exp:grid_y >= (1<<16)) {
        perm[1] = perm[0];
        perm[0] = 1;
      }

      if ($exp:grid_z >= (1<<16)) {
        perm[2] = perm[0];
        perm[0] = 2;
      }

      size_t grid[3];
      grid[perm[0]] = $exp:grid_x;
      grid[perm[1]] = $exp:grid_y;
      grid[perm[2]] = $exp:grid_z;

      void *$id:args_arr[] = { $inits:args'' };
      typename int64_t $id:time_start = 0, $id:time_end = 0;
      if (ctx->debugging) {
        fprintf(ctx->log, "Launching %s with grid size [%ld, %ld, %ld] and block size [%ld, %ld, %ld]; shared memory: %d bytes.\n",
                $string:(pretty kernel_name),
                (long int)$exp:grid_x, (long int)$exp:grid_y, (long int)$exp:grid_z,
                (long int)$exp:block_x, (long int)$exp:block_y, (long int)$exp:block_z,
                (int)$exp:shared_tot);
        $id:time_start = get_wall_time();
      }
      $items:bef
      CUDA_SUCCEED_OR_RETURN(
        cuLaunchKernel(ctx->$id:kernel_name,
                       grid[0], grid[1], grid[2],
                       $exp:block_x, $exp:block_y, $exp:block_z,
                       $exp:shared_tot, NULL,
                       $id:args_arr, NULL));
      $items:aft
      if (ctx->debugging) {
        CUDA_SUCCEED_FATAL(cuCtxSynchronize());
        $id:time_end = get_wall_time();
        fprintf(ctx->log, "Kernel %s runtime: %ldus\n",
                $string:(pretty kernel_name), $id:time_end - $id:time_start);
      }
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
    expNotZero e = [C.cexp|$exp:e != 0|]
    expAnd a b = [C.cexp|$exp:a && $exp:b|]
    expsNotZero = foldl expAnd [C.cexp|1|] . map expNotZero
    mkArgs (ValueKArg e t@(FloatType Float16)) = do
      arg <- newVName "kernel_arg"
      e' <- GC.compileExp e
      GC.item [C.citem|$ty:(primStorageType t) $id:arg = $exp:(toStorage t e');|]
      pure (arg, Nothing)
    mkArgs (ValueKArg e t) =
      (,Nothing) <$> GC.compileExpToName "kernel_arg" t e
    mkArgs (MemKArg v) = do
      v' <- GC.rawMem v
      arg <- newVName "kernel_arg"
      GC.decl [C.cdecl|typename CUdeviceptr $id:arg = $exp:v';|]
      return (arg, Nothing)
    mkArgs (SharedMemoryKArg (Count c)) = do
      num_bytes <- GC.compileExp c
      size <- newVName "shared_size"
      offset <- newVName "shared_offset"
      GC.decl [C.cdecl|unsigned int $id:size = $exp:num_bytes;|]
      return (offset, Just (size, offset))
