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

import Control.Monad hiding (mapM)
import Data.Text qualified as T
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep (primStorageType, toStorage)
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
  (ws,)
    <$> GC.compileProg
      "opencl"
      version
      params
      operations
      ( generateBoilerplate
          opencl_code
          opencl_prelude
          cost_centres
          kernels
          types
          failures
      )
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
  let (decl, blocking) =
        case val of
          C.Const {} -> ([C.citem|static $ty:t $id:val' = $exp:val;|], [C.cexp|CL_FALSE|])
          _ -> ([C.citem|$ty:t $id:val' = $exp:val;|], [C.cexp|CL_TRUE|])
  GC.stm
    [C.cstm|{$item:decl
                  OPENCL_SUCCEED_OR_RETURN(
                    clEnqueueWriteBuffer(ctx->queue, $exp:mem, $exp:blocking,
                                         $exp:i * sizeof($ty:t), sizeof($ty:t),
                                         &$id:val',
                                         0, NULL, $exp:(profilingEvent copyScalarToDev)));
                }|]
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
    [C.cstm|OPENCL_SUCCEED_OR_RETURN(
                   clEnqueueReadBuffer(ctx->queue, $exp:mem,
                                       ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
                                       $exp:i * sizeof($ty:t), sizeof($ty:t),
                                       &$id:val,
                                       0, NULL, $exp:(profilingEvent copyScalarFromDev)));
              |]
  GC.stm
    [C.cstm|if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
            { return 1; }|]
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
copyOpenCLMemory _ destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  -- Be aware that OpenCL swaps the usual order of operands for
  -- memcpy()-like functions.  The order below is not a typo.
  GC.stm
    [C.cstm|{
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueCopyBuffer(ctx->queue,
                            $exp:srcmem, $exp:destmem,
                            (size_t)$exp:srcidx, (size_t)$exp:destidx,
                            (size_t)$exp:nbytes,
                            0, NULL, $exp:(profilingEvent copyDevToDev)));
      if (ctx->debugging) {
        OPENCL_SUCCEED_FATAL(clFinish(ctx->queue));
      }
    }
  }|]
copyOpenCLMemory _ destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyOpenCLMemory _ _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openclMemoryType :: GC.MemoryType OpenCL ()
openclMemoryType "device" = pure [C.cty|typename cl_mem|]
openclMemoryType space =
  error $ "OpenCL backend does not support '" ++ space ++ "' memory space."

kernelConstToExp :: KernelConst -> C.Exp
kernelConstToExp (SizeConst key) =
  [C.cexp|*ctx->tuning_params.$id:key|]
kernelConstToExp (SizeMaxConst size_class) =
  [C.cexp|ctx->$id:field|]
  where
    field = "max_" <> prettyString size_class

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
callKernel (LaunchKernel safety name args num_workgroups workgroup_size) = do
  -- The other failure args are set automatically when the kernel is
  -- first created.
  when (safety == SafetyFull) $
    GC.stm
      [C.cstm|
      OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->program->$id:name, 1,
                                              sizeof(ctx->failure_is_an_option),
                                              &ctx->failure_is_an_option));
    |]

  (arg_params, arg_set, call_args) <-
    unzip3 <$> zipWithM onArg [(0 :: Int) ..] args

  num_workgroups' <- mapM GC.compileExp num_workgroups
  workgroup_size' <- mapM compileGroupDim workgroup_size
  local_bytes <- foldM localBytes [C.cexp|0|] args

  kernel_fname <- genKernelFunction name safety arg_params arg_set

  let grid_x : grid_y : grid_z : _ = num_workgroups' ++ repeat [C.cexp|1|]
      group_x : group_y : group_z : _ = workgroup_size' ++ repeat [C.cexp|1|]

  GC.stm
    [C.cstm|{
           err = $id:kernel_fname(ctx,
                                  $exp:grid_x,$exp:grid_y,$exp:grid_z,
                                  $exp:group_x, $exp:group_y, $exp:group_z,
                                  $exp:local_bytes,
                                  $args:call_args);
           if (err != FUTHARK_SUCCESS) { goto cleanup; }
           }|]

  when (safety >= SafetyFull) $
    GC.stm [C.cstm|ctx->failure_is_an_option = 1;|]
  where
    localBytes cur (SharedMemoryKArg num_bytes) = do
      num_bytes' <- GC.compileExp $ unCount num_bytes
      pure [C.cexp|$exp:cur + $exp:num_bytes'|]
    localBytes cur _ = pure cur

    onArg i (ValueKArg e t) = do
      let arg = "arg" <> show i
      e' <- GC.compileExp e
      pure
        ( [C.cparam|$ty:(primStorageType t) $id:arg|],
          ([C.cexp|sizeof($id:arg)|], [C.cexp|&$id:arg|]),
          toStorage t e'
        )
    onArg i (MemKArg v) = do
      let arg = "arg" <> show i
      v' <- GC.rawMem v
      pure
        ( [C.cparam|typename cl_mem $id:arg|],
          ([C.cexp|sizeof($id:arg)|], [C.cexp|&$id:arg|]),
          v'
        )
    onArg i (SharedMemoryKArg (Count c)) = do
      let arg = "arg" <> show i
      num_bytes <- GC.compileExp c
      pure
        ( [C.cparam|unsigned int $id:arg|],
          ([C.cexp|$id:arg|], [C.cexp|NULL|]),
          num_bytes
        )

genKernelFunction ::
  KernelName ->
  KernelSafety ->
  [C.Param] ->
  [(C.Exp, C.Exp)] ->
  GC.CompilerM op s Name
genKernelFunction kernel_name safety arg_params arg_set = do
  let kernel_fname = "gpu_kernel_" <> kernel_name
  GC.libDecl
    [C.cedecl|static int $id:kernel_fname(
                struct futhark_context* ctx,
                unsigned int grid_x, unsigned int grid_y, unsigned int grid_z,
                unsigned int block_x, unsigned int block_y, unsigned int block_z,
                unsigned int local_bytes, $params:arg_params) {
    (void)local_bytes;
    if (grid_x * grid_y * grid_z * block_x * block_y * block_z != 0) {
      const size_t global_work_size[3] = {grid_x*block_x, grid_y*block_y, grid_z*block_z};
      const size_t local_work_size[3] = {block_x, block_y, block_z};
      typename int64_t time_start = 0, time_end = 0;
      $stms:set_args
      if (ctx->debugging) {
        fprintf(ctx->log, "Launching %s with grid size [%d, %d, %d] and group size [%d, %d, %d]; local memory: %d bytes.\n",
                $string:(prettyString kernel_name),
                grid_x, grid_y, grid_z,
                block_x, block_y, block_z,
                local_bytes);
        time_start = get_wall_time();
      }
      typename cl_event *pevent = $exp:(profilingEvent kernel_name);
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueNDRangeKernel(ctx->queue, ctx->program->$id:kernel_name, 3, NULL,
                               global_work_size, local_work_size,
                               0, NULL, pevent));
      if (ctx->debugging) {
        OPENCL_SUCCEED_FATAL(clFinish(ctx->queue));
        time_end = get_wall_time();
        long int time_diff = time_end - time_start;
        fprintf(ctx->log, "kernel %s runtime: %ldus\n",
                $string:(prettyString kernel_name), time_diff);
      }
    }
    return FUTHARK_SUCCESS;
  }|]

  pure kernel_fname
  where
    set_args = zipWith setKernelArg [numFailureParams safety ..] arg_set
    setKernelArg i (size, e) =
      [C.cstm|OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->program->$id:kernel_name, $int:i, $exp:size, $exp:e));|]
