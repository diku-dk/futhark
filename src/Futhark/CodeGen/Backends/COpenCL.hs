{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

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
import Data.List (intercalate)
import qualified Data.Text as T
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep (primStorageType, toStorage)
import Futhark.CodeGen.ImpCode.OpenCL
import qualified Futhark.CodeGen.ImpGen.OpenCL as ImpGen
import Futhark.IR.GPUMem hiding
  ( CmpSizeLe,
    GetSize,
    GetSizeMax,
  )
import Futhark.MonadFreshNames
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C
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
      sizes
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
      operations
      ( generateBoilerplate
          opencl_code
          opencl_prelude
          cost_centres
          kernels
          types
          sizes
          failures
      )
      include_opencl_h
      [Space "device", DefaultSpace]
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
          GC.opsStaticArray = staticOpenCLArray,
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
                    clEnqueueWriteBuffer(ctx->opencl.queue, $exp:mem, $exp:blocking,
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
                   clEnqueueReadBuffer(ctx->opencl.queue, $exp:mem,
                                       ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
                                       $exp:i * sizeof($ty:t), sizeof($ty:t),
                                       &$id:val,
                                       0, NULL, $exp:(profilingEvent copyScalarFromDev)));
              |]
  GC.stm
    [C.cstm|if (ctx->failure_is_an_option &&
                     futhark_context_sync(ctx) != 0) { return 1; }|]
  return [C.cexp|$id:val|]
readOpenCLScalar _ _ _ space _ =
  error $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: GC.Allocate OpenCL ()
allocateOpenCLBuffer mem size tag "device" =
  GC.stm [C.cstm|ctx->error = OPENCL_SUCCEED_NONFATAL(opencl_alloc(&ctx->opencl, (size_t)$exp:size, $exp:tag, &$exp:mem));|]
allocateOpenCLBuffer _ _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' space."

deallocateOpenCLBuffer :: GC.Deallocate OpenCL ()
deallocateOpenCLBuffer mem tag "device" =
  GC.stm [C.cstm|OPENCL_SUCCEED_OR_RETURN(opencl_free(&ctx->opencl, $exp:mem, $exp:tag));|]
deallocateOpenCLBuffer _ _ space =
  error $ "Cannot deallocate in '" ++ space ++ "' space"

copyOpenCLMemory :: GC.Copy OpenCL ()
-- The read/write/copy-buffer functions fail if the given offset is
-- out of bounds, even if asked to read zero bytes.  We protect with a
-- branch to avoid this.
copyOpenCLMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes =
  GC.stm
    [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueReadBuffer(ctx->opencl.queue, $exp:srcmem,
                            ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
                            (size_t)$exp:srcidx, (size_t)$exp:nbytes,
                            $exp:destmem + $exp:destidx,
                            0, NULL, $exp:(profilingEvent copyHostToDev)));
      if (ctx->failure_is_an_option &&
          futhark_context_sync(ctx) != 0) { return 1; }
   }
  |]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  GC.stm
    [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueWriteBuffer(ctx->opencl.queue, $exp:destmem, CL_TRUE,
                             (size_t)$exp:destidx, (size_t)$exp:nbytes,
                             $exp:srcmem + $exp:srcidx,
                             0, NULL, $exp:(profilingEvent copyDevToHost)));
    }
  |]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  -- Be aware that OpenCL swaps the usual order of operands for
  -- memcpy()-like functions.  The order below is not a typo.
  GC.stm
    [C.cstm|{
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueCopyBuffer(ctx->opencl.queue,
                            $exp:srcmem, $exp:destmem,
                            (size_t)$exp:srcidx, (size_t)$exp:destidx,
                            (size_t)$exp:nbytes,
                            0, NULL, $exp:(profilingEvent copyDevToDev)));
      if (ctx->debugging) {
        OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
      }
    }
  }|]
copyOpenCLMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyOpenCLMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openclMemoryType :: GC.MemoryType OpenCL ()
openclMemoryType "device" = pure [C.cty|typename cl_mem|]
openclMemoryType space =
  error $ "OpenCL backend does not support '" ++ space ++ "' memory space."

staticOpenCLArray :: GC.StaticArray OpenCL ()
staticOpenCLArray name "device" t vs = do
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
    typename cl_int success;
    ctx->$id:name.references = NULL;
    ctx->$id:name.size = 0;
    ctx->$id:name.mem =
      clCreateBuffer(ctx->opencl.ctx, CL_MEM_READ_WRITE,
                     ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct), NULL,
                     &success);
    OPENCL_SUCCEED_OR_RETURN(success);
    if ($int:num_elems > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueWriteBuffer(ctx->opencl.queue, ctx->$id:name.mem, CL_TRUE,
                             0, $int:num_elems*sizeof($ty:ct),
                             $id:name_realtype,
                             0, NULL, NULL));
    }
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]
staticOpenCLArray _ space _ _ =
  error $ "OpenCL backend cannot create static array in memory space '" ++ space ++ "'"

callKernel :: GC.OpCompiler OpenCL ()
callKernel (GetSize v key) =
  GC.stm [C.cstm|$id:v = *ctx->tuning_params.$id:key;|]
callKernel (CmpSizeLe v key x) = do
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = *ctx->tuning_params.$id:key <= $exp:x';|]
  sizeLoggingCode v key x'
callKernel (GetSizeMax v size_class) =
  let field = "max_" ++ pretty size_class
   in GC.stm [C.cstm|$id:v = ctx->opencl.$id:field;|]
callKernel (LaunchKernel safety name args num_workgroups workgroup_size) = do
  -- The other failure args are set automatically when the kernel is
  -- first created.
  when (safety == SafetyFull) $
    GC.stm
      [C.cstm|
      OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->$id:name, 1,
                                              sizeof(ctx->failure_is_an_option),
                                              &ctx->failure_is_an_option));
    |]

  zipWithM_ setKernelArg [numFailureParams safety ..] args
  num_workgroups' <- mapM GC.compileExp num_workgroups
  workgroup_size' <- mapM GC.compileExp workgroup_size
  local_bytes <- foldM localBytes [C.cexp|0|] args

  launchKernel name num_workgroups' workgroup_size' local_bytes

  when (safety >= SafetyFull) $
    GC.stm [C.cstm|ctx->failure_is_an_option = 1;|]
  where
    setKernelArg i (ValueKArg e pt) = do
      v <- case pt of
        -- We always transfer f16 values to the kernel as 16 bits, but
        -- the actual host type may be typedef'd to a 32-bit float.
        -- This requires some care.
        FloatType Float16 -> do
          v <- newVName "kernel_arg"
          e' <- toStorage pt <$> GC.compileExp e
          GC.decl [C.cdecl|$ty:(primStorageType pt) $id:v = $e';|]
          pure v
        _ -> GC.compileExpToName "kernel_arg" pt e
      GC.stm
        [C.cstm|
            OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->$id:name, $int:i, sizeof($id:v), &$id:v));
          |]
    setKernelArg i (MemKArg v) = do
      v' <- GC.rawMem v
      GC.stm
        [C.cstm|
            OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->$id:name, $int:i, sizeof($exp:v'), &$exp:v'));
          |]
    setKernelArg i (SharedMemoryKArg num_bytes) = do
      num_bytes' <- GC.compileExp $ unCount num_bytes
      GC.stm
        [C.cstm|
            OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->$id:name, $int:i, (size_t)$exp:num_bytes', NULL));
            |]

    localBytes cur (SharedMemoryKArg num_bytes) = do
      num_bytes' <- GC.compileExp $ unCount num_bytes
      return [C.cexp|$exp:cur + $exp:num_bytes'|]
    localBytes cur _ = return cur

launchKernel ::
  C.ToExp a =>
  KernelName ->
  [a] ->
  [a] ->
  a ->
  GC.CompilerM op s ()
launchKernel kernel_name num_workgroups workgroup_dims local_bytes = do
  global_work_size <- newVName "global_work_size"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  time_diff <- newVName "time_diff"
  local_work_size <- newVName "local_work_size"

  let (debug_str, debug_args) = debugPrint global_work_size local_work_size

  GC.stm
    [C.cstm|
    if ($exp:total_elements != 0) {
      const size_t $id:global_work_size[$int:kernel_rank] = {$inits:kernel_dims'};
      const size_t $id:local_work_size[$int:kernel_rank] = {$inits:workgroup_dims'};
      typename int64_t $id:time_start = 0, $id:time_end = 0;
      if (ctx->debugging) {
        fprintf(ctx->log, $string:debug_str, $args:debug_args);
        $id:time_start = get_wall_time();
      }
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueNDRangeKernel(ctx->opencl.queue, ctx->$id:kernel_name, $int:kernel_rank, NULL,
                               $id:global_work_size, $id:local_work_size,
                               0, NULL, $exp:(profilingEvent kernel_name)));
      if (ctx->debugging) {
        OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
        $id:time_end = get_wall_time();
        long int $id:time_diff = $id:time_end - $id:time_start;
        fprintf(ctx->log, "kernel %s runtime: %ldus\n",
                $string:(pretty kernel_name), $id:time_diff);
      }
    }|]
  where
    kernel_rank = length kernel_dims
    kernel_dims = zipWith multExp (map toSize num_workgroups) (map toSize workgroup_dims)
    kernel_dims' = map toInit kernel_dims
    workgroup_dims' = map (toInit . toSize) workgroup_dims
    total_elements = foldl multExp [C.cexp|1|] kernel_dims

    toInit e = [C.cinit|$exp:e|]
    multExp x y = [C.cexp|$exp:x * $exp:y|]
    toSize e = [C.cexp|(size_t)$exp:e|]

    debugPrint :: VName -> VName -> (String, [C.Exp])
    debugPrint global_work_size local_work_size =
      ( "Launching %s with global work size "
          ++ dims
          ++ " and local work size "
          ++ dims
          ++ "; local memory: %d bytes.\n",
        [C.cexp|$string:(pretty kernel_name)|] :
        map (kernelDim global_work_size) [0 .. kernel_rank - 1]
          ++ map (kernelDim local_work_size) [0 .. kernel_rank - 1]
          ++ [[C.cexp|(int)$exp:local_bytes|]]
      )
      where
        dims = "[" ++ intercalate ", " (replicate kernel_rank "%zu") ++ "]"
        kernelDim arr i = [C.cexp|$id:arr[$int:i]|]
