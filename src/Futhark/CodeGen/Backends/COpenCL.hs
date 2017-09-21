{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.COpenCL
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import Control.Monad hiding (mapM)
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenCL
import qualified Futhark.CodeGen.ImpGen.OpenCL as ImpGen
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program opencl_code opencl_prelude kernel_names prog') ->
      Right <$> GC.compileProg operations
                (generateBoilerplate opencl_code opencl_prelude kernel_names) ()
                [Space "device", Space "local", DefaultSpace]
                options prog'
  where operations :: GC.Operations OpenCL ()
        operations = GC.Operations
                     { GC.opsCompiler = callKernel
                     , GC.opsWriteScalar = writeOpenCLScalar
                     , GC.opsReadScalar = readOpenCLScalar
                     , GC.opsAllocate = allocateOpenCLBuffer
                     , GC.opsDeallocate = deallocateOpenCLBuffer
                     , GC.opsCopy = copyOpenCLMemory
                     , GC.opsStaticArray = staticOpenCLArray
                     , GC.opsMemoryType = openclMemoryType
                     , GC.opsFatMemory = True
                     }

        options = [ Option { optionLongName = "platform"
                           , optionShortName = Just 'p'
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|futhark_context_config_set_platform(cfg, optarg);|]
                           }
                  , Option { optionLongName = "device"
                           , optionShortName = Just 'd'
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|futhark_context_config_set_device(cfg, optarg);|]
                           }
                  , Option { optionLongName = "group-size"
                           , optionShortName = Nothing
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|futhark_context_config_set_group_size(cfg, atoi(optarg));|]
                           }
                  , Option { optionLongName = "num-groups"
                           , optionShortName = Nothing
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|futhark_context_config_set_num_groups(cfg, atoi(optarg));|]
                           }
                  , Option { optionLongName = "dump-opencl"
                           , optionShortName = Nothing
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|futhark_context_config_dump_program_to(cfg, optarg);|]
                           }
                  , Option { optionLongName = "load-opencl"
                           , optionShortName = Nothing
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
                           }
                  ]

writeOpenCLScalar :: GC.WriteScalar OpenCL ()
writeOpenCLScalar mem i t "device" _ val = do
  val' <- newVName "write_tmp"
  GC.stm [C.cstm|{
                   $ty:t $id:val' = $exp:val;
                   OPENCL_SUCCEED(
                     clEnqueueWriteBuffer(ctx->opencl.queue, $exp:mem, CL_TRUE,
                                          $exp:i, sizeof($ty:t),
                                          &$id:val',
                                          0, NULL, NULL));
                }|]
writeOpenCLScalar _ _ _ space _ _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: GC.ReadScalar OpenCL ()
readOpenCLScalar mem i t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t $id:val;|]
  GC.stm [C.cstm|
                 OPENCL_SUCCEED(
                   clEnqueueReadBuffer(ctx->opencl.queue, $exp:mem, CL_TRUE,
                                       $exp:i, sizeof($ty:t),
                                       &$id:val,
                                       0, NULL, NULL));
              |]
  return [C.cexp|$id:val|]
readOpenCLScalar _ _ _ space _ =
  fail $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: GC.Allocate OpenCL ()
allocateOpenCLBuffer mem size "device" = do

  errorname <- newVName "clCreateBuffer_succeeded"
  -- clCreateBuffer fails with CL_INVALID_BUFFER_SIZE if we pass 0 as
  -- the size (unlike malloc()), so we make sure we always allocate at
  -- least a single byte.  The alternative is to protect this with a
  -- branch and leave the cl_mem variable uninitialised if the size is
  -- zero, but this would leave sort of a landmine around, that would
  -- blow up if we ever passed it to an OpenCL function.
  GC.stm [C.cstm|{
    typename cl_int $id:errorname;
    $exp:mem = clCreateBuffer(ctx->opencl.ctx, CL_MEM_READ_WRITE,
                              $exp:size > 0 ? $exp:size : 1, NULL,
                              &$id:errorname);
    OPENCL_SUCCEED($id:errorname);
  }|]
allocateOpenCLBuffer _ _ "local" =
  return () -- Hack - these memory blocks do not actually exist.
allocateOpenCLBuffer _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' space"

deallocateOpenCLBuffer :: GC.Deallocate OpenCL ()
deallocateOpenCLBuffer mem "device" =
  GC.stm [C.cstm|OPENCL_SUCCEED(clReleaseMemObject($exp:mem));|]
deallocateOpenCLBuffer _ "local" =
  return () -- Hack - these memory blocks do not actually exist.
deallocateOpenCLBuffer _ space =
  fail $ "Cannot deallocate in '" ++ space ++ "' space"


copyOpenCLMemory :: GC.Copy OpenCL ()
-- The read/write/copy-buffer functions fail if the given offset is
-- out of bounds, even if asked to read zero bytes.  We protect with a
-- branch to avoid this.
copyOpenCLMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes =
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED(
        clEnqueueReadBuffer(ctx->opencl.queue, $exp:srcmem, CL_TRUE,
                            $exp:srcidx, $exp:nbytes,
                            $exp:destmem + $exp:destidx,
                            0, NULL, NULL));
   }
  |]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED(
        clEnqueueWriteBuffer(ctx->opencl.queue, $exp:destmem, CL_TRUE,
                             $exp:destidx, $exp:nbytes,
                             $exp:srcmem + $exp:srcidx,
                             0, NULL, NULL));
    }
  |]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  -- Be aware that OpenCL swaps the usual order of operands for
  -- memcpy()-like functions.  The order below is not a typo.
  GC.stm [C.cstm|{
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED(
        clEnqueueCopyBuffer(ctx->opencl.queue,
                            $exp:srcmem, $exp:destmem,
                            $exp:srcidx, $exp:destidx,
                            $exp:nbytes,
                            0, NULL, NULL));
      if (ctx->debugging) {
        OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
      }
    }
  }|]
copyOpenCLMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyOpenCLMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openclMemoryType :: GC.MemoryType OpenCL ()
openclMemoryType "device" = pure [C.cty|typename cl_mem|]
openclMemoryType "local" = pure [C.cty|unsigned char|] -- dummy type
openclMemoryType space =
  fail $ "OpenCL backend does not support '" ++ space ++ "' memory space."

staticOpenCLArray :: GC.StaticArray OpenCL ()
staticOpenCLArray name "device" t vs = do
  let ct = GC.primTypeToCType t
      vs' = [[C.cinit|$exp:(GC.compilePrimValue v)|] | v <- vs]
      num_elems = length vs
  name_realtype <- newVName $ baseString name ++ "_realtype"
  GC.libDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:num_elems] = {$inits:vs'};|]
  -- Fake a memory block.
  GC.contextField (pretty name) [C.cty|struct memblock_device|] Nothing
  -- During startup, copy the data to where we need it.
  GC.atInit [C.cstm|{
    typename cl_int success;
    ctx->$id:name.references = NULL;
    ctx->$id:name.size = 0;
    ctx->$id:name.mem =
      clCreateBuffer(ctx->opencl.ctx, CL_MEM_READ_WRITE,
                     ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct), NULL,
                     &success);
    OPENCL_SUCCEED(success);
    if ($int:num_elems > 0) {
      OPENCL_SUCCEED(
        clEnqueueWriteBuffer(ctx->opencl.queue, ctx->$id:name.mem, CL_TRUE,
                             0, $int:num_elems*sizeof($ty:ct),
                             $id:name_realtype,
                             0, NULL, NULL));
    }
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]

staticOpenCLArray _ space _ _ =
  fail $ "OpenCL backend cannot create static array in memory space '" ++ space ++ "'"

callKernel :: GC.OpCompiler OpenCL ()
callKernel (GetNumGroups v) =
  -- Must be a power of two.
  GC.stm [C.cstm|$id:v = ctx->opencl.cfg.num_groups;|]
callKernel (GetGroupSize v) =
  GC.stm [C.cstm|$id:v = ctx->opencl.cfg.group_size;|]
callKernel (GetTileSize v) =
  GC.stm [C.cstm|$id:v = ctx->opencl.cfg.tile_size;|]
callKernel (HostCode c) =
  GC.compileCode c

callKernel (LaunchKernel name args kernel_size workgroup_size) = do
  zipWithM_ setKernelArg [(0::Int)..] args
  kernel_size' <- mapM GC.compileExp kernel_size
  workgroup_size' <- mapM GC.compileExp workgroup_size
  launchKernel name kernel_size' workgroup_size'
  where setKernelArg i (ValueKArg e bt) = do
          v <- GC.compileExpToName "kernel_arg" bt e
          GC.stm [C.cstm|
            OPENCL_SUCCEED(clSetKernelArg(ctx->$id:name, $int:i, sizeof($id:v), &$id:v));
          |]

        setKernelArg i (MemKArg v) = do
          v' <- GC.rawMem v
          GC.stm [C.cstm|
            OPENCL_SUCCEED(clSetKernelArg(ctx->$id:name, $int:i, sizeof($exp:v'), &$exp:v'));
          |]

        setKernelArg i (SharedMemoryKArg num_bytes) = do
          num_bytes' <- GC.compileExp $ innerExp num_bytes
          GC.stm [C.cstm|
            OPENCL_SUCCEED(clSetKernelArg(ctx->$id:name, $int:i, $exp:num_bytes', NULL));
            |]

launchKernel :: C.ToExp a =>
                String -> [a] -> [a] -> GC.CompilerM op s ()
launchKernel kernel_name kernel_dims workgroup_dims = do
  global_work_size <- newVName "global_work_size"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  time_diff <- newVName "time_diff"
  local_work_size <- newVName "local_work_size"

  GC.stm [C.cstm|
    if ($exp:total_elements != 0) {
      const size_t $id:global_work_size[$int:kernel_rank] = {$inits:kernel_dims'};
      const size_t $id:local_work_size[$int:kernel_rank] = {$inits:workgroup_dims'};
      typename int64_t $id:time_start, $id:time_end;
      if (ctx->debugging) {
        fprintf(stderr, "Launching %s with global work size [", $string:kernel_name);
        $stms:(printKernelSize global_work_size)
        fprintf(stderr, "].\n");
        $id:time_start = get_wall_time();
      }
      OPENCL_SUCCEED(
        clEnqueueNDRangeKernel(ctx->opencl.queue, ctx->$id:kernel_name, $int:kernel_rank, NULL,
                               $id:global_work_size, $id:local_work_size,
                               0, NULL, NULL));
      if (ctx->debugging) {
        OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
        $id:time_end = get_wall_time();
        long int $id:time_diff = $id:time_end - $id:time_start;
        ctx->$id:(kernelRuntime kernel_name) += $id:time_diff;
        ctx->$id:(kernelRuns kernel_name)++;
        fprintf(stderr, "kernel %s runtime: %ldus\n",
                $string:kernel_name, (int)$id:time_diff);
      }
    }|]
  where kernel_rank = length kernel_dims
        kernel_dims' = map toInit kernel_dims
        workgroup_dims' = map toInit workgroup_dims
        total_elements = foldl multExp [C.cexp|1|] kernel_dims

        toInit e = [C.cinit|$exp:e|]
        multExp x y = [C.cexp|$exp:x * $exp:y|]

        printKernelSize :: VName -> [C.Stm]
        printKernelSize global_work_size =
          intercalate [[C.cstm|fprintf(stderr, ", ");|]] $
          map (printKernelDim global_work_size) [0..kernel_rank-1]
        printKernelDim global_work_size i =
          [[C.cstm|fprintf(stderr, "%zu", $id:global_work_size[$int:i]);|]]
