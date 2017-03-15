{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.COpenCL
  ( compileProg
  ) where

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.Traversable
import Data.List

import Prelude hiding (mapM)

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenCL
import qualified Futhark.CodeGen.ImpGen.OpenCL as ImpGen
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError String)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program opencl_code opencl_prelude kernel_names prog') ->
      Right <$> GenericC.compileProg operations ()
                [Space "device", Space "local", DefaultSpace]
                (openClDecls transposeBlockDim kernel_names opencl_code opencl_prelude)
                openClInit
                [[C.cstm|OPENCL_SUCCEED(clFinish(fut_cl_queue));|]]
                (openClReport kernel_names)
                options prog'
  where operations :: GenericC.Operations OpenCL ()
        operations = GenericC.Operations
                     { GenericC.opsCompiler = callKernel
                     , GenericC.opsWriteScalar = writeOpenCLScalar
                     , GenericC.opsReadScalar = readOpenCLScalar
                     , GenericC.opsAllocate = allocateOpenCLBuffer
                     , GenericC.opsDeallocate = deallocateOpenCLBuffer
                     , GenericC.opsCopy = copyOpenCLMemory
                     , GenericC.opsMemoryType = openclMemoryType
                     , GenericC.opsFatMemory = True
                     }

        options = [ Option { optionLongName = "platform"
                           , optionShortName = Just 'p'
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|set_preferred_platform(optarg);|]
                           }
                  , Option { optionLongName = "device"
                           , optionShortName = Just 'd'
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|set_preferred_device(optarg);|]
                           }
                  , Option { optionLongName = "synchronous"
                           , optionShortName = Just 's'
                           , optionArgument = NoArgument
                           , optionAction = [C.cstm|debugging = 1;|]
                           }
                  , Option { optionLongName = "group-size"
                           , optionShortName = Nothing
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|cl_group_size = atoi(optarg);|]
                           }
                  , Option { optionLongName = "num-groups"
                           , optionShortName = Nothing
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|cl_num_groups = atoi(optarg);|]
                           }
                  , Option { optionLongName = "dump-opencl"
                           , optionShortName = Nothing
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|cl_dump_program_to = optarg;|]
                           }
                  , Option { optionLongName = "read-opencl"
                           , optionShortName = Nothing
                           , optionArgument = RequiredArgument
                           , optionAction = [C.cstm|cl_read_program_from = optarg;|]
                           }
                  ]

writeOpenCLScalar :: GenericC.WriteScalar OpenCL ()
writeOpenCLScalar mem i t "device" _ val = do
  val' <- newVName "write_tmp"
  GenericC.stm [C.cstm|{
                   $ty:t $id:val' = $exp:val;
                   OPENCL_SUCCEED(
                     clEnqueueWriteBuffer(fut_cl_queue, $exp:mem, CL_TRUE,
                                          $exp:i, sizeof($ty:t),
                                          &$id:val',
                                          0, NULL, NULL));
                }|]
writeOpenCLScalar _ _ _ space _ _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: GenericC.ReadScalar OpenCL ()
readOpenCLScalar mem i t "device" _ = do
  val <- newVName "read_res"
  GenericC.decl [C.cdecl|$ty:t $id:val;|]
  GenericC.stm [C.cstm|
                 OPENCL_SUCCEED(
                   clEnqueueReadBuffer(fut_cl_queue, $exp:mem, CL_TRUE,
                                       $exp:i, sizeof($ty:t),
                                       &$id:val,
                                       0, NULL, NULL));
              |]
  return [C.cexp|$id:val|]
readOpenCLScalar _ _ _ space _ =
  fail $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: GenericC.Allocate OpenCL ()
allocateOpenCLBuffer mem size "device" = do

  errorname <- newVName "clCreateBuffer_succeeded"
  -- clCreateBuffer fails with CL_INVALID_BUFFER_SIZE if we pass 0 as
  -- the size (unlike malloc()), so we make sure we always allocate at
  -- least a single byte.  The alternative is to protect this with a
  -- branch and leave the cl_mem variable uninitialised if the size is
  -- zero, but this would leave sort of a landmine around, that would
  -- blow up if we ever passed it to an OpenCL function.
  GenericC.stm [C.cstm|{
    typename cl_int $id:errorname;
    $exp:mem = clCreateBuffer(fut_cl_context, CL_MEM_READ_WRITE,
                              $exp:size > 0 ? $exp:size : 1, NULL,
                              &$id:errorname);
    OPENCL_SUCCEED($id:errorname);
  }|]
allocateOpenCLBuffer _ _ "local" =
  return () -- Hack - these memory blocks do not actually exist.
allocateOpenCLBuffer _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' space"

deallocateOpenCLBuffer :: GenericC.Deallocate OpenCL ()
deallocateOpenCLBuffer mem "device" =
  GenericC.stm [C.cstm|OPENCL_SUCCEED(clReleaseMemObject($exp:mem));|]
deallocateOpenCLBuffer _ "local" =
  return () -- Hack - these memory blocks do not actually exist.
deallocateOpenCLBuffer _ space =
  fail $ "Cannot deallocate in '" ++ space ++ "' space"


copyOpenCLMemory :: GenericC.Copy OpenCL ()
-- The read/write/copy-buffer functions fail if the given offset is
-- out of bounds, even if asked to read zero bytes.  We protect with a
-- branch to avoid this.
copyOpenCLMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes =
  GenericC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED(
        clEnqueueReadBuffer(fut_cl_queue, $exp:srcmem, CL_TRUE,
                            $exp:srcidx, $exp:nbytes,
                            $exp:destmem + $exp:destidx,
                            0, NULL, NULL));
   }
  |]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  GenericC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED(
        clEnqueueWriteBuffer(fut_cl_queue, $exp:destmem, CL_TRUE,
                             $exp:destidx, $exp:nbytes,
                             $exp:srcmem + $exp:srcidx,
                             0, NULL, NULL));
    }
  |]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  -- Be aware that OpenCL swaps the usual order of operands for
  -- memcpy()-like functions.  The order below is not a typo.
  GenericC.stm [C.cstm|{
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED(
        clEnqueueCopyBuffer(fut_cl_queue,
                            $exp:srcmem, $exp:destmem,
                            $exp:srcidx, $exp:destidx,
                            $exp:nbytes,
                            0, NULL, NULL));
      if (debugging) {
        OPENCL_SUCCEED(clFinish(fut_cl_queue));
      }
    }
  }|]
copyOpenCLMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openclMemoryType :: GenericC.MemoryType OpenCL ()
openclMemoryType "device" = pure [C.cty|typename cl_mem|]
openclMemoryType "local" = pure [C.cty|unsigned char|] -- dummy type
openclMemoryType space =
  fail $ "OpenCL backend does not support '" ++ space ++ "' memory space."

callKernel :: GenericC.OpCompiler OpenCL ()
callKernel (GetNumGroups v) =
  -- Must be a power of two.
  GenericC.stm [C.cstm|$id:v = cl_num_groups;|]
callKernel (GetGroupSize v) =
  GenericC.stm [C.cstm|$id:v = cl_group_size;|]
callKernel (GetTileSize v) =
  GenericC.stm [C.cstm|$id:v = cl_tile_size;|]
callKernel (HostCode c) =
  GenericC.compileCode c

callKernel (LaunchKernel name args kernel_size workgroup_size) = do
  zipWithM_ setKernelArg [(0::Int)..] args
  kernel_size' <- mapM GenericC.compileExp kernel_size
  workgroup_size' <- mapM GenericC.compileExp workgroup_size
  launchKernel name kernel_size' workgroup_size'
  where setKernelArg i (ValueKArg e bt) = do
          v <- GenericC.compileExpToName "kernel_arg" bt e
          GenericC.stm [C.cstm|
            OPENCL_SUCCEED(clSetKernelArg($id:name, $int:i, sizeof($id:v), &$id:v));
          |]

        setKernelArg i (MemKArg v) = do
          v' <- GenericC.rawMem v
          GenericC.stm [C.cstm|
            OPENCL_SUCCEED(clSetKernelArg($id:name, $int:i, sizeof($exp:v'), &$exp:v'));
          |]

        setKernelArg i (SharedMemoryKArg num_bytes) = do
          num_bytes' <- GenericC.compileExp $ innerExp num_bytes
          GenericC.stm [C.cstm|
            OPENCL_SUCCEED(clSetKernelArg($id:name, $int:i, $exp:num_bytes', NULL));
            |]

launchKernel :: C.ToExp a =>
                String -> [a] -> [a] -> GenericC.CompilerM op s ()
launchKernel kernel_name kernel_dims workgroup_dims = do
  global_work_size <- newVName "global_work_size"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  time_diff <- newVName "time_diff"
  local_work_size <- newVName "local_work_size"

  GenericC.stm [C.cstm|
    if ($exp:total_elements != 0) {
      const size_t $id:global_work_size[$int:kernel_rank] = {$inits:kernel_dims'};
      const size_t $id:local_work_size[$int:kernel_rank] = {$inits:workgroup_dims'};
      typename int64_t $id:time_start, $id:time_end;
      if (debugging) {
        fprintf(stderr, "Launching %s with global work size [", $string:kernel_name);
        $stms:(printKernelSize global_work_size)
        fprintf(stderr, "].\n");
        $id:time_start = get_wall_time();
      }
      OPENCL_SUCCEED(
        clEnqueueNDRangeKernel(fut_cl_queue, $id:kernel_name, $int:kernel_rank, NULL,
                               $id:global_work_size, $id:local_work_size,
                               0, NULL, NULL));
      if (debugging) {
        OPENCL_SUCCEED(clFinish(fut_cl_queue));
        $id:time_end = get_wall_time();
        long int $id:time_diff = $id:time_end - $id:time_start;
        if (detail_timing) {
          $id:(kernelRuntime kernel_name) += $id:time_diff;
          $id:(kernelRuns kernel_name)++;
          fprintf(stderr, "kernel %s runtime: %ldus\n",
                  $string:kernel_name, (int)$id:time_diff);
        }
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
