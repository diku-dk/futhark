{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.OpenCL
  ( compileProg
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Traversable hiding (forM, mapM)
import qualified Data.HashSet as HS
import Data.List

import Prelude

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.Representation.ExplicitMemory (Prog, pretty)
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.KernelImp
import qualified Futhark.CodeGen.KernelImpGen as KernelImpGen
import Futhark.MonadFreshNames

compileProg :: Prog -> Either String String
compileProg prog = do
  prog' <- KernelImpGen.compileProg prog
  let header = unlines [ "#include <CL/cl.h>\n"
                       , "#define FUT_KERNEL(s) #s"
                       , blockDimPragma
                       ]
  (kernels, requirements) <- compileKernels $ getKernels prog'
  return $
    header ++
    GenericC.compileProg operations () (openClDecls kernels requirements)
    openClInit (openClReport $ map fst kernels) prog'
  where operations :: GenericC.Operations CallKernel ()
        operations = GenericC.Operations
                     { GenericC.opsCompiler = callKernel
                     , GenericC.opsWriteScalar = writeOpenCLScalar
                     , GenericC.opsReadScalar = readOpenCLScalar
                     , GenericC.opsAllocate = allocateOpenCLBuffer
                     , GenericC.opsCopy = copyOpenCLMemory
                     , GenericC.opsMemoryType = openclMemoryType
                     }

writeOpenCLScalar :: GenericC.WriteScalar CallKernel ()
writeOpenCLScalar mem i t "device" val = do
  val' <- newVName "write_tmp"
  GenericC.stm [C.cstm|{
                   $ty:t $id:val' = $exp:val;
                   assert(clEnqueueWriteBuffer(fut_cl_queue, $id:mem, CL_TRUE,
                                               $exp:i, sizeof($ty:t),
                                               &$id:val',
                                               0, NULL, NULL)
                          == CL_SUCCESS);
                   assert(clFinish(fut_cl_queue) == CL_SUCCESS);
                }|]
writeOpenCLScalar _ _ _ space _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: GenericC.ReadScalar CallKernel ()
readOpenCLScalar mem i t "device" = do
  val <- newVName "read_res"
  GenericC.decl [C.cdecl|$ty:t $id:val;|]
  GenericC.stm [C.cstm|{
                 assert(clEnqueueReadBuffer(fut_cl_queue, $id:mem, CL_TRUE,
                                            $exp:i, sizeof($ty:t),
                                            &$id:val,
                                            0, NULL, NULL)
                        == CL_SUCCESS);
                 assert(clFinish(fut_cl_queue) == CL_SUCCESS);
              }|]
  return [C.cexp|$id:val|]
readOpenCLScalar _ _ _ space =
  fail $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: GenericC.Allocate CallKernel ()
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
    $id:mem = clCreateBuffer(fut_cl_context, CL_MEM_READ_WRITE,
                             $exp:size > 0 ? $exp:size : 1, NULL,
                             &$id:errorname);
    assert($id:errorname == 0);
  }|]
allocateOpenCLBuffer _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' space"


copyOpenCLMemory :: GenericC.Copy CallKernel ()
-- The read/write/copy-buffer functions fail if the given offset is
-- out of bounds, even if asked to read zero bytes.  We protect with a
-- branch to avoid this.
copyOpenCLMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes =
  GenericC.stm [C.cstm|{
    if ($exp:nbytes > 0) {
      assert(clEnqueueReadBuffer(fut_cl_queue, $id:srcmem, CL_TRUE,
                                 $exp:srcidx, $exp:nbytes,
                                 $id:destmem + $exp:destidx,
                                 0, NULL, NULL)
             == CL_SUCCESS);
      assert(clFinish(fut_cl_queue) == CL_SUCCESS);
   }
  }|]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  GenericC.stm [C.cstm|{
    if ($exp:nbytes > 0) {
      assert(clEnqueueWriteBuffer(fut_cl_queue, $id:destmem, CL_TRUE,
                                  $exp:destidx, $exp:nbytes,
                                  $id:srcmem + $exp:srcidx,
                                  0, NULL, NULL)
             == CL_SUCCESS);
      assert(clFinish(fut_cl_queue) == CL_SUCCESS);
    }
  }|]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  -- Be aware that OpenCL swaps the usual order of operands for
  -- memcpy()-like functions.  The order below is not a typo.
  GenericC.stm [C.cstm|{
    if ($exp:nbytes > 0) {
      assert(clEnqueueCopyBuffer(fut_cl_queue,
                                 $id:srcmem, $id:destmem,
                                 $exp:srcidx, $exp:destidx,
                                 $exp:nbytes,
                                 0, NULL, NULL)
             == CL_SUCCESS);
      assert(clFinish(fut_cl_queue) == CL_SUCCESS);
    }
  }|]
copyOpenCLMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openclMemoryType :: GenericC.MemoryType CallKernel ()
openclMemoryType "device" = pure [C.cty|typename cl_mem|]
openclMemoryType space =
  fail $ "OpenCL backend does not support '" ++ space ++ "' memory space."

callKernel :: GenericC.OpCompiler CallKernel ()
callKernel (Kernel kernel) = do
  zipWithM_ mkBuffer [(0::Int)..] $ kernelUses kernel
  let kernel_size = GenericC.dimSizeToExp $ kernelSize kernel

  launchKernel kernel_name [kernel_size] Nothing
  return GenericC.Done
  where mkBuffer i (MemoryUse mem _) =
          GenericC.stm [C.cstm|{
            assert(clSetKernelArg($id:kernel_name, $int:i, sizeof($id:mem), &$id:mem)
                   == CL_SUCCESS);
            }|]

        mkBuffer i (ScalarUse hostvar _) =
          GenericC.stm [C.cstm|{
            assert(clSetKernelArg($id:kernel_name, $int:i, sizeof($id:hostvar), &$id:hostvar)
                   == CL_SUCCESS);
          }|]

        kernel_name = genericKernelName kernel

callKernel kernel@(MapTranspose bt destmem destoffset srcmem srcoffset num_arrays x_elems y_elems) = do
  destoffset' <- GenericC.compileExp destoffset
  srcoffset' <- GenericC.compileExp srcoffset
  x_elems' <- GenericC.compileExp x_elems
  y_elems' <- GenericC.compileExp y_elems
  x_elems_name <- newVName "x_elems"
  y_elems_name <- newVName "y_elems"
  destoffset_name <- newVName "destoffset"
  srcoffset_name <- newVName "srcoffset"
  GenericC.stm [C.cstm|{
    int $id:x_elems_name = $exp:x_elems';
    int $id:y_elems_name = $exp:y_elems';
    int $id:destoffset_name = $exp:destoffset';
    int $id:srcoffset_name = $exp:srcoffset';
    assert(clSetKernelArg($id:kernel_name, 0, sizeof(cl_mem), &$id:destmem)
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 1, sizeof(int), &$id:destoffset_name)
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 2, sizeof(cl_mem), &$id:srcmem)
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 3, sizeof(int), &$id:srcoffset_name)
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 4, sizeof(int), &$id:x_elems_name)
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 5, sizeof(int), &$id:y_elems_name)
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 6, (FUT_BLOCK_DIM + 1) * FUT_BLOCK_DIM * sizeof($ty:ty), NULL)
           == CL_SUCCESS);
  }|]
  kernel_size <- mapM GenericC.compileExp [x_elems, y_elems, num_arrays]
  let workgroup_size = Just [[C.cexp|FUT_BLOCK_DIM|], [C.cexp|FUT_BLOCK_DIM|], [C.cexp|1|]]
  launchKernel kernel_name kernel_size workgroup_size
  return GenericC.Done
  where kernel_name = kernelName kernel
        ty = GenericC.scalarTypeToCType bt

launchKernel :: C.ToExp a =>
                String -> [a] -> Maybe [a] -> GenericC.CompilerM op s ()
launchKernel kernel_name kernel_dims workgroup_dims = do
  global_work_size <- newVName "global_work_size"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  time_diff <- newVName "time_diff"

  local_work_size_arg <- case workgroup_dims of
    Nothing ->
      return [C.cexp|NULL|]
    Just es -> do
      local_work_size <- newVName "local_work_size"
      let workgroup_dims' = map toInit es
      GenericC.decl [C.cdecl|const size_t $id:local_work_size[$int:kernel_rank] = {$inits:workgroup_dims'};|]
      return [C.cexp|$id:local_work_size|]

  GenericC.stm [C.cstm|{
    const size_t $id:global_work_size[$int:kernel_rank] = {$inits:kernel_dims'};
    struct timeval $id:time_start, $id:time_end, $id:time_diff;
    fprintf(stderr, "kernel size %s: [", $string:(textual global_work_size));
    $stms:(printKernelSize global_work_size)
    gettimeofday(&$id:time_start, NULL);
    fprintf(stderr, "]\n");
    assert(clEnqueueNDRangeKernel(fut_cl_queue, $id:kernel_name, $int:kernel_rank, NULL,
                                  $id:global_work_size, $exp:local_work_size_arg,
                                  0, NULL, NULL)
           == CL_SUCCESS);
    assert(clFinish(fut_cl_queue) == CL_SUCCESS);
    gettimeofday(&$id:time_end, NULL);
    timeval_subtract(&$id:time_diff, &$id:time_end, &$id:time_start);
    $id:kernel_total_runtime += $id:time_diff.tv_sec*1e6+$id:time_diff.tv_usec;
    $id:kernel_runs++;
    fprintf(stderr, "kernel %s runtime: %dus\n",
            $string:kernel_name,
            (int)(($id:time_diff.tv_sec*1e6+$id:time_diff.tv_usec)));
    }|]
  where kernel_total_runtime = kernel_name ++ "_total_runtime"
        kernel_runs = kernel_name ++ "_runs"
        kernel_rank = length kernel_dims
        kernel_dims' = map toInit kernel_dims

        toInit e = [C.cinit|$exp:e|]

        printKernelSize :: VName -> [C.Stm]
        printKernelSize global_work_size =
          intercalate [[C.cstm|fprintf(stderr, ", ");|]] $
          map (printKernelDim global_work_size) [0..kernel_rank-1]
        printKernelDim global_work_size i =
          [[C.cstm|fprintf(stderr, "%zu", $id:global_work_size[$int:i]);|]]

pointerQuals ::  Monad m => String -> m [C.TypeQual]
pointerQuals "global"     = return [C.ctyquals|__global|]
pointerQuals "local"      = return [C.ctyquals|__local|]
pointerQuals "private"    = return [C.ctyquals|__private|]
pointerQuals "constant"   = return [C.ctyquals|__constant|]
pointerQuals "write_only" = return [C.ctyquals|__write_only|]
pointerQuals "read_only"  = return [C.ctyquals|__read_only|]
pointerQuals "kernel"     = return [C.ctyquals|__kernel|]
pointerQuals s            = fail $ "'" ++ s ++ "' is not an OpenCL kernel address space."

type UsedFunctions = [(String,C.Func)] -- The ordering is important!

data OpenClRequirements =
  OpenClRequirements { _kernelUsedFunctions :: UsedFunctions
                     , _kernelPragmas :: [String]
                     }

instance Monoid OpenClRequirements where
  mempty =
    OpenClRequirements [] []

  OpenClRequirements used1 pragmas1 `mappend` OpenClRequirements used2 pragmas2 =
    OpenClRequirements (nubBy cmpFst $ used1 <> used2) (nub $ pragmas1 ++ pragmas2)
    where cmpFst (x, _) (y, _) = x == y

usedFunction :: String -> C.Func -> GenericC.CompilerM op UsedFunctions ()
usedFunction name func = GenericC.modifyUserState insertIfMissing
  where insertIfMissing funcs
          | name `elem` map fst funcs = funcs
          | otherwise                 = funcs ++ [(name, func)]

inKernelOperations :: GenericC.Operations InKernel UsedFunctions
inKernelOperations = GenericC.Operations
                     { GenericC.opsCompiler = const $ return GenericC.Done
                     , GenericC.opsMemoryType = kernelMemoryType
                     , GenericC.opsWriteScalar = GenericC.writeScalarPointerWithQuals pointerQuals
                     , GenericC.opsReadScalar = GenericC.readScalarPointerWithQuals pointerQuals
                     , GenericC.opsAllocate = cannotAllocate
                     , GenericC.opsCopy = copyInKernel
                     }
  where cannotAllocate :: GenericC.Allocate InKernel UsedFunctions
        cannotAllocate _ =
          fail "Cannot allocate memory in kernel"

        copyInKernel :: GenericC.Copy InKernel UsedFunctions
        copyInKernel dest destidx (Space "global") src srcidx (Space "global") nbytes = do
          usedFunction "memcpy" $
            genericMemcpy [C.ctyquals|__global|] [C.ctyquals|__global|]
          usedFunction "memmove" $
            genericMemmove [C.ctyquals|__global|] [C.ctyquals|__global|]
          GenericC.stm [C.cstm|memmove($id:dest + $exp:destidx,
                                       $id:src + $exp:srcidx,
                                       $exp:nbytes);|]
        copyInKernel _ _ destspace _ _ srcspace _ =
          error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

        kernelMemoryType space = do
          quals <- pointerQuals space
          return [C.cty|$tyquals:quals $ty:defaultMemBlockType|]

compileKernels :: [CallKernel] -> Either String ([(String, C.Func)], OpenClRequirements)
compileKernels kernels = do
  (funcs, reqs) <- unzip <$> mapM compileKernel kernels
  return (zip (map kernelName kernels) funcs, mconcat reqs)

compileKernel :: CallKernel -> Either String (C.Func, OpenClRequirements)
compileKernel (Kernel kernel) =
  let (funbody, s) =
        GenericC.runCompilerM (Program []) inKernelOperations blankNameSource mempty $
        GenericC.collect $ GenericC.compileCode $ kernelBody kernel

      used_funs = GenericC.compUserState s

      asParam (ScalarUse name bt) =
        let ctp = GenericC.scalarTypeToCType bt
        in [C.cparam|$ty:ctp $id:name|]
      asParam (MemoryUse name _) =
        [C.cparam|__global unsigned char *$id:name|]

      params = map asParam $ kernelUses kernel

      kernel_funs = functionsCalled $ kernelBody kernel
      used_in_kernel = (`HS.member` kernel_funs) . nameFromString . fst
      funs32_used = filter used_in_kernel funs32
      funs64_used = filter used_in_kernel funs64
      extra_pragmas = if null funs64_used
                      then []
                      else ["#pragma OPENCL EXTENSION cl_khr_fp64 : enable"]

      funs32 = [("toFloat32", c_toFloat32),
                ("trunc32", c_trunc32),
                ("log32", c_log32),
                ("sqrt32", c_sqrt32),
                ("exp32", c_exp32)]

      funs64 = [("toFloat64", c_toFloat64),
                ("trunc64", c_trunc64),
                ("log64", c_log64),
                ("sqrt64", c_sqrt64),
                ("exp64", c_exp64)]

  in Right ([C.cfun|__kernel void $id:(genericKernelName kernel) ($params:params) {
                 const uint $id:(kernelThreadNum kernel) = get_global_id(0);
                 $items:funbody
             }|],
            OpenClRequirements (used_funs ++ funs32_used ++ funs64_used) extra_pragmas)

compileKernel kernel@(MapTranspose bt _ _ _ _ _ _ _) =
  Right ([C.cfun|
  // This kernel is optimized to ensure all global reads and writes are coalesced,
  // and to avoid bank conflicts in shared memory.  The shared memory array is sized
  // to (BLOCK_DIM+1)*BLOCK_DIM.  This pads each row of the 2D block in shared memory
  // so that bank conflicts do not occur when threads address the array column-wise.
  __kernel void $id:(kernelName kernel)(__global $ty:ty *odata,
                                        unsigned int odata_offset,
                                        __global $ty:ty *idata,
                                        unsigned int idata_offset,
                                        unsigned int width,
                                        unsigned int height,
                                        __local $ty:ty* block) {
    unsigned int x_index;
    unsigned int y_index;
    unsigned int our_array_offset;

    // Adjust the input and output arrays with the basic offset.
    odata += odata_offset;
    idata += idata_offset;

    // Adjust the input and output arrays for the third dimension.
    our_array_offset = get_global_id(2) * width * height;
    odata += our_array_offset;
    idata += our_array_offset;

    // read the matrix tile into shared memory
    x_index = get_global_id(0);
    y_index = get_global_id(1);

    if((x_index < width) && (y_index < height))
    {
        unsigned int index_in = y_index * width + x_index;
        block[get_local_id(1)*(FUT_BLOCK_DIM+1)+get_local_id(0)] = idata[index_in];
    }

    barrier(CLK_LOCAL_MEM_FENCE);

    // Write the transposed matrix tile to global memory.
    x_index = get_group_id(1) * FUT_BLOCK_DIM + get_local_id(0);
    y_index = get_group_id(0) * FUT_BLOCK_DIM + get_local_id(1);
    if((x_index < height) && (y_index < width))
    {
        unsigned int index_out = y_index * height + x_index;
        odata[index_out] = block[get_local_id(0)*(FUT_BLOCK_DIM+1)+get_local_id(1)];
    }
  }|],
         OpenClRequirements [] [blockDimPragma])
  where ty = GenericC.scalarTypeToCType bt

openClProgramHeader :: OpenClRequirements -> [C.Definition]
openClProgramHeader (OpenClRequirements used_funs pragmas) =
  [ [C.cedecl|$esc:pragma|] | pragma <- pragmas ] ++
  [ [C.cedecl|$func:used_fun|] | (_, used_fun) <- used_funs ]

openClProgram :: [(String, C.Func)] -> OpenClRequirements -> [C.Definition]
openClProgram kernels requirements =
  [C.cunit|
// Program header and utility functions
   $edecls:header

// Kernel definitions
   $edecls:funcs
          |]
  where header =
          openClProgramHeader requirements
        funcs =
          [[C.cedecl|$func:kernel_func|] |
           (_, kernel_func) <- kernels ]

openClDecls :: [(String, C.Func)] -> OpenClRequirements -> [C.Definition]
openClDecls kernels requirements =
  clGlobals ++ kernelDeclarations ++ [buildKernelFunction, setupFunction]
  where clGlobals =
          [ [C.cedecl|typename cl_context fut_cl_context;|]
          , [C.cedecl|typename cl_command_queue fut_cl_queue;|]
          , [C.cedecl|$esc:("static const char fut_opencl_src[] = FUT_KERNEL(\n"++
                              pretty (openClProgram kernels requirements) ++
                              ");")|]
          ]

        kernelDeclarations =
          concat
          [ [ [C.cedecl|static typename cl_kernel $id:name;|]
            , [C.cedecl|static typename suseconds_t $id:(name ++ "_total_runtime") = 0;|]
            , [C.cedecl|static int $id:(name ++ "_runs") = 0;|]
            ]
          | (name, _) <- kernels ]

        setupFunction = [C.cedecl|
void setup_opencl() {

  typename cl_int error;
  typename cl_platform_id platform;
  typename cl_device_id device;
  typename cl_uint platforms, devices;
  // Fetch the Platform and Device IDs; we only want one.
  error = clGetPlatformIDs(1, &platform, &platforms);
  assert(error == 0);
  assert(platforms > 0);
  error = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 1, &device, &devices);
  assert(error == 0);
  assert(devices > 0);
  typename cl_context_properties properties[] = {
    CL_CONTEXT_PLATFORM,
    (typename cl_context_properties)platform,
    0
  };
  // Note that nVidia's OpenCL requires the platform property
  fut_cl_context = clCreateContext(properties, 1, &device, NULL, NULL, &error);
  fut_cl_queue = clCreateCommandQueue(fut_cl_context, device, 0, &error);

  // Some drivers complain if we compile empty programs, so bail out early if so.
  if (strlen(fut_opencl_src) == 0) return;

  // Build the OpenCL program.
  size_t src_size;
  typename cl_program prog;
  fprintf(stderr, "look at me, loading this program:\n%s\n", fut_opencl_src);
  error = 0;
  src_size = sizeof(fut_opencl_src);
  const char* src_ptr[] = {fut_opencl_src};
  prog = clCreateProgramWithSource(fut_cl_context, 1, src_ptr, &src_size, &error);
  assert(error == 0);
  char compile_opts[1024];
  snprintf(compile_opts, sizeof(compile_opts), "-DFUT_BLOCK_DIM=%d", FUT_BLOCK_DIM);
  assert(build_opencl_program(prog, device, compile_opts) == CL_SUCCESS);

  // Load all the kernels.
  $stms:(map (loadKernelByName . fst) kernels)
}
    |]

        buildKernelFunction = [C.cedecl|
typename cl_build_status build_opencl_program(typename cl_program program, typename cl_device_id device, const char* options) {
  typename cl_int ret_val = clBuildProgram(program, 1, &device, options, NULL, NULL);

  // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
  if (ret_val != CL_SUCCESS && ret_val != CL_BUILD_PROGRAM_FAILURE) {
    assert(ret_val == 0);
  }

  typename cl_build_status build_status;
  ret_val = clGetProgramBuildInfo(program,
                                  device,
                                  CL_PROGRAM_BUILD_STATUS,
                                  sizeof(cl_build_status),
                                  &build_status,
                                  NULL);
  assert(ret_val == 0);

  if (build_status != CL_SUCCESS) {
    char *build_log;
    size_t ret_val_size;
    ret_val = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &ret_val_size);
    assert(ret_val == 0);

    build_log = malloc(ret_val_size+1);
    clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, ret_val_size, build_log, NULL);
    assert(ret_val == 0);

    // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
    build_log[ret_val_size] = '\0';

    fprintf(stderr, "Build log:\n%s", build_log);

    free(build_log);
  }

  return build_status;
}
|]

loadKernelByName :: String -> C.Stm
loadKernelByName name = [C.cstm|{
  $id:name = clCreateKernel(prog, $string:name, &error);
  assert(error == 0);
  fprintf(stderr, "Created kernel %s.\n", $string:name);
  }|]

openClInit :: [C.Stm]
openClInit =
  [[C.cstm|setup_opencl();|]]

openClReport :: [String] -> [C.Stm]
openClReport = map reportKernel
  where reportKernel name =
          let runs = name ++ "_runs"
              total_runtime = name ++ "_total_runtime"
          in [C.cstm|
               fprintf(stderr,
                       "Kernel %s executed %d times, with average runtime:\t %6dus\n",
                       $string:name,
                       $id:runs,
                       $id:total_runtime / ($id:runs != 0 ? $id:runs : 1));
             |]


kernelId :: GenericKernel -> Int
kernelId = baseTag . kernelThreadNum

genericKernelName :: GenericKernel -> String
genericKernelName = ("kernel_"++) . show . kernelId

kernelName :: CallKernel -> String
kernelName (Kernel k) =
  genericKernelName k
kernelName (MapTranspose bt _ _ _ _ _ _ _) =
  "fut_kernel_map_transpose_" ++ pretty bt

getKernels :: Program -> [CallKernel]
getKernels = nubBy sameKernel . execWriter . traverse getFunKernels
  where getFunKernels kernel =
          tell [kernel] >> return kernel
        sameKernel (MapTranspose bt1 _ _ _ _ _ _ _) (MapTranspose bt2 _ _ _ _ _ _ _) =
          bt1 == bt2
        sameKernel _ _ = False

blockDim :: Int
blockDim = 16

blockDimPragma :: String
blockDimPragma = "#define FUT_BLOCK_DIM " ++ show blockDim

-- | @memcpy@ parametrised by the address space of the destination and
-- source pointers.
genericMemcpy :: [C.TypeQual] -> [C.TypeQual] -> C.Func
genericMemcpy dest_quals src_quals =
  [C.cfun|
  // From musl.
  $tyquals:dest_quals *memcpy($tyquals:dest_quals void *restrict dest,
                              $tyquals:src_quals const void *restrict src,
                              size_t n)
  {
    $tyquals:dest_quals unsigned char *d = dest;
    $tyquals:src_quals const unsigned char *s = src;
    // These were #defines in the musl source code.
    size_t SS = sizeof(size_t);
    size_t ALIGN = sizeof(size_t)-1;
    size_t ONES=((size_t)-1/UCHAR_MAX);
    if (((typename uintptr_t)d & ALIGN) != ((typename uintptr_t)s & ALIGN)) {
      goto misaligned;
    }
    for (; ((typename uintptr_t)d & ALIGN) && n; n--) {
      *d++ = *s++;
    }
    if (n) {
      $tyquals:dest_quals size_t *wd = ($tyquals:dest_quals void *)d;
      $tyquals:src_quals const size_t *ws = ($tyquals:src_quals const void *)s;
      for (; n>=SS; n-=SS) *wd++ = *ws++;
      d = ($tyquals:dest_quals void *)wd;
      s = ($tyquals:dest_quals const void *)ws;

      misaligned:
      for (; n; n--) {
        *d++ = *s++;
      }
    }
    return dest;
  }
   |]

-- | @memmove@ parametrised by the address space of the destination and
-- source pointers.
genericMemmove :: [C.TypeQual] -> [C.TypeQual] -> C.Func
genericMemmove dest_quals src_quals =
  [C.cfun|
  // From musl.
  $tyquals:dest_quals *memmove($tyquals:dest_quals void *dest,
                               $tyquals:src_quals const void *src,
                               size_t n)
  {
    $tyquals:dest_quals char *d = dest;
    $tyquals:src_quals char *s = src;
    size_t WS = sizeof(size_t);
    if (d==s) { return d; }
    if (s+n <= d || d+n <= s) { return memcpy(d, s, n); }
    if (d<s) {
      if ((typename uintptr_t)s % WS == (typename uintptr_t)d % WS) {
        while ((typename uintptr_t)d % WS) {
        if (!n--) { return dest; }
          *d++ = *s++;
        }
        for (; n>=WS; n-=WS, d+=WS, s+=WS) {
          *($tyquals:dest_quals size_t *)d = *($tyquals:src_quals size_t *)s;
        }
      }
      for (; n; n--) { *d++ = *s++; }
    } else {
      if ((typename uintptr_t)s % WS == (typename uintptr_t)d % WS) {
        while ((typename uintptr_t)(d+n) % WS) {
          if (!n--) { return dest; }
          d[n] = s[n];
        }
        while (n>=WS) {
          n-=WS;
          *($tyquals:dest_quals size_t *)(d+n) = *($tyquals:src_quals size_t *)(s+n);
        }
      }
      while (n) { n--; d[n] = s[n]; }
    }
    return dest;
  }
   |]
