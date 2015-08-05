{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.OpenCL
  ( compileProg
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Traversable hiding (forM)
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
  let header = unlines [ "#include <CL/cl.h>\n" ]
  kernels <- forM (getKernels prog') $ \kernel -> do
    (kernel', used_functions) <- compileKernel kernel
    return (kernelName kernel,
            [C.cinit|$string:(openClKernelHeader used_functions kernel ++
                              "\n" ++
                              pretty kernel')|])
  return $
    header ++
    GenericC.compileProg operations () (openClDecls kernels) openClInit prog'
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
callKernel kernel = do
  zipWithM_ mkBuffer [(0::Int)..] $ kernelUses kernel
  global_work_size <- newVName "global_work_size"
  let kernel_size = GenericC.dimSizeToExp $ kernelSize kernel

  GenericC.stm [C.cstm|{
    size_t $id:global_work_size = $exp:kernel_size;
    assert(clEnqueueNDRangeKernel(fut_cl_queue, $id:kernel_name, 1, NULL,
                                  &$id:global_work_size, NULL,
                                  0, NULL, NULL)
           == CL_SUCCESS);
    }|]

  GenericC.stm [C.cstm|assert(clFinish(fut_cl_queue) == CL_SUCCESS);|]
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

        kernel_name = kernelName kernel

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

compileKernel :: CallKernel -> Either String (C.Func, UsedFunctions)
compileKernel kernel =
  let (funbody,s) =
        GenericC.runCompilerM (Program []) inKernelOperations blankNameSource mempty $
        GenericC.collect $ GenericC.compileCode $ kernelBody kernel

      asParam (ScalarUse name bt) =
        let ctp = GenericC.scalarTypeToCType bt
        in [C.cparam|$ty:ctp $id:name|]
      asParam (MemoryUse name _) =
        [C.cparam|__global unsigned char *$id:name|]

      params = map asParam $ kernelUses kernel
  in Right ([C.cfun|__kernel void $id:(kernelName kernel) ($params:params) {
                 const uint $id:(kernelThreadNum kernel) = get_global_id(0);
                 $items:funbody
             }|],
            GenericC.compUserState s)

openClDecls :: [(String, C.Initializer)] -> [C.Definition]
openClDecls kernels =
  clGlobals ++ kernelSourceDeclarations ++ kernelDeclarations ++ [buildKernelFunction, setupFunction]
  where clGlobals =
          [ [C.cedecl|typename cl_context fut_cl_context;|]
          , [C.cedecl|typename cl_command_queue fut_cl_queue;|]
          ]

        kernelSourceDeclarations =
          [ [C.cedecl|const char $id:(name++"_src")[] = $init:kernel;|]
          | (name, kernel) <- kernels ]

        kernelDeclarations =
          [ [C.cedecl|typename cl_kernel $id:name;|]
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
  // Load all the kernels.
  $stms:(map (loadKernelByName . fst) kernels)
}
    |]

        buildKernelFunction = [C.cedecl|
typename cl_build_status build_opencl_kernel(typename cl_program program, typename cl_device_id device, const char* options) {
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
  size_t src_size;
  typename cl_program prog;
  fprintf(stderr, "look at me, loading this kernel:\n%s\n", $id:srcname);
  error = 0;
  src_size = sizeof($id:srcname);
  const char* src_ptr[] = {$id:srcname};
  prog = clCreateProgramWithSource(fut_cl_context, 1, src_ptr, &src_size, &error);
  assert(error == 0);
  assert(build_opencl_kernel(prog, device, "") == CL_SUCCESS);
  $id:name = clCreateKernel(prog, $string:name, &error);
  assert(error == 0);
  fprintf(stderr, "I guess it worked.\n");
  }|]
  where srcname = name ++ "_src"

openClInit :: [C.Stm]
openClInit =
  [[C.cstm|setup_opencl();|]]

kernelId :: CallKernel -> Int
kernelId = baseTag . kernelThreadNum

kernelName :: CallKernel -> String
kernelName = ("kernel_"++) . show . kernelId

getKernels :: Program -> [CallKernel]
getKernels = execWriter . traverse getFunKernels
  where getFunKernels kernel =
          tell [kernel] >> return kernel

openClKernelHeader :: UsedFunctions -> CallKernel -> String
openClKernelHeader used_functions kernel =
  unlines $
  pragmas ++ map pretty (map snd used_functions ++ funs32_used ++ funs64_used)
  where kernel_funs = functionsCalled $ kernelBody kernel
        used_in_kernel = (`HS.member` kernel_funs) . nameFromString . fst
        funs32_used = map snd $ filter used_in_kernel funs32
        funs64_used = map snd $ filter used_in_kernel funs64
        pragmas = if null funs64_used
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
    size_t SS = sizeof(size_t), ALIGN = sizeof(size_t)-1, ONES=((size_t)-1/UCHAR_MAX);
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
      while (n) { n--, d[n] = s[n]; }
    }
    return dest;
  }
   |]
