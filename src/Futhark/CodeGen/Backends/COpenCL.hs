{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.COpenCL
  ( compileProg
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Traversable hiding (forM, mapM, sequence)
import qualified Data.HashSet as HS
import Data.List

import Prelude

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import qualified Futhark.CodeGen.OpenCL.Kernels as Kernels
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
                       , "#include <err.h>\n"
                       , "#define FUT_KERNEL(s) #s"
                       , "#define OPENCL_SUCCEED(e) opencl_succeed(e, #e, __FILE__, __LINE__)"
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
openclMemoryType "local" = pure [C.cty|unsigned char|] -- dummy type
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

        kernel_name = mapKernelName kernel

callKernel (Reduce kernel) = do
  zipWithM_ mkLocalMemory [(0::Int)..] $ reductionThreadLocalMemory kernel
  zipWithM_ mkBuffer [num_local_mems..] $ reductionUses kernel

  launchKernel kernel_name [kernel_size] (Just [workgroup_size])
  return GenericC.Done
  where num_local_mems = length $ reductionThreadLocalMemory kernel
        kernel_name = reduceKernelName kernel
        workgroup_size = GenericC.dimSizeToExp $ reductionGroupSize kernel
        num_workgroups = GenericC.dimSizeToExp $ reductionNumGroups kernel
        kernel_size = [C.cexp|$exp:workgroup_size * $exp:num_workgroups|]

        mkLocalMemory i (_, num_bytes) =
          GenericC.stm [C.cstm|{
                            assert(clSetKernelArg($id:kernel_name, $int:i,
                                                  $exp:num_bytes',
                                                  NULL)
                                   == CL_SUCCESS);
                        }|]
          where num_bytes' = GenericC.dimSizeToExp num_bytes

        mkBuffer i (MemoryUse mem _) =
          GenericC.stm [C.cstm|{
            assert(clSetKernelArg($id:kernel_name, $int:i, sizeof($id:mem), &$id:mem)
                   == CL_SUCCESS);
            }|]

        mkBuffer i (ScalarUse hostvar _) =
          GenericC.stm [C.cstm|{
            assert(clSetKernelArg($id:kernel_name, $int:i, sizeof($id:hostvar), &$id:hostvar)
                   == CL_SUCCESS);
          }|]

callKernel kernel@(MapTranspose bt destmem destoffset srcmem srcoffset num_arrays x_elems y_elems) = do
  destoffset' <- GenericC.compileExpToName "destoffset" Int destoffset
  srcoffset' <- GenericC.compileExpToName "srcoffset" Int  srcoffset
  x_elems' <- GenericC.compileExpToName "x_elems" Int x_elems
  y_elems' <- GenericC.compileExpToName "y_elems" Int y_elems
  GenericC.stm [C.cstm|{
    assert(clSetKernelArg($id:kernel_name, 0, sizeof(cl_mem), &$id:destmem)
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 1, sizeof(int), &$id:destoffset')
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 2, sizeof(cl_mem), &$id:srcmem)
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 3, sizeof(int), &$id:srcoffset')
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 4, sizeof(int), &$id:x_elems')
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 5, sizeof(int), &$id:y_elems')
           == CL_SUCCESS);
    assert(clSetKernelArg($id:kernel_name, 6, (FUT_BLOCK_DIM + 1) * FUT_BLOCK_DIM * sizeof($ty:ty), NULL)
           == CL_SUCCESS);
  }|]
  kernel_size <- sequence [roundedToBlockSize <$> GenericC.compileExp x_elems,
                           roundedToBlockSize <$> GenericC.compileExp y_elems,
                           GenericC.compileExp num_arrays]
  let workgroup_size = Just [[C.cexp|FUT_BLOCK_DIM|], [C.cexp|FUT_BLOCK_DIM|], [C.cexp|1|]]
  launchKernel kernel_name kernel_size workgroup_size
  return GenericC.Done
  where kernel_name = kernelName kernel
        ty = GenericC.scalarTypeToCType bt
        roundedToBlockSize e =
          [C.cexp|$exp:e +
                   (FUT_BLOCK_DIM - $exp:e % FUT_BLOCK_DIM) % FUT_BLOCK_DIM
           |]

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
    if ($exp:total_elements != 0) {
      const size_t $id:global_work_size[$int:kernel_rank] = {$inits:kernel_dims'};
      struct timeval $id:time_start, $id:time_end, $id:time_diff;
      fprintf(stderr, "kernel size %s: [", $string:(textual global_work_size));
      $stms:(printKernelSize global_work_size)
      fprintf(stderr, "]\n");
      gettimeofday(&$id:time_start, NULL);
      OPENCL_SUCCEED(
        clEnqueueNDRangeKernel(fut_cl_queue, $id:kernel_name, $int:kernel_rank, NULL,
                               $id:global_work_size, $exp:local_work_size_arg,
                               0, NULL, NULL));
      OPENCL_SUCCEED(clFinish(fut_cl_queue));
      gettimeofday(&$id:time_end, NULL);
      timeval_subtract(&$id:time_diff, &$id:time_end, &$id:time_start);
      $id:kernel_total_runtime += $id:time_diff.tv_sec*1e6+$id:time_diff.tv_usec;
      $id:kernel_runs++;
      fprintf(stderr, "kernel %s runtime: %dus\n",
              $string:kernel_name,
              (int)(($id:time_diff.tv_sec*1e6+$id:time_diff.tv_usec)));
    }
    }|]
  where kernel_total_runtime = kernel_name ++ "_total_runtime"
        kernel_runs = kernel_name ++ "_runs"
        kernel_rank = length kernel_dims
        kernel_dims' = map toInit kernel_dims
        total_elements = foldl multExp [C.cexp|1|] kernel_dims

        toInit e = [C.cinit|$exp:e|]
        multExp x y = [C.cexp|$exp:x * $exp:y|]

        printKernelSize :: VName -> [C.Stm]
        printKernelSize global_work_size =
          intercalate [[C.cstm|fprintf(stderr, ", ");|]] $
          map (printKernelDim global_work_size) [0..kernel_rank-1]
        printKernelDim global_work_size i =
          [[C.cstm|fprintf(stderr, "%zu", $id:global_work_size[$int:i]);|]]

pointerQuals ::  Monad m => String -> m [C.TypeQual]
pointerQuals "global"     = return [C.ctyquals|__global|]
pointerQuals "local"      = return [C.ctyquals|__local volatile|]
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

inKernelOperations :: GenericC.Operations InKernel UsedFunctions
inKernelOperations = GenericC.Operations
                     { GenericC.opsCompiler = kernelOps
                     , GenericC.opsMemoryType = kernelMemoryType
                     , GenericC.opsWriteScalar = GenericC.writeScalarPointerWithQuals pointerQuals
                     , GenericC.opsReadScalar = GenericC.readScalarPointerWithQuals pointerQuals
                     , GenericC.opsAllocate = cannotAllocate
                     , GenericC.opsCopy = copyInKernel
                     }
  where kernelOps :: GenericC.OpCompiler InKernel UsedFunctions
        kernelOps (GetGroupId v i) = do
          GenericC.stm [C.cstm|$id:v = get_group_id($int:i);|]
          return GenericC.Done
        kernelOps (GetLocalId v i) = do
          GenericC.stm [C.cstm|$id:v = get_local_id($int:i);|]
          return GenericC.Done
        kernelOps (GetLocalSize v i) = do
          GenericC.stm [C.cstm|$id:v = get_local_size($int:i);|]
          return GenericC.Done
        kernelOps (GetGlobalId v i) = do
          GenericC.stm [C.cstm|$id:v = get_global_id($int:i);|]
          return GenericC.Done
        kernelOps (GetGlobalSize v i) = do
          GenericC.stm [C.cstm|$id:v = get_global_size($int:i);|]
          return GenericC.Done

        cannotAllocate :: GenericC.Allocate InKernel UsedFunctions
        cannotAllocate _ =
          fail "Cannot allocate memory in kernel"

        copyInKernel :: GenericC.Copy InKernel UsedFunctions
        copyInKernel _ _ _ _ _ _ _ =
          fail $ "Cannot bulk copy in kernel."

        kernelMemoryType space = do
          quals <- pointerQuals space
          return [C.cty|$tyquals:quals $ty:defaultMemBlockType|]

compileKernels :: [CallKernel] -> Either String ([(String, C.Func)], OpenClRequirements)
compileKernels kernels = do
  (funcs, reqs) <- unzip <$> mapM compileKernel kernels
  return (concat funcs, mconcat reqs)

compileKernel :: CallKernel -> Either String ([(String, C.Func)], OpenClRequirements)
compileKernel (Kernel kernel) =
  let (funbody, s) =
        GenericC.runCompilerM (Program []) inKernelOperations blankNameSource mempty $
        GenericC.collect $ GenericC.compileCode $ kernelBody kernel

      used_funs = GenericC.compUserState s

      params = map useAsParam $ kernelUses kernel

      kernel_funs = functionsCalled $ kernelBody kernel

  in Right ([(mapKernelName kernel,
             [C.cfun|__kernel void $id:(mapKernelName kernel) ($params:params) {
                 const uint $id:(kernelThreadNum kernel) = get_global_id(0);
                 $items:funbody
             }|])],
            OpenClRequirements (used_funs ++ requiredFunctions kernel_funs) [])

compileKernel (Reduce kernel) =
  let ((kernel_prologue, fold_body, red_body,
        write_fold_result, write_final_result), s) =
        GenericC.runCompilerM (Program []) inKernelOperations blankNameSource mempty $ do
          kernel_prologue_ <-
            GenericC.collect $ GenericC.compileCode $ reductionPrologue kernel
          fold_body_ <-
            GenericC.collect $ GenericC.compileCode $ reductionFoldOperation kernel
          red_body_ <-
            GenericC.collect $ GenericC.compileCode $ reductionReduceOperation kernel
          write_fold_result_ <-
            GenericC.collect $ GenericC.compileCode $ reductionWriteFoldResult kernel

          write_final_result_ <-
            GenericC.collect $ GenericC.compileCode $ reductionWriteFinalResult kernel

          return (kernel_prologue_, fold_body_, red_body_,
                  write_fold_result_, write_final_result_)

      used_funs = GenericC.compUserState s

      use_params = map useAsParam $ reductionUses kernel

      kernel_funs = functionsCalled (reductionReduceOperation kernel) <>
                    functionsCalled (reductionFoldOperation kernel)

      local_memory_params =
        flip evalState (blankNameSource :: VNameSource) $
        mapM prepareLocalMemory $ reductionThreadLocalMemory kernel

      prologue = kernel_prologue

      opencl_kernel =
        Kernels.reduce Kernels.Reduction
         { Kernels.reductionKernelName =
            reduceKernelName kernel
         , Kernels.reductionOffsetName =
             textual $ reductionOffsetName kernel
         , Kernels.reductionInputArrayIndexName =
             textual $ reductionKernelName kernel

         , Kernels.reductionPrologue = prologue
         , Kernels.reductionFoldOperation = fold_body
         , Kernels.reductionWriteFoldResult = write_fold_result
         , Kernels.reductionReduceOperation = red_body
         , Kernels.reductionWriteFinalResult = write_final_result

         , Kernels.reductionKernelArgs =
             local_memory_params ++ use_params
         }

  in Right ([(reduceKernelName kernel, opencl_kernel)],
            OpenClRequirements (used_funs ++ requiredFunctions kernel_funs) [])
  where prepareLocalMemory (mem, _) =
          return ([C.cparam|__local volatile unsigned char* restrict $id:mem|])

compileKernel kernel@(MapTranspose bt _ _ _ _ _ _ _) =
  Right ([(kernelName kernel, Kernels.mapTranspose (kernelName kernel) ty)],
         mempty)
  where ty = GenericC.scalarTypeToCType bt

useAsParam :: KernelUse -> C.Param
useAsParam (ScalarUse name bt) =
  let ctp = GenericC.scalarTypeToCType bt
  in [C.cparam|$ty:ctp $id:name|]
useAsParam (MemoryUse name _) =
  [C.cparam|__global unsigned char *$id:name|]

requiredFunctions :: HS.HashSet Name -> [(String, C.Func)]
requiredFunctions kernel_funs =
  let used_in_kernel = (`HS.member` kernel_funs) . nameFromString . fst
      funs32_used = filter used_in_kernel funs32
      funs64_used = filter used_in_kernel funs64

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
  in funs32_used ++ funs64_used

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
  clGlobals ++ kernelDeclarations ++ openclFunctions
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

        openclFunctions = [C.cunit|
const char* opencl_error_string(unsigned int err)
{
    switch (err) {
        case CL_SUCCESS:                            return "Success!";
        case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
        case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
        case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
        case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
        case CL_OUT_OF_RESOURCES:                   return "Out of resources";
        case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
        case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
        case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
        case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
        case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
        case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
        case CL_MAP_FAILURE:                        return "Map failure";
        case CL_INVALID_VALUE:                      return "Invalid value";
        case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
        case CL_INVALID_PLATFORM:                   return "Invalid platform";
        case CL_INVALID_DEVICE:                     return "Invalid device";
        case CL_INVALID_CONTEXT:                    return "Invalid context";
        case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
        case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
        case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
        case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
        case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
        case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
        case CL_INVALID_SAMPLER:                    return "Invalid sampler";
        case CL_INVALID_BINARY:                     return "Invalid binary";
        case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
        case CL_INVALID_PROGRAM:                    return "Invalid program";
        case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
        case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
        case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
        case CL_INVALID_KERNEL:                     return "Invalid kernel";
        case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
        case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
        case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
        case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
        case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
        case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
        case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
        case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
        case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
        case CL_INVALID_EVENT:                      return "Invalid event";
        case CL_INVALID_OPERATION:                  return "Invalid operation";
        case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
        case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
        case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
        default:                                    return "Unknown";
    }
}

void opencl_succeed(unsigned int ret,
                    const char *call,
                    const char *file,
                    int line) {
  if (ret != CL_SUCCESS) {
    errx(-1, "%s.%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
        file, line, call, ret, opencl_error_string(ret));
  }
}

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
  assert(error == 0);

  fut_cl_queue = clCreateCommandQueue(fut_cl_context, device, 0, &error);
  assert(error == 0);

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
  snprintf(compile_opts, sizeof(compile_opts), "-DFUT_BLOCK_DIM=%d -DWAVE_SIZE=32", FUT_BLOCK_DIM);
  OPENCL_SUCCEED(build_opencl_program(prog, device, compile_opts));

  // Load all the kernels.
  $stms:(map (loadKernelByName . fst) kernels)
}

size_t futhark_num_groups() {
  return 128; /* Must be a power of two */
}

size_t futhark_group_size() {
  return 512;
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


mapKernelName :: MapKernel -> String
mapKernelName = ("map_kernel_"++) . show . baseTag . kernelThreadNum

reduceKernelName :: ReduceKernel -> String
reduceKernelName = ("red_kernel_"++) . show . baseTag . reductionKernelName

kernelName :: CallKernel -> String
kernelName (Kernel k) =
  mapKernelName k
kernelName (Reduce k) =
  reduceKernelName k
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
