{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.OpenCL
  ( compileProg
  ) where

import Control.Monad
import Control.Monad.Writer



import Prelude

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.Representation.ExplicitMemory (Prog, pretty)
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.KernelImp
import qualified Futhark.CodeGen.KernelImpGen as KernelImpGen


compileProg :: Prog -> Either String String
compileProg prog = do
  prog' <- KernelImpGen.compileProg prog
  kernels <- forM (getKernels prog') $ \kernel -> do
    kernel' <- compileKernel kernel
    return (kernelName kernel, [C.cinit|$string:(pretty kernel')|])
  return $
    "#include <CL/cl.h>\n" ++
    GenericC.compileProg callKernels (openClDecls kernels) openClInit prog'

callKernels :: GenericC.OpCompiler Kernel
callKernels kernel = do
  _ <- fail $ "Pretend that I call " ++ kernelName kernel ++ " here"
  return GenericC.Done

compileKernel :: Kernel -> Either String C.Func
compileKernel = undefined

openClDecls :: [(String, C.Initializer)] -> [C.Definition]
openClDecls kernels =
  clGlobals ++ kernelDeclarations ++ [setupFunction]
  where clGlobals =
          [ [C.cedecl|typename cl_context fut_cl_context;|]
          , [C.cedecl|typename cl_command_queue fut_cl_queue;|]
          ]

        kernelDeclarations =
          [ [C.cedecl|typename cl_kernel $id:name = $init:kernel;|]
          | (name, kernel) <- kernels ]

        setupFunction = [C.cedecl|
void setup_opencl() {
  typename cl_int error;
  typename cl_platform_id platform;
  typename cl_device_id device;
  typename cl_uint platforms, devices;
  // Fetch the Platform and Device IDs; we only want one.
  error = clGetPlatformIDs(1, &platform, &platforms);
  error = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 1, &device, &devices);
  typename cl_context_properties properties[] = {
    CL_CONTEXT_PLATFORM,
    (typename cl_context_properties)platform,
    0
  };
  // Note that nVidia's OpenCL requires the platform property
  fut_cl_context = clCreateContext(properties, 1, &device, NULL, NULL, &error);
  fut_cl_queue = clCreateCommandQueue(fut_cl_context, device, 0, &error);
}
    |]

openClInit :: [C.Stm]
openClInit =
  [[C.cstm|setup_opencl_trivially();|]]

kernelId :: Kernel -> Int
kernelId = baseTag . kernelThreadNum

kernelName :: Kernel -> String
kernelName = ("kernel_"++) . show . kernelId

getKernels :: Program -> [Kernel]
getKernels = execWriter . traverse getFunKernels
  where getFunKernels kernel =
          tell [kernel] >> return kernel
