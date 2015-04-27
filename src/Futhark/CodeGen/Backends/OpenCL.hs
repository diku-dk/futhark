{-# LANGUAGE QuasiQuotes #-}
module Futhark.CodeGen.Backends.OpenCL
  ( compileProg
  ) where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.Representation.ExplicitMemory

import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.KernelImp
import qualified Futhark.CodeGen.KernelImpGen as KernelImpGen
import Futhark.MonadFreshNames

compileProg :: Prog -> Either String String
compileProg = fmap (("#include <CL/cl.h>\n" ++) .
                    GenericC.compileProg kernelCompiler openClDecls openClInit) .
              KernelImpGen.compileProg

kernelCompiler :: GenericC.OpCompiler Kernel
kernelCompiler kernel = do
  return GenericC.Done

openClDecls :: [C.Definition]
openClDecls = [
    [C.cedecl|typename cl_context fut_cl_context;|]

  , [C.cedecl|typename cl_command_queue fut_cl_queue;|]

  , [C.cedecl|
void setup_opencl_trivially() {
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
  ]

openClInit :: [C.Stm]
openClInit =
  [[C.cstm|setup_opencl_trivially();|]]
