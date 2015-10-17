{-# LANGUAGE QuasiQuotes #-}
module Futhark.CodeGen.Backends.COpenCL.Boilerplate
  ( openClDecls
  , openClInit
  , openClReport
  ) where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C
import Futhark.Util.Pretty

openClDecls :: [String] -> [C.Definition] -> [C.Definition]
openClDecls kernel_names opencl_program =
  kernelDeclarations ++ openclBoilerplate
  where kernelDeclarations =
          [C.cedecl|$esc:("static const char fut_opencl_src[] = FUT_KERNEL(\n"++
                         pretty opencl_program ++
                         ");")|] :
          concat
          [ [ [C.cedecl|static typename cl_kernel $id:name;|]
            , [C.cedecl|static typename suseconds_t $id:(name ++ "_total_runtime") = 0;|]
            , [C.cedecl|static int $id:(name ++ "_runs") = 0;|]
            ]
          | name <- kernel_names ]

        openclBoilerplate = [C.cunit|
typename cl_context fut_cl_context;
typename cl_command_queue fut_cl_queue;
int cl_verbosity = 1;

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

typename cl_platform_id get_opencl_platform() {
  typename cl_platform_id platform;
  typename cl_uint platforms;
  // Fetch the Platform and Device IDs; we only want one.
  OPENCL_SUCCEED(clGetPlatformIDs(1, &platform, &platforms));
  assert(platforms > 0);
  return platform;
}

typename cl_device_id get_opencl_device(typename cl_platform_id platform) {
  typename cl_device_id device;
  typename cl_uint devices;
  typename devices;
  OPENCL_SUCCEED(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 1, &device, &devices));
  assert(devices > 0);
  return device;
}

char* opencl_platform_info(typename cl_platform_id platform,
                           typename cl_platform_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED(clGetPlatformInfo(platform, param, 0, NULL, &req_bytes));

  info = malloc(req_bytes);

  OPENCL_SUCCEED(clGetPlatformInfo(platform, param, req_bytes, info, NULL));

  return info;
}

void describe_opencl_platform(typename cl_platform_id platform) {
  char* platform_name = opencl_platform_info(platform, CL_PLATFORM_NAME);
  char* platform_vendor = opencl_platform_info(platform, CL_PLATFORM_VENDOR);
  fprintf(stderr, "Using platform: %s (by %s)\n", platform_name, platform_vendor);
  free(platform_name);
  free(platform_vendor);
}

char* opencl_device_info(typename cl_device_id device,
                         typename cl_device_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED(clGetDeviceInfo(device, param, 0, NULL, &req_bytes));

  info = malloc(req_bytes);

  OPENCL_SUCCEED(clGetDeviceInfo(device, param, req_bytes, info, NULL));

  return info;
}

void describe_opencl_device(typename cl_device_id device) {
  char* device_name = opencl_device_info(device, CL_DEVICE_NAME);
  fprintf(stderr, "Using device: %s\n", device_name);
  free(device_name);
}

void setup_opencl() {

  typename cl_int error;
  typename cl_platform_id platform;
  typename cl_device_id device;
  typename cl_uint platforms, devices;

  platform = get_opencl_platform();
  if (cl_verbosity > 0) {
    describe_opencl_platform(platform);
  }

  device = get_opencl_device(platform);
  if (cl_verbosity > 0) {
    describe_opencl_device(device);
  }

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
  error = 0;
  src_size = sizeof(fut_opencl_src);
  const char* src_ptr[] = {fut_opencl_src};
  prog = clCreateProgramWithSource(fut_cl_context, 1, src_ptr, &src_size, &error);
  assert(error == 0);
  char compile_opts[1024];
  snprintf(compile_opts, sizeof(compile_opts), "-DFUT_BLOCK_DIM=%d -DWAVE_SIZE=32", FUT_BLOCK_DIM);
  OPENCL_SUCCEED(build_opencl_program(prog, device, compile_opts));

  // Load all the kernels.
  $stms:(map (loadKernelByName) kernel_names)
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
