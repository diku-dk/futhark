{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.COpenCL.Boilerplate
  ( openClDecls
  , openClInit
  , openClReport

  , kernelRuntime
  , kernelRuns
  ) where

import Data.FileEmbed
import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.CodeGen.OpenCL.Kernels
import Futhark.Util (chunk)

openClDecls :: Int -> [String] -> String -> String
            -> [C.Definition]
openClDecls block_dim kernel_names opencl_program opencl_prelude =
  openCL_prelude ++ openCL_boilerplate ++ kernel_declarations
  where opencl_program_fragments =
          -- Some C compilers limit the size of literal strings, so
          -- chunk the entire program into small bits here, and
          -- concatenate it again at runtime.
          [ [C.cinit|$string:s|] | s <- chunk 2000 (opencl_prelude++opencl_program) ]
        nullptr = [C.cinit|NULL|]
        kernel_declarations =
          [C.cedecl|static const char *fut_opencl_program[] =
                    {$inits:(opencl_program_fragments++[nullptr])};|] :
          concat
          [ [ [C.cedecl|static typename cl_kernel $id:name;|]
            , [C.cedecl|static int $id:(kernelRuntime name) = 0;|]
            , [C.cedecl|static int $id:(kernelRuns name) = 0;|]
            ]
          | name <- kernel_names ] ++
          [[C.cedecl|
void setup_opencl_and_load_kernels() {
  typename cl_int error;
  typename cl_program prog = setup_opencl(fut_opencl_program);

  // Load all the kernels.
  $stms:(map (loadKernelByName) kernel_names)
}|]] ++ [[C.cedecl|
void post_opencl_setup(struct opencl_device_option *option) {
  $stms:(map lockstepWidthHeuristicsCode lockstepWidthHeuristicsTable)
}|]]

        openCL_prelude = [ [C.cedecl|$esc:("#define FUT_BLOCK_DIM " ++ show block_dim)|] ]

        openCL_h = $(embedStringFile "rts/c/opencl.h")

        openCL_boilerplate = [C.cunit|$esc:openCL_h|]

loadKernelByName :: String -> C.Stm
loadKernelByName name = [C.cstm|{
  $id:name = clCreateKernel(prog, $string:name, &error);
  assert(error == 0);
  if (debugging) {
    fprintf(stderr, "Created kernel %s.\n", $string:name);
  }
  }|]

openClInit :: [C.Stm]
openClInit =
  [[C.cstm|setup_opencl_and_load_kernels();|]]

kernelRuntime :: String -> String
kernelRuntime = (++"total_runtime")

kernelRuns :: String -> String
kernelRuns = (++"runs")

openClReport :: [String] -> [C.BlockItem]
openClReport names =
  declares ++
  [[C.citem|if (debugging) { $items:report_kernels }|],
   report_total]
  where longest_name = foldl max 0 $ map length names
        report_kernels = concatMap reportKernel names
        format_string name =
          let padding = replicate (longest_name - length name) ' '
          in unwords ["Kernel",
                      name ++ padding,
                      "executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n"]
        reportKernel name =
          let runs = kernelRuns name
              total_runtime = kernelRuntime name
          in [[C.citem|
               fprintf(stderr,
                       $string:(format_string name),
                       $id:runs,
                       (long int) $id:total_runtime / ($id:runs != 0 ? $id:runs : 1),
                       (long int) $id:total_runtime);
              |],
              [C.citem|total_runtime += $id:total_runtime;|],
              [C.citem|total_runs += $id:runs;|]]

        declares = [[C.citem|int total_runtime = 0;|],
                    [C.citem|int total_runs = 0;|]]
        report_total = [C.citem|
                          if (debugging) {
                            fprintf(stderr, "Ran %d kernels with cumulative runtime: %6ldus\n",
                                    total_runs, total_runtime);
                          }
                        |]

lockstepWidthHeuristicsCode :: LockstepWidthHeuristic -> C.Stm
lockstepWidthHeuristicsCode
  (LockstepWidthHeuristic platform_name device_type width) =
  [C.cstm|
   if (strcmp(option->platform_name, $string:platform_name) == 0 &&
      option->device_type == $exp:(clDeviceType device_type)) {
     cl_lockstep_width = $int:width;
     if (debugging) {
       fprintf(stderr, "Setting lockstep width to: %d\n", cl_lockstep_width);
     }
   }
   |]
  where clDeviceType DeviceGPU = [C.cexp|CL_DEVICE_TYPE_GPU|]
        clDeviceType DeviceCPU = [C.cexp|CL_DEVICE_TYPE_CPU|]
