-- | @futhark pyopencl@
module Futhark.CLI.PyOpenCL (main) where

import Futhark.Actions (compilePyOpenCLAction)
import Futhark.Compiler.CLI
import Futhark.Passes

-- | Run @futhark pyopencl@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile PyOpenCL"
  "Generate Python + OpenCL code from optimised Futhark program."
  gpumemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compilePyOpenCLAction fcfg mode outpath) prog
