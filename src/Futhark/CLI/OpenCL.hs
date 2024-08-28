-- | @futhark opencl@
module Futhark.CLI.OpenCL (main) where

import Futhark.Actions (compileOpenCLAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpumemPipeline)

-- | Run @futhark opencl@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile OpenCL"
  "Generate OpenCL/C code from optimised Futhark program."
  gpumemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileOpenCLAction fcfg mode outpath) prog
