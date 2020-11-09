{-# LANGUAGE FlexibleContexts #-}

-- | @futhark opencl@
module Futhark.CLI.OpenCL (main) where

import Futhark.Actions (compileOpenCLAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpuPipeline)

-- | Run @futhark opencl@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile OpenCL"
  "Generate OpenCL/C code from optimised Futhark program."
  gpuPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileOpenCLAction fcfg mode outpath) prog
