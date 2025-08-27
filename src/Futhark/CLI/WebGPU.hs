-- | @futhark webgpu@
module Futhark.CLI.WebGPU (main) where

import Futhark.Actions (compileWebGPUAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpumemPipeline)

-- | Run @futhark webgpu@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile WebGPU"
  "Generate WebGPU C code from optimised Futhark program."
  gpumemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileWebGPUAction fcfg mode outpath) prog
