{-# LANGUAGE FlexibleContexts #-}

-- | @futhark vulkan@
module Futhark.CLI.Vulkan (main) where

import Futhark.Actions (compileVulkanAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpuPipeline)

-- | Run @futhark opencl@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile Vulkan"
  "Generate Vulkan/C code from optimised Futhark program."
  gpuPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileVulkanAction fcfg mode outpath) prog
