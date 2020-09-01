{-# LANGUAGE FlexibleContexts #-}

-- | @futhark cuda@
module Futhark.CLI.CUDA (main) where

import Futhark.Actions (compileCUDAAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpuPipeline)

-- | Run @futhark cuda@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile CUDA"
  "Generate CUDA/C code from optimised Futhark program."
  gpuPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileCUDAAction fcfg mode outpath) prog
