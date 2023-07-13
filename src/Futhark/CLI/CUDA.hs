-- | @futhark cuda@
module Futhark.CLI.CUDA (main) where

import Futhark.Actions (compileCUDAAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpumemPipeline)

-- | Run @futhark cuda@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile CUDA"
  "Generate CUDA/C code from optimised Futhark program."
  gpumemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileCUDAAction fcfg mode outpath) prog
