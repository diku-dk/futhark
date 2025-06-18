-- | @futhark cudatc@
module Futhark.CLI.CUDATC (main) where

import Futhark.Actions (compileCUDATCAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpumemtcPipeline)

-- | Run @futhark cudatc@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile CUDA with support for tensor cores"
  "Generate CUDA/C code with tensor core operations from optimised Futhark program."
  gpumemtcPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileCUDATCAction fcfg mode outpath) prog
