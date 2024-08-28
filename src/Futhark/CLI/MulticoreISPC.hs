-- | @futhark multicore@
module Futhark.CLI.MulticoreISPC (main) where

import Futhark.Actions (compileMulticoreToISPCAction)
import Futhark.Compiler.CLI
import Futhark.Passes (mcmemPipeline)

-- | Run @futhark multicore@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile to multicore ISPC"
  "Generate multicore ISPC code from optimised Futhark program."
  mcmemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileMulticoreToISPCAction fcfg mode outpath) prog
