-- | @futhark multicore@
module Futhark.CLI.Multicore (main) where

import Futhark.Actions (compileMulticoreAction)
import Futhark.Compiler.CLI
import Futhark.Passes (mcmemPipeline)

-- | Run @futhark multicore@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile to multicore C"
  "Generate multicore C code from optimised Futhark program."
  mcmemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileMulticoreAction fcfg mode outpath) prog
