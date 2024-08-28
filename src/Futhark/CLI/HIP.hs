-- | @futhark hip@
module Futhark.CLI.HIP (main) where

import Futhark.Actions (compileHIPAction)
import Futhark.Compiler.CLI
import Futhark.Passes (gpumemPipeline)

-- | Run @futhark hip@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile HIP"
  "Generate HIP/C code from optimised Futhark program."
  gpumemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileHIPAction fcfg mode outpath) prog
