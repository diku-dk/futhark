-- | @futhark c@
module Futhark.CLI.C (main) where

import Futhark.Actions (compileCAction)
import Futhark.Compiler.CLI
import Futhark.Passes (seqmemPipeline)

-- | Run @futhark c@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile sequential C"
  "Generate sequential C code from optimised Futhark program."
  seqmemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileCAction fcfg mode outpath) prog
