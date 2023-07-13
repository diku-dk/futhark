-- | @futhark py@
module Futhark.CLI.Python (main) where

import Futhark.Actions (compilePythonAction)
import Futhark.Compiler.CLI
import Futhark.Passes

-- | Run @futhark py@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile sequential Python"
  "Generate sequential Python code from optimised Futhark program."
  seqmemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compilePythonAction fcfg mode outpath) prog
