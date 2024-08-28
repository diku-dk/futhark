-- | @futhark wasm@
module Futhark.CLI.WASM (main) where

import Futhark.Actions (compileCtoWASMAction)
import Futhark.Compiler.CLI
import Futhark.Passes (seqmemPipeline)

-- | Run @futhark c@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile to WASM"
  "Generate WASM with the sequential C backend code from optimised Futhark program."
  seqmemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileCtoWASMAction fcfg mode outpath) prog
