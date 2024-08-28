-- | @futhark wasm-multicore@
module Futhark.CLI.MulticoreWASM (main) where

import Futhark.Actions (compileMulticoreToWASMAction)
import Futhark.Compiler.CLI
import Futhark.Passes (mcmemPipeline)

-- | Run @futhark c@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile to multicore WASM"
  "Generate multicore WASM with the multicore C backend code from optimised Futhark program."
  mcmemPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileMulticoreToWASMAction fcfg mode outpath) prog
