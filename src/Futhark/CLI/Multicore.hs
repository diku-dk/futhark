{-# LANGUAGE FlexibleContexts #-}

-- | @futhark multicore@
module Futhark.CLI.Multicore (main) where

import Futhark.Actions (compileMulticoreAction)
import Futhark.Compiler.CLI
import Futhark.Passes (multicorePipeline)

-- | Run @futhark multicore@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile to multicore C"
  "Generate multicore C code from optimised Futhark program."
  multicorePipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileMulticoreAction fcfg mode outpath) prog
