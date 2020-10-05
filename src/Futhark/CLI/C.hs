{-# LANGUAGE FlexibleContexts #-}

-- | @futhark c@
module Futhark.CLI.C (main) where

import Futhark.Actions (compileCAction)
import Futhark.Compiler.CLI
import Futhark.Passes (sequentialCpuPipeline)

-- | Run @futhark c@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile sequential C"
  "Generate sequential C code from optimised Futhark program."
  sequentialCpuPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileCAction fcfg mode outpath) prog
