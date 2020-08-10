{-# LANGUAGE FlexibleContexts #-}
-- | @futhark c@
module Futhark.CLI.C (main) where

import Futhark.Actions (compileCAction)
import Futhark.Passes (sequentialCpuPipeline)
import Futhark.Compiler.CLI

-- | Run @futhark c@
main :: String -> [String] -> IO ()
main = compilerMain () []
       "Compile sequential C" "Generate sequential C code from optimised Futhark program."
       sequentialCpuPipeline $ \fcfg () mode outpath prog ->
  actionProcedure (compileCAction fcfg mode outpath) prog
