{-# LANGUAGE FlexibleContexts #-}

module Futhark.CLI.MPI (main) where

import Futhark.Actions (compileMPIAction)
import Futhark.Compiler.CLI
import Futhark.Passes (mpiPipeline)

main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile to MPI C"
  "Generate MPI C code from optimised Futhark program."
  mpiPipeline
  $ \fcfg () mode outpath prog ->
    actionProcedure (compileMPIAction fcfg mode outpath) prog
