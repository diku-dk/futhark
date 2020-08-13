{-# LANGUAGE FlexibleContexts #-}
module Futhark.CLI.Multicore (main) where

import Futhark.Actions (compileMulticoreAction)
import Futhark.Passes (multicorePipeline)
import Futhark.Compiler.CLI

main :: String -> [String] -> IO ()
main = compilerMain () []
       "Compile to multicore C" "Generate multicore C code from optimised Futhark program."
       multicorePipeline $ \fcfg () mode outpath prog -> do
  actionProcedure (compileMulticoreAction fcfg mode outpath) prog
