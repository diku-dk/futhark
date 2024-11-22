-- | @futhark cudatc@
module Futhark.CLI.CUDATC (main) where

import Futhark.Actions (compileCUDATCAction)
import Futhark.Compiler.CLI
import Futhark.Util.Options
import Futhark.Passes (gpumemtcPipeline)

commandLineOptions :: [FunOptDescr String]
commandLineOptions =
  [ Option
      "I"
      ["cute-include"]
      (ReqArg (Right . const) "FILE")
      "Include path for CuTe/Cutlass"
  ]

-- | Run @futhark cuda@.
main :: String -> [String] -> IO ()
main = compilerMain
  ""
  commandLineOptions
  "Compile CUDA with support for tensor cores"
  "Generate CUDA/C code with tensor core operations from optimised Futhark program."
  gpumemtcPipeline $ \fcfg cuteincludepath mode outpath prog -> 
    actionProcedure (compileCUDATCAction fcfg mode cuteincludepath outpath) prog
