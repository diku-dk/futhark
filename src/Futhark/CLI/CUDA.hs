{-# LANGUAGE FlexibleContexts #-}

-- | @futhark cuda@
module Futhark.CLI.CUDA (main) where

import Control.Monad.IO.Class
import qualified Futhark.CodeGen.Backends.CCUDA as CCUDA
import Futhark.Compiler.CLI
import Futhark.Passes
import Futhark.Util
import System.Exit
import System.FilePath

-- | Run @futhark cuda@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile CUDA"
  "Generate CUDA/C code from optimised Futhark program."
  gpuPipeline
  $ \fcfg () mode outpath prog -> do
    cprog <- handleWarnings fcfg $ CCUDA.compileProg prog
    let cpath = outpath `addExtension` "c"
        hpath = outpath `addExtension` "h"
        extra_options =
          [ "-lcuda",
            "-lcudart",
            "-lnvrtc"
          ]
    case mode of
      ToLibrary -> do
        let (header, impl) = CCUDA.asLibrary cprog
        liftIO $ writeFile hpath header
        liftIO $ writeFile cpath impl
      ToExecutable -> do
        liftIO $ writeFile cpath $ CCUDA.asExecutable cprog
        let args =
              [cpath, "-O", "-std=c99", "-lm", "-o", outpath]
                ++ extra_options
        ret <- liftIO $ runProgramWithExitCode "gcc" args mempty
        case ret of
          Left err ->
            externalErrorS $ "Failed to run gcc: " ++ show err
          Right (ExitFailure code, _, gccerr) ->
            externalErrorS $
              "gcc failed with code "
                ++ show code
                ++ ":\n"
                ++ gccerr
          Right (ExitSuccess, _, _) ->
            return ()
