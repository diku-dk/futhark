{-# LANGUAGE FlexibleContexts #-}

-- | @futhark c@
module Futhark.CLI.C (main) where

import Control.Monad.IO.Class
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import Futhark.Compiler.CLI
import Futhark.Passes
import Futhark.Pipeline
import Futhark.Util
import System.Exit
import System.FilePath

-- | Run @futhark c@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile sequential C"
  "Generate sequential C code from optimised Futhark program."
  sequentialCpuPipeline
  $ \fcfg () mode outpath prog -> do
    cprog <- handleWarnings fcfg $ SequentialC.compileProg prog
    let cpath = outpath `addExtension` "c"
        hpath = outpath `addExtension` "h"

    case mode of
      ToLibrary -> do
        let (header, impl) = SequentialC.asLibrary cprog
        liftIO $ writeFile hpath header
        liftIO $ writeFile cpath impl
      ToExecutable -> do
        liftIO $ writeFile cpath $ SequentialC.asExecutable cprog
        ret <-
          liftIO $
            runProgramWithExitCode
              "gcc"
              [cpath, "-O3", "-std=c99", "-lm", "-o", outpath]
              mempty
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
