{-# LANGUAGE FlexibleContexts #-}

-- | @futhark pyopencl@
module Futhark.CLI.PyOpenCL (main) where

import Control.Monad.IO.Class
import qualified Futhark.CodeGen.Backends.PyOpenCL as PyOpenCL
import Futhark.Compiler.CLI
import Futhark.Passes
import System.Directory
import System.FilePath

-- | Run @futhark pyopencl@.
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile PyOpenCL"
  "Generate Python + OpenCL code from optimised Futhark program."
  gpuPipeline
  $ \fcfg () mode outpath prog -> do
    let class_name =
          case mode of
            ToLibrary -> takeBaseName outpath
            _ -> "internal"
    pyprog <- handleWarnings fcfg $ PyOpenCL.compileProg mode class_name prog

    case mode of
      ToLibrary ->
        liftIO $ writeFile (outpath `addExtension` "py") pyprog
      _ -> liftIO $ do
        writeFile outpath pyprog
        perms <- liftIO $ getPermissions outpath
        setPermissions outpath $ setOwnerExecutable True perms
