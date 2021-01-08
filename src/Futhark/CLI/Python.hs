{-# LANGUAGE FlexibleContexts #-}

-- | @futhark py@
module Futhark.CLI.Python (main) where

import Control.Monad.IO.Class
import qualified Futhark.CodeGen.Backends.SequentialPython as SequentialPy
import Futhark.Compiler.CLI
import Futhark.Passes
import System.Directory
import System.FilePath

-- | Run @futhark py@
main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile sequential Python"
  "Generate sequential Python code from optimised Futhark program."
  sequentialCpuPipeline
  $ \fcfg () mode outpath prog -> do
    let class_name =
          case mode of
            ToLibrary -> takeBaseName outpath
            _ -> "internal"
    pyprog <- handleWarnings fcfg $ SequentialPy.compileProg mode class_name prog

    case mode of
      ToLibrary ->
        liftIO $ writeFile (outpath `addExtension` "py") pyprog
      _ -> liftIO $ do
        writeFile outpath pyprog
        perms <- liftIO $ getPermissions outpath
        setPermissions outpath $ setOwnerExecutable True perms
