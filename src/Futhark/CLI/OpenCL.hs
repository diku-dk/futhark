{-# LANGUAGE FlexibleContexts #-}
module Futhark.CLI.OpenCL (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Exit
import qualified System.Info

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.COpenCL as COpenCL
import Futhark.Util
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI

main :: String -> [String] -> IO ()
main = compilerMain () []
       "Compile OpenCL" "Generate OpenCL/C code from optimised Futhark program."
       gpuPipeline $ \() mode outpath prog -> do
         cprog <- either (`internalError` prettyText prog) return =<<
                  COpenCL.compileProg prog
         let cpath = outpath `addExtension` "c"
             hpath = outpath `addExtension` "h"
             extra_options
               | System.Info.os == "darwin" =
                   ["-framework", "OpenCL"]
               | System.Info.os == "mingw32" =
                   ["-lOpenCL64"]
               | otherwise =
                   ["-lOpenCL"]

         case mode of
           ToLibrary -> do
             let (header, impl) = COpenCL.asLibrary cprog
             liftIO $ writeFile hpath header
             liftIO $ writeFile cpath impl
           ToExecutable -> do
             liftIO $ writeFile cpath $ COpenCL.asExecutable cprog
             ret <- liftIO $ runProgramWithExitCode "gcc"
                    ([cpath, "-O", "-std=c99", "-lm", "-o", outpath] ++ extra_options) ""
             case ret of
               Left err ->
                 externalErrorS $ "Failed to run gcc: " ++ show err
               Right (ExitFailure code, _, gccerr) ->
                 externalErrorS $ "gcc failed with code " ++
                 show code ++ ":\n" ++ gccerr
               Right (ExitSuccess, _, _) ->
                 return ()
