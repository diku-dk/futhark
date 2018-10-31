{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Exit

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.CCuda as CCuda
import Futhark.Util
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI

main :: IO ()
main = compilerMain () []
       "Compile CUDA" "Generate CUDA/C code from optimised Futhark program."
       gpuPipeline $ \() mode outpath prog -> do
         cprog <- either (`internalError` prettyText prog) return =<<
                  CCuda.compileProg prog
         let cpath = outpath `addExtension` "c"
             hpath = outpath `addExtension` "h"
             extra_options = [ "-lcuda"   -- XXX
                             , "-lnvrtc"   -- XXX
                             , "-L/opt/cuda/lib64/"   -- XXX
                             , "-I/opt/cuda/include/"   -- XXX
                             ]
         case mode of
           ToLibrary -> do
             let (header, impl) = CCuda.asLibrary cprog
             liftIO $ writeFile hpath header
             liftIO $ writeFile cpath impl
           ToExecutable -> do
             liftIO $ writeFile cpath $ CCuda.asExecutable cprog
             let args = [cpath, "-O3", "-std=c99", "-lm", "-o", outpath]
                        ++ extra_options
             ret <- liftIO $ runProgramWithExitCode "gcc" args ""
             case ret of
               Left err ->
                 externalErrorS $ "Failed to run gcc: " ++ show err
               Right (ExitFailure code, _, gccerr) ->
                 externalErrorS $ "gcc failed with code " ++
                 show code ++ ":\n" ++ gccerr
               Right (ExitSuccess, _, _) ->
                 return ()
