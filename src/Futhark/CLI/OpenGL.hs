{-# LANGUAGE FlexibleContexts #-}
module Futhark.CLI.OpenGL (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Exit
import qualified System.Info

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.COpenGL as COpenGL
import Futhark.Util
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI

main :: String -> [String] -> IO ()
main = compilerMain () []
       "Compile OpenGL" "Generate OpenGL/C code from optimised Futhark program."
       gpuPipeline $ \() mode outpath prog -> do
         cprog <- either (`internalError` prettyText prog) return =<<
                  COpenGL.compileProg prog
         let cpath = outpath `addExtension` "c"
             hpath = outpath `addExtension` "h"
             extra_options = [ "-L/rts/c/glad/include/glad"
                             , "-ldl"
                             , "-lGL"
                             , "-lX11"
                             , "-lXi"
                             , "-lXrandr"
                             ]

         case mode of
           ToLibrary -> do
             let (header, impl) = COpenGL.asLibrary cprog
             liftIO $ writeFile hpath header
             liftIO $ writeFile cpath impl
           ToExecutable -> do
             liftIO $ writeFile cpath $ COpenGL.asExecutable cprog
             let args = [cpath, "-O", "-std=c99", "-lm", "-o", outpath]
                        ++ extra_options
             ret <- liftIO $ runProgramWithExitCode "gcc" args mempty
             case ret of
               Left err ->
                 externalErrorS $ "Failed to run gcc: " ++ show err
               Right (ExitFailure code, _, gccerr) ->
                 externalErrorS $ "gcc failed with code " ++
                 show code ++ ":\n" ++ gccerr
               Right (ExitSuccess, _, _) ->
                 return ()
