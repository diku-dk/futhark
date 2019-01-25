{-# LANGUAGE FlexibleContexts #-}
module Futhark.CLI.C (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Exit

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI
import Futhark.Util

main :: String -> [String] -> IO ()
main = compilerMain () []
       "Compile sequential C" "Generate sequential C code from optimised Futhark program."
       sequentialCpuPipeline $ \() mode outpath prog -> do
         cprog <- either (`internalError` prettyText prog) return =<<
                  SequentialC.compileProg prog
         let cpath = outpath `addExtension` "c"
             hpath = outpath `addExtension` "h"

         case mode of
           ToLibrary -> do
             let (header, impl) = SequentialC.asLibrary cprog
             liftIO $ writeFile hpath header
             liftIO $ writeFile cpath impl
           ToExecutable -> do
             liftIO $ writeFile cpath $ SequentialC.asExecutable cprog
             ret <- liftIO $ runProgramWithExitCode "gcc"
                    [cpath, "-O3", "-std=c99", "-lm", "-o", outpath] ""
             case ret of
               Left err ->
                 externalErrorS $ "Failed to run gcc: " ++ show err
               Right (ExitFailure code, _, gccerr) ->
                 externalErrorS $ "gcc failed with code " ++
                 show code ++ ":\n" ++ gccerr
               Right (ExitSuccess, _, _) ->
                 return ()
