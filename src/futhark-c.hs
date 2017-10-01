{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Process
import System.Exit

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI

main :: IO ()
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
             (gccCode, _, gccerr) <-
               liftIO $ readProcessWithExitCode "gcc"
               [cpath, "-O3", "-std=c99", "-lm", "-o", outpath] ""
             case gccCode of
               ExitFailure code -> externalErrorS $ "gcc failed with code " ++
                                   show code ++ ":\n" ++ gccerr
               ExitSuccess      -> return ()
