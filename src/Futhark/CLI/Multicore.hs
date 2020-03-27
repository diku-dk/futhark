{-# LANGUAGE FlexibleContexts #-}
module Futhark.CLI.Multicore (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Exit

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.MulticoreC as MulticoreC
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI
import Futhark.Util

main :: String -> [String] -> IO ()
main = compilerMain () []
       "Compile to mulcitore C" "Generate multicore C code from optimised Futhark program."
       multicorePipeline $ \() mode outpath prog -> do
         cprog <- either (`internalError` prettyText prog) return =<<
                  MulticoreC.compileProg prog
         let cpath = outpath `addExtension` "c"
             hpath = outpath `addExtension` "h"

         case mode of
           ToLibrary -> do
             let (header, impl) = MulticoreC.asLibrary cprog
             liftIO $ writeFile hpath header
             liftIO $ writeFile cpath impl
           ToExecutable -> do
             liftIO $ writeFile cpath $ MulticoreC.asExecutable cprog
             ret <- liftIO $ runProgramWithExitCode "gcc"
                    [cpath, "-O3", "-pthread", "-std=c99", "-lm", "-o", outpath] mempty
             case ret of
               Left err ->
                 externalErrorS $ "Failed to run gcc: " ++ show err
               Right (ExitFailure code, _, gccerr) ->
                 externalErrorS $ "gcc failed with code " ++
                 show code ++ ":\n" ++ gccerr
               Right (ExitSuccess, _, _) ->
                 return ()
