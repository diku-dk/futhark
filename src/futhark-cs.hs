{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import System.Exit
import System.Environment

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.SequentialCSharp as SequentialCS
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI
import Futhark.Util

main :: IO ()
main = compilerMain () []
       "Compile sequential C#" "Generate sequential C# code from optimised Futhark program."
       sequentialCpuPipeline $ \() mode outpath prog -> do
           let class_name =
                 case mode of ToLibrary -> Just $ takeBaseName outpath
                              ToExecutable -> Nothing
           csprog <- either (`internalError` prettyText prog) return =<<
                     SequentialCS.compileProg class_name prog

           let cspath = outpath `addExtension` "cs"
           liftIO $ writeFile cspath csprog

           mono_libs <- liftIO $ getEnv "MONO_PATH"
           case mode of
             ToLibrary -> do
               let dllpath = outpath `addExtension` "dll"
               ret <- liftIO $ runProgramWithExitCode "csc"
                 ["-out:" ++ dllpath
                 , "-target:library"
                 , "-lib:"++mono_libs
                 , "-r:Cloo.clSharp.dll"
                 , "-r:Mono.Options.dll"
                 , cspath
                 , "/unsafe"] ""
               case ret of
                 Left err ->
                   externalErrorS $ "Failed to run csc: " ++ show err
                 Right (ExitFailure code, cscwarn, cscerr) ->
                   externalErrorS $ "csc failed with code " ++ show code ++ ":\n" ++ cscerr ++ cscwarn
                 Right (ExitSuccess, _, _) -> liftIO $ return ()

             ToExecutable -> do
               ret <- liftIO $ runProgramWithExitCode "csc"
                 ["-out:" ++ outpath
                 , "-lib:"++mono_libs
                 , "-r:Cloo.clSharp.dll"
                 , "-r:Mono.Options.dll"
                 , cspath
                 , "/unsafe"] ""
               case ret of
                 Left err ->
                   externalErrorS $ "Failed to run csc: " ++ show err
                 Right (ExitFailure code, cscwarn, cscerr) ->
                   externalErrorS $ "csc failed with code " ++ show code ++ ":\n" ++ cscerr ++ cscwarn
                 Right (ExitSuccess, _, _) -> liftIO $ do
                   perms <- liftIO $ getPermissions outpath
                   setPermissions outpath $ setOwnerExecutable True perms
