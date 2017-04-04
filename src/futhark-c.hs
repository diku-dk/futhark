{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.IO.Class
import Data.Maybe
import System.FilePath
import System.Process
import System.Exit
import System.Console.GetOpt

import Futhark.Pipeline
import Futhark.Passes
import Futhark.Compiler
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText)

main :: IO ()
main = reportingIOErrors $
       mainWithOptions newCompilerConfig commandLineOptions inspectNonOptions
  where inspectNonOptions [file] config = Just $ compile config file
        inspectNonOptions _      _      = Nothing

compile :: CompilerConfig -> FilePath -> IO ()
compile config filepath =
  runCompilerOnProgram (futharkConfig config)
  (sequentialPipeline Executable) (cCodeAction filepath config) filepath

cCodeAction :: FilePath -> CompilerConfig -> Action ExplicitMemory
cCodeAction filepath config =
  Action { actionName = "Compile sequential C"
         , actionDescription = "Generate sequential C code from optimised Futhark program."
         , actionProcedure = procedure
         }
  where procedure prog = do
          cprog <- either (`internalError` prettyText prog) return =<< SequentialC.compileProg prog
          let binpath = outputFilePath filepath config
              cpath = binpath `replaceExtension` "c"
          liftIO $ writeFile cpath cprog
          (gccCode, _, gccerr) <-
            liftIO $ readProcessWithExitCode "gcc"
            [cpath, "-o", binpath, "-lm", "-O3", "-std=c99"] ""
          case gccCode of
            ExitFailure code -> externalErrorS $ "gcc failed with code " ++ show code ++ ":\n" ++ gccerr
            ExitSuccess      -> return ()

type CompilerOption = OptDescr (Either (IO ()) (CompilerConfig -> CompilerConfig))

commandLineOptions :: [CompilerOption]
commandLineOptions =
  [ Option "o" []
    (ReqArg (\filename -> Right $ \config -> config { compilerOutput = Just filename })
     "FILE")
    "Name of the compiled binary."
  , Option "v" ["verbose"]
    (OptArg (\file -> Right $ \config -> config { compilerVerbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  ]

data CompilerConfig =
  CompilerConfig { compilerOutput :: Maybe FilePath
                 , compilerVerbose :: Maybe (Maybe FilePath)
                 }

newCompilerConfig :: CompilerConfig
newCompilerConfig = CompilerConfig { compilerOutput = Nothing
                                   , compilerVerbose = Nothing
                                   }

outputFilePath :: FilePath -> CompilerConfig -> FilePath
outputFilePath srcfile =
  fromMaybe (srcfile `replaceExtension` "") . compilerOutput

futharkConfig :: CompilerConfig -> FutharkConfig
futharkConfig config =
  newFutharkConfig { futharkVerbose = compilerVerbose config }
