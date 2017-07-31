{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.FilePath
import System.Directory
import System.Console.GetOpt

import Futhark.Pipeline
import Futhark.Passes
import Futhark.Compiler
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.CodeGen.Backends.SequentialPython as SequentialPy
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Futlib.Prelude

import Prelude

main :: IO ()
main = reportingIOErrors $
       mainWithOptions newCompilerConfig commandLineOptions inspectNonOptions
  where inspectNonOptions [file] config = Just $ compile config file
        inspectNonOptions _      _      = Nothing

compile :: CompilerConfig -> FilePath -> IO ()
compile config filepath =
  runCompilerOnProgram (futharkConfig config) preludeBasis
  sequentialCpuPipeline (pyCodeAction filepath config) filepath

pyCodeAction :: FilePath -> CompilerConfig -> Action ExplicitMemory
pyCodeAction filepath config =
  Action { actionName = "Compile sequential Python"
         , actionDescription = "Generate sequential Python code from optimised Futhark program."
         , actionProcedure = procedure
         }
  where procedure prog = do
          let class_name
                | compilerModule config = Just $ takeBaseName filepath
                | otherwise             = Nothing
          pyprog <- either (`internalError` prettyText prog) return =<< SequentialPy.compileProg class_name prog
          let binpath = outputFilePath filepath config
          let pypath = if compilerModule config
                       then binpath `replaceExtension` "py"
                       else binpath
          liftIO $ writeFile pypath pyprog
          unless (compilerModule config) $ do
            perms <- liftIO $ getPermissions pypath
            liftIO $ setPermissions pypath $ setOwnerExecutable True perms

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
  , Option [] ["library"]
    (NoArg $ Right $ \config -> config { compilerModule = True })
    "Generate a library instead of an executable."
  ]

data CompilerConfig =
  CompilerConfig { compilerOutput :: Maybe FilePath
                 , compilerVerbose :: Maybe (Maybe FilePath)
                 , compilerModule :: Bool
                 }

newCompilerConfig :: CompilerConfig
newCompilerConfig = CompilerConfig { compilerOutput = Nothing
                                   , compilerVerbose = Nothing
                                   , compilerModule = False
                                   }

outputFilePath :: FilePath -> CompilerConfig -> FilePath
outputFilePath srcfile =
  fromMaybe (srcfile `replaceExtension` "") . compilerOutput

futharkConfig :: CompilerConfig -> FutharkConfig
futharkConfig config =
  newFutharkConfig { futharkVerbose = compilerVerbose config }
