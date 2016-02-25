module Main (main) where

import Control.Category ((>>>))
import Control.Monad.IO.Class
import Data.Maybe
import System.FilePath
import System.Process
import System.Console.GetOpt

import Futhark.Pipeline
import Futhark.Passes
import Futhark.Compiler
import Futhark.Representation.SOACS (SOACS)
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Pass.ExplicitAllocations
import qualified Futhark.CodeGen.Backends.SequentialPython as SequentialPy
import Futhark.Optimise.InPlaceLowering
import Futhark.Optimise.CSE
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.Simplify
import Futhark.Util.Options
import Futhark.Optimise.DoubleBuffer

import Prelude

main :: IO ()
main = mainWithOptions newCompilerConfig commandLineOptions inspectNonOptions
  where inspectNonOptions [file] config = Just $ compile config file
        inspectNonOptions _      _      = Nothing

compile :: CompilerConfig -> FilePath -> IO ()
compile config filepath =
  runCompilerOnProgram (futharkConfig config)
  compilerPipeline (pyCodeAction filepath config) filepath


pyCodeAction :: FilePath -> CompilerConfig -> Action ExplicitMemory
pyCodeAction filepath config =
  Action { actionName = "Compile sequential C"
         , actionDescription = "Generate sequential C code from optimised Futhark program."
         , actionProcedure = procedure
         }
  where procedure prog = do
          pyprog <- either compileFail return =<< SequentialPy.compileProg (compilerModule config) prog
          let binpath = outputFilePath filepath config
          let pypath = if compilerModule config
                       then binpath `replaceExtension` "py"
                       else binpath
          liftIO $ writeFile pypath pyprog
          _ <- liftIO $ createProcess (proc "chmod" ["+x", pypath])
          return ()

type CompilerOption = OptDescr (Either (IO ()) (CompilerConfig -> CompilerConfig))

commandLineOptions :: [CompilerOption]
commandLineOptions =
  [ Option "o" []
    (ReqArg (\filename -> Right $ \config -> config { compilerOutput = Just filename })
     "FILE")
    "Name of the compiled binary."
  , Option "V" ["verbose"]
    (OptArg (\file -> Right $ \config -> config { compilerVerbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["unsafe"]
    (NoArg $ Right $ \config -> config { compilerUnsafe = True })
    "Do not perform bound- and size-checks in generated code."
  , Option [] ["module"]
    (NoArg $ Right $ \config -> config { compilerModule = True })
    "Generate the file as a module."
  ]

data CompilerConfig =
  CompilerConfig { compilerOutput :: Maybe FilePath
                 , compilerVerbose :: Maybe (Maybe FilePath)
                 , compilerUnsafe :: Bool
                 , compilerModule :: Bool
                 }

newCompilerConfig :: CompilerConfig
newCompilerConfig = CompilerConfig { compilerOutput = Nothing
                                   , compilerVerbose = Nothing
                                   , compilerUnsafe = False
                                   , compilerModule = False
                                   }

outputFilePath :: FilePath -> CompilerConfig -> FilePath
outputFilePath srcfile =
  fromMaybe (srcfile `replaceExtension` "") . compilerOutput

futharkConfig :: CompilerConfig -> FutharkConfig
futharkConfig config =
  newFutharkConfig { futharkVerbose = compilerVerbose config
                   , futharkBoundsCheck = not $ compilerUnsafe config
                   }

compilerPipeline :: Pipeline SOACS ExplicitMemory
compilerPipeline =
  standardPipeline >>>
  onePass firstOrderTransform >>>
  passes [ simplifyKernels
         , inPlaceLowering
         ] >>>
  onePass explicitAllocations >>>
  passes [ simplifyExplicitMemory
         , performCSE
         , simplifyExplicitMemory
         , doubleBuffer
         , simplifyExplicitMemory
         ]
