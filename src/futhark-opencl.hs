module Main (main) where

import Data.Maybe
import System.FilePath
import System.Process
import System.IO
import System.Exit
import System.Console.GetOpt

import Futhark.Pipeline
import Futhark.Passes
import Futhark.Compiler
import qualified Futhark.CodeGen.Backends.OpenCL as OpenCL
import Futhark.Util.Options

main :: IO ()
main = mainWithOptions newCompilerConfig commandLineOptions inspectNonOptions
  where inspectNonOptions [file] config = Just $ compile config file
        inspectNonOptions _      _      = Nothing


compile :: CompilerConfig -> FilePath -> IO ()
compile config filepath = do
  (msgs, res) <- runPipelineOnProgram (futharkConfig config) filepath
  hPutStr stderr msgs
  case res of
    Left err -> do
      hPutStrLn stderr $ errorDesc err
      exitWith $ ExitFailure 2
    Right (Basic _) ->
      error "Pipeline produced program without memory annotations."
    Right (ExplicitMemory prog) ->
      case OpenCL.compileProg prog of
        Left err -> do
          hPutStrLn stderr err
          exitWith $ ExitFailure 2
        Right cprog -> do
          let binpath = outputFilePath filepath config
              cpath = binpath `replaceExtension` "c"
          writeFile cpath cprog
          (gccCode, _, gccerr) <-
            readProcessWithExitCode "gcc"
            [cpath, "-o", binpath, "-lm", "-O3", "-std=c99", "-lOpenCL"] ""
          case gccCode of
            ExitFailure code -> error $ "gcc failed with code " ++ show code ++ ":\n" ++ gccerr
            ExitSuccess      -> return ()

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
  FutharkConfig { futharkpipeline = compilerPipeline
                , futharkaction = interpretAction'
                , futharkcheckAliases = True
                , futharkverbose = compilerVerbose config
                , futharkboundsCheck = True
                }

compilerPipeline :: [Pass]
compilerPipeline =
  [ uttransform
  , eotransform
  , inlinetransform
  , commonSubexpressionElimination
  , eotransform
  , hotransform
  , commonSubexpressionElimination
  , eotransform
  , removeDeadFunctions
  , sequentialiseKernels
  , eotransform
  , inPlaceLowering
  , explicitMemory
  , eotransform
  , commonSubexpressionElimination
  , eotransform
  , doubleBuffer
  , eotransform
  ]
