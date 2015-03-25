module Main (main) where

import Data.Maybe
import Data.Version
import System.FilePath
import System.Process
import System.IO
import System.Exit
import System.Console.GetOpt

import Futhark.Pipeline
import Futhark.Passes
import Futhark.Compiler
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import Futhark.Version
import Futhark.Util.Options

main :: IO ()
main = mainWithOptions newCompilerConfig commandLineOptions inspectNonOptions
  where inspectNonOptions [file] config = Just $ compile config file
        inspectNonOptions _      _      = Nothing


compile :: CompilerConfig -> FilePath -> IO ()
compile config filepath = do
  (_, res) <- runPipelineOnProgram (futharkConfig config) filepath
  case res of
    Left err -> do
      hPutStrLn stderr $ errorDesc err
      exitWith $ ExitFailure 2
    Right (Basic _) ->
      error "Pipeline produced program without memory annotations."
    Right (ExplicitMemory prog) ->
      case SequentialC.compileProg prog of
        Left err -> do
          hPutStrLn stderr err
          exitWith $ ExitFailure 2
        Right cprog -> do
          let binpath = outputFilePath filepath config `replaceExtension` ""
              cpath = binpath `replaceExtension` "c"
          writeFile cpath cprog
          (gccCode, _, gccerr) <-
            readProcessWithExitCode "gcc"
            [cpath, "-o", binpath, "-lm", "-O3", "-std=c99"] ""
          case gccCode of
            ExitFailure code -> error $ "gcc failed with code " ++ show code ++ ":\n" ++ gccerr
            ExitSuccess      -> return ()

type CompilerOption = OptDescr (Either (IO ()) (CompilerConfig -> CompilerConfig))

commandLineOptions :: [CompilerOption]
commandLineOptions =
  [ Option "v" ["version"]
    (NoArg $ Left $ do putStrLn $ "Futhark " ++ showVersion version
                       putStrLn "(C) HIPERFIT research centre"
                       putStrLn "Department of Computer Science, University of Copenhagen (DIKU)"
                       exitSuccess)
    "Print version information and exit."

  , Option "o" []
    (ReqArg (\filename -> Right $ \config -> config { compilerOutput = Just filename })
     "FILE")
    "Name of the compiled binary."
  ]

data CompilerConfig =
  CompilerConfig { compilerOutput :: Maybe FilePath }

newCompilerConfig :: CompilerConfig
newCompilerConfig = CompilerConfig { compilerOutput = Nothing
                                   }

outputFilePath :: FilePath -> CompilerConfig -> FilePath
outputFilePath srcfile =
  fromMaybe (srcfile `replaceExtension` "") . compilerOutput

futharkConfig :: CompilerConfig -> FutharkConfig
futharkConfig _ =
  FutharkConfig { futharkpipeline = compilerPipeline
                , futharkaction = interpretAction'
                , futharkcheckAliases = True
                , futharkverbose = Nothing
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
  , fotransform
  , eotransform
  , inPlaceLowering
  , explicitMemory
  , eotransform
  , commonSubexpressionElimination
  , eotransform
  ]
