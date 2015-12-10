module Main (main) where

import Control.Category ((>>>))
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import System.Process
import System.IO
import System.Exit
import System.Console.GetOpt

import Futhark.Pipeline
import Futhark.Passes
import Futhark.Compiler
import Futhark.Representation.SOACS (SOACS)
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Pass.ExplicitAllocations
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import Futhark.Optimise.InPlaceLowering
import Futhark.Optimise.CSE
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.Simplify
import Futhark.Util.Options
import Futhark.Util.Log
import Futhark.Optimise.DoubleBuffer
import Futhark.Representation.AST.Pretty

main :: IO ()
main = mainWithOptions newCompilerConfig commandLineOptions inspectNonOptions
  where inspectNonOptions [file] config = Just $ compile config file
        inspectNonOptions _      _      = Nothing

compile :: CompilerConfig -> FilePath -> IO ()
compile config filepath = do
  (res, msgs) <- runPipelineOnProgram (futharkConfig config) compilerPipeline filepath
  when (isJust $ compilerVerbose config) $
    T.hPutStr stderr $ toText msgs
  case res of
    Left err -> do
      dumpError (futharkConfig config) err
      exitWith $ ExitFailure 2
    Right prog ->
      case SequentialC.compileProg prog of
        Left err -> do
          dumpError (futharkConfig config) $
            CompileError (T.pack err) $
            T.pack $ pretty prog
          exitWith $ ExitFailure 2
        Right cprog -> do
          let binpath = outputFilePath filepath config
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
  [ Option "o" []
    (ReqArg (\filename -> Right $ \config -> config { compilerOutput = Just filename })
     "FILE")
    "Name of the compiled binary."
  , Option [] ["real-as-single"]
    (NoArg $ Right $ \config -> config { compilerRealConfiguration = RealAsFloat32 } )
    "Map 'real' to 32-bit floating point."
  , Option [] ["real-as-double"]
    (NoArg $ Right $ \config -> config { compilerRealConfiguration = RealAsFloat64 } )
    "Map 'real' to 64-bit floating point (the default)."
  , Option "V" ["verbose"]
    (OptArg (\file -> Right $ \config -> config { compilerVerbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["unsafe"]
    (NoArg $ Right $ \config -> config { compilerUnsafe = True })
    "Do not perform bound- and size-checks in generated code."
  ]

data CompilerConfig =
  CompilerConfig { compilerOutput :: Maybe FilePath
                 , compilerVerbose :: Maybe (Maybe FilePath)
                 , compilerRealConfiguration :: RealConfiguration
                 , compilerUnsafe :: Bool
                 }

newCompilerConfig :: CompilerConfig
newCompilerConfig = CompilerConfig { compilerOutput = Nothing
                                   , compilerVerbose = Nothing
                                   , compilerRealConfiguration = RealAsFloat64
                                   , compilerUnsafe = False
                                   }

outputFilePath :: FilePath -> CompilerConfig -> FilePath
outputFilePath srcfile =
  fromMaybe (srcfile `replaceExtension` "") . compilerOutput

futharkConfig :: CompilerConfig -> FutharkConfig
futharkConfig config =
  newFutharkConfig { futharkVerbose = compilerVerbose config
                   , futharkRealConfiguration = compilerRealConfiguration config
                   , futharkBoundsCheck = not $ compilerUnsafe config
                   }

compilerPipeline :: Pipeline SOACS ExplicitMemory
compilerPipeline =
  standardPipeline >>>
  passes [ firstOrderTransform
         , simplifySOACS
         , inPlaceLowering
         ] >>>
  onePass explicitAllocations >>>
  passes [ simplifyExplicitMemory
         , performCSE
         , simplifyExplicitMemory
         , doubleBuffer
         , simplifyExplicitMemory
         ]
