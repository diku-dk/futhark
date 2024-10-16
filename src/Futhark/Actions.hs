-- | All (almost) compiler pipelines end with an 'Action', which does
-- something with the result of the pipeline.
module Futhark.Actions
  ( printAction,
    printAliasesAction,
    printLastUseGPU,
    printFusionGraph,
    printInterferenceGPU,
    printMemAliasGPU,
    printMemoryAccessAnalysis,
    callGraphAction,
    impCodeGenAction,
    kernelImpCodeGenAction,
    multicoreImpCodeGenAction,
    metricsAction,
    compileCAction,
    compileCtoWASMAction,
    compileOpenCLAction,
    compileCUDAAction,
    compileHIPAction,
    compileMulticoreAction,
    compileMulticoreToISPCAction,
    compileMulticoreToWASMAction,
    compilePythonAction,
    compilePyOpenCLAction,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.Alias
import Futhark.Analysis.CallGraph (buildCallGraph)
import Futhark.Analysis.Interference qualified as Interference
import Futhark.Analysis.LastUse qualified as LastUse
import Futhark.Analysis.MemAlias qualified as MemAlias
import Futhark.Analysis.Metrics
import Futhark.CodeGen.Backends.CCUDA qualified as CCUDA
import Futhark.CodeGen.Backends.COpenCL qualified as COpenCL
import Futhark.CodeGen.Backends.HIP qualified as HIP
import Futhark.CodeGen.Backends.MulticoreC qualified as MulticoreC
import Futhark.CodeGen.Backends.MulticoreISPC qualified as MulticoreISPC
import Futhark.CodeGen.Backends.MulticoreWASM qualified as MulticoreWASM
import Futhark.CodeGen.Backends.PyOpenCL qualified as PyOpenCL
import Futhark.CodeGen.Backends.SequentialC qualified as SequentialC
import Futhark.CodeGen.Backends.SequentialPython qualified as SequentialPy
import Futhark.CodeGen.Backends.SequentialWASM qualified as SequentialWASM
import Futhark.CodeGen.ImpGen.GPU qualified as ImpGenGPU
import Futhark.CodeGen.ImpGen.Multicore qualified as ImpGenMulticore
import Futhark.CodeGen.ImpGen.Sequential qualified as ImpGenSequential
import Futhark.Compiler.CLI
import Futhark.IR
import Futhark.IR.GPUMem (GPUMem)
import Futhark.IR.MCMem (MCMem)
import Futhark.IR.SOACS (SOACS)
import Futhark.IR.SeqMem (SeqMem)
import Futhark.Optimise.Fusion.GraphRep qualified
import Futhark.Util (runProgramWithExitCode, unixEnvironment)
import Futhark.Version (versionString)
import System.Directory
import System.Exit
import System.FilePath
import System.Info qualified

-- | Print the result to stdout.
printAction :: (ASTRep rep) => Action rep
printAction =
  Action
    { actionName = "Prettyprint",
      actionDescription = "Prettyprint the resulting internal representation on standard output.",
      actionProcedure = liftIO . putStrLn . prettyString
    }

-- | Print the result to stdout, alias annotations.
printAliasesAction :: (AliasableRep rep) => Action rep
printAliasesAction =
  Action
    { actionName = "Prettyprint",
      actionDescription = "Prettyprint the resulting internal representation on standard output.",
      actionProcedure = liftIO . putStrLn . prettyString . aliasAnalysis
    }

-- | Print last use information to stdout.
printLastUseGPU :: Action GPUMem
printLastUseGPU =
  Action
    { actionName = "print last use gpu",
      actionDescription = "Print last use information on gpu.",
      actionProcedure =
        liftIO
          . putStrLn
          . prettyString
          . bimap M.toList (M.toList . fmap M.toList)
          . LastUse.lastUseGPUMem
          . aliasAnalysis
    }

-- | Print fusion graph to stdout.
printFusionGraph :: Action SOACS
printFusionGraph =
  Action
    { actionName = "print fusion graph",
      actionDescription = "Print fusion graph in Graphviz format.",
      actionProcedure =
        liftIO
          . mapM_
            ( putStrLn
                . Futhark.Optimise.Fusion.GraphRep.pprg
                . Futhark.Optimise.Fusion.GraphRep.mkDepGraphForFun
            )
          . progFuns
    }

-- | Print interference information to stdout.
printInterferenceGPU :: Action GPUMem
printInterferenceGPU =
  Action
    { actionName = "print interference gpu",
      actionDescription = "Print interference information on gpu.",
      actionProcedure = liftIO . print . Interference.analyseProgGPU
    }

-- | Print memory alias information to stdout
printMemAliasGPU :: Action GPUMem
printMemAliasGPU =
  Action
    { actionName = "print mem alias gpu",
      actionDescription = "Print memory alias information on gpu.",
      actionProcedure = liftIO . print . MemAlias.analyzeGPUMem
    }

-- | Print result of array access analysis on the IR
printMemoryAccessAnalysis :: (Analyse rep) => Action rep
printMemoryAccessAnalysis =
  Action
    { actionName = "array-access-analysis",
      actionDescription = "Prettyprint the array access analysis to standard output.",
      actionProcedure = liftIO . putStrLn . prettyString . analyseDimAccesses
    }

-- | Print call graph to stdout.
callGraphAction :: Action SOACS
callGraphAction =
  Action
    { actionName = "call-graph",
      actionDescription = "Prettyprint the callgraph of the result to standard output.",
      actionProcedure = liftIO . putStrLn . prettyString . buildCallGraph
    }

-- | Print metrics about AST node counts to stdout.
metricsAction :: (OpMetrics (Op rep)) => Action rep
metricsAction =
  Action
    { actionName = "Compute metrics",
      actionDescription = "Print metrics on the final AST.",
      actionProcedure = liftIO . putStr . show . progMetrics
    }

-- | Convert the program to sequential ImpCode and print it to stdout.
impCodeGenAction :: Action SeqMem
impCodeGenAction =
  Action
    { actionName = "Compile imperative",
      actionDescription = "Translate program into imperative IL and write it on standard output.",
      actionProcedure = liftIO . putStrLn . prettyString . snd <=< ImpGenSequential.compileProg
    }

-- | Convert the program to GPU ImpCode and print it to stdout.
kernelImpCodeGenAction :: Action GPUMem
kernelImpCodeGenAction =
  Action
    { actionName = "Compile imperative kernels",
      actionDescription = "Translate program into imperative IL with kernels and write it on standard output.",
      actionProcedure = liftIO . putStrLn . prettyString . snd <=< ImpGenGPU.compileProgHIP
    }

-- | Convert the program to CPU multicore ImpCode and print it to stdout.
multicoreImpCodeGenAction :: Action MCMem
multicoreImpCodeGenAction =
  Action
    { actionName = "Compile to imperative multicore",
      actionDescription = "Translate program into imperative multicore IL and write it on standard output.",
      actionProcedure = liftIO . putStrLn . prettyString . snd <=< ImpGenMulticore.compileProg
    }

-- Lines that we prepend (in comments) to generated code.
headerLines :: [T.Text]
headerLines = T.lines $ "Generated by Futhark " <> versionString

cHeaderLines :: [T.Text]
cHeaderLines = map ("// " <>) headerLines

pyHeaderLines :: [T.Text]
pyHeaderLines = map ("# " <>) headerLines

cPrependHeader :: T.Text -> T.Text
cPrependHeader = (T.unlines cHeaderLines <>)

pyPrependHeader :: T.Text -> T.Text
pyPrependHeader = (T.unlines pyHeaderLines <>)

cmdCC :: String
-- TODO: new action instead?
cmdCC = fromMaybe "cc" $ lookup "CC" unixEnvironment

cmdCFLAGS :: [String] -> [String]
cmdCFLAGS def = maybe def words $ lookup "CFLAGS" unixEnvironment

cmdISPCFLAGS :: [String] -> [String]
cmdISPCFLAGS def = maybe def words $ lookup "ISPCFLAGS" unixEnvironment

runCC :: String -> String -> [String] -> [String] -> FutharkM ()
runCC cpath outpath cflags_def ldflags = do
  ret <-
    liftIO $
      runProgramWithExitCode
        cmdCC
        ( [cpath, "-o", outpath]
            ++ cmdCFLAGS cflags_def
            ++
            -- The default LDFLAGS are always added.
            ldflags
        )
        mempty
  case ret of
    Left err ->
      externalErrorS $ "Failed to run " ++ cmdCC ++ ": " ++ show err
    Right (ExitFailure code, _, gccerr) ->
      externalErrorS $
        cmdCC
          ++ " failed with code "
          ++ show code
          ++ ":\n"
          ++ gccerr
    Right (ExitSuccess, _, _) ->
      pure ()

runISPC :: String -> String -> String -> String -> [String] -> [String] -> [String] -> FutharkM ()
runISPC ispcpath outpath cpath ispcextension ispc_flags cflags_def ldflags = do
  ret_ispc <-
    liftIO $
      runProgramWithExitCode
        cmdISPC
        ( [ispcpath, "-o", ispcbase `addExtension` "o"]
            ++ ["--addressing=64", "--pic"]
            ++ cmdISPCFLAGS ispc_flags -- These flags are always needed
        )
        mempty
  ret <-
    liftIO $
      runProgramWithExitCode
        cmdCC
        ( [ispcbase `addExtension` "o"]
            ++ [cpath, "-o", outpath]
            ++ cmdCFLAGS cflags_def
            ++
            -- The default LDFLAGS are always added.
            ldflags
        )
        mempty
  case ret_ispc of
    Left err ->
      externalErrorS $ "Failed to run " ++ cmdISPC ++ ": " ++ show err
    Right (ExitFailure code, _, ispcerr) -> throwError cmdISPC code ispcerr
    Right (ExitSuccess, _, _) ->
      case ret of
        Left err ->
          externalErrorS $ "Failed to run ispc: " ++ show err
        Right (ExitFailure code, _, gccerr) -> throwError cmdCC code gccerr
        Right (ExitSuccess, _, _) ->
          pure ()
  where
    cmdISPC = "ispc"
    ispcbase = outpath <> ispcextension
    throwError prog code err =
      externalErrorS $
        prog
          ++ " failed with code "
          ++ show code
          ++ ":\n"
          ++ err

-- | The @futhark c@ action.
compileCAction :: FutharkConfig -> CompilerMode -> FilePath -> Action SeqMem
compileCAction fcfg mode outpath =
  Action
    { actionName = "Compile to sequential C",
      actionDescription = "Compile to sequential C",
      actionProcedure = helper
    }
  where
    helper prog = do
      cprog <- handleWarnings fcfg $ SequentialC.compileProg versionString prog
      let cpath = outpath `addExtension` "c"
          hpath = outpath `addExtension` "h"
          jsonpath = outpath `addExtension` "json"

      case mode of
        ToLibrary -> do
          let (header, impl, manifest) = SequentialC.asLibrary cprog
          liftIO $ T.writeFile hpath $ cPrependHeader header
          liftIO $ T.writeFile cpath $ cPrependHeader impl
          liftIO $ T.writeFile jsonpath manifest
        ToExecutable -> do
          liftIO $ T.writeFile cpath $ SequentialC.asExecutable cprog
          runCC cpath outpath ["-O3", "-std=c99"] ["-lm"]
        ToServer -> do
          liftIO $ T.writeFile cpath $ SequentialC.asServer cprog
          runCC cpath outpath ["-O3", "-std=c99"] ["-lm"]

-- | The @futhark opencl@ action.
compileOpenCLAction :: FutharkConfig -> CompilerMode -> FilePath -> Action GPUMem
compileOpenCLAction fcfg mode outpath =
  Action
    { actionName = "Compile to OpenCL",
      actionDescription = "Compile to OpenCL",
      actionProcedure = helper
    }
  where
    helper prog = do
      cprog <- handleWarnings fcfg $ COpenCL.compileProg versionString prog
      let cpath = outpath `addExtension` "c"
          hpath = outpath `addExtension` "h"
          jsonpath = outpath `addExtension` "json"
          extra_options
            | System.Info.os == "darwin" =
                ["-framework", "OpenCL"]
            | System.Info.os == "mingw32" =
                ["-lOpenCL64"]
            | otherwise =
                ["-lOpenCL"]

      case mode of
        ToLibrary -> do
          let (header, impl, manifest) = COpenCL.asLibrary cprog
          liftIO $ T.writeFile hpath $ cPrependHeader header
          liftIO $ T.writeFile cpath $ cPrependHeader impl
          liftIO $ T.writeFile jsonpath manifest
        ToExecutable -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ COpenCL.asExecutable cprog
          runCC cpath outpath ["-O", "-std=c99"] ("-lm" : extra_options)
        ToServer -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ COpenCL.asServer cprog
          runCC cpath outpath ["-O", "-std=c99"] ("-lm" : extra_options)

-- | The @futhark cuda@ action.
compileCUDAAction :: FutharkConfig -> CompilerMode -> FilePath -> Action GPUMem
compileCUDAAction fcfg mode outpath =
  Action
    { actionName = "Compile to CUDA",
      actionDescription = "Compile to CUDA",
      actionProcedure = helper
    }
  where
    helper prog = do
      cprog <- handleWarnings fcfg $ CCUDA.compileProg versionString prog
      let cpath = outpath `addExtension` "c"
          hpath = outpath `addExtension` "h"
          jsonpath = outpath `addExtension` "json"
          extra_options =
            [ "-lcuda",
              "-lcudart",
              "-lnvrtc"
            ]
      case mode of
        ToLibrary -> do
          let (header, impl, manifest) = CCUDA.asLibrary cprog
          liftIO $ T.writeFile hpath $ cPrependHeader header
          liftIO $ T.writeFile cpath $ cPrependHeader impl
          liftIO $ T.writeFile jsonpath manifest
        ToExecutable -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ CCUDA.asExecutable cprog
          runCC cpath outpath ["-O", "-std=c99"] ("-lm" : extra_options)
        ToServer -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ CCUDA.asServer cprog
          runCC cpath outpath ["-O", "-std=c99"] ("-lm" : extra_options)

-- | The @futhark hip@ action.
compileHIPAction :: FutharkConfig -> CompilerMode -> FilePath -> Action GPUMem
compileHIPAction fcfg mode outpath =
  Action
    { actionName = "Compile to HIP",
      actionDescription = "Compile to HIP",
      actionProcedure = helper
    }
  where
    helper prog = do
      cprog <- handleWarnings fcfg $ HIP.compileProg versionString prog
      let cpath = outpath `addExtension` "c"
          hpath = outpath `addExtension` "h"
          jsonpath = outpath `addExtension` "json"
          extra_options =
            [ "-lamdhip64",
              "-lhiprtc"
            ]
      case mode of
        ToLibrary -> do
          let (header, impl, manifest) = HIP.asLibrary cprog
          liftIO $ T.writeFile hpath $ cPrependHeader header
          liftIO $ T.writeFile cpath $ cPrependHeader impl
          liftIO $ T.writeFile jsonpath manifest
        ToExecutable -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ HIP.asExecutable cprog
          runCC cpath outpath ["-O", "-std=c99"] ("-lm" : extra_options)
        ToServer -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ HIP.asServer cprog
          runCC cpath outpath ["-O", "-std=c99"] ("-lm" : extra_options)

-- | The @futhark multicore@ action.
compileMulticoreAction :: FutharkConfig -> CompilerMode -> FilePath -> Action MCMem
compileMulticoreAction fcfg mode outpath =
  Action
    { actionName = "Compile to multicore",
      actionDescription = "Compile to multicore",
      actionProcedure = helper
    }
  where
    helper prog = do
      cprog <- handleWarnings fcfg $ MulticoreC.compileProg versionString prog
      let cpath = outpath `addExtension` "c"
          hpath = outpath `addExtension` "h"
          jsonpath = outpath `addExtension` "json"

      case mode of
        ToLibrary -> do
          let (header, impl, manifest) = MulticoreC.asLibrary cprog
          liftIO $ T.writeFile hpath $ cPrependHeader header
          liftIO $ T.writeFile cpath $ cPrependHeader impl
          liftIO $ T.writeFile jsonpath manifest
        ToExecutable -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ MulticoreC.asExecutable cprog
          runCC cpath outpath ["-O3", "-std=c99"] ["-lm", "-pthread"]
        ToServer -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ MulticoreC.asServer cprog
          runCC cpath outpath ["-O3", "-std=c99"] ["-lm", "-pthread"]

-- | The @futhark ispc@ action.
compileMulticoreToISPCAction :: FutharkConfig -> CompilerMode -> FilePath -> Action MCMem
compileMulticoreToISPCAction fcfg mode outpath =
  Action
    { actionName = "Compile to multicore ISPC",
      actionDescription = "Compile to multicore ISPC",
      actionProcedure = helper
    }
  where
    helper prog = do
      let cpath = outpath `addExtension` "c"
          hpath = outpath `addExtension` "h"
          jsonpath = outpath `addExtension` "json"
          ispcpath = outpath `addExtension` "kernels.ispc"
          ispcextension = "_ispc"
      (cprog, ispc) <- handleWarnings fcfg $ MulticoreISPC.compileProg versionString prog
      case mode of
        ToLibrary -> do
          let (header, impl, manifest) = MulticoreC.asLibrary cprog
          liftIO $ T.writeFile hpath $ cPrependHeader header
          liftIO $ T.writeFile cpath $ cPrependHeader impl
          liftIO $ T.writeFile ispcpath ispc
          liftIO $ T.writeFile jsonpath manifest
        ToExecutable -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ MulticoreC.asExecutable cprog
          liftIO $ T.writeFile ispcpath ispc
          runISPC ispcpath outpath cpath ispcextension ["-O3", "--woff"] ["-O3", "-std=c99"] ["-lm", "-pthread"]
        ToServer -> do
          liftIO $ T.writeFile cpath $ cPrependHeader $ MulticoreC.asServer cprog
          liftIO $ T.writeFile ispcpath ispc
          runISPC ispcpath outpath cpath ispcextension ["-O3", "--woff"] ["-O3", "-std=c99"] ["-lm", "-pthread"]

pythonCommon ::
  (CompilerMode -> String -> prog -> FutharkM (Warnings, T.Text)) ->
  FutharkConfig ->
  CompilerMode ->
  FilePath ->
  prog ->
  FutharkM ()
pythonCommon codegen fcfg mode outpath prog = do
  let class_name =
        case mode of
          ToLibrary -> takeBaseName outpath
          _ -> "internal"
  pyprog <- handleWarnings fcfg $ codegen mode class_name prog

  case mode of
    ToLibrary ->
      liftIO $ T.writeFile (outpath `addExtension` "py") $ pyPrependHeader pyprog
    _ -> liftIO $ do
      T.writeFile outpath $ "#!/usr/bin/env python3\n" <> pyPrependHeader pyprog
      perms <- liftIO $ getPermissions outpath
      setPermissions outpath $ setOwnerExecutable True perms

-- | The @futhark python@ action.
compilePythonAction :: FutharkConfig -> CompilerMode -> FilePath -> Action SeqMem
compilePythonAction fcfg mode outpath =
  Action
    { actionName = "Compile to PyOpenCL",
      actionDescription = "Compile to Python with OpenCL",
      actionProcedure = pythonCommon SequentialPy.compileProg fcfg mode outpath
    }

-- | The @futhark pyopencl@ action.
compilePyOpenCLAction :: FutharkConfig -> CompilerMode -> FilePath -> Action GPUMem
compilePyOpenCLAction fcfg mode outpath =
  Action
    { actionName = "Compile to PyOpenCL",
      actionDescription = "Compile to Python with OpenCL",
      actionProcedure = pythonCommon PyOpenCL.compileProg fcfg mode outpath
    }

cmdEMCFLAGS :: [String] -> [String]
cmdEMCFLAGS def = maybe def words $ lookup "EMCFLAGS" unixEnvironment

runEMCC :: String -> String -> FilePath -> [String] -> [String] -> [String] -> Bool -> FutharkM ()
runEMCC cpath outpath classpath cflags_def ldflags expfuns lib = do
  ret <-
    liftIO $
      runProgramWithExitCode
        "emcc"
        ( [cpath, "-o", outpath]
            ++ ["-lnodefs.js"]
            ++ ["-s", "--extern-post-js", classpath]
            ++ ( if lib
                   then ["-s", "EXPORT_NAME=loadWASM"]
                   else []
               )
            ++ ["-s", "WASM_BIGINT"]
            ++ cmdCFLAGS cflags_def
            ++ cmdEMCFLAGS [""]
            ++ [ "-s",
                 "EXPORTED_FUNCTIONS=["
                   ++ intercalate "," ("'_malloc'" : "'_free'" : expfuns)
                   ++ "]"
               ]
            -- The default LDFLAGS are always added.
            ++ ldflags
        )
        mempty
  case ret of
    Left err ->
      externalErrorS $ "Failed to run emcc: " ++ show err
    Right (ExitFailure code, _, emccerr) ->
      externalErrorS $
        "emcc failed with code "
          ++ show code
          ++ ":\n"
          ++ emccerr
    Right (ExitSuccess, _, _) ->
      pure ()

-- | The @futhark wasm@ action.
compileCtoWASMAction :: FutharkConfig -> CompilerMode -> FilePath -> Action SeqMem
compileCtoWASMAction fcfg mode outpath =
  Action
    { actionName = "Compile to sequential C",
      actionDescription = "Compile to sequential C",
      actionProcedure = helper
    }
  where
    helper prog = do
      (cprog, jsprog, exps) <-
        handleWarnings fcfg $ SequentialWASM.compileProg versionString prog
      case mode of
        ToLibrary -> do
          writeLibs cprog jsprog
          liftIO $ T.appendFile classpath SequentialWASM.libraryExports
          runEMCC cpath mjspath classpath ["-O3", "-msimd128"] ["-lm"] exps True
        _ -> do
          -- Non-server executables are not supported.
          writeLibs cprog jsprog
          liftIO $ T.appendFile classpath SequentialWASM.runServer
          runEMCC cpath outpath classpath ["-O3", "-msimd128"] ["-lm"] exps False
    writeLibs cprog jsprog = do
      let (h, imp, _) = SequentialC.asLibrary cprog
      liftIO $ T.writeFile hpath h
      liftIO $ T.writeFile cpath imp
      liftIO $ T.writeFile classpath jsprog

    cpath = outpath `addExtension` "c"
    hpath = outpath `addExtension` "h"
    mjspath = outpath `addExtension` "mjs"
    classpath = outpath `addExtension` ".class.js"

-- | The @futhark wasm-multicore@ action.
compileMulticoreToWASMAction :: FutharkConfig -> CompilerMode -> FilePath -> Action MCMem
compileMulticoreToWASMAction fcfg mode outpath =
  Action
    { actionName = "Compile to sequential C",
      actionDescription = "Compile to sequential C",
      actionProcedure = helper
    }
  where
    helper prog = do
      (cprog, jsprog, exps) <-
        handleWarnings fcfg $ MulticoreWASM.compileProg versionString prog

      case mode of
        ToLibrary -> do
          writeLibs cprog jsprog
          liftIO $ T.appendFile classpath MulticoreWASM.libraryExports
          runEMCC cpath mjspath classpath ["-O3", "-msimd128"] ["-lm", "-pthread"] exps True
        _ -> do
          -- Non-server executables are not supported.
          writeLibs cprog jsprog
          liftIO $ T.appendFile classpath MulticoreWASM.runServer
          runEMCC cpath outpath classpath ["-O3", "-msimd128"] ["-lm", "-pthread"] exps False

    writeLibs cprog jsprog = do
      let (h, imp, _) = MulticoreC.asLibrary cprog
      liftIO $ T.writeFile hpath h
      liftIO $ T.writeFile cpath imp
      liftIO $ T.writeFile classpath jsprog

    cpath = outpath `addExtension` "c"
    hpath = outpath `addExtension` "h"
    mjspath = outpath `addExtension` "mjs"
    classpath = outpath `addExtension` ".class.js"
