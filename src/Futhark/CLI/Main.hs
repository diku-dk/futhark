-- | The main function for the @futhark@ command line program.
module Futhark.CLI.Main (main) where

import Control.Exception
import Data.List (sortOn)
import Data.Maybe
import Data.Text.IO qualified as T
import Futhark.CLI.Autotune qualified as Autotune
import Futhark.CLI.Bench qualified as Bench
import Futhark.CLI.Benchcmp qualified as Benchcmp
import Futhark.CLI.C qualified as C
import Futhark.CLI.CUDA qualified as CCUDA
import Futhark.CLI.Check qualified as Check
import Futhark.CLI.Datacmp qualified as Datacmp
import Futhark.CLI.Dataset qualified as Dataset
import Futhark.CLI.Defs qualified as Defs
import Futhark.CLI.Dev qualified as Dev
import Futhark.CLI.Doc qualified as Doc
import Futhark.CLI.Eval qualified as Eval
import Futhark.CLI.HIP qualified as HIP
import Futhark.CLI.LSP qualified as LSP
import Futhark.CLI.Literate qualified as Literate
import Futhark.CLI.Misc qualified as Misc
import Futhark.CLI.Multicore qualified as Multicore
import Futhark.CLI.MulticoreISPC qualified as MulticoreISPC
import Futhark.CLI.MulticoreWASM qualified as MulticoreWASM
import Futhark.CLI.OpenCL qualified as OpenCL
import Futhark.CLI.Pkg qualified as Pkg
import Futhark.CLI.Profile qualified as Profile
import Futhark.CLI.PyOpenCL qualified as PyOpenCL
import Futhark.CLI.Python qualified as Python
import Futhark.CLI.Query qualified as Query
import Futhark.CLI.REPL qualified as REPL
import Futhark.CLI.Run qualified as Run
import Futhark.CLI.Script qualified as Script
import Futhark.CLI.Test qualified as Test
import Futhark.CLI.WASM qualified as WASM
import Futhark.Error
import Futhark.Util (maxinum, showText)
import Futhark.Util.Options
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import System.Environment
import System.Exit
import System.IO
import Prelude

type Command = String -> [String] -> IO ()

commands :: [(String, (Command, String))]
commands =
  sortOn
    fst
    [ ("dev", (Dev.main, "Run compiler passes directly.")),
      ("eval", (Eval.main, "Evaluate Futhark expressions passed in as arguments")),
      ("repl", (REPL.main, "Run interactive Read-Eval-Print-Loop.")),
      ("run", (Run.main, "Run a program through the (slow!) interpreter.")),
      ("c", (C.main, "Compile to sequential C.")),
      ("opencl", (OpenCL.main, "Compile to C calling OpenCL.")),
      ("cuda", (CCUDA.main, "Compile to C calling CUDA.")),
      ("hip", (HIP.main, "Compile to C calling HIP.")),
      ("multicore", (Multicore.main, "Compile to multicore C.")),
      ("python", (Python.main, "Compile to sequential Python.")),
      ("pyopencl", (PyOpenCL.main, "Compile to Python calling PyOpenCL.")),
      ("wasm", (WASM.main, "Compile to WASM with sequential C")),
      ("wasm-multicore", (MulticoreWASM.main, "Compile to WASM with multicore C")),
      ("ispc", (MulticoreISPC.main, "Compile to multicore ISPC")),
      ("test", (Test.main, "Test Futhark programs.")),
      ("bench", (Bench.main, "Benchmark Futhark programs.")),
      ("dataset", (Dataset.main, "Generate random test data.")),
      ("datacmp", (Datacmp.main, "Compare Futhark data files for equality.")),
      ("dataget", (Misc.mainDataget, "Extract test data.")),
      ("doc", (Doc.main, "Generate documentation for Futhark code.")),
      ("pkg", (Pkg.main, "Manage local packages.")),
      ("check", (Check.main, "Type-check a program.")),
      ("check-syntax", (Misc.mainCheckSyntax, "Syntax-check a program.")),
      ("imports", (Misc.mainImports, "Print all non-builtin imported Futhark files.")),
      ("hash", (Misc.mainHash, "Print hash of program AST.")),
      ("autotune", (Autotune.main, "Autotune threshold parameters.")),
      ("defs", (Defs.main, "Show location and name of all definitions.")),
      ("query", (Query.main, "Query semantic information about program.")),
      ("literate", (Literate.main, "Process a literate Futhark program.")),
      ("script", (Script.main, "Run FutharkScript expressions.")),
      ("lsp", (LSP.main, "Run LSP server.")),
      ("thanks", (Misc.mainThanks, "Express gratitude.")),
      ("tokens", (Misc.mainTokens, "Print tokens from Futhark file.")),
      ("benchcmp", (Benchcmp.main, "Compare two benchmark results.")),
      ("profile", (Profile.main, "Analyse profiling data."))
    ]

msg :: String
msg =
  unlines $
    ["<command> options...", "Commands:", ""]
      ++ [ "   " <> cmd <> replicate (k - length cmd) ' ' <> desc
           | (cmd, (_, desc)) <- commands
         ]
  where
    k = maxinum (map (length . fst) commands) + 3

-- | Catch all IO exceptions and print a better error message if they
-- happen.
reportingIOErrors :: IO () -> IO ()
reportingIOErrors =
  flip
    catches
    [ Handler onExit,
      Handler onICE,
      Handler onIOException,
      Handler onError
    ]
  where
    onExit :: ExitCode -> IO ()
    onExit = throwIO

    onICE :: InternalError -> IO ()
    onICE (Error CompilerLimitation s) = do
      T.hPutStrLn stderr "Known compiler limitation encountered.  Sorry."
      T.hPutStrLn stderr "Revise your program or try a different Futhark compiler."
      T.hPutStrLn stderr s
      exitWith $ ExitFailure 1
    onICE (Error CompilerBug s) = do
      T.hPutStrLn stderr "Internal compiler error."
      T.hPutStrLn stderr "Please report this at https://github.com/diku-dk/futhark/issues."
      T.hPutStrLn stderr s
      exitWith $ ExitFailure 1

    onError :: SomeException -> IO ()
    onError e
      | Just UserInterrupt <- asyncExceptionFromException e =
          pure () -- This corresponds to CTRL-C, which is not an error.
      | otherwise = do
          T.hPutStrLn stderr "Internal compiler error (unhandled IO exception)."
          T.hPutStrLn stderr "Please report this at https://github.com/diku-dk/futhark/issues"
          T.hPutStrLn stderr $ showText e
          exitWith $ ExitFailure 1

    onIOException :: IOException -> IO ()
    onIOException e
      | ioe_type e == ResourceVanished =
          exitWith $ ExitFailure 1
      | otherwise = throw e

-- | The @futhark@ executable.
main :: IO ()
main = reportingIOErrors $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  setLocaleEncoding utf8
  args <- getArgs
  prog <- getProgName
  case args of
    cmd : args'
      | Just (m, _) <- lookup cmd commands -> m (unwords [prog, cmd]) args'
    _ -> mainWithOptions () [] msg (const . const Nothing) prog args
