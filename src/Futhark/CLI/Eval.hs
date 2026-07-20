-- | @futhark eval@
module Futhark.CLI.Eval (main) where

import Control.Monad
  ( forM_,
  )
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Eval
  ( EvalConfig (..),
    evalConfig,
    newFutharkiState,
    runExpr,
  )
import Futhark.Util.Options
  ( ArgDescr (NoArg, ReqArg),
    FunOptDescr,
    OptDescr (Option),
    mainWithOptions,
  )
import Futhark.Util.Pretty
  ( hPutDocLn,
    putDocLn,
  )
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

-- | Run @futhark eval@.
main :: String -> [String] -> IO ()
main = mainWithOptions evalConfig options "options... <exprs...>" run
  where
    run [] _ = Nothing
    run exprs config = Just $ runExprs exprs config

runExprs :: [String] -> EvalConfig -> IO ()
runExprs exprs cfg = do
  maybe_new_state <- newFutharkiState cfg M.empty
  interpreter_state <- case maybe_new_state of
    Left reason -> do
      hPutDocLn stderr reason
      exitWith $ ExitFailure 2
    Right s -> pure s
  forM_ exprs $ \expr -> putDocLn =<< runExpr interpreter_state (T.pack expr)

options :: [FunOptDescr EvalConfig]
options =
  [ Option
      "f"
      ["file"]
      ( ReqArg
          (\entry -> Right $ \config -> config {evalFile = Just entry})
          "NAME"
      )
      "The file to load before evaluating expressions.",
    Option
      "w"
      ["no-warnings"]
      (NoArg $ Right $ \config -> config {evalPrintWarnings = False})
      "Do not print warnings.",
    Option
      "p"
      ["pass-option"]
      ( ReqArg
          ( \opt ->
              Right $ \config ->
                config {evalExtraOptions = opt : evalExtraOptions config}
          )
          "OPT"
      )
      "Pass this option to programs being run.",
    Option
      []
      ["pass-compiler-option"]
      ( ReqArg
          ( \opt ->
              Right $ \config ->
                config {evalCompilerOptions = opt : evalCompilerOptions config}
          )
          "OPT"
      )
      "Pass this option to the compiler.",
    Option
      ""
      ["skip-compilation"]
      (NoArg $ Right $ \config -> config {evalSkipCompilation = True})
      "Use already compiled server-mode program.",
    Option
      []
      ["backend"]
      ( ReqArg
          (\backend -> Right $ \config -> config {evalBackend = Just backend})
          "BACKEND"
      )
      "The compiler backend used (defaults to interpreted)."
  ]
