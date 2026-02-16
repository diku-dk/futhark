-- | @futhark eval@
module Futhark.CLI.Eval (main) where

import Control.Monad
  ( forM_,
  )
import Data.Map qualified as M
import Futhark.Eval
  ( EvalIO (runEvalIO),
    InterpreterConfig (..),
    interpreterConfig,
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
main = mainWithOptions interpreterConfig options "options... <exprs...>" run
  where
    run [] _ = Nothing
    run exprs config = Just $ runExprs exprs config

runExprs :: [String] -> InterpreterConfig -> IO ()
runExprs exprs cfg = do
  let InterpreterConfig _ file = cfg
  maybe_new_state <- runEvalIO $ newFutharkiState cfg file M.empty
  interpreter_state <- case maybe_new_state of
    Left reason -> do
      hPutDocLn stderr reason
      exitWith $ ExitFailure 2
    Right s -> pure s
  forM_ exprs $ \expr -> putDocLn =<< runEvalIO (runExpr interpreter_state expr)

options :: [FunOptDescr InterpreterConfig]
options =
  [ Option
      "f"
      ["file"]
      ( ReqArg
          ( \entry -> Right $ \config ->
              config {interpreterFile = Just entry}
          )
          "NAME"
      )
      "The file to load before evaluating expressions.",
    Option
      "w"
      ["no-warnings"]
      (NoArg $ Right $ \config -> config {interpreterPrintWarnings = False})
      "Do not print warnings."
  ]
