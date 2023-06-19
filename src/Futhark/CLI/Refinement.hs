module Futhark.CLI.Refinement (main) where

import Control.Monad
import Control.Monad.IO.Class
import Futhark.Analysis.Refinement
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Util.Pretty (hPutDoc, prettyString)
import Language.Futhark.Warnings
import System.IO

data RefineConfig = RefineConfig
  { printSuccesses :: Bool,
    checkWarn :: Bool,
    printAlg :: Bool
  }

newRefineConfig :: RefineConfig
newRefineConfig = RefineConfig False True False

options :: [FunOptDescr RefineConfig]
options =
  [ Option
      "v"
      []
      (NoArg $ Right $ \cfg -> cfg {printSuccesses = True})
      "Print all checks.",
    Option
      "w"
      []
      (NoArg $ Right $ \cfg -> cfg {checkWarn = False})
      "Disable all typechecker warnings.",
    Option
      "a"
      []
      (NoArg $ Right $ \cfg -> cfg {printAlg = True})
      "Print the algebraic environment."
  ]

-- | Run @futhark refinement@.
main :: String -> [String] -> IO ()
main = mainWithOptions newRefineConfig options "program" $ \args cfg ->
  case args of
    [file] -> Just $ do
      (warnings, imps, vns) <- readProgramOrDie file
      when (checkWarn cfg && anyWarnings warnings) $
        liftIO $
          hPutDoc stderr $
            prettyWarnings warnings
      let (_, algenv, failed, checked) = refineProg vns imps
      putStrLn "Failed checks:"
      liftIO $ mapM_ putStrLn failed
      when (printSuccesses cfg) $ do
        putStrLn "Successful checks:"
        liftIO $ mapM_ putStrLn checked
      when (printAlg cfg) $ do
        liftIO $ putStrLn $ prettyString algenv
    _ -> Nothing
