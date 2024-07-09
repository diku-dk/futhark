-- | @futhark check@
module Futhark.CLI.Check (main) where

import Control.Monad
import Control.Monad.IO.Class
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Util.Pretty (hPutDoc)
import Language.Futhark.Warnings
import System.Exit
import System.IO

data CheckConfig = CheckConfig {checkWarn :: Bool, checkWerror :: Bool}

newCheckConfig :: CheckConfig
newCheckConfig = CheckConfig True False

options :: [FunOptDescr CheckConfig]
options =
  [ Option
      "w"
      []
      (NoArg $ Right $ \cfg -> cfg {checkWarn = False})
      "Disable all warnings.",
    Option
      []
      ["Werror"]
      (NoArg $ Right $ \cfg -> cfg {checkWerror = True})
      "Treat warnings as errors."
  ]

-- | Run @futhark check@.
main :: String -> [String] -> IO ()
main = mainWithOptions newCheckConfig options "program" $ \args cfg ->
  case args of
    [file] -> Just $ do
      (warnings, _, _) <- readProgramOrDie file
      when (checkWarn cfg && anyWarnings warnings) $ do
        liftIO $ hPutDoc stderr $ prettyWarnings warnings
        when (checkWerror cfg) $ do
          hPutStrLn stderr "\nTreating above warnings as errors due to --Werror."
          exitWith $ ExitFailure 2
    _ -> Nothing
