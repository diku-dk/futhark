module Futhark.CLI.Verify (main) where

import Futhark.Analysis.Properties.Convert (mkIndexFnProg)
import Futhark.Compiler
import Futhark.Util.Options
import System.Environment
import Data.Maybe (fromMaybe)

-- | The configuration for the futhark verify command.
newtype VerifyConfig = VerifyConfig {
  verifyIndexFnLevel :: Maybe String
}

-- | Initial configuration.
initialVerifyConfig :: VerifyConfig
initialVerifyConfig = VerifyConfig {
  verifyIndexFnLevel = Nothing
}

-- | Command line options.
commandLineOptions :: [FunOptDescr VerifyConfig]
commandLineOptions = [
  Option "f" ["log"]
    (ReqArg (\s -> Right $ \config -> config {verifyIndexFnLevel = Just s}) "LEVEL")
    "Set the log level (integer)."
  ]

-- | Run @futhark verify@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialVerifyConfig commandLineOptions "program" $ \args config ->
  case args of
    [file] -> Just $ do
      let indexFnLevel = fromMaybe "1" (verifyIndexFnLevel config)
      setEnv "FUTHARK_INDEXFN" indexFnLevel
      (_warnings, imports, vns) <- readProgramOrDie file
      let result = mkIndexFnProg vns (reverse imports)
      result `seq` pure ()
    _ ->
      Nothing
