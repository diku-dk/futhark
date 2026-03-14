module Futhark.CLI.Verify (main) where

import Futhark.Analysis.Properties.Convert (mkIndexFnProg)
import Futhark.Compiler
import Futhark.Util.Options
import System.Environment

-- | The configuration for the futhark verify command.
data VerifyConfig = VerifyConfig {}

-- | Initial configuration.
initialVerifyConfig :: VerifyConfig
initialVerifyConfig = VerifyConfig {}

-- | Command line options.
commandLineOptions :: [FunOptDescr VerifyConfig]
commandLineOptions = []

-- | Run @futhark verify@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialVerifyConfig commandLineOptions "program" $ \args _config ->
  case args of
    [file] -> Just $ do
      setEnv "FUTHARK_INDEXFN" "1"
      (_warnings, imports, vns) <- readProgramOrDie file
      let result = mkIndexFnProg vns (reverse imports)
      result `seq` pure ()
    _ ->
      Nothing
