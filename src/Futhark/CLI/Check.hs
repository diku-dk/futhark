module Futhark.CLI.Check (main) where

import Control.Monad
import Control.Monad.IO.Class
import System.Console.GetOpt
import System.IO

import Futhark.Compiler
import Futhark.Util.Options

newtype CheckConfig = CheckConfig { checkWarn :: Bool }

newCheckConfig :: CheckConfig
newCheckConfig = CheckConfig True

options :: [FunOptDescr CheckConfig]
options = [Option "w" []
           (NoArg $ Right $ \cfg -> cfg { checkWarn = False })
           "Disable all warnings."]

main :: String -> [String] -> IO ()
main = mainWithOptions newCheckConfig options "program" $ \args cfg ->
  case args of
    [file] -> Just $ do
      (warnings, _, _) <- readProgramOrDie file
      when (checkWarn cfg) $
        liftIO $ hPutStr stderr $ show warnings
    _ -> Nothing
