module Futhark.CLI.Script (main) where

import Control.Monad.Except
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.CLI.Literate
  ( Options (..),
    initialOptions,
    prepareServer,
    scriptCommandLineOptions,
  )
import Futhark.Script
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText)
import System.Exit
import System.IO

commandLineOptions :: [FunOptDescr Options]
commandLineOptions =
  scriptCommandLineOptions
    ++ [ Option
           "D"
           ["debug"]
           ( NoArg $ Right $ \config ->
               config
                 { scriptExtraOptions = "-D" : scriptExtraOptions config,
                   scriptVerbose = scriptVerbose config + 1
                 }
           )
           "Enable debugging.",
         Option
           "L"
           ["log"]
           ( NoArg $ Right $ \config ->
               config
                 { scriptExtraOptions = "-L" : scriptExtraOptions config,
                   scriptVerbose = scriptVerbose config + 1
                 }
           )
           "Enable logging."
       ]

-- | Run @futhark script@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialOptions commandLineOptions "program script" $ \args opts ->
  case args of
    [prog, script] -> Just $ do
      script' <- case parseExpFromText "command line argument" $ T.pack script of
        Left e -> do
          T.hPutStrLn stderr e
          exitFailure
        Right x -> pure x
      prepareServer prog opts $ \s -> do
        r <-
          runExceptT $ getExpValue s =<< evalExp (scriptBuiltin ".") s script'
        case r of
          Left e -> do
            T.hPutStrLn stderr e
            exitFailure
          Right v ->
            T.putStrLn $ prettyText v
    _ -> Nothing
