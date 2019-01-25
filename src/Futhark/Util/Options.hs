-- | Common code for parsing command line options based on getopt.
module Futhark.Util.Options
       ( FunOptDescr
       , mainWithOptions
       , commonOptions
       ) where

import Control.Monad.IO.Class
import System.IO
import System.Exit
import System.Console.GetOpt

import Futhark.Version

-- | A command line option that either purely updates a configuration,
-- or performs an IO action (and stops).
type FunOptDescr cfg = OptDescr (Either (IO ()) (cfg -> cfg))

-- | Generate a main action that parses the given command line options
-- (while always adding 'commonOptions').
mainWithOptions :: cfg
                -> [FunOptDescr cfg]
                -> String
                -> ([String] -> cfg -> Maybe (IO ()))
                -> String
                -> [String]
                -> IO ()
mainWithOptions emptyConfig commandLineOptions usage f prog args =
  case getOpt' Permute commandLineOptions' args of
    (opts, nonopts, [], []) ->
      case applyOpts opts of
        Right config
          | Just m <- f nonopts config -> m
          | otherwise                  -> invalid nonopts [] []
        Left m       -> m
    (_, nonopts, unrecs, errs) -> invalid nonopts unrecs errs
  where applyOpts opts = do fs <- sequence opts
                            return $ foldl (.) id (reverse fs) emptyConfig

        invalid nonopts unrecs errs = do help <- helpStr prog usage commandLineOptions'
                                         badOptions help nonopts errs unrecs

        commandLineOptions' =
          commonOptions prog usage commandLineOptions ++ commandLineOptions

helpStr :: String -> String -> [OptDescr a] -> IO String
helpStr prog usage opts = do
  let header = unlines ["Usage: " ++ prog ++ " " ++ usage, "Options:"]
  return $ usageInfo header opts

badOptions :: String -> [String] -> [String] -> [String] -> IO ()
badOptions usage nonopts errs unrecs = do
  mapM_ (errput . ("Junk argument: " ++)) nonopts
  mapM_ (errput . ("Unrecognised argument: " ++)) unrecs
  hPutStr stderr $ concat errs ++ usage
  exitWith $ ExitFailure 1

-- | Short-hand for 'liftIO . hPutStrLn stderr'
errput :: MonadIO m => String -> m ()
errput = liftIO . hPutStrLn stderr

-- | Common definitions for @-v@ and @-h@, given the list of all other
-- options.
commonOptions :: String -> String -> [FunOptDescr cfg] -> [FunOptDescr cfg]
commonOptions prog usage options =
  [ Option "V" ["version"]
    (NoArg $ Left $ do header
                       exitSuccess)
    "Print version information and exit."

  , Option "h" ["help"]
    (NoArg $ Left $ do header
                       putStrLn ""
                       putStrLn =<< helpStr prog usage (commonOptions prog usage [] ++ options)
                       exitSuccess)
    "Print help and exit."
  ]
  where header = do
          putStrLn $ "Futhark " ++ versionString
          putStrLn "Copyright (C) DIKU, University of Copenhagen, released under the ISC license."
          putStrLn "This is free software: you are free to change and redistribute it."
          putStrLn "There is NO WARRANTY, to the extent permitted by law."
