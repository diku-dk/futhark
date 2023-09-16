-- | Common code for parsing command line options based on getopt.
module Futhark.Util.Options
  ( FunOptDescr,
    mainWithOptions,
    commonOptions,
    optionsError,
    module System.Console.GetOpt,
  )
where

import Control.Monad.IO.Class
import Data.List (sortBy)
import Data.Text.IO qualified as T
import Futhark.Version
import System.Console.GetOpt
import System.Environment (getProgName)
import System.Exit
import System.IO

-- | A command line option that either purely updates a configuration,
-- or performs an IO action (and stops).
type FunOptDescr cfg = OptDescr (Either (IO ()) (cfg -> cfg))

-- | Generate a main action that parses the given command line options
-- (while always adding 'commonOptions').
mainWithOptions ::
  cfg ->
  [FunOptDescr cfg] ->
  String ->
  ([String] -> cfg -> Maybe (IO ())) ->
  String ->
  [String] ->
  IO ()
mainWithOptions emptyConfig commandLineOptions usage f prog args =
  case getOpt' Permute commandLineOptions' args of
    (opts, nonopts, [], []) ->
      case applyOpts opts of
        Right config
          | Just m <- f nonopts config -> m
          | otherwise -> invalid nonopts [] []
        Left m -> m
    (_, nonopts, unrecs, errs) -> invalid nonopts unrecs errs
  where
    applyOpts opts = do
      fs <- sequence opts
      pure $ foldl (.) id (reverse fs) emptyConfig

    invalid nonopts unrecs errs = do
      help <- helpStr prog usage commandLineOptions'
      badOptions help nonopts errs unrecs

    commandLineOptions' =
      commonOptions prog usage commandLineOptions ++ commandLineOptions

helpStr :: String -> String -> [OptDescr a] -> IO String
helpStr prog usage opts = do
  let header = unlines ["Usage: " ++ prog ++ " " ++ usage, "Options:"]
  pure $ usageInfo header $ sortBy cmp opts
  where
    -- Sort first by long option, then by short name, then by description.  Hopefully
    -- everything has a long option.
    cmp (Option _ (a : _) _ _) (Option _ (b : _) _ _) = compare a b
    cmp (Option (a : _) _ _ _) (Option (b : _) _ _ _) = compare a b
    cmp (Option _ _ _ a) (Option _ _ _ b) = compare a b

badOptions :: String -> [String] -> [String] -> [String] -> IO ()
badOptions usage nonopts errs unrecs = do
  mapM_ (errput . ("Junk argument: " ++)) nonopts
  mapM_ (errput . ("Unrecognised argument: " ++)) unrecs
  hPutStr stderr $ concat errs ++ usage
  exitWith $ ExitFailure 1

-- | Short-hand for 'liftIO . hPutStrLn stderr'
errput :: (MonadIO m) => String -> m ()
errput = liftIO . hPutStrLn stderr

-- | Common definitions for @-v@ and @-h@, given the list of all other
-- options.
commonOptions :: String -> String -> [FunOptDescr cfg] -> [FunOptDescr cfg]
commonOptions prog usage options =
  [ Option
      "V"
      ["version"]
      ( NoArg $
          Left $ do
            header
            exitSuccess
      )
      "Print version information and exit.",
    Option
      "h"
      ["help"]
      ( NoArg $
          Left $ do
            header
            putStrLn ""
            putStrLn =<< helpStr prog usage (commonOptions prog usage [] ++ options)
            exitSuccess
      )
      "Print help and exit."
  ]
  where
    header = do
      T.putStrLn $ "Futhark " <> versionString
      T.putStrLn "Copyright (C) DIKU, University of Copenhagen, released under the ISC license."
      T.putStrLn "This is free software: you are free to change and redistribute it."
      T.putStrLn "There is NO WARRANTY, to the extent permitted by law."

-- | Terminate the program with this error message (but don't report
-- it as an ICE, as happens with 'error').
optionsError :: String -> IO ()
optionsError s = do
  prog <- getProgName
  hPutStrLn stderr $ prog <> ": " <> s
  exitWith $ ExitFailure 2
