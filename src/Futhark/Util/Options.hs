-- | Common code for parsing command line options based on getopt.
module Futhark.Util.Options
       ( FunOptDescr
       , mainWithOptions
       ) where

import Data.Version
import System.Environment
import Control.Monad.IO.Class
import System.IO
import System.Exit
import System.Console.GetOpt

import Futhark.Version

type FunOptDescr cfg = OptDescr (Either (IO ()) (cfg -> cfg))

mainWithOptions :: cfg
                -> [FunOptDescr cfg]
                -> ([String] -> cfg -> Maybe (IO ()))
                -> IO ()
mainWithOptions emptyConfig commandLineOptions f = do
  args <- getArgs
  case getOpt' Permute commandLineOptions args of
    (opts, nonopts, [], []) ->
      case applyOpts opts of
        Right config
          | Just m <- f nonopts config -> m
          | otherwise                  -> invalid nonopts [] []
        Left m       -> m
    (_, nonopts, unrecs, errs) -> invalid nonopts unrecs errs
  where applyOpts opts = do fs <- sequence opts
                            return $ foldl (.) id fs emptyConfig

        invalid nonopts unrecs errs = do usage <- usageStr commandLineOptions
                                         badOptions usage nonopts errs unrecs

usageStr :: [OptDescr a] -> IO String
usageStr opts = do
  prog <- getProgName
  let header = "Help for " ++ prog ++ " (Futhark " ++ showVersion version ++ ")"
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
