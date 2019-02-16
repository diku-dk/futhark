{-# LANGUAGE FlexibleContexts #-}
-- Various small subcommands that are too simple to deserve their own file.
module Futhark.CLI.Misc
  ( mainCheck
  , imports
  )
where

import Data.List (isPrefixOf)
import Control.Monad.State
import System.IO
import System.Exit

import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Pipeline

runFutharkM' :: FutharkM () -> IO ()
runFutharkM' m = do
  res <- runFutharkM m NotVerbose
  case res of
    Left err -> do
      dumpError newFutharkConfig err
      exitWith $ ExitFailure 2
    Right () -> return ()

mainCheck :: String -> [String] -> IO ()
mainCheck = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ runFutharkM' $ check file
    _ -> Nothing
  where check file = do (warnings, _, _) <- readProgram file
                        liftIO $ hPutStr stderr $ show warnings

imports :: String -> [String] -> IO ()
imports = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ runFutharkM' $ findImports file
    _ -> Nothing
  where findImports file = do
          (_, prog_imports, _) <- readProgram file
          liftIO $ putStrLn $ unwords $ map (++ ".fut")
            $ filter (\f -> not (("futlib/" `isPrefixOf` f) ||
                                 ("lib/" `isPrefixOf` f)))
            $ map fst prog_imports
