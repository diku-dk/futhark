{-# LANGUAGE FlexibleContexts #-}
-- Various small subcommands that are too simple to deserve their own file.
module Futhark.CLI.Misc
  ( mainCheck
  , mainImports
  , mainDataget
  )
where

import qualified Data.ByteString.Lazy as BS
import Data.List (isPrefixOf, isInfixOf, nubBy)
import Data.Function (on)
import Control.Monad.State
import System.FilePath
import System.IO
import System.Exit

import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Pipeline
import Futhark.Test

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

mainImports :: String -> [String] -> IO ()
mainImports = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ runFutharkM' $ findImports file
    _ -> Nothing
  where findImports file = do
          (_, prog_imports, _) <- readProgram file
          liftIO $ putStr $ unlines $ map (++ ".fut")
            $ filter (\f -> not ("futlib/" `isPrefixOf` f))
            $ map fst prog_imports

mainDataget :: String -> [String] -> IO ()
mainDataget = mainWithOptions () [] "program dataset" $ \args () ->
  case args of
    [file, dataset] -> Just $ dataget file dataset
    _ -> Nothing
  where dataget prog dataset = do
          let dir = takeDirectory prog

          runs <- testSpecRuns <$> testSpecFromFile prog

          let exact = filter ((dataset==) . runDescription) runs
              infixes = filter ((dataset `isInfixOf`) . runDescription) runs

          case nubBy ((==) `on` runDescription) $
               if null exact then infixes else exact of
            [x] -> BS.putStr =<< getValuesBS dir (runInput x)

            [] -> do hPutStr stderr $ "No dataset '" ++ dataset ++ "'.\n"
                     hPutStr stderr "Available datasets:\n"
                     mapM_ (hPutStrLn stderr . ("  "++) . runDescription) runs
                     exitFailure

            runs' -> do hPutStr stderr $ "Dataset '" ++ dataset ++ "' ambiguous:\n"
                        mapM_ (hPutStrLn stderr . ("  "++) . runDescription) runs'
                        exitFailure

        testSpecRuns = testActionRuns . testAction
        testActionRuns CompileTimeFailure{} = []
        testActionRuns (RunCases ios _ _) = concatMap iosTestRuns ios
