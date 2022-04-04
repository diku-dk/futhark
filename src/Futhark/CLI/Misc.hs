{-# LANGUAGE FlexibleContexts #-}

-- | Various small subcommands that are too simple to deserve their own file.
module Futhark.CLI.Misc
  ( mainImports,
    mainHash,
    mainDataget,
    mainCheckSyntax,
    mainThanks,
  )
where

import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import Data.Function (on)
import Data.List (isInfixOf, nubBy)
import qualified Data.Text.IO as T
import Futhark.Compiler
import Futhark.Test
import Futhark.Util (hashText)
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyTextOneLine)
import Language.Futhark.Prop (isBuiltin)
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath
import System.IO
import System.Random

-- | @futhark imports@
mainImports :: String -> [String] -> IO ()
mainImports = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      (_, prog_imports, _) <- readProgramOrDie file
      liftIO . putStr . unlines . map (++ ".fut") . filter (not . isBuiltin) $
        map fst prog_imports
    _ -> Nothing

-- | @futhark hash@
mainHash :: String -> [String] -> IO ()
mainHash = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      prog <- filter (not . isBuiltin . fst) <$> readUntypedProgramOrDie file
      -- The 'map snd' is an attempt to get rid of the file names so
      -- they won't affect the hashing.
      liftIO $ T.putStrLn $ hashText $ prettyTextOneLine $ map snd prog
    _ -> Nothing

-- | @futhark dataget@
mainDataget :: String -> [String] -> IO ()
mainDataget = mainWithOptions () [] "program dataset" $ \args () ->
  case args of
    [file, dataset] -> Just $ dataget file dataset
    _ -> Nothing
  where
    dataget prog dataset = do
      let dir = takeDirectory prog

      runs <- testSpecRuns <$> testSpecFromProgramOrDie prog

      let exact = filter ((dataset ==) . runDescription) runs
          infixes = filter ((dataset `isInfixOf`) . runDescription) runs

      futhark <- FutharkExe <$> getExecutablePath

      case nubBy ((==) `on` runDescription) $
        if null exact then infixes else exact of
        [x] -> BS.putStr =<< getValuesBS futhark dir (runInput x)
        [] -> do
          hPutStr stderr $ "No dataset '" ++ dataset ++ "'.\n"
          hPutStr stderr "Available datasets:\n"
          mapM_ (hPutStrLn stderr . ("  " ++) . runDescription) runs
          exitFailure
        runs' -> do
          hPutStr stderr $ "Dataset '" ++ dataset ++ "' ambiguous:\n"
          mapM_ (hPutStrLn stderr . ("  " ++) . runDescription) runs'
          exitFailure

    testSpecRuns = testActionRuns . testAction
    testActionRuns CompileTimeFailure {} = []
    testActionRuns (RunCases ios _ _) = concatMap iosTestRuns ios

-- | @futhark check-syntax@
mainCheckSyntax :: String -> [String] -> IO ()
mainCheckSyntax = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ void $ readUntypedProgramOrDie file
    _ -> Nothing

-- | @futhark thanks@
mainThanks :: String -> [String] -> IO ()
mainThanks = mainWithOptions () [] "" $ \args () ->
  case args of
    [] -> Just $ do
      i <- randomRIO (0, n - 1)
      putStrLn $ responses !! i
    _ -> Nothing
  where
    n = length responses
    responses =
      [ "You're welcome!",
        "Tell all your friends about Futhark!",
        "Likewise!",
        "And thank you in return for trying the language!"
      ]
