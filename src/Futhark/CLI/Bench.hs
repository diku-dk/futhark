{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Simple tool for benchmarking Futhark programs.  Use the @--json@
-- flag for machine-readable output.
module Futhark.CLI.Bench ( main ) where

import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Text as T
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Exit
import Text.Printf
import Text.Regex.TDFA

import Futhark.Bench
import Futhark.Test
import Futhark.Util (pmapIO)
import Futhark.Util.Options

data BenchOptions = BenchOptions
                   { optBackend :: String
                   , optFuthark :: Maybe String
                   , optRunner :: String
                   , optRuns :: Int
                   , optExtraOptions :: [String]
                   , optCompilerOptions :: [String]
                   , optJSON :: Maybe FilePath
                   , optTimeout :: Int
                   , optSkipCompilation :: Bool
                   , optExcludeCase :: [String]
                   , optIgnoreFiles :: [Regex]
                   , optEntryPoint :: Maybe String
                   , optTuning :: Maybe String
                   , optConcurrency :: Maybe Int
                   }

initialBenchOptions :: BenchOptions
initialBenchOptions = BenchOptions "c" Nothing "" 10 [] [] Nothing (-1) False
                      ["nobench", "disable"] [] Nothing (Just "tuning") Nothing

runBenchmarks :: BenchOptions -> [FilePath] -> IO ()
runBenchmarks opts paths = do
  -- We force line buffering to ensure that we produce running output.
  -- Otherwise, CI tools and the like may believe we are hung and kill
  -- us.
  hSetBuffering stdout LineBuffering

  benchmarks <- filter (not . ignored . fst) <$> testSpecsFromPathsOrDie paths
  -- Try to avoid concurrency at both program and data set level.
  let opts' = if length paths /= 1
              then opts { optConcurrency = Just 1}
              else opts
  (skipped_benchmarks, compiled_benchmarks) <-
    partitionEithers <$> pmapIO (optConcurrency opts) (compileBenchmark opts') benchmarks

  when (anyFailedToCompile skipped_benchmarks) exitFailure

  results <- concat <$> mapM (runBenchmark opts)
             (sortBy (comparing fst) compiled_benchmarks)
  case optJSON opts of
    Nothing -> return ()
    Just file -> LBS.writeFile file $ encodeBenchResults results
  when (anyFailed results) exitFailure

  where ignored f = any (`match` f) $ optIgnoreFiles opts

anyFailed :: [BenchResult] -> Bool
anyFailed = any failedBenchResult
  where failedBenchResult (BenchResult _ xs) =
          any failedResult xs
        failedResult (DataResult _ Left{}) = True
        failedResult _                     = False

anyFailedToCompile :: [SkipReason] -> Bool
anyFailedToCompile = not . all (==Skipped)

data SkipReason = Skipped | FailedToCompile
  deriving (Eq)

compileOptions :: BenchOptions -> IO CompileOptions
compileOptions opts = do
  futhark <- maybe getExecutablePath return $ optFuthark opts
  return $ CompileOptions { compFuthark = futhark
                          , compBackend = optBackend opts
                          , compOptions = optCompilerOptions opts
                          }

compileBenchmark :: BenchOptions -> (FilePath, ProgramTest)
                 -> IO (Either SkipReason (FilePath, [InputOutputs]))
compileBenchmark opts (program, spec) =
  case testAction spec of
    RunCases cases _ _ | "nobench" `notElem` testTags spec,
                         "disable" `notElem` testTags spec,
                         any hasRuns cases ->
      if optSkipCompilation opts
        then do
        exists <- doesFileExist $ binaryName program
        if exists
          then return $ Right (program, cases)
          else do putStrLn $ binaryName program ++ " does not exist, but --skip-compilation passed."
                  return $ Left FailedToCompile
        else do

        putStr $ "Compiling " ++ program ++ "...\n"

        compile_opts <- compileOptions opts

        res <- prepareBenchmarkProgram (optConcurrency opts) compile_opts program cases

        case res of
          Left (err, errstr) -> do
            putStrLn err
            maybe (return ()) SBS.putStrLn errstr
            return $ Left FailedToCompile
          Right () ->
            return $ Right (program, cases)

    _ ->
      return $ Left Skipped
  where hasRuns (InputOutputs _ runs) = not $ null runs

runBenchmark :: BenchOptions -> (FilePath, [InputOutputs]) -> IO [BenchResult]
runBenchmark opts (program, cases) = mapM forInputOutputs $ filter relevant cases
  where forInputOutputs (InputOutputs entry_name runs) = do
          (tuning_opts, tuning_desc) <- determineTuning (optTuning opts) program

          putStr $ "Results for " ++ program' ++ tuning_desc ++ ":\n"
          let opts' = opts { optExtraOptions =
                               optExtraOptions opts ++ tuning_opts }
          BenchResult program' . catMaybes <$>
            mapM (runBenchmarkCase opts' program entry_name pad_to) runs
          where program' = if entry_name == "main"
                           then program
                           else program ++ ":" ++ T.unpack entry_name

        relevant = maybe (const True) (==) (optEntryPoint opts) . T.unpack . iosEntryPoint

        pad_to = foldl max 0 $ concatMap (map (length . runDescription) . iosTestRuns) cases

reportResult :: [RunResult] -> IO ()
reportResult [] =
  print (0::Int)
reportResult results = do
  let runtimes = map (fromIntegral . runMicroseconds) results
      avg = sum runtimes / fromIntegral (length runtimes)
      rel_dev = stddevp runtimes / mean runtimes :: Double
  putStrLn $ printf "%10.2f" avg ++ "μs (avg. of " ++ show (length runtimes) ++
    " runs; RSD: " ++ printf "%.2f" rel_dev ++ ")"

runOptions :: BenchOptions -> RunOptions
runOptions opts = RunOptions { runRunner = optRunner opts
                             , runRuns = optRuns opts
                             , runExtraOptions = optExtraOptions opts
                             , runTimeout = optTimeout opts
                             }

runBenchmarkCase :: BenchOptions -> FilePath -> T.Text -> Int -> TestRun
                 -> IO (Maybe DataResult)
runBenchmarkCase _ _ _ _ (TestRun _ _ RunTimeFailure{} _ _) =
  return Nothing -- Not our concern, we are not a testing tool.
runBenchmarkCase opts _ _ _ (TestRun tags _ _ _ _)
  | any (`elem` tags) $ optExcludeCase opts =
      return Nothing
runBenchmarkCase opts program entry pad_to tr@(TestRun _ input_spec (Succeeds expected_spec) _ dataset_desc) = do
  -- Report the dataset name before running the program, so that if an
  -- error occurs it's easier to see where.
  putStr $ "dataset " ++ dataset_desc ++ ": " ++
    replicate (pad_to - length dataset_desc) ' '
  hFlush stdout

  res <- benchmarkDataset (runOptions opts) program entry input_spec expected_spec
         (testRunReferenceOutput program entry tr)
  case res of
    Left err -> do
      liftIO $ putStrLn $ T.unpack err
      return $ Just $ DataResult dataset_desc $ Left err
    Right (runtimes, errout) -> do
      reportResult runtimes
      return $ Just $ DataResult dataset_desc $ Right (runtimes, errout)

commandLineOptions :: [FunOptDescr BenchOptions]
commandLineOptions = [
    Option "r" ["runs"]
    (ReqArg (\n ->
              case reads n of
                [(n', "")] | n' >= 0 ->
                  Right $ \config ->
                  config { optRuns = n'
                         }
                _ ->
                  Left $ error $ "'" ++ n ++ "' is not a non-negative integer.")
     "RUNS")
    "Run each test case this many times."
  , Option [] ["backend"]
    (ReqArg (\backend -> Right $ \config -> config { optBackend = backend })
     "PROGRAM")
    "The compiler used (defaults to 'futhark-c')."
  , Option [] ["futhark"]
    (ReqArg (\prog -> Right $ \config -> config { optFuthark = Just prog })
     "PROGRAM")
    "The binary used for operations (defaults to same binary as 'futhark bench')."
  , Option [] ["runner"]
    (ReqArg (\prog -> Right $ \config -> config { optRunner = prog }) "PROGRAM")
    "The program used to run the Futhark-generated programs (defaults to nothing)."
  , Option "p" ["pass-option"]
    (ReqArg (\opt ->
               Right $ \config ->
               config { optExtraOptions = opt : optExtraOptions config })
     "OPT")
    "Pass this option to programs being run."
  , Option [] ["pass-compiler-option"]
    (ReqArg (\opt ->
               Right $ \config ->
               config { optCompilerOptions = opt : optCompilerOptions config })
     "OPT")
    "Pass this option to the compiler (or typechecker if in -t mode)."
  , Option [] ["json"]
    (ReqArg (\file ->
               Right $ \config -> config { optJSON = Just file})
    "FILE")
    "Scatter results in JSON format here."
  , Option [] ["timeout"]
    (ReqArg (\n ->
               case reads n of
                 [(n', "")]
                   | n' < max_timeout ->
                   Right $ \config -> config { optTimeout = fromIntegral n' }
                 _ ->
                   Left $ error $ "'" ++ n ++
                   "' is not an integer smaller than" ++ show max_timeout ++ ".")
    "SECONDS")
    "Number of seconds before a dataset is aborted."
  , Option [] ["skip-compilation"]
    (NoArg $ Right $ \config -> config { optSkipCompilation = True })
    "Use already compiled program."
  , Option [] ["exclude-case"]
    (ReqArg (\s -> Right $ \config ->
                config { optExcludeCase = s : optExcludeCase config })
      "TAG")
    "Do not run test cases with this tag."
  , Option [] ["ignore-files"]
    (ReqArg (\s -> Right $ \config ->
                config { optIgnoreFiles = makeRegex s : optIgnoreFiles config })
      "REGEX")
    "Ignore files matching this regular expression."
  , Option "e" ["entry-point"]
    (ReqArg (\s -> Right $ \config ->
                config { optEntryPoint = Just s })
      "NAME")
    "Only run this entry point."
  , Option [] ["tuning"]
    (ReqArg (\s -> Right $ \config -> config { optTuning = Just s })
    "EXTENSION")
    "Look for tuning files with this extension (defaults to .tuning)."
  , Option [] ["no-tuning"]
    (NoArg $ Right $ \config -> config { optTuning = Nothing })
    "Do not load tuning files."
  , Option [] ["concurrency"]
    (ReqArg (\n ->
               case reads n of
                 [(n', "")]
                   | n' > 0 ->
                   Right $ \config -> config { optConcurrency = Just n' }
                 _ ->
                   Left $ error $ "'" ++ n ++ "' is not a positive integer.")
    "NUM")
    "Number of benchmarks to prepare (not run) concurrently."
  ]
  where max_timeout :: Int
        max_timeout = maxBound `div` 1000000

main :: String -> [String] -> IO ()
main = mainWithOptions initialBenchOptions commandLineOptions "options... programs..." $ \progs config ->
  Just $ runBenchmarks config progs

--- The following extracted from hstats package by Marshall Beddoe:
--- https://hackage.haskell.org/package/hstats-0.3

-- | Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x' -> (m+(x'-m)/(n+1),n+1)) (0,0) x

-- | Standard deviation of population
stddevp :: (Floating a) => [a] -> a
stddevp xs = sqrt $ pvar xs

-- | Population variance
pvar :: (Floating a) => [a] -> a
pvar xs = centralMoment xs (2::Int)

-- | Central moments
centralMoment :: (Floating b, Integral t) => [b] -> t -> b
centralMoment _  1 = 0
centralMoment xs r = sum (map (\x -> (x-m)^r) xs) / n
    where
      m = mean xs
      n = fromIntegral $ length xs
