{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @futhark bench@
module Futhark.CLI.Bench (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Except hiding (throwError)
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either
import Data.Function ((&))
import Data.IORef
import Data.List (foldl', sortBy)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Futhark.Bench
import Futhark.Server
import Futhark.Test
import Futhark.Util (atMostChars, fancyTerminal, maxinum, maybeNth, pmapIO)
import Futhark.Util.Console
import Futhark.Util.Options
import System.Console.ANSI (clearLine)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.Printf
import Text.Regex.TDFA

data BenchOptions = BenchOptions
  { optBackend :: String,
    optFuthark :: Maybe String,
    optRunner :: String,
    optRuns :: Int,
    optExtraOptions :: [String],
    optCompilerOptions :: [String],
    optJSON :: Maybe FilePath,
    optTimeout :: Int,
    optSkipCompilation :: Bool,
    optExcludeCase :: [String],
    optIgnoreFiles :: [Regex],
    optEntryPoint :: Maybe String,
    optTuning :: Maybe String,
    optConcurrency :: Maybe Int,
    optVerbose :: Int,
    optTestSpec :: Maybe FilePath
  }

initialBenchOptions :: BenchOptions
initialBenchOptions =
  BenchOptions
    "c"
    Nothing
    ""
    10
    []
    []
    Nothing
    (-1)
    False
    ["nobench", "disable"]
    []
    Nothing
    (Just "tuning")
    Nothing
    0
    Nothing

runBenchmarks :: BenchOptions -> [FilePath] -> IO ()
runBenchmarks opts paths = do
  -- We force line buffering to ensure that we produce running output.
  -- Otherwise, CI tools and the like may believe we are hung and kill
  -- us.
  hSetBuffering stdout LineBuffering

  benchmarks <- filter (not . ignored . fst) <$> testSpecsFromPathsOrDie paths
  -- Try to avoid concurrency at both program and data set level.
  let opts' =
        if length paths /= 1
          then opts {optConcurrency = Just 1}
          else opts
  (skipped_benchmarks, compiled_benchmarks) <-
    partitionEithers <$> pmapIO (optConcurrency opts) (compileBenchmark opts') benchmarks

  when (anyFailedToCompile skipped_benchmarks) exitFailure

  putStrLn $ "Reporting average runtime of " ++ show (optRuns opts) ++ " runs for each dataset."

  futhark <- FutharkExe . compFuthark <$> compileOptions opts

  maybe_results <-
    mapM
      (runBenchmark opts futhark)
      (sortBy (comparing fst) compiled_benchmarks)
  let results = concat $ catMaybes maybe_results
  case optJSON opts of
    Nothing -> pure ()
    Just file -> LBS.writeFile file $ encodeBenchResults results
  when (any isNothing maybe_results || anyFailed results) exitFailure
  where
    ignored f = any (`match` f) $ optIgnoreFiles opts

anyFailed :: [BenchResult] -> Bool
anyFailed = any failedBenchResult
  where
    failedBenchResult (BenchResult _ xs) =
      any failedResult xs
    failedResult (DataResult _ Left {}) = True
    failedResult _ = False

anyFailedToCompile :: [SkipReason] -> Bool
anyFailedToCompile = not . all (== Skipped)

data SkipReason = Skipped | FailedToCompile
  deriving (Eq)

compileOptions :: BenchOptions -> IO CompileOptions
compileOptions opts = do
  futhark <- maybe getExecutablePath pure $ optFuthark opts
  pure $
    CompileOptions
      { compFuthark = futhark,
        compBackend = optBackend opts,
        compOptions = optCompilerOptions opts
      }

compileBenchmark ::
  BenchOptions ->
  (FilePath, ProgramTest) ->
  IO (Either SkipReason (FilePath, [InputOutputs]))
compileBenchmark opts (program, program_spec) = do
  spec <- maybe (pure program_spec) testSpecFromFileOrDie $ optTestSpec opts
  case testAction spec of
    RunCases cases _ _
      | "nobench" `notElem` testTags spec,
        "disable" `notElem` testTags spec,
        any hasRuns cases ->
        if optSkipCompilation opts
          then do
            exists <- doesFileExist $ binaryName program
            if exists
              then pure $ Right (program, cases)
              else do
                putStrLn $ binaryName program ++ " does not exist, but --skip-compilation passed."
                pure $ Left FailedToCompile
          else do
            putStr $ "Compiling " ++ program ++ "...\n"

            compile_opts <- compileOptions opts

            res <- prepareBenchmarkProgram (optConcurrency opts) compile_opts program cases

            case res of
              Left (err, errstr) -> do
                putStrLn $ inRed err
                maybe (pure ()) SBS.putStrLn errstr
                pure $ Left FailedToCompile
              Right () ->
                pure $ Right (program, cases)
    _ ->
      pure $ Left Skipped
  where
    hasRuns (InputOutputs _ runs) = not $ null runs

withProgramServer :: FilePath -> FilePath -> [String] -> (Server -> IO a) -> IO (Maybe a)
withProgramServer program runner extra_options f = do
  -- Explicitly prefixing the current directory is necessary for
  -- readProcessWithExitCode to find the binary when binOutputf has
  -- no path component.
  let binOutputf = dropExtension program
      binpath = "." </> binOutputf

      (to_run, to_run_args)
        | null runner = (binpath, extra_options)
        | otherwise = (runner, binpath : extra_options)

  liftIO $ (Just <$> withServer (futharkServerCfg to_run to_run_args) f) `catch` onError
  where
    onError :: SomeException -> IO (Maybe a)
    onError e = do
      putStrLn $ inBold $ inRed $ "\nFailed to run " ++ program
      putStrLn $ inRed $ show e
      pure Nothing

runBenchmark :: BenchOptions -> FutharkExe -> (FilePath, [InputOutputs]) -> IO (Maybe [BenchResult])
runBenchmark opts futhark (program, cases) = do
  (tuning_opts, tuning_desc) <- determineTuning (optTuning opts) program
  let runopts = "-L" : optExtraOptions opts ++ tuning_opts
  withProgramServer program (optRunner opts) runopts $ \server ->
    mapM (forInputOutputs server tuning_desc) $ filter relevant cases
  where
    forInputOutputs server tuning_desc (InputOutputs entry_name runs) = do
      putStr $ inBold $ "\n" ++ program' ++ tuning_desc ++ ":\n"
      BenchResult program' . catMaybes
        <$> mapM (runBenchmarkCase server opts futhark program entry_name pad_to) runs
      where
        program' =
          if entry_name == "main"
            then program
            else program ++ ":" ++ T.unpack entry_name

    relevant = maybe (const True) (==) (optEntryPoint opts) . T.unpack . iosEntryPoint

    pad_to = foldl max 0 $ concatMap (map (length . atMostChars 40 . runDescription) . iosTestRuns) cases

runOptions :: (Int -> IO ()) -> BenchOptions -> RunOptions
runOptions f opts =
  RunOptions
    { runRuns = optRuns opts,
      runTimeout = optTimeout opts,
      runVerbose = optVerbose opts,
      runResultAction = Just f
    }

progressBar :: Int -> Int -> Int -> String
progressBar cur bound steps =
  "|" ++ map cell [1 .. steps] ++ "| " ++ show cur ++ "/" ++ show bound
  where
    step_size :: Double
    step_size = fromIntegral bound / fromIntegral steps
    cur' = fromIntegral cur
    chars = " ▏▎▍▍▌▋▊▉█"
    char i = fromMaybe ' ' $ maybeNth (i :: Int) chars
    num_chars = fromIntegral $ length chars

    cell :: Int -> Char
    cell i
      | i' * step_size <= cur' = char 9
      | otherwise =
        char (floor (((cur' - (i' - 1) * step_size) * num_chars) / step_size))
      where
        i' = fromIntegral i

descString :: String -> Int -> String
descString desc pad_to = desc ++ ": " ++ replicate (pad_to - length desc) ' '

interimResult :: Int -> Int -> Int -> String
interimResult us_sum i runs =
  printf "%10.0fμs " avg ++ progressBar i runs 10
  where
    avg :: Double
    avg = fromIntegral us_sum / fromIntegral i

mkProgressPrompt :: Int -> Int -> String -> IO (Maybe Int -> IO ())
mkProgressPrompt runs pad_to dataset_desc
  | fancyTerminal = do
    count <- newIORef (0, 0)
    pure $ \us -> do
      putStr "\r" -- Go to start of line.
      let p s =
            putStr $
              descString (atMostChars 40 dataset_desc) pad_to ++ s
      (us_sum, i) <- readIORef count
      case us of
        Nothing -> p $ replicate 13 ' ' ++ progressBar i runs 10
        Just us' -> do
          let us_sum' = us_sum + us'
              i' = i + 1
          writeIORef count (us_sum', i')
          p $ interimResult us_sum' i' runs
      putStr " " -- Just to move the cursor away from the progress bar.
      hFlush stdout
  | otherwise = do
    putStr $ descString dataset_desc pad_to
    hFlush stdout
    pure $ const $ pure ()

reportResult :: [RunResult] -> IO ()
reportResult = putStrLn . reportString
  where
    reportString results =
      printf
        "%10.0fμs (RSD: %.3f; min: %3.0f%%; max: %+3.0f%%)"
        avg
        rsd
        ((minimum runtimes / avg - 1) * 100)
        ((maxinum runtimes / avg - 1) * 100)
      where
        runtimes = map (fromIntegral . runMicroseconds) results
        avg = sum runtimes / fromIntegral (length runtimes)
        rsd = stddevp runtimes / mean runtimes :: Double

runBenchmarkCase ::
  Server ->
  BenchOptions ->
  FutharkExe ->
  FilePath ->
  T.Text ->
  Int ->
  TestRun ->
  IO (Maybe DataResult)
runBenchmarkCase _ _ _ _ _ _ (TestRun _ _ RunTimeFailure {} _ _) =
  pure Nothing -- Not our concern, we are not a testing tool.
runBenchmarkCase _ opts _ _ _ _ (TestRun tags _ _ _ _)
  | any (`elem` tags) $ optExcludeCase opts =
    pure Nothing
runBenchmarkCase server opts futhark program entry pad_to tr@(TestRun _ input_spec (Succeeds expected_spec) _ dataset_desc) = do
  prompt <- mkProgressPrompt (optRuns opts) pad_to dataset_desc

  -- Report the dataset name before running the program, so that if an
  -- error occurs it's easier to see where.
  prompt Nothing

  res <-
    benchmarkDataset
      server
      (runOptions (prompt . Just) opts)
      futhark
      program
      entry
      input_spec
      expected_spec
      (testRunReferenceOutput program entry tr)

  when fancyTerminal $ do
    clearLine
    putStr "\r"
    putStr $ descString (atMostChars 40 dataset_desc) pad_to

  case res of
    Left err -> liftIO $ do
      putStrLn ""
      putStrLn $ inRed $ T.unpack err
      pure $ Just $ DataResult dataset_desc $ Left err
    Right (runtimes, errout) -> do
      reportResult runtimes
      Result runtimes (getMemoryUsage errout) errout
        & Right
        & DataResult dataset_desc
        & Just
        & pure

getMemoryUsage :: T.Text -> M.Map T.Text Int
getMemoryUsage t =
  foldMap matchMap $ T.lines t
  where
    mem_regex = "Peak memory usage for space '([^']+)': ([0-9]+) bytes." :: T.Text
    matchMap line = case (line =~ mem_regex :: (T.Text, T.Text, T.Text, [T.Text])) of
      (_, _, _, [device, bytes]) -> M.singleton device (read $ T.unpack bytes)
      _ -> mempty

commandLineOptions :: [FunOptDescr BenchOptions]
commandLineOptions =
  [ Option
      "r"
      ["runs"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")] | n' > 0 ->
                  Right $ \config ->
                    config
                      { optRuns = n'
                      }
                _ ->
                  Left . optionsError $ "'" ++ n ++ "' is not a positive integer."
          )
          "RUNS"
      )
      "Run each test case this many times.",
    Option
      []
      ["backend"]
      ( ReqArg
          (\backend -> Right $ \config -> config {optBackend = backend})
          "PROGRAM"
      )
      "The compiler used (defaults to 'futhark-c').",
    Option
      []
      ["futhark"]
      ( ReqArg
          (\prog -> Right $ \config -> config {optFuthark = Just prog})
          "PROGRAM"
      )
      "The binary used for operations (defaults to same binary as 'futhark bench').",
    Option
      []
      ["runner"]
      (ReqArg (\prog -> Right $ \config -> config {optRunner = prog}) "PROGRAM")
      "The program used to run the Futhark-generated programs (defaults to nothing).",
    Option
      "p"
      ["pass-option"]
      ( ReqArg
          ( \opt ->
              Right $ \config ->
                config {optExtraOptions = opt : optExtraOptions config}
          )
          "OPT"
      )
      "Pass this option to programs being run.",
    Option
      []
      ["pass-compiler-option"]
      ( ReqArg
          ( \opt ->
              Right $ \config ->
                config {optCompilerOptions = opt : optCompilerOptions config}
          )
          "OPT"
      )
      "Pass this option to the compiler (or typechecker if in -t mode).",
    Option
      []
      ["json"]
      ( ReqArg
          ( \file ->
              Right $ \config -> config {optJSON = Just file}
          )
          "FILE"
      )
      "Scatter results in JSON format here.",
    Option
      []
      ["timeout"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")]
                  | n' < max_timeout ->
                    Right $ \config -> config {optTimeout = fromIntegral n'}
                _ ->
                  Left . optionsError $
                    "'" ++ n ++ "' is not an integer smaller than" ++ show max_timeout ++ "."
          )
          "SECONDS"
      )
      "Number of seconds before a dataset is aborted.",
    Option
      []
      ["skip-compilation"]
      (NoArg $ Right $ \config -> config {optSkipCompilation = True})
      "Use already compiled program.",
    Option
      []
      ["exclude-case"]
      ( ReqArg
          ( \s -> Right $ \config ->
              config {optExcludeCase = s : optExcludeCase config}
          )
          "TAG"
      )
      "Do not run test cases with this tag.",
    Option
      []
      ["ignore-files"]
      ( ReqArg
          ( \s -> Right $ \config ->
              config {optIgnoreFiles = makeRegex s : optIgnoreFiles config}
          )
          "REGEX"
      )
      "Ignore files matching this regular expression.",
    Option
      "e"
      ["entry-point"]
      ( ReqArg
          ( \s -> Right $ \config ->
              config {optEntryPoint = Just s}
          )
          "NAME"
      )
      "Only run this entry point.",
    Option
      []
      ["tuning"]
      ( ReqArg
          (\s -> Right $ \config -> config {optTuning = Just s})
          "EXTENSION"
      )
      "Look for tuning files with this extension (defaults to .tuning).",
    Option
      []
      ["no-tuning"]
      (NoArg $ Right $ \config -> config {optTuning = Nothing})
      "Do not load tuning files.",
    Option
      []
      ["concurrency"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")]
                  | n' > 0 ->
                    Right $ \config -> config {optConcurrency = Just n'}
                _ ->
                  Left . optionsError $ "'" ++ n ++ "' is not a positive integer."
          )
          "NUM"
      )
      "Number of benchmarks to prepare (not run) concurrently.",
    Option
      []
      ["spec-file"]
      (ReqArg (\s -> Right $ \config -> config {optTestSpec = Just s}) "FILE")
      "Use test specification from this file.",
    Option
      "v"
      ["verbose"]
      (NoArg $ Right $ \config -> config {optVerbose = optVerbose config + 1})
      "Enable logging.  Pass multiple times for more."
  ]
  where
    max_timeout :: Int
    max_timeout = maxBound `div` 1000000

excludeBackend :: BenchOptions -> BenchOptions
excludeBackend config =
  config {optExcludeCase = "no_" <> optBackend config : optExcludeCase config}

-- | Run @futhark bench@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialBenchOptions commandLineOptions "options... programs..." $ \progs config ->
  case progs of
    [] -> Nothing
    _ -> Just $ runBenchmarks (excludeBackend config) progs

--- The following extracted from hstats package by Marshall Beddoe:
--- https://hackage.haskell.org/package/hstats-0.3

-- | Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x' -> (m + (x' - m) / (n + 1), n + 1)) (0, 0) x

-- | Standard deviation of population
stddevp :: (Floating a) => [a] -> a
stddevp xs = sqrt $ pvar xs

-- | Population variance
pvar :: (Floating a) => [a] -> a
pvar xs = centralMoment xs (2 :: Int)

-- | Central moments
centralMoment :: (Floating b, Integral t) => [b] -> t -> b
centralMoment _ 1 = 0
centralMoment xs r = sum (map (\x -> (x - m) ^ r) xs) / n
  where
    m = mean xs
    n = fromIntegral $ length xs
