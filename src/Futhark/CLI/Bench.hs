-- | @futhark bench@
module Futhark.CLI.Bench (main) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as SBS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either
import Data.Function ((&))
import Data.IORef
import Data.List (intersect, sortBy)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Vector.Unboxed qualified as U
import Futhark.Bench
import Futhark.Server
import Futhark.Test
import Futhark.Util (atMostChars, fancyTerminal, pmapIO, showText)
import Futhark.Util.Options
import Futhark.Util.Pretty (AnsiStyle, Color (..), annotate, bold, color, line, pretty, prettyText, putDoc)
import Futhark.Util.ProgressBar
import Statistics.Resampling (Estimator (..), resample)
import Statistics.Resampling.Bootstrap (bootstrapBCA)
import Statistics.Types (cl95, confIntLDX, confIntUDX, estError, estPoint)
import System.Console.ANSI (clearLine)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Random.MWC (create)
import Text.Printf
import Text.Regex.TDFA

putStyleLn :: AnsiStyle -> T.Text -> IO ()
putStyleLn s t = putDoc $ annotate s (pretty t <> line)

putRedLn, putBoldRedLn, putBoldLn :: T.Text -> IO ()
putRedLn = putStyleLn (color Red)
putBoldRedLn = putStyleLn (color Red <> bold)
putBoldLn = putStyleLn bold

data BenchOptions = BenchOptions
  { optBackend :: String,
    optFuthark :: Maybe String,
    optRunner :: String,
    optMinRuns :: Int,
    optMinTime :: NominalDiffTime,
    optExtraOptions :: [String],
    optCompilerOptions :: [String],
    optJSON :: Maybe FilePath,
    optTimeout :: Int,
    optSkipCompilation :: Bool,
    optExcludeCase :: [T.Text],
    optIgnoreFiles :: [Regex],
    optEntryPoint :: Maybe String,
    optTuning :: Maybe String,
    optCacheExt :: Maybe String,
    optConvergencePhase :: Bool,
    optConvergenceMaxTime :: NominalDiffTime,
    optConcurrency :: Maybe Int,
    optProfile :: Bool,
    optVerbose :: Int,
    optTestSpec :: Maybe FilePath
  }

initialBenchOptions :: BenchOptions
initialBenchOptions =
  BenchOptions
    { optBackend = "c",
      optFuthark = Nothing,
      optRunner = "",
      optMinRuns = 10,
      optMinTime = 0.5,
      optExtraOptions = [],
      optCompilerOptions = [],
      optJSON = Nothing,
      optTimeout = -1,
      optSkipCompilation = False,
      optExcludeCase = ["nobench", "disable"],
      optIgnoreFiles = [],
      optEntryPoint = Nothing,
      optTuning = Just "tuning",
      optCacheExt = Nothing,
      optConvergencePhase = True,
      optConvergenceMaxTime = 5 * 60,
      optConcurrency = Nothing,
      optProfile = False,
      optVerbose = 0,
      optTestSpec = Nothing
    }

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

  putStrLn $
    "Reporting arithmetic mean runtime of at least "
      <> show (optMinRuns opts)
      <> " runs for each dataset (min "
      <> show (optMinTime opts)
      <> ")."
  when (optConvergencePhase opts) . putStrLn $
    "More runs automatically performed for up to "
      <> show (optConvergenceMaxTime opts)
      <> " to ensure accurate measurement."

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
      | null $
          optExcludeCase opts
            `intersect` testTags spec
            <> testTags program_spec,
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
                  putRedLn $ T.pack err
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
      putBoldRedLn $ "\nFailed to run " <> T.pack program
      putRedLn $ showText e
      pure Nothing

-- Truncate dataset name display after this many characters.
maxDatasetNameLength :: Int
maxDatasetNameLength = 40

runBenchmark :: BenchOptions -> FutharkExe -> (FilePath, [InputOutputs]) -> IO (Maybe [BenchResult])
runBenchmark opts futhark (program, cases) = do
  (tuning_opts, tuning_desc) <- determineTuning (optTuning opts) program
  let runopts =
        optExtraOptions opts
          ++ tuning_opts
          ++ determineCache (optCacheExt opts) program
          ++ if optProfile opts then ["--profile", "--log"] else []
  withProgramServer program (optRunner opts) runopts $ \server ->
    mapM (forInputOutputs server tuning_desc) $ filter relevant cases
  where
    forInputOutputs server tuning_desc (InputOutputs entry_name runs) = do
      putBoldLn $ "\n" <> T.pack program' <> T.pack tuning_desc <> ":"
      BenchResult program' . catMaybes
        <$> mapM (runBenchmarkCase server opts futhark program entry_name pad_to) runs
      where
        program' =
          if entry_name == "main"
            then program
            else program ++ ":" ++ T.unpack entry_name

    relevant = maybe (const True) (==) (optEntryPoint opts) . T.unpack . iosEntryPoint

    len = T.length . atMostChars maxDatasetNameLength . runDescription
    pad_to = foldl max 0 $ concatMap (map len . iosTestRuns) cases

runOptions :: ((Int, Maybe Double) -> IO ()) -> BenchOptions -> RunOptions
runOptions f opts =
  RunOptions
    { runMinRuns = optMinRuns opts,
      runMinTime = optMinTime opts,
      runTimeout = optTimeout opts,
      runVerbose = optVerbose opts,
      runConvergencePhase = optConvergencePhase opts,
      runConvergenceMaxTime = optConvergenceMaxTime opts,
      runResultAction = f,
      runProfile = optProfile opts
    }

descText :: T.Text -> Int -> T.Text
descText desc pad_to = desc <> ": " <> T.replicate (pad_to - T.length desc) " "

progress :: Double -> T.Text
progress elapsed =
  progressBar
    ( ProgressBar
        { progressBarSteps = 10,
          progressBarBound = 1,
          progressBarElapsed = elapsed
        }
    )

interimResult :: Int -> Int -> Double -> T.Text
interimResult us_sum runs elapsed =
  T.pack (printf "%10.0fμs " avg)
    <> progress elapsed
    <> (" " <> prettyText runs <> " runs")
  where
    avg :: Double
    avg = fromIntegral us_sum / fromIntegral runs

convergenceBar :: (T.Text -> IO ()) -> IORef Int -> Int -> Int -> Double -> IO ()
convergenceBar p spin_count us_sum i rse' = do
  spin_idx <- readIORef spin_count
  let spin = progressSpinner spin_idx
  p $ T.pack $ printf "%10.0fμs %s (RSE of mean: %2.4f; %4d runs)" avg spin rse' i
  writeIORef spin_count (spin_idx + 1)
  where
    avg :: Double
    avg = fromIntegral us_sum / fromIntegral i

data BenchPhase = Initial | Convergence

mkProgressPrompt :: BenchOptions -> Int -> T.Text -> UTCTime -> IO ((Maybe Int, Maybe Double) -> IO ())
mkProgressPrompt opts pad_to dataset_desc start_time
  | fancyTerminal = do
      count <- newIORef (0, 0)
      phase_var <- newIORef Initial
      spin_count <- newIORef 0
      pure $ \(us, rse) -> do
        T.putStr "\r" -- Go to start of line.
        let p s =
              T.putStr $
                descText (atMostChars maxDatasetNameLength dataset_desc) pad_to <> s

        (us_sum, i) <- readIORef count

        now <- liftIO getCurrentTime
        let determineProgress i' =
              let time_elapsed = toDouble (realToFrac (diffUTCTime now start_time) / optMinTime opts)
                  runs_elapsed = fromIntegral i' / fromIntegral (optMinRuns opts)
               in -- The progress bar is the _shortest_ of the
                  -- time-based or runs-based estimate.  This is
                  -- intended to avoid a situation where the progress
                  -- bar is full but stuff is still happening.  On the
                  -- other hand, it means it can sometimes shrink.
                  min time_elapsed runs_elapsed

        phase <- readIORef phase_var

        case (us, phase, rse) of
          (Nothing, _, _) ->
            let elapsed = determineProgress i
             in p $ T.pack (replicate 13 ' ') <> progress elapsed
          (Just us', Initial, Nothing) -> do
            let us_sum' = us_sum + us'
                i' = i + 1
            writeIORef count (us_sum', i')
            let elapsed = determineProgress i'
            p $ interimResult us_sum' i' elapsed
          (Just us', Initial, Just rse') -> do
            -- Switched from phase 1 to convergence; discard all
            -- prior results.
            writeIORef count (us', 1)
            writeIORef phase_var Convergence
            convergenceBar p spin_count us' 1 rse'
          (Just us', Convergence, Just rse') -> do
            let us_sum' = us_sum + us'
                i' = i + 1
            writeIORef count (us_sum', i')
            convergenceBar p spin_count us_sum' i' rse'
          (Just _, Convergence, Nothing) ->
            pure () -- Probably should not happen.
        putStr " " -- Just to move the cursor away from the progress bar.
        hFlush stdout
  | otherwise = do
      T.putStr $ descText dataset_desc pad_to
      hFlush stdout
      pure $ const $ pure ()
  where
    toDouble = fromRational . toRational

reportResult :: [RunResult] -> (Double, Double) -> IO ()
reportResult results (ci_lower, ci_upper) = do
  let runtimes = map (fromIntegral . runMicroseconds) results
      avg = sum runtimes / fromIntegral (length runtimes) :: Double
  putStrLn $ printf "%10.0fμs (95%% CI: [%10.1f, %10.1f])" avg ci_lower ci_upper

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
runBenchmarkCase server opts futhark program entry pad_to tr = do
  let (TestRun _ input_spec (Succeeds expected_spec) _ dataset_desc) = tr
  start_time <- liftIO getCurrentTime
  prompt <- mkProgressPrompt opts pad_to dataset_desc start_time

  -- Report the dataset name before running the program, so that if an
  -- error occurs it's easier to see where.
  prompt (Nothing, Nothing)

  res <-
    benchmarkDataset
      server
      (runOptions (prompt . first Just) opts)
      futhark
      program
      entry
      input_spec
      expected_spec
      (testRunReferenceOutput program entry tr)

  when fancyTerminal $ do
    clearLine
    T.putStr "\r"
    T.putStr $ descText (atMostChars maxDatasetNameLength dataset_desc) pad_to

  case res of
    Left err -> liftIO $ do
      putStrLn ""
      putRedLn err
      pure $ Just $ DataResult dataset_desc $ Left err
    Right (runtimes, errout, report) -> do
      let vec_runtimes = U.fromList $ map (fromIntegral . runMicroseconds) runtimes
      g <- create
      resampled <- liftIO $ resample g [Mean] 70000 vec_runtimes
      let bootstrapCI =
            case bootstrapBCA cl95 vec_runtimes resampled of
              boot : _ ->
                ( estPoint boot - confIntLDX (estError boot),
                  estPoint boot + confIntUDX (estError boot)
                )
              _ -> (0, 0)

      reportResult runtimes bootstrapCI

      -- We throw away the 'errout' unless profiling is enabled,
      -- because it is otherwise useless and adds too much to the
      -- .json file size.
      let errout' = guard (optProfile opts) >> Just errout
          report' = guard (optProfile opts) >> Just report
      Result runtimes (getMemoryUsage report) errout' report'
        & Right
        & DataResult dataset_desc
        & Just
        & pure

getMemoryUsage :: ProfilingReport -> M.Map T.Text Int
getMemoryUsage = fmap fromInteger . profilingMemory

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
                      { optMinRuns = n'
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
      "Use already compiled server-mode program.",
    Option
      []
      ["exclude-case"]
      ( ReqArg
          ( \s -> Right $ \config ->
              config {optExcludeCase = T.pack s : optExcludeCase config}
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
      ["cache-extension"]
      ( ReqArg
          (\s -> Right $ \config -> config {optCacheExt = Just s})
          "EXTENSION"
      )
      "Use cache files with this extension (none by default).",
    Option
      []
      ["no-tuning"]
      (NoArg $ Right $ \config -> config {optTuning = Nothing})
      "Do not load tuning files.",
    Option
      []
      ["no-convergence-phase"]
      (NoArg $ Right $ \config -> config {optConvergencePhase = False})
      "Do not run convergence phase.",
    Option
      []
      ["convergence-max-seconds"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")]
                  | n' > 0 ->
                      Right $ \config -> config {optConvergenceMaxTime = fromInteger n'}
                _ ->
                  Left . optionsError $ "'" ++ n ++ "' is not a positive integer."
          )
          "NUM"
      )
      "Limit convergence phase to this number of seconds.",
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
      "Enable logging.  Pass multiple times for more.",
    Option
      "P"
      ["profile"]
      (NoArg $ Right $ \config -> config {optProfile = True})
      "Collect profiling information."
  ]
  where
    max_timeout :: Int
    max_timeout = maxBound `div` 1000000

excludeBackend :: BenchOptions -> BenchOptions
excludeBackend config =
  config
    { optExcludeCase =
        "no_" <> T.pack (optBackend config)
          : optExcludeCase config
    }

-- | Run @futhark bench@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialBenchOptions commandLineOptions "options... programs..." $ \progs config ->
  case progs of
    [] -> Nothing
    _
      | optProfile config && isNothing (optJSON config) ->
          Just $ optionsError "--profile cannot be used without --json."
      | otherwise ->
          Just $ runBenchmarks (excludeBackend config) progs
