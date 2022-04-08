{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Facilities for handling Futhark benchmark results.  A Futhark
-- benchmark program is just like a Futhark test program.
module Futhark.Bench
  ( RunResult (..),
    DataResult (..),
    BenchResult (..),
    Result (..),
    encodeBenchResults,
    decodeBenchResults,
    binaryName,
    benchmarkDataset,
    RunOptions (..),
    prepareBenchmarkProgram,
    CompileOptions (..),
  )
where

-- added imports!
-- for timing

import Control.Applicative
import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.Vector.Unboxed as U
import Futhark.Server
import Futhark.Test
import Statistics.Autocorrelation
import Statistics.Resampling (Bootstrap (..), Estimator (..), resample)
import Statistics.Types
import System.Exit
import System.FilePath
import System.Process.ByteString (readProcessWithExitCode)
import System.Random.MWC (create)
import System.Timeout (timeout)

-- | The runtime of a single succesful run.
newtype RunResult = RunResult {runMicroseconds :: Int}
  deriving (Eq, Show)

-- | The measurements resulting from various successful runs of a
-- benchmark (same dataset).
data Result = Result
  { runResults :: [RunResult],
    memoryMap :: M.Map T.Text Int,
    stdErr :: T.Text
  }
  deriving (Eq, Show)

-- | The results for a single named dataset is either an error message, or
-- runtime measurements, the number of bytes used, and the stderr that was
-- produced.
data DataResult = DataResult String (Either T.Text Result)
  deriving (Eq, Show)

-- | The results for all datasets for some benchmark program.
data BenchResult = BenchResult FilePath [DataResult]
  deriving (Eq, Show)

newtype DataResults = DataResults {unDataResults :: [DataResult]}

newtype BenchResults = BenchResults {unBenchResults :: [BenchResult]}

instance JSON.ToJSON Result where
  toJSON (Result runres memmap err) = JSON.toJSON (runres, memmap, err)

instance JSON.FromJSON Result where
  parseJSON = fmap (\(runres, memmap, err) -> Result runres memmap err) . JSON.parseJSON

instance JSON.ToJSON RunResult where
  toJSON = JSON.toJSON . runMicroseconds

instance JSON.FromJSON RunResult where
  parseJSON = fmap RunResult . JSON.parseJSON

instance JSON.ToJSON DataResults where
  toJSON (DataResults rs) =
    JSON.object $ map dataResultJSON rs
  toEncoding (DataResults rs) =
    JSON.pairs $ mconcat $ map (uncurry (JSON..=) . dataResultJSON) rs

instance JSON.FromJSON DataResults where
  parseJSON = JSON.withObject "datasets" $ \o ->
    DataResults <$> mapM datasetResult (JSON.toList o)
    where
      datasetResult (k, v) =
        DataResult (JSON.toString k)
          <$> ((Right <$> success v) <|> (Left <$> JSON.parseJSON v))
      success = JSON.withObject "result" $ \o ->
        Result <$> o JSON..: "runtimes" <*> o JSON..: "bytes" <*> o JSON..: "stderr"

dataResultJSON :: DataResult -> (JSON.Key, JSON.Value)
dataResultJSON (DataResult desc (Left err)) =
  (JSON.fromString desc, JSON.toJSON err)
dataResultJSON (DataResult desc (Right (Result runtimes bytes progerr))) =
  ( JSON.fromString desc,
    JSON.object
      [ ("runtimes", JSON.toJSON $ map runMicroseconds runtimes),
        ("bytes", JSON.toJSON bytes),
        ("stderr", JSON.toJSON progerr)
      ]
  )

benchResultJSON :: BenchResult -> (JSON.Key, JSON.Value)
benchResultJSON (BenchResult prog r) =
  ( JSON.fromString prog,
    JSON.object [("datasets", JSON.toJSON $ DataResults r)]
  )

instance JSON.ToJSON BenchResults where
  toJSON (BenchResults rs) =
    JSON.object $ map benchResultJSON rs

instance JSON.FromJSON BenchResults where
  parseJSON = JSON.withObject "benchmarks" $ \o ->
    BenchResults <$> mapM onBenchmark (JSON.toList o)
    where
      onBenchmark (k, v) =
        BenchResult (JSON.toString k)
          <$> JSON.withObject "benchmark" onBenchmark' v
      onBenchmark' o =
        fmap unDataResults . JSON.parseJSON =<< o JSON..: "datasets"

-- | Transform benchmark results to a JSON bytestring.
encodeBenchResults :: [BenchResult] -> LBS.ByteString
encodeBenchResults = JSON.encode . BenchResults

-- | Decode benchmark results from a JSON bytestring.
decodeBenchResults :: LBS.ByteString -> Either String [BenchResult]
decodeBenchResults = fmap unBenchResults . JSON.eitherDecode'

--- Running benchmarks

-- | How to run a benchmark.
data RunOptions = RunOptions
  { runRuns :: Int,
    runTimeout :: Int,
    runVerbose :: Int,
    -- | Invoked for every runtime measured during the run.  Can be
    -- used to provide a progress bar.
    runResultAction :: (Int, Maybe Double) -> IO ()
  }

square :: Double -> Double
square x =
  x * x

relativeStdErr :: Sample -> Double
relativeStdErr vec =
  let mu = U.foldl1 (+) vec / fromIntegral (U.length vec)
   in let std_err = sqrt $ U.foldl1 (+) (U.map (square . subtract mu) vec) / fromIntegral (U.length vec - 1)
       in std_err / mu

-- Returns the next run count.
nextRunCount :: Int -> Double -> Double -> Int -> Int
nextRunCount runs rsd acor min_runs
  | runs < min_runs = min_runs - runs -- Minimum runs specified.
  | acor > 0.95 && rsd > 0.0010 = div runs 2
  | acor > 0.75 && rsd > 0.0015 = div runs 2
  | acor > 0.65 && rsd > 0.0025 = div runs 2
  | acor > 0.45 && rsd > 0.0050 = div runs 2
  | rsd > 0.01 = div runs 2
  | otherwise = 0

type BenchM = ExceptT T.Text IO

-- Keep on running benchmark until a completion criteria is met.
runLoop ::
  BenchM (RunResult, [T.Text]) ->
  RunOptions ->
  [(RunResult, [T.Text])] ->
  BenchM [(RunResult, [T.Text])]
runLoop do_run opts r = do
  let run_times = U.fromList $ map (fromIntegral . runMicroseconds . fst) r

  g <- create
  resampled <- liftIO $ resample g [Mean] 2500 run_times

  let rsd = relativeStdErr $ resamples (snd $ head resampled)
  let acor = case autocorrelation run_times of
        (x, _, _) -> fromMaybe 1 (x U.!? 1)

  let actions = do
        x <- do_run
        liftIO $ runResultAction opts ((runMicroseconds . fst) x, Just rsd)
        pure x

  case nextRunCount (length r) rsd acor (runRuns opts) of
    0 -> pure r
    x -> do
      r' <- replicateM x actions
      runLoop do_run opts (r ++ r')

-- Each benchmark is run for at least 0.5s wallclock time.
runTimed ::
  BenchM (RunResult, [T.Text]) ->
  RunOptions ->
  NominalDiffTime ->
  [(RunResult, [T.Text])] ->
  BenchM [(RunResult, [T.Text])]
runTimed do_run opts elapsed r = do
  let actions = do
        x <- do_run
        liftIO $ runResultAction opts ((runMicroseconds . fst) x, Nothing)
        pure x

  before <- liftIO getCurrentTime
  r' <- replicateM (1 + length r) actions
  after <- liftIO getCurrentTime

  let elapsed' = elapsed + diffUTCTime after before
  if 0.5 < elapsed'
    then pure (r ++ r')
    else runTimed do_run opts elapsed' (r ++ r')

-- | Run the benchmark program on the indicated dataset.
benchmarkDataset ::
  Server ->
  RunOptions ->
  FutharkExe ->
  FilePath ->
  T.Text ->
  Values ->
  Maybe Success ->
  FilePath ->
  IO (Either T.Text ([RunResult], T.Text))
benchmarkDataset server opts futhark program entry input_spec expected_spec ref_out = runExceptT $ do
  output_types <- cmdEither $ cmdOutputs server entry
  input_types <- cmdEither $ cmdInputs server entry
  let outs = ["out" <> T.pack (show i) | i <- [0 .. length output_types - 1]]
      ins = ["in" <> T.pack (show i) | i <- [0 .. length input_types - 1]]

  cmdMaybe . liftIO $ cmdClear server

  let freeOuts = cmdMaybe (cmdFree server outs)
      freeIns = cmdMaybe (cmdFree server ins)
      loadInput = valuesAsVars server (zip ins $ map inputType input_types) futhark dir input_spec
      reloadInput = freeIns >> loadInput

  loadInput

  let runtime l
        | Just l' <- T.stripPrefix "runtime: " l,
          [(x, "")] <- reads $ T.unpack l' =
            Just x
        | otherwise =
            Nothing

      doRun = do
        call_lines <- cmdEither (cmdCall server entry outs ins)
        when (any inputConsumed input_types) reloadInput
        case mapMaybe runtime call_lines of
          [call_runtime] -> pure (RunResult call_runtime, call_lines)
          [] -> throwError "Could not find runtime in output."
          ls -> throwError $ "Ambiguous runtimes: " <> T.pack (show ls)

  maybe_call_logs <- liftIO . timeout (runTimeout opts * 1000000) . runExceptT $ do
    -- First one uncounted warmup run.
    void $ cmdEither $ cmdCall server entry outs ins
    freeOuts

    ys <- runTimed (doRun <* freeOuts) opts 0 []

    xs <- runLoop (doRun <* freeOuts) opts ys

    y <- doRun

    pure $ xs ++ [y]

  call_logs <- case maybe_call_logs of
    Nothing ->
      throwError . T.pack $
        "Execution exceeded " ++ show (runTimeout opts) ++ " seconds."
    Just x -> liftEither x

  freeIns

  report <- cmdEither $ cmdReport server

  vs <- readResults server outs <* freeOuts

  maybe_expected <-
    liftIO $ maybe (pure Nothing) (fmap Just . getExpectedValues) expected_spec

  case maybe_expected of
    Just expected -> checkResult program expected vs
    Nothing -> pure ()

  pure
    ( map fst call_logs,
      T.unlines $ map (T.unlines . snd) call_logs <> report
    )
  where
    getExpectedValues (SuccessValues vs) =
      getValues futhark dir vs
    getExpectedValues SuccessGenerateValues =
      getExpectedValues $ SuccessValues $ InFile ref_out

    dir = takeDirectory program

-- | How to compile a benchmark.
data CompileOptions = CompileOptions
  { compFuthark :: String,
    compBackend :: String,
    compOptions :: [String]
  }

progNotFound :: String -> String
progNotFound s = s ++ ": command not found"

-- | Compile and produce reference datasets.
prepareBenchmarkProgram ::
  MonadIO m =>
  Maybe Int ->
  CompileOptions ->
  FilePath ->
  [InputOutputs] ->
  m (Either (String, Maybe SBS.ByteString) ())
prepareBenchmarkProgram concurrency opts program cases = do
  let futhark = compFuthark opts

  ref_res <- runExceptT $ ensureReferenceOutput concurrency (FutharkExe futhark) "c" program cases
  case ref_res of
    Left err ->
      pure $
        Left
          ( "Reference output generation for " ++ program ++ " failed:\n"
              ++ unlines (map T.unpack err),
            Nothing
          )
    Right () -> do
      (futcode, _, futerr) <-
        liftIO $
          readProcessWithExitCode
            futhark
            ( [compBackend opts, program, "-o", binaryName program, "--server"]
                <> compOptions opts
            )
            ""

      case futcode of
        ExitSuccess -> pure $ Right ()
        ExitFailure 127 -> pure $ Left (progNotFound futhark, Nothing)
        ExitFailure _ -> pure $ Left ("Compilation of " ++ program ++ " failed:\n", Just futerr)
