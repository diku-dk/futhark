-- | Facilities for handling Futhark benchmark results.  A Futhark
-- benchmark program is just like a Futhark test program.
module Futhark.Bench
  ( RunResult (..),
    DataResult (..),
    BenchResult (..),
    Result (..),
    encodeBenchResults,
    decodeBenchResults,
    benchmarkDataset,
    RunOptions (..),
    prepareBenchmarkProgram,
    BinaryName,
    CompileOptions (..),
    module Futhark.Profile,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (..), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.ByteString.Char8 qualified as SBS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.DList qualified as DL
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Time.Clock
import Data.Vector.Unboxed qualified as U
import Futhark.Profile
import Futhark.Server
import Futhark.Test
import Futhark.Util (showText)
import Statistics.Autocorrelation (autocorrelation)
import Statistics.Sample (fastStdDev, mean)
import System.Exit
import System.FilePath
import System.Process.ByteString (readProcessWithExitCode)
import System.Timeout (timeout)

-- | The runtime of a single succesful run.
newtype RunResult = RunResult {runMicroseconds :: Int}
  deriving (Eq, Show)

-- | The measurements resulting from various successful runs of a
-- benchmark (same dataset).
data Result = Result
  { -- | Runtime of every run.
    runResults :: [RunResult],
    -- | Memory usage.
    memoryMap :: M.Map T.Text Int,
    -- | The error output produced during execution.  Often 'Nothing'
    -- for space reasons, and otherwise only the output from the last
    -- run.
    stdErr :: Maybe T.Text,
    -- | Profiling report.  This will have been measured based on the
    -- last run.
    report :: Maybe ProfilingReport
  }
  deriving (Eq, Show)

-- | The results for a single named dataset is either an error message, or
-- a result.
data DataResult = DataResult T.Text (Either T.Text Result)
  deriving (Eq, Show)

-- | The results for all datasets for some benchmark program.
data BenchResult = BenchResult
  { benchResultProg :: FilePath,
    benchResultResults :: [DataResult]
  }
  deriving (Eq, Show)

newtype DataResults = DataResults {unDataResults :: [DataResult]}

newtype BenchResults = BenchResults {unBenchResults :: [BenchResult]}

instance JSON.ToJSON Result where
  toJSON (Result runres memmap err profiling) =
    JSON.toJSON (runres, memmap, err, profiling)

instance JSON.FromJSON Result where
  parseJSON = fmap f . JSON.parseJSON
    where
      f (runres, memmap, err, profiling) = Result runres memmap err profiling

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
        DataResult (JSON.toText k)
          <$> ((Right <$> success v) <|> (Left <$> JSON.parseJSON v))
      success = JSON.withObject "result" $ \o ->
        Result
          <$> o JSON..: "runtimes"
          <*> o JSON..: "bytes"
          <*> o JSON..:? "stderr"
          <*> o JSON..:? "profiling"

dataResultJSON :: DataResult -> (JSON.Key, JSON.Value)
dataResultJSON (DataResult desc (Left err)) =
  (JSON.fromText desc, JSON.toJSON err)
dataResultJSON (DataResult desc (Right (Result runtimes bytes progerr_opt profiling_opt))) =
  ( JSON.fromText desc,
    JSON.object $
      [ ("runtimes", JSON.toJSON $ map runMicroseconds runtimes),
        ("bytes", JSON.toJSON bytes)
      ]
        <> case progerr_opt of
          Nothing -> []
          Just progerr -> [("stderr", JSON.toJSON progerr)]
        <> case profiling_opt of
          Nothing -> []
          Just profiling -> [("profiling", JSON.toJSON profiling)]
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
  { -- | Applies both to initial and convergence phase.
    runMinRuns :: Int,
    runMinTime :: NominalDiffTime,
    runTimeout :: Int,
    runVerbose :: Int,
    -- | If true, run the convergence phase.
    runConvergencePhase :: Bool,
    -- | Stop convergence once this much time has passed.
    runConvergenceMaxTime :: NominalDiffTime,
    -- | Invoked for every runtime measured during the run.  Can be
    -- used to provide a progress bar.
    runResultAction :: (Int, Maybe Double) -> IO (),
    -- | Perform a final run at the end with profiling information
    -- enabled.
    runProfile :: Bool
  }

-- | A list of @(autocorrelation,rse)@ pairs.  When the
-- autocorrelation is above the first element and the RSE is above the
-- second element, we want more runs.
convergenceCriteria :: [(Double, Double)]
convergenceCriteria =
  [ (0.95, 0.0010),
    (0.75, 0.0015),
    (0.65, 0.0025),
    (0.45, 0.0050),
    (0.00, 0.0100)
  ]

-- Returns the next run count.
nextRunCount :: Int -> Double -> Double -> Int
nextRunCount runs rse acor = if any check convergenceCriteria then div runs 2 else 0
  where
    check (acor_lb, rse_lb) = acor > acor_lb && rse > rse_lb

type BenchM = ExceptT T.Text IO

-- Do the minimum number of runs.
runMinimum ::
  BenchM (RunResult, [T.Text]) ->
  RunOptions ->
  Int ->
  NominalDiffTime ->
  DL.DList (RunResult, [T.Text]) ->
  BenchM (DL.DList (RunResult, [T.Text]))
runMinimum do_run opts runs_done elapsed r = do
  let actions = do
        x <- do_run
        liftIO $ runResultAction opts (runMicroseconds (fst x), Nothing)
        pure x

  -- Figure out how much we have left to do.
  let todo
        | runs_done < runMinRuns opts =
            runMinRuns opts - runs_done
        | otherwise =
            -- Guesstimate how many runs we need to reach the minimum
            -- time.
            let time_per_run = elapsed / fromIntegral runs_done
             in ceiling ((runMinTime opts - elapsed) / time_per_run)

  -- Note that todo might be negative if minimum time has been exceeded.
  if todo <= 0
    then pure r
    else do
      before <- liftIO getCurrentTime
      r' <- DL.fromList <$> replicateM todo actions
      after <- liftIO getCurrentTime
      let elapsed' = elapsed + diffUTCTime after before
      runMinimum do_run opts (runs_done + todo) elapsed' (r <> r')

-- Do more runs until a convergence criterion is reached.
runConvergence ::
  BenchM (RunResult, [T.Text]) ->
  RunOptions ->
  DL.DList (RunResult, [T.Text]) ->
  BenchM (DL.DList (RunResult, [T.Text]))
runConvergence do_run opts initial_r =
  let runtimes = resultRuntimes (DL.toList initial_r)
      (n, _, rse, acor) = runtimesMetrics runtimes
   in -- If the runtimes collected during the runMinimum phase are
      -- unstable enough that we need more in order to converge, we throw
      -- away the runMinimum runtimes.  This is because they often exhibit
      -- behaviour similar to a "warmup" period, and hence function as
      -- outliers that poison the metrics we use to determine convergence.
      -- By throwing them away we converge much faster, and still get the
      -- right result.
      case nextRunCount n rse acor of
        x
          | x > 0,
            runConvergencePhase opts ->
              moreRuns mempty mempty rse (x `max` runMinRuns opts)
          | otherwise ->
              pure initial_r
  where
    resultRuntimes =
      U.fromList . map (fromIntegral . runMicroseconds . fst)

    runtimesMetrics runtimes =
      let n = U.length runtimes
          rse = (fastStdDev runtimes / sqrt (fromIntegral n)) / mean runtimes
          (x, _, _) = autocorrelation runtimes
       in ( n,
            realToFrac (U.sum runtimes) :: NominalDiffTime,
            rse,
            fromMaybe 1 (x U.!? 1)
          )

    sample rse = do
      x <- do_run
      liftIO $ runResultAction opts (runMicroseconds (fst x), Just rse)
      pure x

    moreRuns runtimes r rse x = do
      r' <- replicateM x $ sample rse
      loop (runtimes <> resultRuntimes r') (r <> DL.fromList r')

    loop runtimes r = do
      let (n, total, rse, acor) = runtimesMetrics runtimes
      case nextRunCount n rse acor of
        x
          | x > 0,
            total < runConvergenceMaxTime opts ->
              moreRuns runtimes r rse x
          | otherwise ->
              pure r

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
  IO (Either T.Text ([RunResult], T.Text, ProfilingReport))
benchmarkDataset server opts futhark program entry input_spec expected_spec ref_out = runExceptT $ do
  output_types <- cmdEither $ cmdOutputs server entry
  input_types <- cmdEither $ cmdInputs server entry
  let outs = ["out" <> showText i | i <- [0 .. length output_types - 1]]
      ins = ["in" <> showText i | i <- [0 .. length input_types - 1]]

  cmdMaybe . liftIO $ cmdClear server

  cmdMaybe . liftIO $ cmdPauseProfiling server

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
          ls -> throwError $ "Ambiguous runtimes: " <> showText ls

  maybe_call_logs <- liftIO . timeout (runTimeout opts * 1000000) . runExceptT $ do
    -- First one uncounted warmup run.
    void $ cmdEither $ cmdCall server entry outs ins

    ys <- runMinimum (freeOuts *> doRun) opts 0 0 mempty

    xs <- runConvergence (freeOuts *> doRun) opts ys

    -- Possibly a profiled run at the end.
    profile_log <-
      if not (runProfile opts)
        then pure Nothing
        else do
          cmdMaybe . liftIO $ cmdUnpauseProfiling server
          profile_log <- freeOuts *> doRun
          cmdMaybe . liftIO $ cmdPauseProfiling server
          pure $ Just profile_log

    vs <- readResults server outs <* freeOuts

    pure (vs, DL.toList xs, profile_log)

  (vs, call_logs, profile_log) <- case maybe_call_logs of
    Nothing ->
      throwError . T.pack $
        "Execution exceeded " ++ show (runTimeout opts) ++ " seconds."
    Just x -> liftEither x

  freeIns

  report <- cmdEither $ cmdReport server

  report' <-
    maybe (throwError "Program produced invalid profiling report.") pure $
      profilingReportFromText (T.unlines report)

  maybe_expected <-
    liftIO $ maybe (pure Nothing) (fmap Just . getExpectedValues) expected_spec

  case maybe_expected of
    Just expected -> checkResult program expected vs
    Nothing -> pure ()

  pure
    ( map fst call_logs,
      T.unlines $ snd $ fromMaybe (last call_logs) profile_log,
      report'
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
  (MonadIO m) =>
  Maybe Int ->
  CompileOptions ->
  BinaryName ->
  FilePath ->
  [InputOutputs] ->
  m (Either (String, Maybe SBS.ByteString) ())
prepareBenchmarkProgram concurrency opts binaryName program cases = do
  let futhark = compFuthark opts

  ref_res <- runExceptT $ ensureReferenceOutput concurrency (FutharkExe futhark) "c" program cases
  case ref_res of
    Left err ->
      pure $
        Left
          ( "Reference output generation for "
              ++ program
              ++ " failed:\n"
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
