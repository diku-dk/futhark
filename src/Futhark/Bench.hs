{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Facilities for handling Futhark benchmark results.  A Futhark
-- benchmark program is just like a Futhark test program.
module Futhark.Bench
  ( RunResult (..)
  , DataResult(..)
  , BenchResult(..)
  , encodeBenchResults
  , decodeBenchResults

  , binaryName

  , benchmarkDataset
  , RunOptions(..)

  , prepareBenchmarkProgram
  , CompileOptions(..)
  )
  where

import Control.Applicative
import Control.Monad.Except
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.FilePath
import System.Exit
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.Process.ByteString (readProcessWithExitCode)
import System.Timeout (timeout)

import Futhark.Test

newtype RunResult = RunResult { runMicroseconds :: Int }
                  deriving (Eq, Show)
data DataResult = DataResult String (Either T.Text ([RunResult], T.Text))
                deriving (Eq, Show)
data BenchResult = BenchResult FilePath [DataResult]
                 deriving (Eq, Show)

-- Intermediate types to help write the JSON instances.
newtype DataResults = DataResults { unDataResults :: [DataResult] }
newtype BenchResults = BenchResults { unBenchResults :: [BenchResult] }

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
    DataResults <$> mapM datasetResult (HM.toList o)
    where datasetResult (k, v) =
            DataResult (T.unpack k) <$>
            ((Right <$> success v) <|> (Left <$> JSON.parseJSON v))
          success = JSON.withObject "result" $ \o ->
            (,) <$> o JSON..: "runtimes" <*> o JSON..: "stderr"

dataResultJSON :: DataResult -> (T.Text, JSON.Value)
dataResultJSON (DataResult desc (Left err)) =
  (T.pack desc, JSON.toJSON err)
dataResultJSON (DataResult desc (Right (runtimes, progerr))) =
  (T.pack desc, JSON.object
                [("runtimes", JSON.toJSON $ map runMicroseconds runtimes),
                 ("stderr", JSON.toJSON progerr)])

benchResultJSON :: BenchResult -> (T.Text, JSON.Value)
benchResultJSON (BenchResult prog r) =
  (T.pack prog,
   JSON.Object $ HM.singleton "datasets" (JSON.toJSON $ DataResults r))

instance JSON.ToJSON BenchResults where
  toJSON (BenchResults rs) =
    JSON.Object $ HM.fromList $ map benchResultJSON rs

instance JSON.FromJSON BenchResults where
  parseJSON = JSON.withObject "benchmarks" $ \o ->
    BenchResults <$> mapM onBenchmark (HM.toList o)
    where onBenchmark (k, v) =
            BenchResult (T.unpack k) <$>
            JSON.withObject "benchmark" onBenchmark' v
          onBenchmark' o =
            fmap unDataResults . JSON.parseJSON =<< o JSON..: "datasets"

-- | Transform benchmark results to a JSON bytestring.
encodeBenchResults :: [BenchResult] -> LBS.ByteString
encodeBenchResults = JSON.encode . BenchResults

-- | Decode benchmark results from a JSON bytestring.
decodeBenchResults :: LBS.ByteString -> Either String [BenchResult]
decodeBenchResults = fmap unBenchResults . JSON.eitherDecode'

--- Running benchmarks

readRuntime :: T.Text -> Maybe Int
readRuntime s = case reads $ T.unpack s of
  [(runtime, _)] -> Just runtime
  _              -> Nothing

didNotFail :: FilePath -> ExitCode -> T.Text -> ExceptT T.Text IO ()
didNotFail _ ExitSuccess _ =
  return ()
didNotFail program (ExitFailure code) stderr_s =
  throwError $ T.pack $ program ++ " failed with error code " ++ show code ++
  " and output:\n" ++ T.unpack stderr_s

compareResult :: (MonadError T.Text m, MonadIO m) =>
                 FilePath -> (SBS.ByteString, [Value]) -> (SBS.ByteString, [Value])
              -> m ()
compareResult program (expected_bs, expected_vs) (actual_bs, actual_vs) =
  case compareValues1 actual_vs expected_vs of
    Just mismatch -> do
      let actualf = program `replaceExtension` "actual"
          expectedf = program `replaceExtension` "expected"
      liftIO $ SBS.writeFile actualf actual_bs
      liftIO $ SBS.writeFile expectedf expected_bs
      throwError $ T.pack actualf <> " and " <> T.pack expectedf <>
        " do not match:\n" <> T.pack (show mismatch)
    Nothing ->
      return ()

runResult :: (MonadError T.Text m, MonadIO m) =>
             FilePath
          -> ExitCode
          -> SBS.ByteString
          -> SBS.ByteString
          -> m (SBS.ByteString, [Value])
runResult program ExitSuccess stdout_s _ =
  case valuesFromByteString "stdout" $ LBS.fromStrict stdout_s of
    Left e   -> do
      let actualf = program `replaceExtension` "actual"
      liftIO $ SBS.writeFile actualf stdout_s
      throwError $ T.pack $ show e <> "\n(See " <> actualf <> ")"
    Right vs -> return (stdout_s, vs)
runResult program (ExitFailure code) _ stderr_s =
  throwError $ T.pack $ binaryName program ++ " failed with error code " ++ show code ++
  " and output:\n" ++ T.unpack (T.decodeUtf8 stderr_s)

-- | How to run a benchmark.
data RunOptions =
  RunOptions
  { runRunner :: String
  , runRuns :: Int
  , runExtraOptions :: [String]
  , runTimeout :: Int
  }

-- | Run the benchmark program on the indicated dataset.
benchmarkDataset :: RunOptions -> FilePath -> T.Text
                 -> Values -> Maybe Success -> FilePath
                 -> IO (Either T.Text ([RunResult], T.Text))
benchmarkDataset opts program entry input_spec expected_spec ref_out =
  -- We store the runtime in a temporary file.
  withSystemTempFile "futhark-bench" $ \tmpfile h -> do
  hClose h -- We will be writing and reading this ourselves.
  input <- getValuesBS dir input_spec
  let getValuesAndBS (SuccessValues vs) = do
        vs' <- getValues dir vs
        bs <- getValuesBS dir vs
        return (LBS.toStrict bs, vs')
      getValuesAndBS SuccessGenerateValues =
        getValuesAndBS $ SuccessValues $ InFile ref_out
  maybe_expected <- maybe (return Nothing) (fmap Just . getValuesAndBS) expected_spec
  let options = runExtraOptions opts ++ ["-e", T.unpack entry,
                                         "-t", tmpfile,
                                         "-r", show $ runRuns opts,
                                         "-b"]

  -- Explicitly prefixing the current directory is necessary for
  -- readProcessWithExitCode to find the binary when binOutputf has
  -- no program component.
  let (to_run, to_run_args)
        | null $ runRunner opts = ("." </> binaryName program, options)
        | otherwise = (runRunner opts, binaryName program : options)

  run_res <-
    timeout (runTimeout opts * 1000000) $
    readProcessWithExitCode to_run to_run_args $
    LBS.toStrict input

  runExceptT $ case run_res of
    Just (progCode, output, progerr) -> do
      case maybe_expected of
        Nothing ->
          didNotFail program progCode $ T.decodeUtf8 progerr
        Just expected ->
          compareResult program expected =<<
          runResult program progCode output progerr
      runtime_result <- liftIO $ T.readFile tmpfile
      runtimes <- case mapM readRuntime $ T.lines runtime_result of
        Just runtimes -> return $ map RunResult runtimes
        Nothing -> throwError $ "Runtime file has invalid contents:\n" <> runtime_result

      return (runtimes, T.decodeUtf8 progerr)
    Nothing ->
      throwError $ T.pack $ "Execution exceeded " ++ show (runTimeout opts) ++ " seconds."

  where dir = takeDirectory program

-- | How to compile a benchmark.
data CompileOptions =
  CompileOptions
  { compFuthark :: String
  , compBackend :: String
  }

progNotFound :: String -> String
progNotFound s = s ++ ": command not found"

-- | Compile and produce reference datasets.
prepareBenchmarkProgram :: MonadIO m =>
                           Maybe Int
                        -> CompileOptions
                        -> FilePath
                        -> [InputOutputs]
                        -> m (Either (String, Maybe SBS.ByteString) ())
prepareBenchmarkProgram concurrency opts program cases = do
  let futhark = compFuthark opts

  ref_res <- runExceptT $ ensureReferenceOutput concurrency futhark "c" program cases
  case ref_res of
    Left err ->
      return $ Left ("Reference output generation for " ++ program ++ " failed:\n" ++
                     unlines (map T.unpack err),
                     Nothing)

    Right () -> do
      (futcode, _, futerr) <- liftIO $ readProcessWithExitCode futhark
                              [compBackend opts, program, "-o", binaryName program] ""

      case futcode of
        ExitSuccess     -> return $ Right ()
        ExitFailure 127 -> return $ Left (progNotFound futhark, Nothing)
        ExitFailure _   -> return $ Left ("Compilation of " ++ program ++ " failed:\n", Just futerr)
