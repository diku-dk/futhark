{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Simple tool for benchmarking Futhark programs.  Use the @--json@
-- flag for machine-readable output.
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except hiding (forM_)
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.Console.GetOpt
import System.FilePath
import System.Directory
import System.IO
import System.IO.Temp
import System.Timeout
import System.Process.ByteString (readProcessWithExitCode)
import System.Exit
import qualified Text.JSON as JSON
import Text.Printf

import Prelude

import Futhark.Test
import Futhark.Util.Pretty (prettyText)
import Futhark.Util.Options

data BenchOptions = BenchOptions
                   { optCompiler :: String
                   , optRuns :: Int
                   , optExtraOptions :: [String]
                   , optJSON :: Maybe FilePath
                   , optTimeout :: Int
                   }

initialBenchOptions :: BenchOptions
initialBenchOptions = BenchOptions "futhark-c" 10 [] Nothing (-1)

-- | The name we use for compiled programs.
binaryName :: FilePath -> FilePath
binaryName = (`replaceExtension` "bin")

newtype RunResult = RunResult { runMicroseconds :: Int }
data DataResult = DataResult String (Either T.Text [RunResult])
data BenchResult = BenchResult FilePath [DataResult]

resultsToJSON :: [BenchResult] -> JSON.JSValue
resultsToJSON = JSON.JSObject . JSON.toJSObject . map benchResultToJSObject
  where benchResultToJSObject
          :: BenchResult
          -> (String, JSON.JSValue)
        benchResultToJSObject (BenchResult prog rs) =
          (prog, JSON.JSObject $ JSON.toJSObject
                 [("datasets", JSON.JSObject $ JSON.toJSObject $
                               map dataResultToJSObject rs)])
        dataResultToJSObject
          :: DataResult
          -> (String, JSON.JSValue)
        dataResultToJSObject (DataResult desc (Left err)) =
          (desc, JSON.showJSON err)
        dataResultToJSObject (DataResult desc (Right runtimes)) =
          (desc, JSON.JSObject $ JSON.toJSObject
                 [("runtimes", JSON.showJSON $ map runMicroseconds runtimes)])

runBenchmarks :: BenchOptions -> [FilePath] -> IO ()
runBenchmarks opts paths = do
  -- We force line buffering to ensure that we produce running output.
  -- Otherwise, CI tools and the like may believe we are hung and kill
  -- us.
  hSetBuffering stdout LineBuffering
  benchmarks <- testSpecsFromPaths paths
  compiled_benchmarks <- catMaybes <$> mapM (compileBenchmark opts) benchmarks
  results <- mapM (runBenchmark opts) compiled_benchmarks
  case optJSON opts of
    Nothing -> return ()
    Just file -> writeFile file $ JSON.encode $ resultsToJSON results

  when (anythingFailed results) $ exitWith $ ExitFailure 1

anythingFailed :: [BenchResult] -> Bool
anythingFailed = any failedBenchResult
  where failedBenchResult (BenchResult _ xs) =
          any failedResult xs
        failedResult (DataResult _ Left{}) = True
        failedResult _                     = False

compileBenchmark :: BenchOptions -> (FilePath, ProgramTest)
                 -> IO (Maybe (FilePath, [InputOutputs]))
compileBenchmark opts (program, spec) =
  case testAction spec of
    RunCases cases | "nobench" `notElem` testTags spec,
                     "disable" `notElem` testTags spec,
                     any hasRuns cases -> do

      putStr $ "Compiling " ++ program ++ "...\n"
      (futcode, _, futerr) <-
        liftIO $ readProcessWithExitCode compiler
        [program, "-o", binaryName program] ""

      case futcode of
        ExitSuccess     -> return $ Just (program, cases)
        ExitFailure 127 -> do putStrLn $ "Failed:\n" ++ progNotFound compiler
                              return Nothing
        ExitFailure _   -> do putStrLn "Failed:\n"
                              SBS.putStrLn futerr
                              return Nothing
    _ ->
      return Nothing
  where compiler = optCompiler opts

        hasRuns (InputOutputs _ runs) = not $ null runs

runBenchmark :: BenchOptions -> (FilePath, [InputOutputs]) -> IO BenchResult
runBenchmark opts (program, cases) = do
  putStr $ "Results for " ++ program ++ ":\n"
  BenchResult program . catMaybes . concat <$> mapM forInputOutputs cases
  where forInputOutputs (InputOutputs "main" runs) =
          mapM (runBenchmarkCase opts program) runs
        forInputOutputs InputOutputs{} =
          return []

reportResult :: [RunResult] -> IO ()
reportResult [] =
  print (0::Int)
reportResult results = do
  let runtimes = map (fromIntegral . runMicroseconds) results
      avg = sum runtimes / genericLength runtimes
      rel_dev = stddevp runtimes / mean runtimes :: Double
  putStrLn $ printf "%.2f" avg ++ "us (average; relative standard deviation: " ++
    printf "%.2f" rel_dev ++ ")"

progNotFound :: String -> String
progNotFound s = s ++ ": command not found"

type BenchM = ExceptT T.Text IO

runBenchM :: BenchM a -> IO (Either T.Text a)
runBenchM = runExceptT

io :: IO a -> BenchM a
io = liftIO

runBenchmarkCase :: BenchOptions -> FilePath -> TestRun -> IO (Maybe DataResult)
runBenchmarkCase _ _ (TestRun _ _ RunTimeFailure{} _) =
  return Nothing -- Not our concern, we are not a testing tool.
runBenchmarkCase _ _ (TestRun NoBench _ _ _) =
  return Nothing -- Too small to bother benchmarking.
runBenchmarkCase opts program (TestRun _ input_spec (Succeeds expected_spec) dataset_desc) =
  -- We store the runtime in a temporary file.
  withSystemTempFile "futhark-bench" $ \tmpfile h -> do
  hClose h -- We will be writing and reading this ourselves.
  input <- getValuesBS dir input_spec
  let getValuesAndBS vs = do
        vs' <- getValues dir vs
        bs <- getValuesBS dir vs
        return (LBS.toStrict bs, vs')
  maybe_expected <- maybe (return Nothing) (fmap Just . getValuesAndBS) expected_spec
  let options = optExtraOptions opts++["-t", tmpfile, "-r", show $ optRuns opts, "-b"]

  -- Report the dataset name before running the program, so that if an
  -- error occurs it's easier to see where.
  putStr $ "dataset " ++ dataset_desc ++ ": "
  hFlush stdout

  -- Explicitly prefixing the current directory is necessary for
  -- readProcessWithExitCode to find the binary when binOutputf has
  -- no program component.
  run_res <-
    timeout (optTimeout opts * 1000000) $
    readProcessWithExitCode ("." </> binaryName program) options $
    LBS.toStrict input

  fmap (Just .  DataResult dataset_desc) $ runBenchM $ case run_res of
    Just (progCode, output, progerr) ->
      do
        case maybe_expected of
          Nothing ->
            didNotFail program progCode $ T.decodeUtf8 progerr
          Just expected ->
            compareResult program expected =<<
            runResult program progCode output progerr
        runtime_result <- io $ T.readFile tmpfile
        runtimes <- case mapM readRuntime $ T.lines runtime_result of
          Just runtimes -> return $ map RunResult runtimes
          Nothing -> itWentWrong $ "Runtime file has invalid contents:\n" <> runtime_result

        io $ reportResult runtimes
        return runtimes
    Nothing ->
      itWentWrong $ T.pack $ "Execution exceeded " ++ show (optTimeout opts) ++ " seconds."

  where dir = takeDirectory program


readRuntime :: T.Text -> Maybe Int
readRuntime s = case reads $ T.unpack s of
  [(runtime, _)] -> Just runtime
  _              -> Nothing

didNotFail :: FilePath -> ExitCode -> T.Text -> BenchM ()
didNotFail _ ExitSuccess _ =
  return ()
didNotFail program (ExitFailure code) stderr_s =
  itWentWrong $ T.pack $ program ++ " failed with error code " ++ show code ++
  " and output:\n" ++ T.unpack stderr_s

itWentWrong :: (MonadError T.Text m, MonadIO m) =>
               T.Text -> m a
itWentWrong t = do
  liftIO $ putStrLn $ T.unpack t
  throwError t

runResult :: (MonadError T.Text m, MonadIO m) =>
             FilePath
          -> ExitCode
          -> SBS.ByteString
          -> SBS.ByteString
          -> m (SBS.ByteString, [Value])
runResult program ExitSuccess stdout_s _ =
  case valuesFromByteString "stdout" $ LBS.fromStrict stdout_s of
    Left e   -> do
      actual <- liftIO $ writeOutFile program "actual" stdout_s
      itWentWrong $ T.pack $ show e <> "\n(See " <> actual <> ")"
    Right vs -> return (stdout_s, vs)
runResult program (ExitFailure code) _ stderr_s =
  itWentWrong $ T.pack $ program ++ " failed with error code " ++ show code ++
  " and output:\n" ++ T.unpack (T.decodeUtf8 stderr_s)

writeOutFile :: FilePath -> String -> SBS.ByteString -> IO FilePath
writeOutFile base ext content =
  attempt (0::Int)
  where template = base `replaceExtension` ext
        attempt i = do
          let filename = template ++ "-" ++ show i
          exists <- doesFileExist filename
          if exists
            then attempt $ i+1
            else do SBS.writeFile filename content
                    return filename

compareResult :: (MonadError T.Text m, MonadIO m) =>
                 FilePath -> (SBS.ByteString, [Value]) -> (SBS.ByteString, [Value])
              -> m ()
compareResult program (expected_bs, expected_vs) (actual_bs, actual_vs) =
  case compareValues actual_vs expected_vs of
    Just mismatch -> do
      actualf <- liftIO $ writeOutFile program "actual" actual_bs
      expectedf <- liftIO $ writeOutFile program "expected" expected_bs
      itWentWrong $ T.pack $
        actualf ++ " and " ++ expectedf ++ " do not match:\n" ++ show mismatch
    Nothing ->
      return ()

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
  , Option [] ["compiler"]
    (ReqArg (\prog ->
              Right $ \config -> config { optCompiler = prog })
     "PROGRAM")
    "The compiler used (defaults to 'futhark-c')."
  , Option "p" ["pass-option"]
    (ReqArg (\opt ->
               Right $ \config ->
               config { optExtraOptions = opt : optExtraOptions config })
     "OPT")
    "Pass this option to programs being run."
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
  ]
  where max_timeout :: Int
        max_timeout = maxBound `div` 1000000

main :: IO ()
main = mainWithOptions initialBenchOptions commandLineOptions $ \progs config ->
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
