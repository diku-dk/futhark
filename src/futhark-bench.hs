{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simple tool for benchmarking Futhark programs.  Supports raw
-- output if you want to do your own analysis, but will normally print
-- the average runtime.

module Main (main) where

import Control.Monad
import Control.Monad.Except hiding (forM_)
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.GetOpt
import System.FilePath
import System.Directory
import System.IO
import System.IO.Temp
import System.Process.Text (readProcessWithExitCode)
import System.Exit
import qualified Text.JSON as JSON
import Text.Printf

import Futhark.Test
import Futhark.Util.Pretty (prettyText)
import Futhark.Util.Options

import Prelude

data BenchOptions = BenchOptions
                   { optCompiler :: String
                   , optRuns :: Int
                   , optRawReporting :: Bool
                   , optExtraOptions :: [String]
                   , optValidate :: Bool
                   , optJSON :: Maybe FilePath
                   }

initialBenchOptions :: BenchOptions
initialBenchOptions = BenchOptions "futhark-c" 10 False [] True Nothing

-- | The name we use for compiled programs.
binaryName :: FilePath -> FilePath
binaryName = (`replaceExtension` "bin")

data RunResult = RunResult { runMicroseconds :: Int }

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
  benchmarks <- testSpecsFromPaths paths
  results <- mapM (uncurry $ runBenchmark opts) benchmarks
  case optJSON opts of
    Nothing -> return ()
    Just file -> writeFile file $ JSON.encode $ resultsToJSON results

runBenchmark :: BenchOptions -> FilePath -> ProgramTest -> IO BenchResult
runBenchmark opts program spec = do
  putStrLn $ program ++ ":"
  case testAction spec of
    RunCases cases -> do
      (futcode, _, futerr) <-
        liftIO $ readProcessWithExitCode compiler
        [program, "-o", binaryName program] ""
      case futcode of
        ExitFailure 127 -> fail $ progNotFound compiler
        ExitFailure _   -> fail $ T.unpack futerr
        ExitSuccess     -> return ()

      BenchResult program . catMaybes <$>
        zipWithM (runBenchmarkCase opts program) [0..] cases
    _ ->
      return $ BenchResult program []
  where compiler = optCompiler opts

reportResult :: Bool -> [RunResult] -> IO ()
reportResult True runtimes =
  putStrLn $ unwords $ map (show . runMicroseconds) runtimes
reportResult False [] =
  print (0::Int)
reportResult False results = do
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

runBenchmarkCase :: BenchOptions -> FilePath -> Int -> TestRun -> IO (Maybe DataResult)
runBenchmarkCase _ _ _ (TestRun _ _ RunTimeFailure{}) =
  return Nothing -- Not our concern, we are not a testing tool.
runBenchmarkCase _ _ _ (TestRun NoBench _ _) =
  return Nothing -- Too small to bother benchmarking.
runBenchmarkCase opts program i (TestRun _ input_spec (Succeeds expected_spec)) =
  -- We store the runtime in a temporary file.
  withSystemTempFile "futhark-bench" $ \tmpfile h -> do
  hClose h -- We will be writing and reading this ourselves.
  input <- getValuesText dir input_spec
  maybe_expected <-
    if optValidate opts
    then maybe (return Nothing) (fmap Just . getValues dir) expected_spec
    else return Nothing
  let options = optExtraOptions opts++["-t", tmpfile, "-r", show $ optRuns opts]

  -- Explicitly prefixing the current directory is necessary for
  -- readProcessWithExitCode to find the binary when binOutputf has
  -- no program component.
  (progCode, output, progerr) <-
    readProcessWithExitCode ("." </> binaryName program) options input
  fmap (Just .  DataResult dataset_desc) $ runBenchM $ do
    case maybe_expected of
      Nothing ->
        didNotFail program progCode progerr
      Just expected ->
        compareResult program expected =<< runResult program progCode output progerr
    runtime_result <- io $ T.readFile tmpfile
    runtimes <- case mapM readRuntime $ T.lines runtime_result of
      Just runtimes -> return $ map RunResult runtimes
      Nothing -> throwError $ "Runtime file has invalid contents:\n" <> runtime_result

    io $ putStr $ "dataset " ++ dataset_desc ++ ": "
    io $ reportResult (optRawReporting opts) runtimes
    return runtimes

  where dir = takeDirectory program
        dataset_desc = case input_spec of
                         InFile path -> path
                         Values{} -> "#" ++ show i

readRuntime :: T.Text -> Maybe Int
readRuntime s = case reads $ T.unpack s of
  [(runtime, _)] -> Just runtime
  _              -> Nothing

didNotFail :: FilePath -> ExitCode -> T.Text -> BenchM ()
didNotFail _ ExitSuccess _ =
  return ()
didNotFail program (ExitFailure code) stderr_s =
  fail $ program ++ " failed with error code " ++ show code ++
  " and output:\n" ++ T.unpack stderr_s

runResult :: MonadIO m =>
             FilePath
          -> ExitCode
          -> T.Text
          -> T.Text
          -> m [Value]
runResult program ExitSuccess stdout_s _ =
  case valuesFromText "stdout" stdout_s of
    Left e   -> do
      actual <- liftIO $ writeOutFile program "actual" stdout_s
      fail $ show e <> "\n(See " <> actual <> ")"
    Right vs -> return vs
runResult program (ExitFailure code) _ stderr_s =
  fail $ program ++ " failed with error code " ++ show code ++
  " and output:\n" ++ T.unpack stderr_s

writeOutFile :: FilePath -> String -> T.Text -> IO FilePath
writeOutFile base ext content =
  attempt (0::Int)
  where template = base `replaceExtension` ext
        attempt i = do
          let filename = template ++ "-" ++ show i
          exists <- doesFileExist filename
          if exists
            then attempt $ i+1
            else do T.writeFile filename content
                    return filename

compareResult :: MonadIO m => FilePath -> [Value] -> [Value]
              -> m ()
compareResult program expectedResult actualResult =
  case compareValues actualResult expectedResult of
    Just mismatch -> do
      actualf <-
        liftIO $ writeOutFile program "actual" $
        T.unlines $ map prettyText actualResult
      expectedf <-
        liftIO $ writeOutFile program "expected" $
        T.unlines $ map prettyText expectedResult
      fail $ actualf ++ " and " ++ expectedf ++ " do not match:\n" ++ show mismatch
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
  , Option [] ["raw"]
    (NoArg $ Right $ \config -> config { optRawReporting = True })
    "Print all runtime numbers, not just the average."
  , Option "p" ["pass-option"]
    (ReqArg (\opt ->
               Right $ \config ->
               config { optExtraOptions = opt : optExtraOptions config })
     "OPT")
    "Pass this option to programs being run."
  , Option "n" ["no-validate"]
    (NoArg $ Right $ \config -> config { optValidate = False })
    "Do not validate results."
  , Option [] ["json"]
    (ReqArg (\file ->
               Right $ \config -> config { optJSON = Just file})
    "FILE")
    "Write results in JSON format here."
  ]

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
