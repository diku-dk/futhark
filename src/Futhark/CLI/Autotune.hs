{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CLI.Autotune (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as SBS
import Data.Tree
import Data.List
import qualified Data.Set as S
import Data.Maybe
import Text.Read (readMaybe)
import Text.Regex.TDFA
import qualified Data.Text as T

import System.Environment (getExecutablePath)
import System.Exit
import System.Process
import System.FilePath
import System.Console.GetOpt

import Futhark.Bench
import Futhark.Test
import Futhark.Util.Options

data AutotuneOptions = AutotuneOptions
                    { optBackend :: String
                    , optFuthark :: Maybe String
                    , optRuns :: Int
                    , optTuning :: Maybe String
                    , optExtraOptions :: [String]
                    , optVerbose :: Int
                    }

initialAutotuneOptions :: AutotuneOptions
initialAutotuneOptions =
  AutotuneOptions "opencl" Nothing 10 (Just "tuning") [] 0

compileOptions :: AutotuneOptions -> IO CompileOptions
compileOptions opts = do
  futhark <- maybe getExecutablePath return $ optFuthark opts
  return $ CompileOptions { compFuthark = futhark
                          , compBackend = optBackend opts
                          , compOptions = mempty
                          }

runOptions :: Path -> Int -> AutotuneOptions -> RunOptions
runOptions path timeout_s opts =
  RunOptions { runRunner = ""
             , runRuns = optRuns opts
             , runExtraOptions = "-L" : map opt path ++ optExtraOptions opts
             , runTimeout = timeout_s
             }
  where opt (name, val) = "--size=" ++ name ++ "=" ++ show val

type Path = [(String, Int)]

regexGroups :: Regex -> String -> Maybe [String]
regexGroups regex s = do
  (_, _, _, groups) <-
    matchM regex s :: Maybe (String, String, String, [String])
  Just groups

comparisons :: String -> [(String,Int)]
comparisons = mapMaybe isComparison . lines
  where regex = makeRegex ("Compared ([^ ]+) <= (-?[0-9]+)" :: String)
        isComparison l = do [thresh, val] <- regexGroups regex l
                            val' <- readMaybe val
                            return (thresh, val')


data RunPurpose = RunSample -- ^ Only a single run.
                | RunBenchmark -- ^ As many runs as needed.

type RunDataset = Int -> Path -> RunPurpose -> IO (Either String ([(String, Int)], Double))

type DatasetName = String

prepare :: AutotuneOptions -> FilePath -> IO [(DatasetName, RunDataset, T.Text)]
prepare opts prog = do
  spec <- testSpecFromFileOrDie prog
  copts <- compileOptions opts

  truns <-
    case testAction spec of
      RunCases ios _ _ | not $ null ios -> do
            when (optVerbose opts > 1) $
               putStrLn $
                 unwords ("Entry points:" : map (T.unpack . iosEntryPoint) ios)

            res <- prepareBenchmarkProgram Nothing copts prog ios
            case res of
              Left (err, errstr) -> do
                putStrLn err
                maybe (return ()) SBS.putStrLn errstr
                exitFailure
              Right () ->
                return ios
      _ ->
        fail "Unsupported test spec."

  let runnableDataset entry_point trun =
        case runExpectedResult trun of
          Succeeds expected
            | null (runTags trun `intersect` ["notune", "disable"]) ->
                Just (runDescription trun, run entry_point trun expected)

          _ -> Nothing

  fmap concat $ forM truns $ \ios ->
    forM (mapMaybe (runnableDataset $ iosEntryPoint ios)
                   (iosTestRuns ios)) $
      \(dataset, do_run) -> do
      res <- do_run 60000 [] RunBenchmark
      case res of Left err -> do
                    putStrLn $ "Error when running " ++ prog ++ ":"
                    putStrLn err
                    exitFailure
                  Right _ ->
                    return (dataset, do_run, iosEntryPoint ios)

  where run entry_point trun expected timeout path purpose = do
          let opts' = case purpose of RunSample -> opts { optRuns = 1 }
                                      RunBenchmark -> opts

              averageRuntime :: ([RunResult], T.Text) ->
                                ([(String, Int)], Double)
              averageRuntime (runres, errout) =
                (comparisons (T.unpack errout),
                 fromIntegral (sum (map runMicroseconds runres)) /
                 fromIntegral (optRuns opts))

              ropts = runOptions path timeout opts'

          when (optVerbose opts > 1) $
            putStrLn $ "Running with options: " ++ unwords (runExtraOptions ropts)

          either (Left . T.unpack) (Right . averageRuntime) <$>
            benchmarkDataset ropts prog entry_point
            (runInput trun) expected
            (testRunReferenceOutput prog entry_point trun)

--- Benchmarking a program

data DatasetResult = DatasetResult [(String, Int)] Double
             deriving Show

type ThresholdForest = Forest (String, Bool)

thresholdMin, thresholdMax :: Int
thresholdMin = 1
thresholdMax = 2000000000

-- | Depth-first list of thresholds to tune in order, and a
-- corresponding assignment of ancestor thresholds to ensure that they
-- are used.
tuningPaths :: ThresholdForest -> [(String, Path)]
tuningPaths = concatMap (treePaths [])
  where treePaths ancestors (Node (v, _) children) =
          concatMap (onChild ancestors v) children ++ [(v, ancestors)]

        onChild ancestors v child@(Node (_, cmp) _) =
          treePaths (ancestors++[(v, t cmp)]) child

        t False = thresholdMax
        t True = thresholdMin

thresholdForest :: FilePath -> IO ThresholdForest
thresholdForest prog = do
  thresholds <- getThresholds <$>
    readProcess ("." </> dropExtension prog) ["--print-sizes"] ""
  let root (v, _) = ((v, False), [])
  return $ unfoldForest (unfold thresholds) $
    map root $ filter (null . snd) thresholds
  where getThresholds = mapMaybe findThreshold . lines
        regex = makeRegex ("(.*)\\ \\(threshold\\ \\((.*)\\)\\)" :: String)

        findThreshold :: String -> Maybe (String, [(String, Bool)])
        findThreshold l = do [grp1, grp2] <- regexGroups regex l
                             return (grp1,
                                     filter (not . null . fst) $
                                     map (\x -> if "!" `isPrefixOf` x
                                                then (drop 1 x, False)
                                                else (x, True)) $
                                     words grp2)

        unfold thresholds ((parent, parent_cmp), ancestors) =
          let ancestors' = parent : ancestors

              isChild (v, v_ancestors) = do
                cmp <- lookup parent v_ancestors
                guard $
                  sort (map fst v_ancestors) ==
                  sort (parent : ancestors)
                return ((v, cmp), ancestors')

          in ((parent, parent_cmp), mapMaybe isChild thresholds)

--- Doing the atual tuning

tuneThreshold :: AutotuneOptions
              -> [(DatasetName, RunDataset, T.Text)]
              -> Path -> (String, Path)
              -> IO Path
tuneThreshold opts datasets already_tuned (v, _v_path) = do
  putStrLn $ unwords ["Tuning threshold", v]

  (e_pars', _) <- getAllEPars already_tuned v
  let e_pars = S.toList $ S.insert thresholdMax $ S.insert thresholdMin e_pars'

  when (optVerbose opts > 0) $
    putStrLn $ unwords ["Got e_pars:", show e_pars]

  when (optVerbose opts > 0) $
    putStrLn $ unwords ["Computing baseline with e_par", show $ head e_pars]

  baseline <- benchmarkThresholdChoice 60000 (head e_pars)

  case baseline of
    Just baseline' -> do
      best_e_par <- binarySearch baseline' $ tail e_pars

      return $ (v, best_e_par) : already_tuned

    Nothing ->
      error $ unwords ["Couldn't get baseline for", v]

  where
    binarySearch :: (Int, Int) -> [Int] -> IO Int
    binarySearch (best_t, best) xs =
      case splitAt (length xs `div` 2) xs of
        (_, []) -> return best
        (lower, middle : upper) -> do
          when (optVerbose opts > 0) $
            putStrLn $ unwords ["Trying e_par", show middle]
          candidate <- benchmarkThresholdChoice (timeout best_t) middle
          case candidate of
            -- We search from the bottom, so if the new number is better than
            -- the previous, we recurse into the upper half
            Just (new_t, _)
              | new_t < best_t ->
                  binarySearch (new_t, middle) upper
              | otherwise ->
                  binarySearch (best_t, best) lower
            Nothing -> do
              when (optVerbose opts > 2) $
                putStrLn $ unwords ["Timing failed for candidate", show middle]

              binarySearch (best_t, best) lower

    timeout :: Int -> Int
    timeout elapsed = ceiling (fromIntegral elapsed * 1.2 :: Double) + 1

    aggregateResults :: [Maybe Int] -> Maybe Int
    aggregateResults xs = sum <$> sequence xs

    benchmarkThresholdChoice :: Int -> Int -> IO (Maybe (Int, Int))
    benchmarkThresholdChoice best_t e_par = do
      benchmarks <- forM datasets $ \(dataset_name, run, entry_point) ->
        if not $ isPrefixOf (T.unpack entry_point ++ ".") v then do
          when (optVerbose opts > 0) $
            putStrLn $ unwords [v, "is irrelevant for", T.unpack entry_point]
          return $ Just 0
        else do
          when (optVerbose opts > 1) $
            putStrLn $ unwords ["Running against dataset:", dataset_name]
          result <- run (timeout best_t) ((v, e_par) : already_tuned) RunBenchmark
          case result of
            Left err -> do
              when (optVerbose opts > 0) $
                putStrLn $ "A run failed:\n" ++ err
              return Nothing

            Right (_, runtime_t) ->
              return $ Just $ ceiling runtime_t

      let total_runtime = aggregateResults benchmarks
      return $ (,) <$> total_runtime <*> pure e_par

    getAllEPars :: Path -> String -> IO (S.Set Int, Int)
    getAllEPars path threshold = do
      result <- forM datasets $ \(_dataset_name, run, entry_point) ->
        if not $ isPrefixOf (T.unpack entry_point ++ ".") threshold then do
          when (optVerbose opts > 0) $
            putStrLn $ unwords [threshold, "is irrelevant for", T.unpack entry_point]
          return (S.empty, 0)

        else do
          sample_run <- run 60000 path RunSample
          case sample_run of
            Left err -> do
              -- If the sampling run fails, we treat it as zero information.
              -- One of our ancestor thresholds will have be set such that
              -- this path is never taken.
              when (optVerbose opts > 0) $ putStrLn $
                "Sampling run failed:\n" ++ err
              return (S.empty, 0)
            Right (cmps, run_t) ->
              return (S.map snd $ S.filter ((==) threshold . fst) $ S.fromList cmps,
                      ceiling run_t)

      let (cmps, run_ts) = unzip result

      return (S.unions cmps, sum run_ts)

--- CLI

tune :: AutotuneOptions -> FilePath -> IO Path
tune opts prog = do
  putStrLn $ "Compiling " ++ prog ++ "..."
  datasets <- prepare opts prog

  forest <- thresholdForest prog
  when (optVerbose opts > 0) $
    putStrLn $ ("Threshold forest:\n"++) $ drawForest $ map (fmap show) forest

  foldM (tuneThreshold opts datasets) [] $ tuningPaths forest

runAutotuner :: AutotuneOptions -> FilePath -> IO ()
runAutotuner opts prog = do
  best <- tune opts prog

  let tuning = unlines $ do (s, n) <- sortOn fst best
                            return $ s ++ "=" ++ show n

  case optTuning opts of
    Nothing -> return ()
    Just suffix -> do
      writeFile (prog <.> suffix) tuning
      putStrLn $ "Wrote " ++ prog <.> suffix

  putStrLn $ "Result of autotuning:\n" ++ tuning

commandLineOptions :: [FunOptDescr AutotuneOptions]
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
     "BACKEND")
    "The compiler used (defaults to 'opencl')."
  , Option [] ["futhark"]
    (ReqArg (\prog -> Right $ \config -> config { optFuthark = Just prog })
     "PROGRAM")
    "The binary used for operations (defaults to 'futhark')."
  , Option [] ["tuning"]
    (ReqArg (\s -> Right $ \config -> config { optTuning = Just s })
    "EXTENSION")
    "Write tuning files with this extension (default: .tuning)."
  , Option "v" ["verbose"]
    (NoArg $ Right $ \config -> config { optVerbose = optVerbose config + 1 })
    "Enable logging.  Pass multiple times for more."
   ]

main :: String -> [String] -> IO ()
main = mainWithOptions initialAutotuneOptions commandLineOptions
       "options... program" $
       \progs config ->
         case progs of [prog] -> Just $ runAutotuner config prog
                       _      -> Nothing
