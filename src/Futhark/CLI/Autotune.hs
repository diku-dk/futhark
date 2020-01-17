{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CLI.Autotune (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as SBS
import Data.Time.Clock.POSIX
import Data.Tree
import Data.List
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
                          , compBackend = optBackend opts }

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

type RunDataset = Path -> RunPurpose -> IO (Either String ([(String, Int)], Double))

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

  -- We wish to let datasets run for the untuned time + 20% + 1 second.
  let timeout elapsed = ceiling (elapsed * 1.2) + 1

  fmap concat $ forM truns $ \ios ->
    forM (mapMaybe (runnableDataset $ iosEntryPoint ios)
                   (iosTestRuns ios)) $
      \(dataset, do_run) -> do
      bef <- toRational <$> getPOSIXTime
      res <- do_run 60000 [] RunBenchmark
      aft <- toRational <$> getPOSIXTime
      case res of Left err -> do
                    putStrLn $ "Error when running " ++ prog ++ ":"
                    putStrLn err
                    exitFailure
                  Right _ -> do
                    let t = timeout $ aft - bef
                    putStrLn $ "Calculated timeout for " ++ dataset ++
                      " : " ++ show t ++ "s"
                    return (dataset, do_run t, iosEntryPoint ios)

  where run entry_point trun expected timeout path purpose = do
          let opts' = case purpose of RunSample -> opts { optRuns = 1 }
                                      RunBenchmark -> opts

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

--- Finding initial comparisons.

--- Extracting threshold hierarchy.

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

intersectRanges :: [(Int, Int)] -> (Int, Int)
intersectRanges = foldl' f (thresholdMin, thresholdMax)
  where f (xmin, xmax) (ymin, ymax) =
          -- XXX: what happens when the intersection is empty?
          (xmin `max` ymin,
           xmax `min` ymax)

tuneThreshold :: AutotuneOptions
              -> [(DatasetName, RunDataset, T.Text)]
              -> Path -> (String, Path)
              -> IO Path
tuneThreshold opts datasets already_tuned (v, v_path) = do
  ranges <-
    forM datasets $ \(dataset_name, run, entry_point) ->

    if not $ isPrefixOf (T.unpack entry_point ++ ".") v then do
        when (optVerbose opts > 0) $
          putStrLn $ unwords [v, "is irrelevant for", T.unpack entry_point]
        return (thresholdMin, thresholdMax)
    else do

      putStrLn $ unwords ["Tuning", v, "on entry point", T.unpack entry_point,
                          "and dataset", dataset_name]

      sample_run <- run path RunSample

      case sample_run of
        Left err -> do
          -- If the sampling run fails, we treat it as zero information.
          -- One of our ancestor thresholds will have be set such that
          -- this path is never taken.
          when (optVerbose opts > 0) $ putStrLn $
            "Sampling run failed:\n" ++ err
          return (thresholdMin, thresholdMax)
        Right (cmps, _) ->
          case lookup v cmps of
            Nothing -> do
              -- A missing comparison is not necessarily a bug - it may
              -- simply mean that this comparison is inside a loop or
              -- branch that is never reached for this dataset.  In such
              -- cases, the optimal range is universal.
              when (optVerbose opts > 0) $ putStrLn "Irrelevant for dataset.\n"
              return (thresholdMin, thresholdMax)
            Just e_par -> do
              t_run <- run path_t RunBenchmark
              f_run <- run path_f RunBenchmark

              let prefer_t = (thresholdMin, e_par)
                  prefer_f = (e_par+1, thresholdMax)

              case (t_run, f_run) of
                (Left err, _) -> do
                  when (optVerbose opts > 0) $
                    putStrLn $ "True comparison run failed:\n" ++ err
                  return prefer_f
                (_, Left err) -> do
                  when (optVerbose opts > 0) $
                    putStrLn $ "False comparison run failed:\n" ++ err
                  return prefer_t
                (Right (_, runtime_t), Right (_, runtime_f)) ->
                  if runtime_t < runtime_f
                  then do when (optVerbose opts > 0) $
                            putStrLn "True branch is fastest."
                          return prefer_t
                  else do when (optVerbose opts > 0) $
                            putStrLn "False branch is fastest."
                          return prefer_f

  let (_lower, upper) = intersectRanges ranges
  return $ (v,upper) : already_tuned

  where path = already_tuned ++ v_path
        path_t = (v, thresholdMin) : path
        path_f = (v, thresholdMax) : path

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
