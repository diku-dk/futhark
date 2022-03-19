{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @futhark autotune@
module Futhark.CLI.Autotune (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as SBS
import Data.Function (on)
import Data.List (elemIndex, intersect, isPrefixOf, minimumBy, sort, sortOn)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tree
import Futhark.Bench
import Futhark.Server
import Futhark.Test
import Futhark.Util (maxinum)
import Futhark.Util.Options
import System.Directory
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath
import System.Process
import Text.Read (readMaybe)
import Text.Regex.TDFA

data AutotuneOptions = AutotuneOptions
  { optBackend :: String,
    optFuthark :: Maybe String,
    optRuns :: Int,
    optTuning :: Maybe String,
    optExtraOptions :: [String],
    optVerbose :: Int,
    optTimeout :: Int,
    optSkipCompilation :: Bool,
    optDefaultThreshold :: Int,
    optTestSpec :: Maybe FilePath
  }

initialAutotuneOptions :: AutotuneOptions
initialAutotuneOptions =
  AutotuneOptions "opencl" Nothing 10 (Just "tuning") [] 0 60 False thresholdMax Nothing

compileOptions :: AutotuneOptions -> IO CompileOptions
compileOptions opts = do
  futhark <- maybe getExecutablePath return $ optFuthark opts
  return $
    CompileOptions
      { compFuthark = futhark,
        compBackend = optBackend opts,
        compOptions = mempty
      }

runOptions :: Int -> AutotuneOptions -> RunOptions
runOptions timeout_s opts =
  RunOptions
    { runRuns = optRuns opts,
      runTimeout = timeout_s,
      runVerbose = optVerbose opts,
      runResultAction = Nothing
    }

type Path = [(String, Int)]

regexGroups :: Regex -> String -> Maybe [String]
regexGroups regex s = do
  (_, _, _, groups) <-
    matchM regex s :: Maybe (String, String, String, [String])
  Just groups

comparisons :: String -> [(String, Int)]
comparisons = mapMaybe isComparison . lines
  where
    regex = makeRegex ("Compared ([^ ]+) <= (-?[0-9]+)" :: String)
    isComparison l = do
      [thresh, val] <- regexGroups regex l
      val' <- readMaybe val
      return (thresh, val')

type RunDataset = Server -> Int -> Path -> IO (Either String ([(String, Int)], Int))

type DatasetName = String

serverOptions :: AutotuneOptions -> [String]
serverOptions opts =
  "--default-threshold" :
  show (optDefaultThreshold opts) :
  "-L" :
  optExtraOptions opts

setTuningParam :: Server -> String -> Int -> IO ()
setTuningParam server name val =
  either (error . T.unpack . T.unlines . failureMsg) (const $ pure ())
    =<< cmdSetTuningParam server (T.pack name) (T.pack (show val))

setTuningParams :: Server -> Path -> IO ()
setTuningParams server = mapM_ (uncurry $ setTuningParam server)

restoreTuningParams :: AutotuneOptions -> Server -> Path -> IO ()
restoreTuningParams opts server = mapM_ opt
  where
    opt (name, _) = setTuningParam server name (optDefaultThreshold opts)

prepare :: AutotuneOptions -> FutharkExe -> FilePath -> IO [(DatasetName, RunDataset, T.Text)]
prepare opts futhark prog = do
  spec <-
    maybe (testSpecFromProgramOrDie prog) testSpecFromFileOrDie $
      optTestSpec opts
  copts <- compileOptions opts

  truns <-
    case testAction spec of
      RunCases ios _ _ | not $ null ios -> do
        when (optVerbose opts > 1) $
          putStrLn $
            unwords ("Entry points:" : map (T.unpack . iosEntryPoint) ios)

        if optSkipCompilation opts
          then do
            exists <- doesFileExist $ binaryName prog
            if exists
              then return ios
              else do
                putStrLn $ binaryName prog ++ " does not exist, but --skip-compilation passed."
                exitFailure
          else do
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
              Just
                ( runDescription trun,
                  \server -> run server entry_point trun expected
                )
          _ -> Nothing

  fmap concat . forM truns $ \ios -> do
    let cases =
          mapMaybe (runnableDataset $ iosEntryPoint ios) (iosTestRuns ios)
    forM cases $ \(dataset, do_run) ->
      return (dataset, do_run, iosEntryPoint ios)
  where
    run server entry_point trun expected timeout path = do
      let bestRuntime :: ([RunResult], T.Text) -> ([(String, Int)], Int)
          bestRuntime (runres, errout) =
            ( comparisons (T.unpack errout),
              minimum $ map runMicroseconds runres
            )

          ropts = runOptions timeout opts

      when (optVerbose opts > 1) $
        putStrLn $ "Trying path: " ++ show path

      -- Setting the tuning parameters is a stateful action, so we
      -- must be careful to restore the defaults below.  This is
      -- because we rely on parameters not in 'path' to have their
      -- default value.
      setTuningParams server path

      either (Left . T.unpack) (Right . bestRuntime)
        <$> benchmarkDataset
          server
          ropts
          futhark
          prog
          entry_point
          (runInput trun)
          expected
          (testRunReferenceOutput prog entry_point trun)
        <* restoreTuningParams opts server path

--- Benchmarking a program

data DatasetResult = DatasetResult [(String, Int)] Double
  deriving (Show)

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
  where
    treePaths ancestors (Node (v, _) children) =
      concatMap (onChild ancestors v) children ++ [(v, ancestors)]

    onChild ancestors v child@(Node (_, cmp) _) =
      treePaths (ancestors ++ [(v, t cmp)]) child

    t False = thresholdMax
    t True = thresholdMin

thresholdForest :: FilePath -> IO ThresholdForest
thresholdForest prog = do
  thresholds <-
    getThresholds
      <$> readProcess ("." </> dropExtension prog) ["--print-params"] ""
  let root (v, _) = ((v, False), [])
  return $
    unfoldForest (unfold thresholds) $
      map root $ filter (null . snd) thresholds
  where
    getThresholds = mapMaybe findThreshold . lines
    regex = makeRegex ("(.*) \\(threshold\\(([^ ]+,)(.*)\\)\\)" :: String)

    findThreshold :: String -> Maybe (String, [(String, Bool)])
    findThreshold l = do
      [grp1, _, grp2] <- regexGroups regex l
      return
        ( grp1,
          filter (not . null . fst) $
            map
              ( \x ->
                  if "!" `isPrefixOf` x
                    then (drop 1 x, False)
                    else (x, True)
              )
              $ words grp2
        )

    unfold thresholds ((parent, parent_cmp), ancestors) =
      let ancestors' = parent : ancestors

          isChild (v, v_ancestors) = do
            cmp <- lookup parent v_ancestors
            guard $
              sort (map fst v_ancestors)
                == sort (parent : ancestors)
            return ((v, cmp), ancestors')
       in ((parent, parent_cmp), mapMaybe isChild thresholds)

-- | The performance difference in percentage that triggers a non-monotonicity
-- warning. This is to account for slight variantions in run-time.
epsilon :: Double
epsilon = 1.02

--- Doing the atual tuning

tuneThreshold ::
  AutotuneOptions ->
  Server ->
  [(DatasetName, RunDataset, T.Text)] ->
  (Path, M.Map DatasetName Int) ->
  (String, Path) ->
  IO (Path, M.Map DatasetName Int)
tuneThreshold opts server datasets (already_tuned, best_runtimes0) (v, _v_path) = do
  (tune_result, best_runtimes) <-
    foldM tuneDataset (Nothing, best_runtimes0) datasets
  case tune_result of
    Nothing ->
      return ((v, thresholdMin) : already_tuned, best_runtimes)
    Just (_, threshold) ->
      return ((v, threshold) : already_tuned, best_runtimes)
  where
    tuneDataset :: (Maybe (Int, Int), M.Map DatasetName Int) -> (DatasetName, RunDataset, T.Text) -> IO (Maybe (Int, Int), M.Map DatasetName Int)
    tuneDataset (thresholds, best_runtimes) (dataset_name, run, entry_point) =
      if not $ isPrefixOf (T.unpack entry_point ++ ".") v
        then do
          when (optVerbose opts > 0) $
            putStrLn $ unwords [v, "is irrelevant for", T.unpack entry_point]
          return (thresholds, best_runtimes)
        else do
          putStrLn $
            unwords
              [ "Tuning",
                v,
                "on entry point",
                T.unpack entry_point,
                "and dataset",
                dataset_name
              ]

          sample_run <-
            run
              server
              (optTimeout opts)
              ((v, maybe thresholdMax snd thresholds) : already_tuned)

          case sample_run of
            Left err -> do
              -- If the sampling run fails, we treat it as zero information.
              -- One of our ancestor thresholds will have be set such that
              -- this path is never taken.
              when (optVerbose opts > 0) $
                putStrLn $
                  "Sampling run failed:\n" ++ err
              return (thresholds, best_runtimes)
            Right (cmps, t) -> do
              let (tMin, tMax) = fromMaybe (thresholdMin, thresholdMax) thresholds
              let ePars =
                    S.toAscList $
                      S.map snd $
                        S.filter (candidateEPar (tMin, tMax)) $
                          S.fromList cmps

                  runner :: Int -> Int -> IO (Maybe Int)
                  runner timeout' threshold = do
                    res <- run server timeout' ((v, threshold) : already_tuned)
                    case res of
                      Right (_, runTime) ->
                        return $ Just runTime
                      _ ->
                        return Nothing

              when (optVerbose opts > 1) $
                putStrLn $ unwords ("Got ePars: " : map show ePars)

              (best_t, newMax) <- binarySearch runner (t, tMax) ePars
              let newMinIdx = do
                    i <- pred <$> elemIndex newMax ePars
                    if i < 0 then fail "Invalid lower index" else return i
              let newMin = maxinum $ catMaybes [Just tMin, fmap (ePars !!) newMinIdx]
              best_runtimes' <-
                case dataset_name `M.lookup` best_runtimes of
                  Just rt
                    | fromIntegral rt * epsilon < fromIntegral best_t -> do
                      putStrLn $
                        unwords
                          [ "WARNING! Possible non-monotonicity detected. Previous best run-time for dataset",
                            dataset_name,
                            " was",
                            show rt,
                            "but after tuning threshold",
                            v,
                            "it is",
                            show best_t
                          ]
                      return best_runtimes
                  _ ->
                    return $ M.insertWith min dataset_name best_t best_runtimes
              return (Just (newMin, newMax), best_runtimes')

    bestPair :: [(Int, Int)] -> (Int, Int)
    bestPair = minimumBy (compare `on` fst)

    timeout :: Int -> Int
    -- We wish to let datasets run for the untuned time + 20% + 1 second.
    timeout elapsed = ceiling (fromIntegral elapsed * 1.2 :: Double) + 1

    candidateEPar :: (Int, Int) -> (String, Int) -> Bool
    candidateEPar (tMin, tMax) (threshold, ePar) =
      ePar > tMin && ePar < tMax && threshold == v

    binarySearch :: (Int -> Int -> IO (Maybe Int)) -> (Int, Int) -> [Int] -> IO (Int, Int)
    binarySearch runner best@(best_t, best_e_par) xs =
      case splitAt (length xs `div` 2) xs of
        (lower, middle : middle' : upper) -> do
          when (optVerbose opts > 0) $
            putStrLn $
              unwords
                [ "Trying e_par",
                  show middle,
                  "and",
                  show middle'
                ]
          candidate <- runner (timeout best_t) middle
          candidate' <- runner (timeout best_t) middle'
          case (candidate, candidate') of
            (Just new_t, Just new_t') ->
              if new_t < new_t'
                then -- recurse into lower half
                  binarySearch runner (bestPair [(new_t, middle), best]) lower
                else -- recurse into upper half
                  binarySearch runner (bestPair [(new_t', middle'), best]) upper
            (Just new_t, Nothing) ->
              -- recurse into lower half
              binarySearch runner (bestPair [(new_t, middle), best]) lower
            (Nothing, Just new_t') ->
              -- recurse into upper half
              binarySearch runner (bestPair [(new_t', middle'), best]) upper
            (Nothing, Nothing) -> do
              when (optVerbose opts > 2) $
                putStrLn $
                  unwords
                    [ "Timing failed for candidates",
                      show middle,
                      "and",
                      show middle'
                    ]
              return (best_t, best_e_par)
        (_, _) -> do
          when (optVerbose opts > 0) $
            putStrLn $ unwords ["Trying e_pars", show xs]
          candidates <-
            catMaybes . zipWith (fmap . flip (,)) xs
              <$> mapM (runner $ timeout best_t) xs
          return $ bestPair $ best : candidates

--- CLI

tune :: AutotuneOptions -> FilePath -> IO Path
tune opts prog = do
  futhark <- fmap FutharkExe $ maybe getExecutablePath return $ optFuthark opts

  putStrLn $ "Compiling " ++ prog ++ "..."
  datasets <- prepare opts futhark prog

  forest <- thresholdForest prog
  when (optVerbose opts > 0) $
    putStrLn $ ("Threshold forest:\n" ++) $ drawForest $ map (fmap show) forest

  let progbin = "." </> dropExtension prog
  putStrLn $ "Running with options: " ++ unwords (serverOptions opts)

  withServer (futharkServerCfg progbin (serverOptions opts)) $ \server ->
    fmap fst . foldM (tuneThreshold opts server datasets) ([], mempty) $
      tuningPaths forest

runAutotuner :: AutotuneOptions -> FilePath -> IO ()
runAutotuner opts prog = do
  best <- tune opts prog

  let tuning = unlines $ do
        (s, n) <- sortOn fst best
        return $ s ++ "=" ++ show n

  case optTuning opts of
    Nothing -> return ()
    Just suffix -> do
      writeFile (prog <.> suffix) tuning
      putStrLn $ "Wrote " ++ prog <.> suffix

  putStrLn $ "Result of autotuning:\n" ++ tuning

commandLineOptions :: [FunOptDescr AutotuneOptions]
commandLineOptions =
  [ Option
      "r"
      ["runs"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")] | n' >= 0 ->
                  Right $ \config ->
                    config
                      { optRuns = n'
                      }
                _ ->
                  Left $ error $ "'" ++ n ++ "' is not a non-negative integer."
          )
          "RUNS"
      )
      "Run each test case this many times.",
    Option
      []
      ["backend"]
      ( ReqArg
          (\backend -> Right $ \config -> config {optBackend = backend})
          "BACKEND"
      )
      "The compiler used (defaults to 'opencl').",
    Option
      []
      ["futhark"]
      ( ReqArg
          (\prog -> Right $ \config -> config {optFuthark = Just prog})
          "PROGRAM"
      )
      "The binary used for operations (defaults to 'futhark').",
    Option
      []
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
      ["tuning"]
      ( ReqArg
          (\s -> Right $ \config -> config {optTuning = Just s})
          "EXTENSION"
      )
      "Write tuning files with this extension (default: .tuning).",
    Option
      []
      ["timeout"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")] ->
                  Right $ \config -> config {optTimeout = n'}
                _ ->
                  Left $ error $ "'" ++ n ++ "' is not a non-negative integer."
          )
          "SECONDS"
      )
      "Initial tuning timeout for each dataset. Later tuning runs are based off of the runtime of the first run.",
    Option
      []
      ["skip-compilation"]
      (NoArg $ Right $ \config -> config {optSkipCompilation = True})
      "Use already compiled program.",
    Option
      "v"
      ["verbose"]
      (NoArg $ Right $ \config -> config {optVerbose = optVerbose config + 1})
      "Enable logging.  Pass multiple times for more.",
    Option
      []
      ["spec-file"]
      (ReqArg (\s -> Right $ \config -> config {optTestSpec = Just s}) "FILE")
      "Use test specification from this file."
  ]

-- | Run @futhark autotune@
main :: String -> [String] -> IO ()
main = mainWithOptions initialAutotuneOptions commandLineOptions "options... program" $ \progs config ->
  case progs of
    [prog] -> Just $ runAutotuner config prog
    _ -> Nothing
