-- | @futhark benchcmp@
module Futhark.CLI.Benchcmp (main) where

import Control.Exception (catch)
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either qualified as E
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Futhark.Bench
import Futhark.Util (showText)
import Futhark.Util.Options (mainWithOptions)
import Statistics.Sample qualified as S
import System.Console.ANSI (hSupportsANSI)
import System.IO (stdout)
import Text.Printf (printf)

-- | Record that summerizes a comparison between two benchmarks.
data SpeedUp = SpeedUp
  { -- | What factor the benchmark is improved by.
    speedup :: Double,
    -- | Memory usage.
    memoryUsage :: M.Map T.Text Double,
    -- | If the speedup was significant.
    significant :: Bool
  }
  deriving (Show)

-- | Terminal colors used when printing the comparisons. Some of these are not
-- colors ways of emphasising text.
data Colors = Colors
  { -- | The header color.
    header :: T.Text,
    -- | Okay color
    okblue :: T.Text,
    -- | A second okay color
    okgreen :: T.Text,
    -- | Warning color.
    warning :: T.Text,
    -- | When something fails.
    failing :: T.Text,
    -- | Default color.
    endc :: T.Text,
    -- | Bold text.
    bold :: T.Text,
    -- | Underline text.
    underline :: T.Text
  }

-- | Colors to use for a terminal device.
ttyColors :: Colors
ttyColors =
  Colors
    { header = "\ESC[95m",
      okblue = "\ESC[94m",
      okgreen = "\ESC[92m",
      warning = "\ESC[93m",
      failing = "\ESC[91m",
      endc = "\ESC[0m",
      bold = "\ESC[1m",
      underline = "\ESC[4m"
    }

-- | Colors to use for a non-terminal device.
nonTtyColors :: Colors
nonTtyColors =
  Colors
    { header = "",
      okblue = "",
      okgreen = "",
      warning = "",
      failing = "",
      endc = "",
      bold = "",
      underline = ""
    }

-- | Reads a file without throwing an error.
readFileSafely :: T.Text -> IO (Either T.Text LBS.ByteString)
readFileSafely filepath =
  (Right <$> LBS.readFile (T.unpack filepath)) `catch` couldNotRead
  where
    couldNotRead e = pure $ Left $ showText (e :: IOError)

-- | Converts DataResults to a Map with the text as a key.
toDataResultsMap :: [DataResult] -> M.Map T.Text (Either T.Text Result)
toDataResultsMap = M.fromList . fmap toTuple
  where
    toTuple (DataResult dataset dataResults) = (dataset, dataResults)

-- | Converts BenchResults to a Map with the file path as a key.
toBenchResultsMap ::
  [BenchResult] ->
  M.Map T.Text (M.Map T.Text (Either T.Text Result))
toBenchResultsMap = M.fromList . fmap toTuple
  where
    toTuple (BenchResult path dataResults) =
      (T.pack path, toDataResultsMap dataResults)

-- | Given a file path to a json file which has the form of a futhark benchmark
-- result, it will try to parse the file to a Map of Maps. The key
-- in the outer most dictionary is a file path the inner key is the dataset.
decodeFileBenchResultsMap ::
  T.Text ->
  IO (Either T.Text (M.Map T.Text (M.Map T.Text (Either T.Text Result))))
decodeFileBenchResultsMap path = do
  file <- readFileSafely path
  pure $ toBenchResultsMap <$> (file >>= (first T.pack . decodeBenchResults))

-- | Will return a text with an error saying there is a missing program in a
-- given result.
formatMissingProg :: T.Text -> T.Text -> T.Text -> T.Text
formatMissingProg = ((T.pack .) .) . printf "In %s but not %s: program %s"

-- | Will return a text with an error saying there is a missing dataset in a
-- given result.
formatMissingData :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text
formatMissingData =
  (((T.pack .) .) .) . printf "In %s but not %s: program %s dataset %s"

-- | Will return texts that say there are a missing program.
formatManyMissingProg :: T.Text -> T.Text -> [T.Text] -> [T.Text]
formatManyMissingProg a_path b_path =
  zipWith3 formatMissingProg a_paths b_paths
  where
    a_paths = repeat a_path
    b_paths = repeat b_path

-- | Will return texts that say there are missing datasets for a program.
formatManyMissingData :: T.Text -> T.Text -> T.Text -> [T.Text] -> [T.Text]
formatManyMissingData prog a_path b_path =
  L.zipWith4 formatMissingData a_paths b_paths progs
  where
    a_paths = repeat a_path
    b_paths = repeat b_path
    progs = repeat prog

-- | Finds the keys two Maps does not have in common and returns a appropiate
-- error based on the functioned passed.
missingResults ::
  (T.Text -> T.Text -> [T.Text] -> [T.Text]) ->
  T.Text ->
  T.Text ->
  M.Map T.Text a ->
  M.Map T.Text b ->
  [T.Text]
missingResults toMissingMap a_path b_path a_results b_results = missing
  where
    a_keys = M.keys a_results
    b_keys = M.keys b_results
    a_missing = toMissingMap a_path b_path $ a_keys L.\\ b_keys
    b_missing = toMissingMap b_path a_path $ b_keys L.\\ a_keys
    missing = a_missing `L.union` b_missing

-- | Compares the memory usage of two results.
computeMemoryUsage ::
  M.Map T.Text Int ->
  M.Map T.Text Int ->
  M.Map T.Text Double
computeMemoryUsage a b = M.intersectionWith divide b $ M.filter (/= 0) a
  where
    divide x y = fromIntegral x / fromIntegral y

-- | Compares two results and thereby computes the Speed Up records.
compareResult :: Result -> Result -> SpeedUp
compareResult a b =
  SpeedUp
    { speedup = speedup',
      significant = significant',
      memoryUsage = memory_usage
    }
  where
    runResultToDouble :: RunResult -> Double
    runResultToDouble = fromIntegral . runMicroseconds
    toVector = V.fromList . (runResultToDouble <$>) . runResults
    a_memory_usage = memoryMap a
    b_memory_usage = memoryMap b
    a_run_results = toVector a
    b_run_results = toVector b
    a_std = S.stdDev a_run_results
    b_std = S.stdDev b_run_results
    a_mean = S.mean a_run_results
    b_mean = S.mean b_run_results
    diff = abs $ a_mean - b_mean
    speedup' = a_mean / b_mean
    significant' = diff > a_std / 2 + b_std / 2
    memory_usage = computeMemoryUsage a_memory_usage b_memory_usage

-- | Given two Maps containing datasets as keys and results as values, compare
-- the results and return the errors in a tuple.
compareDataResults ::
  T.Text ->
  T.Text ->
  T.Text ->
  M.Map T.Text (Either T.Text Result) ->
  M.Map T.Text (Either T.Text Result) ->
  (M.Map T.Text SpeedUp, ([T.Text], [T.Text]))
compareDataResults prog a_path b_path a_data b_data = result
  where
    formatMissing = formatManyMissingData prog
    partition = E.partitionEithers . fmap sequence . M.toList
    (a_errors, a_data') = second M.fromList $ partition a_data
    (b_errors, b_data') = second M.fromList $ partition b_data
    missing = missingResults formatMissing a_path b_path a_data' b_data'
    exists = M.intersectionWith compareResult a_data' b_data'
    errors = a_errors ++ b_errors
    result = (exists, (errors, missing))

-- | Given two Maps containing program file paths as keys and values as datasets
-- with results. Compare the results for each dataset in each program and
-- return the errors in a tuple.
compareBenchResults ::
  T.Text ->
  T.Text ->
  M.Map T.Text (M.Map T.Text (Either T.Text Result)) ->
  M.Map T.Text (M.Map T.Text (Either T.Text Result)) ->
  (M.Map T.Text (M.Map T.Text SpeedUp), ([T.Text], [T.Text]))
compareBenchResults a_path b_path a_bench b_bench = (exists, errors_missing)
  where
    missing = missingResults formatManyMissingProg a_path b_path a_bench b_bench
    result = M.intersectionWithKey auxiliary a_bench b_bench
    auxiliary prog = compareDataResults prog a_path b_path
    exists = M.filter (not . null) $ fst <$> result
    errors_missing' = bimap concat concat . unzip . M.elems $ snd <$> result
    errors_missing = second (missing ++) errors_missing'

-- | Formats memory usage such that it is human readable. If the memory usage
-- is not significant an empty text is returned.
memoryFormatter :: Colors -> T.Text -> Double -> T.Text
memoryFormatter colors key value
  | value < 0.99 = memoryFormat $ okgreen colors
  | value > 1.01 = memoryFormat $ failing colors
  | otherwise = ""
  where
    memoryFormat c = T.pack $ printf "%s%4.2fx@%s%s" c value key endc'
    endc' = endc colors

-- | Given a SpeedUp record the memory usage will be formatted to a colored
-- human readable text.
toMemoryText :: Colors -> SpeedUp -> T.Text
toMemoryText colors data_result
  | T.null memory_text = ""
  | otherwise = " (mem: " <> memory_text <> ")"
  where
    memory_text = M.foldrWithKey formatFolder "" memory
    memory = memoryUsage data_result
    formatFolder key value lst = lst <> memoryFormatter colors key value

-- | Given a text shorten it to a given length and add a suffix as the last
-- word.
shorten :: Int -> T.Text -> T.Text -> T.Text
shorten c end string
  | T.length string > c = (T.unwords . init $ T.words shortened) <> " " <> end
  | otherwise = string
  where
    end_len = T.length end
    (shortened, _) = T.splitAt (c - end_len) string

-- | Given a text add padding to the right of the text in form of spaces.
rightPadding :: Int -> T.Text -> T.Text
rightPadding c = T.pack . printf s
  where
    s = "%-" <> show c <> "s"

-- | Given a SpeedUp record print the SpeedUp in a human readable manner.
printSpeedUp :: Colors -> T.Text -> SpeedUp -> IO ()
printSpeedUp colors dataset data_result = do
  let color
        | significant data_result && speedup data_result > 1.01 = okgreen colors
        | significant data_result && speedup data_result < 0.99 = failing colors
        | otherwise = ""
  let short_dataset = rightPadding 64 . (<> ":") $ shorten 63 "[...]" dataset
  let memoryText = toMemoryText colors data_result
  let speedup' = speedup data_result
  let endc' = endc colors
  let format = "  %s%s%10.2fx%s%s"
  putStrLn $ printf format short_dataset color speedup' endc' memoryText

-- | Given a Map of SpeedUp records where the key is the program, print the
-- SpeedUp in a human readable manner.
printProgSpeedUps :: Colors -> T.Text -> M.Map T.Text SpeedUp -> IO ()
printProgSpeedUps colors prog bench_result = do
  putStrLn ""
  putStrLn $ printf "%s%s%s%s" (header colors) (bold colors) prog (endc colors)
  mapM_ (uncurry (printSpeedUp colors)) $ M.toList bench_result

-- | Given a Map of programs with dataset speedups and relevant errors, print
-- the errors and print the speedups in a human readable manner.
printComparisons ::
  Colors ->
  M.Map T.Text (M.Map T.Text SpeedUp) ->
  ([T.Text], [T.Text]) ->
  IO ()
printComparisons colors speedups (errors, missing) = do
  mapM_ (putStrLn . T.unpack) $ L.sort missing
  mapM_ (putStrLn . T.unpack) $ L.sort errors
  mapM_ (uncurry (printProgSpeedUps colors)) $ M.toList speedups

-- | Run @futhark benchcmp@
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "<file> <file>" f
  where
    f [a_path', b_path'] () = Just $ do
      let a_path = T.pack a_path'
      let b_path = T.pack b_path'
      a_either <- decodeFileBenchResultsMap a_path
      b_either <- decodeFileBenchResultsMap b_path

      isTty <- hSupportsANSI stdout

      let colors =
            if isTty
              then ttyColors
              else nonTtyColors

      let comparePrint =
            (uncurry (printComparisons colors) .)
              . compareBenchResults a_path b_path

      case (a_either, b_either) of
        (Left a, Left b) -> putStrLn . T.unpack $ (a <> "\n" <> b)
        (Left a, _) -> putStrLn . T.unpack $ a
        (_, Left b) -> putStrLn . T.unpack $ b
        (Right a, Right b) -> comparePrint a b
    f _ _ = Nothing
