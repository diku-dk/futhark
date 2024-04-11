-- | @futhark profile@
module Futhark.CLI.Profile (main) where

import Control.Exception (catch)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Bench
import Futhark.Util (showText)
import Futhark.Util.Options
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Exit
import System.FilePath
import System.IO
import Text.Printf

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x : xs) (y : ys)
  | x == y = x : commonPrefix xs ys
  | otherwise = []

longestCommonPrefix :: [FilePath] -> FilePath
longestCommonPrefix [] = ""
longestCommonPrefix (x : xs) = foldr commonPrefix x xs

memoryReport :: M.Map T.Text Integer -> T.Text
memoryReport = T.unlines . ("Peak memory usage in bytes" :) . map f . M.toList
  where
    f (space, bytes) = space <> ": " <> showText bytes

padRight :: Int -> T.Text -> T.Text
padRight k s = s <> T.replicate (k - T.length s) " "

padLeft :: Int -> T.Text -> T.Text
padLeft k s = T.replicate (k - T.length s) " " <> s

data EvSummary = EvSummary
  { evCount :: Integer,
    evSum :: Double,
    evMin :: Double,
    evMax :: Double
  }

tabulateEvents :: [ProfilingEvent] -> T.Text
tabulateEvents = mkRows . M.toList . M.fromListWith comb . map pair
  where
    pair (ProfilingEvent name dur _) = (name, EvSummary 1 dur dur dur)
    comb (EvSummary xn xdur xmin xmax) (EvSummary yn ydur ymin ymax) =
      EvSummary (xn + yn) (xdur + ydur) (min xmin ymin) (max xmax ymax)
    numpad = 15
    mkRows rows =
      let longest = foldl max numpad $ map (T.length . fst) rows
          header = headerRow longest
          splitter = T.map (const '-') header
          bottom =
            T.unwords
              [ showText (sum (map (evCount . snd) rows)),
                "events with a total runtime of",
                T.pack $ printf "%.2fμs" $ sum $ map (evSum . snd) rows
              ]
       in T.unlines $
            header
              : splitter
              : map (mkRow longest) rows
                <> [splitter, bottom]
    headerRow longest =
      T.unwords
        [ padLeft longest "Cost centre",
          padLeft numpad "count",
          padLeft numpad "sum",
          padLeft numpad "avg",
          padLeft numpad "min",
          padLeft numpad "max"
        ]
    mkRow longest (name, ev) =
      T.unwords
        [ padRight longest name,
          padLeft numpad (showText (evCount ev)),
          padLeft numpad $ T.pack $ printf "%.2fμs" (evSum ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" $ evSum ev / fromInteger (evCount ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" (evMin ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" (evMax ev)
        ]

timeline :: [ProfilingEvent] -> T.Text
timeline = T.unlines . L.intercalate [""] . map onEvent
  where
    onEvent (ProfilingEvent name duration description) =
      [name, "Duration: " <> showText duration <> " μs"] <> T.lines description

data TargetFiles = TargetFiles
  { summaryFile :: FilePath,
    timelineFile :: FilePath
  }

writeAnalysis :: TargetFiles -> ProfilingReport -> IO ()
writeAnalysis tf r = do
  T.writeFile (summaryFile tf) $
    memoryReport (profilingMemory r)
      <> "\n\n"
      <> tabulateEvents (profilingEvents r)
  T.writeFile (timelineFile tf) $
    timeline (profilingEvents r)

prepareDir :: FilePath -> IO FilePath
prepareDir json_path = do
  let top_dir = takeFileName json_path -<.> "prof"
  T.hPutStrLn stderr $ "Writing results to " <> T.pack top_dir <> "/"
  removePathForcibly top_dir
  pure top_dir

analyseProfilingReport :: FilePath -> ProfilingReport -> IO ()
analyseProfilingReport json_path r = do
  top_dir <- prepareDir json_path
  createDirectoryIfMissing True top_dir
  let tf =
        TargetFiles
          { summaryFile = top_dir </> "summary",
            timelineFile = top_dir </> "timeline"
          }
  writeAnalysis tf r

analyseBenchResults :: FilePath -> [BenchResult] -> IO ()
analyseBenchResults json_path bench_results = do
  top_dir <- prepareDir json_path
  T.hPutStrLn stderr $ "Stripping '" <> T.pack prefix <> "' from program paths."
  mapM_ (onBenchResult top_dir) bench_results
  where
    prefix = longestCommonPrefix $ map benchResultProg bench_results

    -- Eliminate characters that are filesystem-meaningful.
    escape '/' = '_'
    escape c = c

    problem prog_name name what =
      T.hPutStrLn stderr $ prog_name <> " dataset " <> name <> ": " <> what

    onBenchResult top_dir (BenchResult prog_path data_results) = do
      let (prog_path', entry) = span (/= ':') prog_path
          prog_name = drop (length prefix) prog_path'
          prog_dir = top_dir </> dropExtension prog_name </> drop 1 entry
      createDirectoryIfMissing True prog_dir
      mapM_ (onDataResult prog_dir (T.pack prog_name)) data_results

    onDataResult _ prog_name (DataResult name (Left _)) =
      problem prog_name name "execution failed"
    onDataResult prog_dir prog_name (DataResult name (Right res)) = do
      let name' = prog_dir </> T.unpack (T.map escape name)
      case stdErr res of
        Nothing -> problem prog_name name "no log recorded"
        Just text -> T.writeFile (name' <.> ".log") text
      case report res of
        Nothing -> problem prog_name name "no profiling information"
        Just r ->
          let tf =
                TargetFiles
                  { summaryFile = name' <> ".summary",
                    timelineFile = name' <> ".timeline"
                  }
           in writeAnalysis tf r

readFileSafely :: FilePath -> IO (Either String BS.ByteString)
readFileSafely filepath =
  (Right <$> BS.readFile filepath) `catch` couldNotRead
  where
    couldNotRead e = pure $ Left $ show (e :: IOError)

-- | Run @futhark profile@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "<file>" f
  where
    f [json_path] () = Just $ do
      s <- readFileSafely json_path
      case s of
        Left a -> do
          hPutStrLn stderr a
          exitWith $ ExitFailure 2
        Right s' ->
          case decodeBenchResults s' of
            Left _ ->
              case decodeProfilingReport s' of
                Nothing -> do
                  hPutStrLn stderr $
                    "Cannot recognise " <> json_path <> " as benchmark results or a profiling report."
                Just pr ->
                  analyseProfilingReport json_path pr
            Right br -> analyseBenchResults json_path br
    f _ _ = Nothing
