module Futhark.CLI.Profile (main) where

import Control.Exception (catch)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Bench
import Futhark.Util (showText)
import Futhark.Util.Options
import System.Directory (createDirectoryIfMissing, removePathForcibly)
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

tabulateEvents :: [ProfilingEvent] -> T.Text
tabulateEvents = mkRows . M.toList . M.fromListWith comb . map pair
  where
    pair (ProfilingEvent name dur _) = (name, (1, dur))
    comb (xn, xdur) (yn, ydur) = (xn + yn, xdur + ydur)
    numpad = 15
    mkRows rows =
      let longest = foldl max numpad $ map (T.length . fst) rows
          header = headerRow longest
          splitter = T.map (const '-') header
          bottom =
            T.unwords
              [ showText (sum (map (fst . snd) rows)),
                "events with a total runtime of ",
                T.pack $ printf "%.2fμs" $ sum $ map (snd . snd) rows
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
          padLeft numpad "total",
          padLeft numpad "mean"
        ]
    mkRow longest (name, (n, dur)) =
      T.unwords
        [ padRight longest name,
          padLeft numpad (showText n),
          padLeft numpad $ T.pack $ printf "%.2fμs" dur,
          padLeft numpad $ T.pack $ printf "%.2fμs" $ dur / fromInteger n
        ]

analyseProfileReport :: FilePath -> [BenchResult] -> IO ()
analyseProfileReport json_path bench_results = do
  let top_dir = takeFileName json_path -<.> "prof"
  T.hPutStrLn stderr $ "Writing results to " <> T.pack top_dir <> "/"
  T.hPutStrLn stderr $ "Stripping '" <> T.pack prefix <> "' from program paths."
  removePathForcibly top_dir
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
          T.writeFile (name' <> ".summary") $
            memoryReport (profilingMemory r)
              <> "\n\n"
              <> tabulateEvents (profilingEvents r)

readFileSafely :: FilePath -> IO (Either String BS.ByteString)
readFileSafely filepath =
  (Right <$> BS.readFile filepath) `catch` couldNotRead
  where
    couldNotRead e = pure $ Left $ show (e :: IOError)

decodeFileBenchResults ::
  FilePath ->
  IO (Either String [BenchResult])
decodeFileBenchResults path = do
  file <- readFileSafely path
  pure $ file >>= decodeBenchResults

-- | Run @futhark profile@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "<file>" f
  where
    f [json_path] () = Just $ do
      res_either <- decodeFileBenchResults json_path

      case res_either of
        Left a -> hPutStrLn stderr a
        Right a -> analyseProfileReport json_path a
    f _ _ = Nothing
