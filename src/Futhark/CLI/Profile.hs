{-# LANGUAGE TemplateHaskell #-}
-- | @futhark profile@
module Futhark.CLI.Profile (main) where

import Control.Arrow ((&&&), (>>>))
import Control.Exception (catch)
import Control.Monad (forM_, (<$!>), (>=>))
import Control.Monad.Except (ExceptT, liftEither, mapExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (first, second)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List qualified as L
import Data.Loc (posFile)
import Data.Map qualified as M
import Data.Monoid (Sum (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.IO qualified as LT
import Futhark.Bench
  ( BenchResult (BenchResult, benchResultProg),
    DataResult (DataResult),
    ProfilingEvent (ProfilingEvent),
    ProfilingReport (profilingEvents, profilingMemory),
    Result (report, stdErr),
    decodeBenchResults,
    decodeProfilingReport,
  )
import Futhark.Profile.EventSummary qualified as ES
import Futhark.Profile.Html (generateHeatmapHtml)
import Futhark.Profile.SourceRange qualified as SR
import Futhark.Util (showText)
import Futhark.Util.Options (mainWithOptions)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath
  ( dropExtension,
    makeRelative,
    takeDirectory,
    takeFileName,
    (-<.>),
    (<.>),
    (</>),
  )
import System.IO (hPutStrLn, stderr)
import Text.Blaze.Html.Renderer.Text qualified as H
import Text.Printf (printf)
import Data.FileEmbed (embedStringFile)

cssFile :: T.Text
cssFile = $(embedStringFile "rts/futhark-profile/style.css")

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

tabulateEvents :: M.Map (T.Text, T.Text) ES.EvSummary -> T.Text
tabulateEvents = mkRows . M.toList
  where
    numpad = 15
    mkRows rows =
      let longest = foldl max numpad $ map (T.length . fst . fst) rows
          total = sum $ map (ES.evSum . snd) rows
          header = headerRow longest
          splitter = T.map (const '-') header
          eventSummary =
            T.unwords
              [ showText (sum (map (ES.evCount . snd) rows)),
                "events with a total runtime of",
                T.pack $ printf "%.2fμs" total
              ]
          costCentreSources =
            let costCentreSourceBlocks =
                  map (costCentreSourceLines . fst)
                    . L.sortOn (fst . fst)
                    $ rows
                costCentreHeaderTitle = " Cost Centre Source Locations "
                costCentreHeader = T.center (T.length header) '=' costCentreHeaderTitle
             in concat $
                  [costCentreHeader, T.empty]
                    : L.intersperse [T.empty] costCentreSourceBlocks
          costCentreSourceLines (name, provenance) =
            let sources = T.splitOn "->" provenance
                orderedSources = L.sort sources
             in name
                  : map ("- " <>) orderedSources
       in T.unlines $
            header
              : splitter
              : map (mkRow longest total . first fst) rows
                <> [splitter, eventSummary]
                <> replicate 5 T.empty
                <> costCentreSources
    headerRow longest =
      T.unwords
        [ padLeft longest "Cost centre",
          padLeft numpad "count",
          padLeft numpad "sum",
          padLeft numpad "avg",
          padLeft numpad "min",
          padLeft numpad "max",
          padLeft numpad "fraction"
        ]
    mkRow longest total (name, ev) =
      T.unwords
        [ padRight longest name,
          padLeft numpad (showText (ES.evCount ev)),
          padLeft numpad $ T.pack $ printf "%.2fμs" (ES.evSum ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" $ ES.evSum ev / fromInteger (ES.evCount ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" (ES.evMin ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" (ES.evMax ev),
          padLeft numpad $ T.pack $ printf "%.4f" (ES.evSum ev / total)
        ]

timeline :: [ProfilingEvent] -> T.Text
timeline = T.unlines . L.intercalate [""] . map onEvent
  where
    onEvent (ProfilingEvent name duration provenance _details) =
      [ name,
        "Duration: " <> showText duration <> " μs",
        "At: " <> provenance
      ]

data TargetFiles = TargetFiles
  { summaryFile :: FilePath,
    timelineFile :: FilePath,
    htmlDir :: FilePath
  }

writeAnalysis :: TargetFiles -> ProfilingReport -> IO ()
writeAnalysis tf r = runExceptT >=> handleException $ do
  let evSummaryMap = ES.eventSummaries $ profilingEvents r

  -- heatmap html
  writeHtml (htmlDir tf) evSummaryMap

  -- profile.summary
  liftIO $
    T.writeFile (summaryFile tf) $
      memoryReport (profilingMemory r)
        <> "\n\n"
        <> tabulateEvents evSummaryMap

  -- profile.timeline
  liftIO $
    T.writeFile (timelineFile tf) $
      timeline (profilingEvents r)
  where
    handleException :: Either T.Text () -> IO ()
    handleException = either (T.hPutStrLn stderr) pure

expandEvSummaryMap :: M.Map (T.Text, T.Text) ES.EvSummary -> M.Map (T.Text, T.Text) ES.EvSummary
expandEvSummaryMap =
  M.toList
    >>> concatMap expandEvSummary
    >>> M.fromList
  where
    expandEvSummary ((name, provenance), evSummary) =
      map (\(sourceLoc, splitSummary) -> ((name, sourceLoc), splitSummary)) $
        splitEvSummarySources provenance evSummary

    splitEvSummarySources :: T.Text -> ES.EvSummary -> [(T.Text, ES.EvSummary)]
    splitEvSummarySources provenance summary =
      let sourceLocations = T.splitOn "->" provenance
       in map (,summary) sourceLocations

buildProvenanceSummaryMap ::
  (Monad m) =>
  -- | Keys are: (ccName, ccProvenance)
  M.Map (T.Text, T.Text) ES.EvSummary ->
  -- | Keys are: (ccName, ccProvenance)
  ExceptT T.Text m (M.Map (T.Text, SR.SourceRange) ES.EvSummary)
buildProvenanceSummaryMap evSummaryMap =
  let -- there are no compound provenance blocks in this map anymore
      singleSourceEvSummaryMap = expandEvSummaryMap evSummaryMap
      -- parse the provenance text
      provenanceEvSummaryMap =
        M.mapKeys (second SR.parse) singleSourceEvSummaryMap

      -- throw error text on parse failure, will short-circuit
      filterBadSourceRangeParses ((name, parseResult), evSummary) = case parseResult of
        Left err -> throwError $ "Parse failure in provenance information\n" <> err
        Right sourceRange -> pure ((name, sourceRange), evSummary)
   in mapM filterBadSourceRangeParses (M.toList provenanceEvSummaryMap)
        & fmap M.fromList

writeHtml ::
  -- | target directory path
  FilePath ->
  -- | mapping keys are (name, provenance)
  M.Map (T.Text, T.Text) ES.EvSummary ->
  ExceptT T.Text IO ()
writeHtml htmlDirPath evSummaryMap = do
  -- for every source location of each cost centre: accumulated eventsummary
  provenanceSummaries <- buildProvenanceSummaryMap evSummaryMap

  -- text source files
  sourceFiles <-
    fmap normalizeNewlines
      <$!> loadAllFiles
        (posFile . SR.startPos . snd <$> M.keys provenanceSummaries)

  htmlFiles <-
    -- for each file, for each source range: events
    let perFileSummaries =
          let sourceFileNames = M.keysSet sourceFiles
           in summarizeAndSplitByFile sourceFileNames provenanceSummaries
        totalEvTime = getSum . foldMap (Sum . ES.evSum . snd)
        maxTotalEvTime =
          concatMap toList perFileSummaries
            & map totalEvTime
            & maximum

        -- for each file, for each source range:
        --  events and fraction of total time
        summariesWithRatio = M.map (M.map (evRatio &&& id)) perFileSummaries
          where
            evRatio = (/ maxTotalEvTime) . totalEvTime

        generateSingleFile filePath =
          generateHeatmapHtml
            filePath
            (sourceFiles M.! filePath)
     in M.traverseWithKey generateSingleFile summariesWithRatio
          & mapExceptT (pure . runIdentity)
  
  liftIO $ T.writeFile (htmlDirPath </> "style.css") cssFile

  liftIO $ forM_ (M.toList htmlFiles) $ \(srcFilePath, html) -> do
    let absPath = htmlDirPath </> makeRelative "/" (T.unpack $ srcFilePath <> ".html")
    writeLazyTextFile absPath (H.renderHtml html)

writeLazyTextFile :: FilePath -> LT.Text -> IO ()
writeLazyTextFile filepath content = do
  createDirectoryIfMissing True $ takeDirectory filepath
  LT.writeFile filepath content

summarizeAndSplitByFile ::
  -- | Names of all text files
  S.Set T.Text ->
  -- | Mapping from (ccName, ccProvenance) to event summary
  M.Map (T.Text, SR.SourceRange) ES.EvSummary ->
  -- | Non-Overlapping Events with SourceRanges separated by file
  -- invariant: sourcerange.rangeStartPos.file is always equal to the map key
  M.Map T.Text (M.Map SR.SourceRange (Seq.Seq (T.Text, ES.EvSummary)))
summarizeAndSplitByFile files summaries =
  let separatedByFile =
        let accumulateFiles m ((ccName, sourceRange), summary) =
              -- relies on the fact that all the keys are already present in the map
              M.adjust
                (Seq.|> (sourceRange, (ccName, summary)))
                (T.pack . posFile . SR.startPos $ sourceRange)
                m
         in M.toList summaries
              & foldl' accumulateFiles (M.fromSet (const Seq.empty) files)

      separateSourceRanges ::
        -- \| All possibly overlapping SourceRanges
        Seq.Seq (SR.SourceRange, (T.Text, ES.EvSummary)) ->
        -- \| Ordered non-overlapping sourceranges with merged attached informations
        M.Map SR.SourceRange (Seq.Seq (T.Text, ES.EvSummary))
      separateSourceRanges = foldl' accumulateRange M.empty . fmap (second Seq.singleton)
        where
          accumulateRange ::
            -- \| Mapping of non-overlapping ranges, the T.Text is a ccName
            M.Map SR.SourceRange (Seq.Seq (T.Text, ES.EvSummary)) ->
            -- \| New SourceRange that must be merged and inserted
            (SR.SourceRange, Seq.Seq (T.Text, ES.EvSummary)) ->
            -- \| Mapping of non-overlapping ranges
            M.Map SR.SourceRange (Seq.Seq (T.Text, ES.EvSummary))
          accumulateRange ranges (range, aux) = case M.lookupLE range ranges of
            -- there is no lower range
            Nothing -> case M.lookupGE range ranges of
              -- there is no higher range
              Nothing -> M.insert range aux ranges -- nothing to merge at all

              -- higher ranges was found
              Just higher@(higherRange, _) ->
                if range `SR.overlapsWith` higherRange
                  then
                    let mergedRanges = SR.mergeSemigroup (range, aux) higher
                        rangesWithoutHigher = M.delete higherRange ranges
                     in foldl accumulateRange rangesWithoutHigher mergedRanges
                  -- ranges don't overlap, don't merge
                  else M.insert range aux ranges
            -- lower range was found
            Just lower@(lowerRange, _) ->
              if range `SR.overlapsWith` lowerRange
                then
                  let mergedRanges = SR.mergeSemigroup (range, aux) lower
                      rangesWithoutLower = M.delete lowerRange ranges
                   in foldl accumulateRange rangesWithoutLower mergedRanges
                -- lower range does not overlap
                else case M.lookupGE range ranges of -- check the higher bound
                -- nothing to merge
                  Nothing -> M.insert range aux ranges
                  -- higher range was found
                  Just higher@(higherRange, _) ->
                    if range `SR.overlapsWith` higherRange
                      then
                        let mergedRanges = SR.mergeSemigroup (range, aux) higher
                            rangesWithoutHigher = M.delete higherRange ranges
                         in foldl accumulateRange rangesWithoutHigher mergedRanges
                      -- just insert the range normally
                      else M.insert range aux ranges
   in M.map separateSourceRanges separatedByFile

loadAllFiles :: [FilePath] -> ExceptT T.Text IO (M.Map T.Text T.Text)
loadAllFiles files =
  mapM (\path -> (T.pack path,) <$> tryLoadFile path) files
    & fmap M.fromList
  where
    tryLoadFile filePath = do
      bytes <- liftIO $ readFileSafely filePath
      bytes' <- liftEither . first T.pack $ bytes
      liftEither . first T.show $ T.decodeUtf8' (BS.toStrict bytes')

-- | Multi-Replace, replace all combinations of '\r' and '\n' with only '\n'
normalizeNewlines :: T.Text -> T.Text
normalizeNewlines =
  T.replace "\r\n" "\n"
    >>> T.replace "\r" "\n"

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
            timelineFile = top_dir </> "timeline",
            htmlDir = top_dir </> "html/"
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
                    timelineFile = name' <> ".timeline",
                    htmlDir = name' <> ".html/"
                  }
           in writeAnalysis tf r

readFileSafely :: FilePath -> IO (Either String BS.ByteString)
readFileSafely filepath =
  (Right <$> BS.readFile filepath) `catch` couldNotRead
  where
    couldNotRead e = pure $ Left $ show (e :: IOError)

onFile :: FilePath -> IO ()
onFile json_path = do
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

-- | Run @futhark profile@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "[files]" f
  where
    f files () = Just $ mapM_ onFile files
