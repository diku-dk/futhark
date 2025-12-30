{-# LANGUAGE MultiWayIf #-}

-- | @futhark profile@
module Futhark.CLI.Profile (main) where

import Control.Arrow ((&&&), (>>>))
import Control.Exception (assert, catch)
import Control.Monad (void, when, (>=>))
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first, second)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on, (&))
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Loc (Pos (Pos), posCol, posFile, posLine)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Futhark.Bench
  ( BenchResult (BenchResult, benchResultProg),
    DataResult (DataResult),
    ProfilingEvent (ProfilingEvent),
    ProfilingReport (profilingEvents, profilingMemory),
    Result (report, stdErr),
    decodeBenchResults,
    decodeProfilingReport,
  )
import Futhark.Util (showText)
import Futhark.Util.Options (mainWithOptions)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath
  ( dropExtension,
    takeFileName,
    (-<.>),
    (<.>),
    (</>),
  )
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf (printf)

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

eventSummaries :: [ProfilingEvent] -> M.Map (T.Text, T.Text) EvSummary
eventSummaries = M.fromListWith comb . map pair
  where
    pair (ProfilingEvent name dur provenance _details) =
      ((name, provenance), EvSummary 1 dur dur dur)
    comb (EvSummary xn xdur xmin xmax) (EvSummary yn ydur ymin ymax) =
      EvSummary (xn + yn) (xdur + ydur) (min xmin ymin) (max xmax ymax)

tabulateEvents :: M.Map (T.Text, T.Text) EvSummary -> T.Text
tabulateEvents = mkRows . M.toList
  where
    numpad = 15
    mkRows rows =
      let longest = foldl max numpad $ map (T.length . fst . fst) rows
          total = sum $ map (evSum . snd) rows
          header = headerRow longest
          splitter = T.map (const '-') header
          eventSummary =
            T.unwords
              [ showText (sum (map (evCount . snd) rows)),
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
          padLeft numpad (showText (evCount ev)),
          padLeft numpad $ T.pack $ printf "%.2fμs" (evSum ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" $ evSum ev / fromInteger (evCount ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" (evMin ev),
          padLeft numpad $ T.pack $ printf "%.2fμs" (evMax ev),
          padLeft numpad $ T.pack $ printf "%.4f" (evSum ev / total)
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
  let evSummaryMap = eventSummaries $ profilingEvents r

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

expandEvSummaryMap :: M.Map (T.Text, T.Text) EvSummary -> M.Map (T.Text, T.Text) EvSummary
expandEvSummaryMap =
  M.toList
    >>> concatMap expandEvSummary
    >>> M.fromList
  where
    expandEvSummary ((name, provenance), evSummary) =
      map (\(sourceLoc, splitSummary) -> ((name, sourceLoc), splitSummary)) $
        splitEvSummarySources provenance evSummary

    splitEvSummarySources :: T.Text -> EvSummary -> [(T.Text, EvSummary)]
    splitEvSummarySources provenance summary =
      let sourceLocations = T.splitOn "->" provenance
       in map (,summary) sourceLocations

-- | I chose this representation over `Loc` from `srcLoc` because it guarantees the presence of a range.
-- Loc is essentially a 'Maybe (Pos, Pos)', because of the 'NoLoc' constructor.
-- I cannot even imagine dealing with cross-file ranges anyway.

-- The end of the range is exclusive
data SourceRange = SourceRange
  { sourceRangeStartPos :: !Pos,
    -- | invariant: at least a big as start.line
    sourceRangeEndLine :: !Int,
    -- | invariant: at least as big as start.col, unless the range spans multiple lines
    sourceRangeEndColumn :: !Int
  }
  deriving (Show, Eq, Ord)

-- | Extract start line and column
sourceRangeStart :: SourceRange -> (Int, Int)
sourceRangeStart (SourceRange pos _ _) = (posLine pos, posCol pos)

-- | Extract end line and column
sourceRangeEnd :: SourceRange -> (Int, Int)
sourceRangeEnd (SourceRange _ line col) = (line, col)

sourceRangeOverlapsWith :: SourceRange -> SourceRange -> Bool
sourceRangeOverlapsWith a b =
  let rangeOverlaps (sa, ea) (sb, eb) = sa <= eb && sb <= ea
      startA = sourceRangeStart a
      endA = sourceRangeEnd a
      startB = sourceRangeStart b
      endB = sourceRangeEnd b
   in rangeOverlaps (startA, endA) (startB, endB)

sourceRangeIsEmpty :: SourceRange -> Bool
sourceRangeIsEmpty (SourceRange (Pos _ startLine startCol _) endLine endCol) =
  startLine == endLine && startCol == endCol

-- | Parse a source range, respect the invariants noted in the definition
-- and print the MegaParsec errorbundle into a Text.
--
-- >>> parseSourceRange "example.fut:1:1-5"
-- Right (SourceRange {start = Pos "example.fut" 1 1 (-1), endLine = 1, endColumn = 5})
--
-- >>> parseSourceRange "directory/example.fut:15:12-17:1"
-- Right (SourceRange {start = Pos "directory/example.fut" 15 12 (-1), endLine = 17, endColumn = 1})
parseSourceRange :: T.Text -> Either T.Text SourceRange
parseSourceRange text = first textErrorBundle $ P.parse pSourceRange fname text
  where
    fname = ""
    textErrorBundle = T.pack . P.errorBundlePretty

    lineRangeInvariantMessage =
      "End of Line Range is not bigger than or equal to Start of Line Range."
    columnRangeInvariantMessage =
      "End of Column Range is not bigger than or equal to Start of Column Range"

    pSourceRange :: P.Parsec Void T.Text SourceRange
    pSourceRange = do
      fileName <- L.charLiteral `P.manyTill` P.single ':' -- separator
      startLine <- L.decimal
      void $ P.single ':' -- separator
      startCol <- L.decimal

      void $ P.single '-' -- range begin
      rangeEnd1 <- L.decimal
      -- we can't know yet whether this is going to be a line or column position

      (lineRangeEnd, columnRangeEnd) <-
        P.choice
          [ do
              endCol <- P.single ':' *> L.decimal
              pure (rangeEnd1, endCol),
            pure (startLine, rangeEnd1)
          ]

      let lineRangeInvalid = startLine > lineRangeEnd
      when lineRangeInvalid $ fail lineRangeInvariantMessage

      let columnRangeInvalid =
            startLine == lineRangeEnd && startCol > columnRangeEnd
      when columnRangeInvalid $ fail columnRangeInvariantMessage

      pure $
        SourceRange
          { sourceRangeStartPos = Pos fileName startLine startCol (-1),
            sourceRangeEndLine = lineRangeEnd,
            sourceRangeEndColumn = columnRangeEnd
          }

buildProvenanceSummaryMap ::
  (Monad m) =>
  -- | Keys are: (ccName, ccProvenance)
  M.Map (T.Text, T.Text) EvSummary ->
  -- | Keys are: (ccName, ccProvenance)
  ExceptT T.Text m (M.Map (T.Text, SourceRange) EvSummary)
buildProvenanceSummaryMap evSummaryMap =
  let -- there are no compound provenance blocks in this map anymore
      singleSourceEvSummaryMap = expandEvSummaryMap evSummaryMap
      -- parse the provenance text
      provenanceEvSummaryMap =
        M.mapKeys (second parseSourceRange) singleSourceEvSummaryMap

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
  M.Map (T.Text, T.Text) EvSummary ->
  ExceptT T.Text IO ()
writeHtml htmlDirPath evSummaryMap = do
  provenanceSummaries <- buildProvenanceSummaryMap evSummaryMap
  sourceFiles <- loadAllFiles (posFile . sourceRangeStartPos . snd <$> M.keys provenanceSummaries)

  let perFileSummaries =
        let sourceFileNames = M.keysSet sourceFiles
         in summarizeAndSplitByFile sourceFileNames provenanceSummaries

  pure ()

data OneTwoThree a = One a | Two a a | Three a a a
  deriving (Show, Functor, Foldable)

filter123 :: (a -> Bool) -> OneTwoThree a -> Maybe (OneTwoThree a)
filter123 p self@(One x) = if p x then Just self else Nothing
filter123 p self@(Two x y) = case (p x, p y) of
  (True, True) -> Just self
  (True, False) -> Just (One x)
  (False, True) -> Just (One y)
  (False, False) -> Nothing
filter123 p self@(Three x y z) = case (p x, p y, p z) of
  (False, False, False) -> Nothing
  (False, False, True) -> Just (One z)
  (False, True, False) -> Just (One y)
  (False, True, True) -> Just (Two y z)
  (True, False, False) -> Just (One x)
  (True, False, True) -> Just (Two x z)
  (True, True, False) -> Just (Two x y)
  (True, True, True) -> Just self

-- | Assumes that the ranges overlap
mergeRanges ::
  (Semigroup s) =>
  (SourceRange, s) ->
  (SourceRange, s) ->
  OneTwoThree (SourceRange, s)
mergeRanges a@(rangeA, auxA) b@(rangeB, auxB) =
  let orderedBy f x y = if f x < f y then (x, y) else (y, x)
      (startsEarlier, startsLater) = orderedBy (sourceRangeStart . fst) a b

      startsLaterStart = sourceRangeStart . fst $ startsLater
      fname = posFile . sourceRangeStartPos $ rangeA

      (endsEarlier, endsLater) =
        orderedBy
          ((sourceRangeEndLine &&& sourceRangeEndColumn) . fst)
          a
          b

      firstRange =
        (fst startsEarlier)
          { sourceRangeEndLine = fst startsLaterStart,
            sourceRangeEndColumn = snd startsLaterStart
          }
      secondRange =
        (fst startsLater)
          { sourceRangeEndLine = sourceRangeEndLine . fst $ endsEarlier,
            sourceRangeEndColumn = sourceRangeEndColumn . fst $ endsEarlier
          }

      thirdRange =
        let startLine = sourceRangeEndLine secondRange
            endColumn = sourceRangeEndColumn secondRange
         in (fst endsLater)
              { sourceRangeStartPos = Pos fname startLine endColumn (-1)
              }

      rawRanges =
        Three
          (firstRange, snd startsEarlier)
          (secondRange, auxA <> auxB)
          (thirdRange, snd endsLater)
   in case filter123 (sourceRangeIsEmpty . fst) rawRanges of
        Nothing ->
          error . unwords $
            [ "Impossible! `mergeRanges` produced no range at all, input ranges:",
              show rangeA,
              show rangeB
            ]
        Just merged -> merged

summarizeAndSplitByFile ::
  -- | Names of all text files
  S.Set T.Text ->
  -- | Mapping from (ccName, ccProvenance) to event summary
  M.Map (T.Text, SourceRange) EvSummary ->
  -- | Non-Overlapping Events with SourceRanges separated by file
  -- invariant: sourcerange.rangeStartPos.file is always equal to the map key
  M.Map T.Text (M.Map SourceRange (Seq.Seq (T.Text, EvSummary)))
summarizeAndSplitByFile files summaries =
  let overlapping =
        let accumulateSummary m ((ccName, sourceRange), summary) =
              -- relies on the fact that all the keys are already present in the map
              M.adjust
                (Seq.|> (sourceRange, (ccName, summary)))
                (T.pack . posFile . sourceRangeStartPos $ sourceRange)
                m
         in M.toList summaries
              & foldl' accumulateSummary (M.fromSet (const Seq.empty) files)

      separate ::
        -- \| All possibly overlapping SourceRanges
        Seq.Seq (SourceRange, (T.Text, EvSummary)) ->
        -- \| Ordered non-overlapping sourceranges with merged attached informations
        M.Map SourceRange (Seq.Seq (T.Text, EvSummary))
      separate = foldl' accumulateRange M.empty . fmap (second Seq.singleton)
        where
          accumulateRange ::
            -- \| Mapping of non-overlapping ranges, the T.Text is a ccName
            M.Map SourceRange (Seq.Seq (T.Text, EvSummary)) ->
            -- \| New SourceRange that must be merged and inserted
            (SourceRange, Seq.Seq (T.Text, EvSummary)) ->
            -- \| Mapping of non-overlapping ranges
            M.Map SourceRange (Seq.Seq (T.Text, EvSummary))
          accumulateRange ranges (range, aux) = case M.lookupLE range ranges of
            -- there is no lower range
            Nothing -> case M.lookupGE range ranges of
              -- there is no higher range
              Nothing -> M.insert range aux ranges -- nothing to merge at all

              -- higher ranges was found
              Just higher@(higherRange, _) ->
                if range `sourceRangeOverlapsWith` higherRange
                  then
                    let mergedRanges = mergeRanges (range, aux) higher
                        rangesWithoutHigher = M.delete higherRange ranges
                     in foldl accumulateRange rangesWithoutHigher mergedRanges
                  -- ranges don't overlap, don't merge
                  else M.insert range aux ranges
            -- lower range was found
            Just lower@(lowerRange, _) ->
              if range `sourceRangeOverlapsWith` lowerRange
                then
                  let mergedRanges = mergeRanges (range, aux) lower
                      rangesWithoutLower = M.delete lowerRange ranges
                   in foldl accumulateRange rangesWithoutLower mergedRanges
                else case M.lookupGE range ranges of -- check the higher bound
                -- nothing to merge
                  Nothing -> M.insert range aux ranges
                  Just higher@(higherRange, _) ->
                    if range `sourceRangeOverlapsWith` higherRange
                      then
                        let mergedRanges = mergeRanges (range, aux) higher
                            rangesWithoutHigher = M.delete higherRange ranges
                         in foldl accumulateRange rangesWithoutHigher mergedRanges
                      else M.insert range aux ranges

      nonOverlapping = M.map separate overlapping
   in nonOverlapping

loadAllFiles :: [FilePath] -> ExceptT T.Text IO (M.Map T.Text T.Text)
loadAllFiles files =
  mapM (\path -> (T.pack path,) <$> tryLoadFile path) files
    & fmap M.fromList
  where
    tryLoadFile filePath = do
      bytes <- liftIO $ readFileSafely filePath
      bytes' <- liftEither . first T.pack $ bytes
      liftEither . first T.show $ T.decodeUtf8' (BS.toStrict bytes')

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
                    htmlDir = name' <> "html/"
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
