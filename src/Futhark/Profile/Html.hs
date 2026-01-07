{-# LANGUAGE QuasiQuotes #-}

module Futhark.Profile.Html (securedHashPath, generateHeatmapHtml, generateCCOverviewHtml, generateHtmlIndex) where

import Control.Monad (join)
import Control.Monad.State.Strict (State, evalState, get, modify)
import Data.Bifunctor (bimap, first, second)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Loc (posFile)
import Data.Map qualified as M
import Data.Ord (Down (Down))
import Data.Set (Set)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Word (Word8)
import Futhark.Profile.Details (CostCentreDetails (CostCentreDetails, summary), CostCentreName (CostCentreName, getCostCentreName), CostCentres, SourceRangeDetails (SourceRangeDetails, containingCostCentres), SourceRanges, sourceRangeDetailsFraction)
import Futhark.Profile.Details qualified as D
import Futhark.Profile.EventSummary qualified as ES
import Futhark.Profile.SourceRange qualified as SR
import Futhark.Util (hashText, showText)
import Futhark.Util.Html (headHtml, headHtmlWithCss, relativise)
import NeatInterpolation qualified as NI (text, trimming)
import System.FilePath (takeFileName, (<.>), (</>))
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Printf (printf)
import Prelude hiding (span)

securedHashPath :: FilePath -> FilePath
securedHashPath p =
  T.unpack (hashText $ T.pack p) <> "-" <> takeFileName p

type SourcePos = (Int, Int)

data RenderState = RenderState
  { _renderPos :: !SourcePos,
    remainingText :: !T.Text
  }

generateHtmlIndex ::
  -- | Path of the bench dir
  FilePath ->
  M.Map FilePath SourceRanges ->
  CostCentres ->
  H.Html
generateHtmlIndex benchDir _pathToSourceRanges _costCentres = do
  H.docTypeHtml $ do
    headHtmlWithCss (benchDir </> "style.css") pageTitle
    H.h2 $ H.string pageTitle
    introductionIndex (T.pack benchDir)
    sourceFileIndex benchDir (M.keysSet _pathToSourceRanges)
  where
    pageTitle = "Source File Index"

introductionIndex :: T.Text -> H.Html
introductionIndex benchDir = do
  H.text
    [NI.trimming|
          This is the index file for the benchmark files in $benchDir.
          The generated profile markup consists of highlighted source files
          and the cost centre overview.
          |]
  H.h4 "Highlighted Source Files"
  H.text
    [NI.trimming|
          All the source files that were contained in any cost centre for this
          benchmark were higlighted and rendered.
          This means that source ranges are shown with a background
          color progressing from green to yellow to red, depending on the
          amount of total time spent in cost centres containing this range.
          Not every source location gets a color, that is because some parts
          are simply optimized out or were not needed for the compilation of
          this benchmark.
          At the bottom of each Source File there is a listing of all ranges
          in the source file and which cost centres affected the coloring.
          |]
  H.h4 $
    H.a ! A.href (fromString $ T.unpack benchDir </> "cost-centres.html") $
      H.text "Cost Centre Overview"
  H.text
    [NI.trimming|
          The Cost Centre Overview contains all the cost centres recorded
          during the last run of the benchmark. For every cost centre, there
          is a table with statistics and also a list with links to all
          relevant source ranges.
          |]

sourceFileIndex :: FilePath -> Set FilePath -> H.Html
sourceFileIndex benchDir paths = do
  H.h3 $ H.text "Source Files (ordered alphabetically)"
  H.ul $ do
    mapM_ pathListEntry paths
  where
    pathListEntry path =
      H.li
        $ H.a
          ! A.href (fromString pathRef)
        $ H.string path
      where
        pathRef = benchDir </> securedHashPath path <.> ".html"

generateHeatmapHtml ::
  -- | Path where this Html will be placed
  FilePath ->
  -- | Path where the corresponding source was
  FilePath ->
  -- | Source File Text
  T.Text ->
  -- | Non-Overlapping Source Ranges
  M.Map SR.SourceRange SourceRangeDetails ->
  H.Html
generateHeatmapHtml htmlPath sourcePath sourceText sourceRanges =
  H.docTypeHtml $ do
    headHtml htmlPath (sourcePath <> " - Source-Heatmap")
    heatmapBodyHtml htmlPath sourceText sourceRanges

generateCCOverviewHtml :: M.Map CostCentreName CostCentreDetails -> H.Html
generateCCOverviewHtml costCentres = do
  headHtml "cost-centres.html" "Cost Centre Overview"
  H.body $ do
    H.h2 $ H.text "Cost Centres (ordered by fraction)"
    ccDetailTables
  where
    ccDetailTables =
      M.toList costCentres
        & sortOn (Down . D.fraction . snd)
        & mapM_ (uncurry renderCostCentreDetails)

renderCostCentreDetails :: CostCentreName -> CostCentreDetails -> H.Html
renderCostCentreDetails (CostCentreName ccName) (CostCentreDetails ratio sourceRanges summary) = do
  title
  summaryTable
  sourceRangeListing
  where
    title =
      H.h3
        ! A.id (fromString $ T.unpack ccName)
        $ fractionColored ratio
        $ H.a
          ! A.href (fromString $ '#' : T.unpack ccName)
          ! A.class_ "silent-anchor"
        $ H.text ccName

    summaryTable =
      H.table $
        mapM_
          row
          [ ("Fraction", T.pack $ printf "%.4f" ratio),
            ("Event Count", showText count),
            ("Total Time (µs)", T.pack $ printf "%.2f" sum_),
            ("Minimum Time (µs)", T.pack $ printf "%.2f" min_),
            ("Maximum Time (µs)", T.pack $ printf "%.2f" max_)
          ]
      where
        (ES.EvSummary count sum_ min_ max_) = summary
        row (h, d) = H.tr $ do
          H.th $ H.text h
          H.td $ H.text d

    sourceRangeListing = do
      H.h4 $ H.text "Source Ranges"
      H.ol $
        mapM_ (uncurry entry) (M.toList sourceRanges)
      where
        entry range details =
          H.li
            $ fractionColored (sourceRangeDetailsFraction details)
            $ H.a
              ! A.href (fromString entryRef)
              ! A.class_ "silent-anchor"
              ! A.title "Click to Jump to Source"
            $ H.text
            $ sourceRangeText range <> " in " <> T.pack rangeFile
          where
            rangeFile = posFile $ SR.startPos range
            rangeHtmlFile = securedHashPath rangeFile
            entryRef = rangeHtmlFile <> ".html#" <> sourceRangeSpanCssId range

heatmapBodyHtml :: FilePath -> T.Text -> M.Map SR.SourceRange SourceRangeDetails -> H.Html
heatmapBodyHtml sourcePath sourceText sourceRanges =
  H.body $ do
    sourceCodeListing
    detailTables
  where
    rangeList = M.toAscList sourceRanges

    sourceCodeListing =
      H.code . H.pre $
        renderRanges (RenderState (1, 1) sourceText) rangeList

    detailTables = mapM_ (uncurry $ sourceRangeDetails sourcePath) rangeList

sourceRangeDetails :: FilePath -> SR.SourceRange -> SourceRangeDetails -> H.Html
sourceRangeDetails currentPath range details@(SourceRangeDetails ccs) = detailDiv $ do
  H.h3 $ do
    H.text "Source Range from "
    toSpanAnchor $ fractionColored ratio $ H.span $ H.text rangeText

  costCentreTable
  where
    ratio = sourceRangeDetailsFraction details
    costCentreTable = do
      H.h4 $ H.text "Cost Centres"
      H.table $ do
        H.tr $ do
          mapM_
            H.th
            [ "Name",
              "Event Count",
              "Total Time (µs)",
              "Minimum Time (µs)",
              "Maximum Time (µs)",
              "Fraction"
            ]

        mapM_ evRow $
          M.toAscList ccs
            & fmap (first getCostCentreName)

        H.tr $ do
          H.td $ H.text "Total"
          H.td $ H.text $ showText $ ES.evCount evTotal
          mapM_
            (H.td . H.text . T.pack . printf "%.2f")
            [ ES.evSum evTotal,
              ES.evMin evTotal,
              ES.evMax evTotal,
              ratio
            ]
      where
        evRow (ccName, CostCentreDetails fraction _ ev) = H.tr $ do
          mapM_
            H.td
            [ fractionColored fraction
                $ H.span
                  ! A.title "Click to jump to Cost Centre Overview"
                $ H.a
                  ! A.href
                    ( fromString $
                        relativise "cost-centres.html" currentPath
                          <> "#"
                          <> T.unpack ccName
                    )
                  ! A.class_ "silent-anchor"
                $ H.text ccName,
              H.text . showText . ES.evCount $ ev,
              H.text . T.pack . printf "%.2f" . ES.evSum $ ev,
              H.text . T.pack . printf "%.2f" . ES.evMin $ ev,
              H.text . T.pack . printf "%.2f" . ES.evMax $ ev,
              H.text . T.pack . printf "%.4f" $ fraction
            ]
        evTotal =
          M.toList ccs
            & fmap (summary . snd)
            & foldl' combine (ES.EvSummary 0 0 infPos infNeg)
          where
            combine (ES.EvSummary c s lo hi) (ES.EvSummary c' s' lo' hi') =
              ES.EvSummary (c + c') (s + s') (min lo lo') (max hi hi')
            infPos = read "Infinity"
            infNeg = negate infPos

    toSpanAnchor =
      H.a
        ! A.href (fromString $ '#' : sourceRangeSpanCssId range)
        ! A.class_ "silent-anchor"
        ! A.title "Click to jump to source"

    detailDiv =
      H.div
        ! A.id (fromString $ sourceRangeDetailsCssId range)

    rangeText = sourceRangeText range

sourceRangeText :: SR.SourceRange -> T.Text
sourceRangeText range = [NI.text|$lineStart:$colStart to $lineEnd:$colEnd|]
  where
    (lineStart, colStart) = join bimap showText $ SR.startLineCol range
    (lineEnd, colEnd) = join bimap showText $ SR.endLineCol range

-- | Assumes that the annotated ranges are non-overlapping and in ascending order
renderRanges ::
  RenderState ->
  [(SR.SourceRange, SourceRangeDetails)] ->
  -- range, fraction, costCentreCount
  H.Html
renderRanges state [] = H.text . remainingText $ state
renderRanges (RenderState pos text) rs@((range, details) : rest) =
  let rangePos = SR.startLineCol range
   in case pos `compare` rangePos of
        GT -> error "Impossible: Ranges were not in ascending order"
        EQ -> do
          let rangeEndPos = SR.endLineCol range
          let (textHtml, text') = renderTextFromUntil pos rangeEndPos text
          let fraction = sourceRangeDetailsFraction details
          let evCount = M.size $ containingCostCentres details

          decorateSpan range fraction evCount textHtml
          renderRanges (RenderState rangeEndPos text') rest
        LT -> do
          let (textHtml, text') = renderTextFromUntil pos rangePos text
          textHtml
          renderRanges (RenderState rangePos text') rs

sourceRangeSpanCssId :: SR.SourceRange -> String
sourceRangeSpanCssId range = printf "range-l%i-c%i" startLine startCol
  where
    (startLine, startCol) = SR.startLineCol range

sourceRangeDetailsCssId :: SR.SourceRange -> String
sourceRangeDetailsCssId range = "detail-table-" <> sourceRangeSpanCssId range

decorateSpan :: SR.SourceRange -> Double -> Int -> H.Html -> H.Html
decorateSpan range fraction evCount = span . anchor
  where
    anchor =
      H.a
        ! A.class_ "silent-anchor"
        ! A.href (fromString $ '#' : sourceRangeDetailsCssId range)

    spanCssId = sourceRangeSpanCssId range

    span =
      fractionColored fraction
        . ( H.span
              ! A.id (fromString spanCssId)
              ! A.title (fromString $ T.unpack cssHoverText)
          )
      where
        cssHoverText =
          [NI.trimming|Fraction of the total runtime: $textFraction
          Part of $textEvCount cost centres.
          (Click to jump to detail table)
          |]
          where
            textEvCount = showText evCount
            textFraction = T.pack $ printf "%.4f" fraction

fractionColored :: Double -> H.Html -> H.Html
fractionColored fraction = (! A.style cssColorValue)
  where
    cssColorValue =
      fromString . T.unpack $
        [NI.text|background: rgba($textR, $textG, $textB, 1)|]
      where
        (textR, textG, textB) = (showText r, showText g, showText b)
        (r, g, b) = interpolateHeatmapColor fraction

-- | Percentage Argument must be smaller in [0; 1]
interpolateHeatmapColor :: Double -> (Word8, Word8, Word8)
interpolateHeatmapColor percentage =
  if percentage >= 0.5
    then
      let point = (percentage - 0.5) * 2
          -- less is more red, interpolate towards 0
          g = 255 - point * 255
       in (255, round g, 0)
    else
      let point = percentage * 2
          r = 128 + 127 * point
       in (round r, 255, 0)
  where
    _red, _yellow, _green :: (Word8, Word8, Word8)
    _red = (255, 0, 0)
    _yellow = (255, 255, 0)
    _green = (128, 255, 0)

-- >>> splitTextFromTo (4, 40) (4, 43) "abc\ndef"
-- ("abc","\ndef")

splitTextFromTo :: SourcePos -> (Int, Int) -> T.Text -> (T.Text, T.Text)
splitTextFromTo startPos endPos t =
  flip evalState startPos $
    T.spanM stepChar t
  where
    -- general category @LineSeparator@ is not used for Newlines
    isNewline c = case c of
      '\n' -> True
      '\r' -> True
      _ -> False

    stepChar :: Char -> State SourcePos Bool
    stepChar char = do
      -- do the check at the start, this way the last character is always included
      oldPos <- get

      modify $
        if isNewline char
          then \(l, _) -> (succ l, 1)
          else second succ

      pure (oldPos /= endPos)

renderTextFromUntil :: SourcePos -> SourcePos -> T.Text -> (H.Html, T.Text)
renderTextFromUntil startPos endPos t =
  let (included, rest) = splitTextFromTo startPos endPos t
   in (H.text included, rest)
