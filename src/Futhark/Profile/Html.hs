{-# LANGUAGE QuasiQuotes #-}

module Futhark.Profile.Html (generateHeatmapHtml) where

import Control.Monad.Except (Except)
import Control.Monad.State.Strict (State, evalState, get, modify)
import Data.Bifunctor (second)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Word (Word8)
import Futhark.Profile.EventSummary qualified as ES
import Futhark.Profile.SourceRange qualified as SR
import Futhark.Util.Html (headHtml)
import NeatInterpolation qualified as NI (text, trimming)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Printf (printf)

type SourcePos = (Int, Int)

data RenderState = RenderState
  { renderPos :: !SourcePos,
    remainingText :: !T.Text
  }

-- fraction of total time (in [0; 1]), affected events
type CostCentreAnnotations = (Double, Seq.Seq (T.Text, ES.EvSummary))

type AnnotatedRange = (SR.SourceRange, CostCentreAnnotations)

generateHeatmapHtml :: T.Text -> T.Text -> M.Map SR.SourceRange CostCentreAnnotations -> Except T.Text H.Html
generateHeatmapHtml sourcePath sourceText sourceRanges =
  pure . H.docTypeHtml $
    headHtml (T.unpack sourcePath) (T.unpack $ sourcePath <> " - Source-Heatmap")
      <> bodyHtml sourcePath sourceText sourceRanges

bodyHtml :: p -> T.Text -> M.Map SR.SourceRange CostCentreAnnotations -> H.Html
bodyHtml sourcePath sourceText sourceRanges =
  let rangeList = M.toAscList sourceRanges
   in H.body $
        H.code . H.pre $
          renderRanges (RenderState (1, 1) sourceText) rangeList

-- | Assumes that the annotated ranges are non-overlapping and in ascending order
renderRanges :: RenderState -> [AnnotatedRange] -> H.Html
renderRanges state [] = H.text . remainingText $ state
renderRanges (RenderState pos text) rs@((range, (fraction, evs)) : rest) =
  let rangePos = SR.startLineCol range
   in case pos `compare` rangePos of
        GT -> error "Impossible: Ranges were not in ascending order"
        EQ -> do
          let rangeEndPos = SR.endLineCol range
          let (textHtml, text') = renderTextFromUntil pos rangeEndPos text

          decorateSpan fraction evs textHtml
          renderRanges (RenderState rangeEndPos text') rest
        LT -> do
          let (textHtml, text') = renderTextFromUntil pos rangePos text
          textHtml
          renderRanges (RenderState rangePos text') rs

decorateSpan :: Double -> Seq.Seq (T.Text, ES.EvSummary) -> H.Html -> H.Html
decorateSpan fraction evs =
  H.span
    ! A.style (fromString $ T.unpack cssColorValueText)
    ! A.title (fromString $ T.unpack cssHoverText)
  where
    cssHoverText =
      [NI.trimming|Fraction of the total runtime: $textFraction
      Part of $textEvCount cost centres.
      |]
      where
        textEvCount = T.show $ Seq.length evs
        textFraction = T.pack $ printf "%.4f" fraction

    cssColorValueText = [NI.text|background: rgba($textR, $textG, $textB, 1)|]
      where
        (textR, textG, textB) = (T.show r, T.show g, T.show b)
        (r, g, b) = interpolateHeatmapColor fraction

-- | Percentage Argument must be smaller in [0; 1]
interpolateHeatmapColor :: Double -> (Word8, Word8, Word8)
interpolateHeatmapColor percentage =
  if percentage >= 0.5
    then
      let point = percentage * 2
          g = point * 255
       in (255, round g, 0)
    else
      let point = (percentage - 0.5) * 2
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
