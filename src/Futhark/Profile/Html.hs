module Futhark.Profile.Html (generateHeatmapHtml) where

import Control.Monad.Except (Except)
import Control.Monad.State.Strict (State, evalState, get, modify)
import Data.Bifunctor (second)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Word (Word8)
import Futhark.Profile.EventSummary qualified as ES
import Futhark.Profile.SourceRange qualified as SR
import Futhark.Util.Html (headHtml)
import Text.Blaze.Html5 qualified as H

type SourcePos = (Int, Int)

data RenderState = RenderState
  { renderPos :: !SourcePos,
    remainingText :: !T.Text
  }

type CostCentreAnnotations = Seq.Seq (T.Text, ES.EvSummary)
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
renderRanges (RenderState pos text) rs@(annRange : rest) =
  let rangePos = SR.startLineCol (fst annRange)
   in case pos `compare` rangePos of
        GT -> error "Impossible: Ranges were not in ascending order"
        EQ -> do
          let rangeEndPos = SR.endLineCol (fst annRange)
          let (textHtml, text') = renderTextFromUntil pos rangeEndPos text

          H.span textHtml
          renderRanges (RenderState rangeEndPos text') rest
        LT -> do
          let (textHtml, text') = renderTextFromUntil pos rangePos text
          textHtml
          renderRanges (RenderState rangePos text') rs

decorateSpan :: H.Html -> H.Html
decorateSpan = undefined

-- | Percentage Argument must be smaller in [0; 1]
interpolateHeatmapColor :: Double -> (Word8, Word8, Word8)
interpolateHeatmapColor percentage =
  if percentage <= 0.5
    then
      let point = percentage * 2
          g = point * 255
       in (255, round g, 0)
    else
      let point = (percentage - 0.5) * 2
          r = 128 + 127 * point
       in (round r, 255, 0)
  where
    _red = (255, 0, 0, 1)
    _yellow = (255, 255, 0, 1)
    _green = (128, 255, 0, 1)

-- >>> splitTextFromTo (4, 40) (4, 43) "abc\ndef"
-- ("abc","\ndef")

splitTextFromTo :: SourcePos -> (Int, Int) -> T.Text -> (T.Text, T.Text)
splitTextFromTo startPos endPos t =
  flip evalState startPos $
    T.spanM stepChar t
  where
    -- TODO: Line Ending checking for DOS-Style Newlines
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
