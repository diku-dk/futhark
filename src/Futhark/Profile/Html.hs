module Futhark.Profile.Html () where

import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Futhark.Profile.EventSummary qualified as ES
import Futhark.Profile.SourceRange qualified as SR

generateHtmlHeatmapFile :: T.Text -> M.Map SR.SourceRange (Seq.Seq (T.Text, ES.EvSummary)) -> a0
generateHtmlHeatmapFile sourceText sourceRanges = _
