module Futhark.Profile.Html (generateHeatmapHtml) where

import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Futhark.Profile.EventSummary qualified as ES
import Futhark.Profile.SourceRange qualified as SR
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import Futhark.Util.Html (headHtml)
import Control.Monad.Except (Except)

generateHeatmapHtml :: T.Text -> T.Text -> M.Map SR.SourceRange (Seq.Seq (T.Text, ES.EvSummary)) -> Except T.Text H.Html
generateHeatmapHtml sourcePath sourceText sourceRanges = pure . H.docTypeHtml $
  headHtml (T.unpack sourcePath) (T.unpack $ sourcePath <> " - Source-Heatmap")
