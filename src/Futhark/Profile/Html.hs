module Futhark.Profile.Html () where
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Futhark.Profile.SourceRange as SR
import qualified Data.Sequence as Seq
import qualified Futhark.Profile.EventSummary as ES

generateHtmlHeatmapFile :: T.Text -> M.Map SR.SourceRange (Seq.Seq (T.Text, ES.EvSummary)) -> a0
generateHtmlHeatmapFile sourceText sourceRanges = _

