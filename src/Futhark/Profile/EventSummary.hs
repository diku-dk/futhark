module Futhark.Profile.EventSummary (EvSummary(..), eventSummaries) where

import qualified Data.Map as M (fromListWith, Map)
import Futhark.Profile (ProfilingEvent (ProfilingEvent))
import qualified Data.Text as T (Text)

data EvSummary = EvSummary
  { evCount :: Integer,
    evSum :: Double,
    evMin :: Double,
    evMax :: Double
  }
  deriving (Show)

eventSummaries :: [ProfilingEvent] -> M.Map (T.Text, T.Text) EvSummary
eventSummaries = M.fromListWith comb . map pair
  where
    pair (ProfilingEvent name dur provenance _details) =
      ((name, provenance), EvSummary 1 dur dur dur)
    comb (EvSummary xn xdur xmin xmax) (EvSummary yn ydur ymin ymax) =
      EvSummary (xn + yn) (xdur + ydur) (min xmin ymin) (max xmax ymax)
