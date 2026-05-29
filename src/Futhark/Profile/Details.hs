module Futhark.Profile.Details (CostCentreDetails (..), SourceRangeDetails (..), CostCentreName (..), sourceRangeDetailsFraction, CostCentres, SourceRanges) where

import Control.Arrow ((>>>))
import Data.Map (Map)
import Data.Monoid (Sum (Sum, getSum))
import Data.Text (Text)
import Futhark.Profile.EventSummary (EvSummary)
import Futhark.Profile.SourceRange (SourceRange)

type CostCentres = Map CostCentreName CostCentreDetails

type SourceRanges = Map SourceRange SourceRangeDetails

newtype CostCentreName = CostCentreName {getCostCentreName :: Text}
  deriving (Eq, Ord)

data CostCentreDetails = CostCentreDetails
  { -- | Fraction of total program time spent in this cost centre
    fraction :: Double,
    -- | all source ranges included in this cost centre
    sourceRanges :: Map SourceRange SourceRangeDetails,
    -- | statistics about the runtime characteristics
    summary :: EvSummary
  }

newtype SourceRangeDetails = SourceRangeDetails
  { containingCostCentres :: Map CostCentreName CostCentreDetails
  }

sourceRangeDetailsFraction :: SourceRangeDetails -> Double
sourceRangeDetailsFraction =
  containingCostCentres
    >>> foldMap (Sum . fraction)
    >>> getSum
