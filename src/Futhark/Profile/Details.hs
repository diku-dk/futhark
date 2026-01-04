module Futhark.Profile.Details (CostCentreDetails(..), SourceRangeDetails(..), CostCentreName(..), sourceRangeDetailsFraction, CostCentres, SourceRanges) where
import Futhark.Profile.SourceRange (SourceRange)
import Data.Map (Map)
import Futhark.Profile.EventSummary (EvSummary)
import Data.Text (Text)
import Data.Monoid (Sum(Sum, getSum))
import Control.Arrow ((>>>))

type CostCentres = Map CostCentreName CostCentreDetails
type SourceRanges = Map SourceRange SourceRangeDetails

newtype CostCentreName = CostCentreName { getCostCentreName :: Text }
  deriving (Eq, Ord)

data CostCentreDetails = CostCentreDetails
  { fraction :: Double
  -- ^ Fraction of total program time spent in this cost centre
  , sourceRanges :: Map SourceRange SourceRangeDetails
  -- ^ all source ranges included in this cost centre
  , summary :: EvSummary
  -- ^ statistics about the runtime characteristics
  }

newtype SourceRangeDetails = SourceRangeDetails
  { containingCostCentres :: Map CostCentreName CostCentreDetails
  }

sourceRangeDetailsFraction :: SourceRangeDetails -> Double
sourceRangeDetailsFraction = containingCostCentres
  >>> foldMap (Sum . fraction)
  >>> getSum
