module Futhark.Analysis.Proofs.Util
where

import Language.Futhark (VName (VName))
import Futhark.Util.Pretty (pretty)
import Data.Maybe (fromJust)

prettyName (VName vn i) = pretty vn <> pretty (map (fromJust . subscript) (show i))
  where
    subscript = flip lookup $ zip "-0123456789" "₋₀₁₂₃₄₅₆₇₈₉"
