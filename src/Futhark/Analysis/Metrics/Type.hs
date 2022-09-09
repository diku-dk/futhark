-- | The data type definition for "Futhark.Analysis.Metrics", factored
-- out to simplify the module import hierarchies when working on the
-- test modules.
module Futhark.Analysis.Metrics.Type (AstMetrics (..)) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T

-- | AST metrics are simply a collection from identifiable node names
-- to the number of times that node appears.
newtype AstMetrics = AstMetrics (M.Map Text Int)

instance Show AstMetrics where
  show (AstMetrics m) = unlines $ map metric $ M.toList m
    where
      metric (k, v) = T.unpack k ++ " " ++ show v

instance Read AstMetrics where
  readsPrec _ s =
    maybe [] success $ mapM onLine $ lines s
    where
      onLine l = case words l of
        [k, x] | [(n, "")] <- reads x -> Just (T.pack k, n)
        _ -> Nothing
      success m = [(AstMetrics $ M.fromList m, "")]
