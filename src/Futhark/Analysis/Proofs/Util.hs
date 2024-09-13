module Futhark.Analysis.Proofs.Util
where

import Language.Futhark (VName (VName))
import Futhark.Util.Pretty (pretty, Pretty, prettyString)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Data.List (subsequences, (\\))
import Control.Monad (guard)

prettyName (VName vn i) = pretty vn <> pretty (map (fromJust . subscript) (show i))
  where
    subscript = flip lookup $ zip "-0123456789" "₋₀₁₂₃₄₅₆₇₈₉"

tracer :: Pretty a => a -> a
tracer x = trace (prettyString x) x

-- Generate all partitions of `xs` into `k` sublists.
-- Includes sublists that are permutations of other sublists.
-- For example, `combine 3 [1..4]` returns both `[[1],[2],[3,4]]`
-- and `[[2], [1], [3,4]]`.
partitions :: Eq a => Int -> [a] -> [[[a]]]
partitions k xs
  | k == 1 = [[xs]]
  | 2 <= k && k <= length xs = do
    s <- subsequences xs
    guard (not (null s))
    guard (length xs - length s >= k - 1)
    x <- partitions (k-1) (xs \\ s)
    [s : x]
  | otherwise = []
