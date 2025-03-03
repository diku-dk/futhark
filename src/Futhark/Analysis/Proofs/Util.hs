module Futhark.Analysis.Proofs.Util
  ( prettyName,
    prettyHole,
    prettyBinding,
    partitions,
  )
where

import Control.Monad (guard)
import Data.List (subsequences, (\\))
import Data.Maybe (fromJust)
import Futhark.Util.Pretty (Doc, Pretty, align, docString, line, ppTupleLines', pretty, (<+>))
import Language.Futhark (VName (VName))

prettyName :: VName -> Doc ann
prettyName (VName vn i) = pretty vn <> pretty (map (fromJust . subscript) (show i))
  where
    subscript = flip lookup $ zip "-0123456789" "₋₀₁₂₃₄₅₆₇₈₉"

prettyHole :: VName -> Doc ann
prettyHole x = "\ESC[4m•" <> prettyName x <> "\ESC[24m"

prettyBinding :: (Pretty a) => VName -> [a] -> String
prettyBinding vn e =
  docString $
    "\ESC[34mι\ESC[0m "
      <+> prettyName vn
      <+> "="
      <> line
      <> "    "
      <> align (ppTupleLines' $ map pretty e)

-- Generate all partitions of `xs` into `k` sublists.
-- Includes sublists that are permutations of other sublists.
-- For example, `partitions 3 [1..4]` returns both `[[1],[2],[3,4]]`
-- and `[[2], [1], [3,4]]`.
partitions :: (Eq a) => Int -> [a] -> [[[a]]]
partitions k xs
  | k == 1 = [[xs]]
  | 2 <= k && k <= length xs = do
      s <- subsequences xs
      guard (not (null s))
      guard (length xs - length s >= k - 1)
      x <- partitions (k - 1) (xs \\ s)
      [s : x]
  | otherwise = []
