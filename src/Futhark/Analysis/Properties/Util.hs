module Futhark.Analysis.Properties.Util
  ( prettyName,
    prettyHole,
    prettyBinding,
    partitions,
    prettyIndent,
    prettyFun,
    dummyVName,
  )
where

import Control.Monad (guard)
import Data.List (subsequences, (\\))
import Data.Maybe (fromJust)
import Futhark.Util.Pretty
import Language.Futhark (VName (VName), nameFromString)

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

prettyIndent :: (Pretty a) => Int -> a -> String
prettyIndent n e = docStringW 80 $ indent n (pretty e)

prettyFun :: Pretty a => (VName -> a) -> [Char] -> Doc ann
prettyFun f arg_name =
  "λ" <> prettyName arg <> dot <+> pretty (f arg)
  where
    arg = VName (nameFromString arg_name) (-1)

dummyVName :: VName
dummyVName = VName (nameFromString "DUMMY_NAME") (-1)

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
