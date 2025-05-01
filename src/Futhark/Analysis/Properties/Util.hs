module Futhark.Analysis.Properties.Util
  ( prettyName,
    prettyHole,
    prettyBinding,
    partitions,
    prettyIndent,
    warningMsg,
    warningString,
    errorMsg,
    emphString,
    locMsg,
    greenString,
    blueString,
  )
where

import Control.Monad (guard)
import Data.List (subsequences, (\\))
import Data.Maybe (fromJust)
import Futhark.Util.Pretty
import Language.Futhark (Located, VName (VName), locText, srclocOf)
import Data.String (IsString)

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
prettyIndent n e = docStringW 80 $ indent n (hang n $ pretty e)

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

errorMsg :: (Located a) => a -> [Char] -> String
errorMsg loc msg =
  "Error at " <> prettyString (locText (srclocOf loc)) <> ": " <> msg

warningMsg :: (Located a) => a -> String -> String
warningMsg loc msg = do
  warningString $
    prettyString (locText (srclocOf loc)) <> ": " <> msg

locMsg :: (Located a) => a -> String -> String
locMsg loc msg = do
  prettyString (locText (srclocOf loc)) <> ": " <> msg

warningString :: String -> String
warningString s = "\ESC[93m" <> s <> "\ESC[0m"

emphString :: String -> String
emphString s = "\ESC[95m\n|\n| " <> s <> "\n|\ESC[0m\n"

greenString :: String -> String
greenString s = "\ESC[92m" <> s <> "\ESC[0m"

blueString :: (Semigroup a, IsString a) => a -> a
blueString s = "\ESC[96m" <> s <> "\ESC[0m"
