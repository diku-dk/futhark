module Futhark.Analysis.Proofs.Util where

import Control.Monad (guard)
import Data.List (subsequences, (\\))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Futhark.Util.Pretty (Doc, Pretty, docString, pretty, prettyString, (<+>))
import Language.Futhark (VName (VName))
import Futhark.SoP.SoP (SoP, numTerms, term2SoP, sopFromList, sopToList)

prettyName :: VName -> Doc ann
prettyName (VName vn i) = pretty vn <> pretty (map (fromJust . subscript) (show i))
  where
    subscript = flip lookup $ zip "-0123456789" "₋₀₁₂₃₄₅₆₇₈₉"

prettyHole :: VName -> Doc ann
prettyHole x = "•" <> prettyName x

prettyBinding :: (Pretty a1, Pretty a2) => a1 -> a2 -> String
prettyBinding a b = docString $ ">>>" <+> pretty a <+> "=" <+> pretty b

tracer :: (Pretty a) => a -> a
tracer x = trace (prettyString x) x

-- Generate all partitions of `xs` into `k` sublists.
-- Includes sublists that are permutations of other sublists.
-- For example, `combine 3 [1..4]` returns both `[[1],[2],[3,4]]`
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

-- Pair each term in `x` with subterms of `y`, in all possible ways such that
-- all terms in `x` and all terms in `y` are paired.
-- (Analogously, consider terms in `x` to be bins and terms in `y` to be balls.
--  Generate all allocations of balls into bins such that no bin is empty.)
-- For example, x = h1 + h2 and y = a + b + c pairs as follows
-- [[(h1, a), (h2, b+c)],
--  [(h1, a+b), (h2, c)],
--  [(h1, a+c), (h2, b)],
--  ... permutations where h1 and h2 are switched
-- ]
allocateTerms :: (Ord u) => SoP u -> SoP u -> [[(SoP u, SoP u)]]
allocateTerms x y
  | k <= numTerms y = do
      partition <- partitions k ys
      pure $
        zipWith (\t ts -> (uncurry term2SoP t, sopFromList ts)) xs partition
  | otherwise = mempty
  where
    k = numTerms x
    xs = sopToList x
    ys = sopToList y
