module Futhark.Analysis.Proofs.Rewrite where

import Control.Monad (filterM, (<=<))
import Data.List.NonEmpty qualified as NE
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, algebraContext, assume, isFalse, rollbackAlgEnv, simplify)
import Futhark.Analysis.Proofs.IndexFn (Cases (..), IndexFn (..), cases, casesToList)
import Futhark.Analysis.Proofs.Monad (IndexFnM)
import Futhark.Analysis.Proofs.Query (isUnknown)
import Futhark.Analysis.Proofs.Rule (applyRuleBook, rulesIndexFn)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.Unify (renameSame, Renameable)
import Futhark.SoP.SoP (SoP, int2SoP, justConstant, sym2SoP, (.*.), (.+.))
import Futhark.SoP.SoP qualified as SoP

normalizeIndexFn :: IndexFn -> IndexFnM IndexFn
normalizeIndexFn = allCasesAreConstants

allCasesAreConstants :: IndexFn -> IndexFnM IndexFn
allCasesAreConstants v@(IndexFn _ (Cases ((Bool True, _) NE.:| []))) = pure v
allCasesAreConstants (IndexFn it (Cases cs))
  | cs' <- NE.toList cs,
    Just vs <- mapM (justConstant . snd) cs' = do
      let ps = map fst cs'
      let sumOfBools =
            SoP.normalize . foldl1 (.+.) $
              zipWith (\p x -> sym2SoP p .*. int2SoP x) ps vs
      -- tell ["Using simplification rule: integer-valued cases"]
      pure $ IndexFn it $ Cases (NE.singleton (Bool True, sumOfBools))
allCasesAreConstants v = pure v

class (Monad m) => Rewritable v m where
  rewrite :: v -> m v

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = simplify

instance Rewritable Symbol IndexFnM where
  rewrite = simplify

instance Rewritable IndexFn IndexFnM where
  rewrite = convergeRename (rewrite_ <=< applyRuleBook rulesIndexFn)

convergeRename :: (Renameable t, Eq t) => (t -> IndexFnM t) -> t -> IndexFnM t
convergeRename f x = do
  y <- f x
  (x', y') <- renameSame x y
  if x' == y'
    then pure x'
    else do
      convergeRename f y'

rewrite_ :: IndexFn -> IndexFnM IndexFn
rewrite_ = normalizeIndexFn <=< simplifyIndexFn
  where
    simplifyIndexFn fn@(IndexFn it xs) = algebraContext fn $ do
      addRelIterator it
      ys <- simplifyCases xs
      pure $ IndexFn it ys

    simplifyCases cs = do
      let (ps, vs) = unzip $ casesToList cs
      ps_simplified <- mapM rewrite ps
      -- Remove impossible cases.
      cs' <- filterM (fmap isUnknown . isFalse . fst) (zip ps_simplified vs)
      cs'' <- mapM simplifyCase cs'
      cases <$> mergeEquivCases cs''

    -- Simplify x under the assumption that p is true.
    simplifyCase (p, x) = rollbackAlgEnv $ do
      -- Take care to convert x first to hopefully get sums of predicates
      -- translated first.
      assume p
      y <- rewrite x
      pure (p, y)

    -- Attempt to merge cases that are equivalent given their predicates.
    -- For example, in
    --   | k > 0  => sum_{j=0}^{k-1} e_j
    --   | k <= 0 => 0
    -- the second case is covered by the first when k <= 0. So we want just:
    --   | True  => sum_{j=0}^{k-1} e_j
    mergeEquivCases cs@[(_p1, v1), (p2, v2)] = do
      (_, v1') <- simplifyCase (p2, v1)
      if v1' == v2
        then pure [(Bool True, v1)]
        else pure cs
    mergeEquivCases cs = pure cs

rewriteWithoutRules :: IndexFn -> IndexFnM IndexFn
rewriteWithoutRules =
  convergeRename rewrite_
