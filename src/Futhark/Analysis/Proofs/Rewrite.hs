module Futhark.Analysis.Proofs.Rewrite where

import Control.Monad (filterM, (<=<))
import Futhark.Analysis.Proofs.IndexFn (IndexFn (..), cases, casesToList)
import Futhark.Analysis.Proofs.Monad (IndexFnM, algenv, debugPrettyM)
import Futhark.Analysis.Proofs.IndexFnPlus (normalizeIndexFn)
import Futhark.Analysis.Proofs.Query (isFalse, isUnknown)
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, addRelSymbol, rollbackAlgEnv, simplify)
import Futhark.Analysis.Proofs.Rule (applyRuleBook, rulesIndexFn)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.Unify (renameSame)
import Futhark.SoP.SoP (SoP)
import Control.Monad.RWS (gets)

class (Monad m) => Rewritable v m where
  rewrite :: v -> m v

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = simplify

instance Rewritable Symbol IndexFnM where
  rewrite = simplify

instance Rewritable IndexFn IndexFnM where
  rewrite =
    convergeRename $
      normalizeIndexFn <=< simplifyIndexFn <=< applyRuleBook rulesIndexFn
    where
      convergeRename f x = do
        y <- f x
        (x', y') <- renameSame x y
        if x' == y'
          then pure x'
          else do
            convergeRename f y'

      simplifyIndexFn (IndexFn it xs) = rollbackAlgEnv $ do
        debugPrettyM "simplifyIndexFn" (IndexFn it xs)
        addRelIterator it
        algenv <- gets algenv
        debugPrettyM "simplifyIndexFn" algenv
        ys <- simplifyCases xs
        pure $ IndexFn it ys

      simplifyCases cs = do
        let (ps, vs) = unzip $ casesToList cs
        ps' <- mapM rewrite ps
        cs' <- removeImpossibleCases (zip ps' vs)
        cs'' <- mapM simplifyCase cs'
        cases <$> mergeEquivCases cs''

      removeImpossibleCases = filterM (fmap isUnknown . isFalse . fst)

      -- Simplify x under the assumption that p is true.
      simplifyCase (p, x) = rollbackAlgEnv $ do
        addRelSymbol p
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
