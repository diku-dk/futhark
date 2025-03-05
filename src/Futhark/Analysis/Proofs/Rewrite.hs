module Futhark.Analysis.Proofs.Rewrite (rewrite, rewriteWithoutRules) where

import Control.Monad (filterM, (<=<))
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, algebraContext, assume, isFalse, simplify)
import Futhark.Analysis.Proofs.IndexFn (IndexFn (..), cases, casesToList)
import Futhark.Analysis.Proofs.Monad (IndexFnM, rollbackAlgEnv)
import Futhark.Analysis.Proofs.Query (isUnknown)
import Futhark.Analysis.Proofs.Rule (applyRuleBook, rulesIndexFn)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), toCNF)
import Futhark.Analysis.Proofs.Unify (Renameable, renameSame)
import Futhark.SoP.SoP (SoP, justConstant)

class (Monad m) => Rewritable v m where
  rewrite :: v -> m v

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = convergeRename simplify

instance Rewritable Symbol IndexFnM where
  rewrite = convergeRename simplify

instance Rewritable IndexFn IndexFnM where
  rewrite = convergeRename (rewrite_ <=< applyRuleBook rulesIndexFn)

convergeRename :: (Renameable t, Eq t) => (t -> IndexFnM t) -> t -> IndexFnM t
convergeRename f x = do
  y <- f x
  (x', y') <- renameSame x y
  if x' == y'
    then pure x
    else do
      convergeRename f y

rewrite_ :: IndexFn -> IndexFnM IndexFn
rewrite_ fn@(IndexFn it xs) = simplifyIndexFn
  where
    simplifyIndexFn = algebraContext fn $ do
      addRelIterator it
      ys <- simplifyCases xs
      pure $ IndexFn it ys

    simplifyCases cs = do
      let (ps, vs) = unzip $ casesToList cs
      ps_simplified <- mapM rewrite ps
      cs' <-
        eliminateFalsifiableCases (zip ps_simplified vs)
          >>= mapM simplifyCase
          >>= mergeCases
      pure $ cases cs'

    -- Simplify x under the assumption that p is true.
    simplifyCase (p, x)
      | Just _ <- justConstant x =
          pure (p, x)
    simplifyCase (p, x) = rollbackAlgEnv $ do
      assume (toCNF p)
      y <- rewrite x
      pure (p, y)

    -- Attempt to merge cases that are equivalent given their predicates.
    -- For example, in
    --   | k > 0  => sum_{j=0}^{k-1} e_j
    --   | k <= 0 => 0
    -- the second case is covered by the first when k <= 0:
    --   | True  => sum_{j=0}^{k-1} e_j
    --
    -- NOTE only tries to merge adjacent cases.
    mergeCases = merge []
      where
        merge acc [] = pure $ reverse acc
        merge [] (a : as) = merge [a] as
        merge ((c, e) : as) ((c', e') : bs) = do
          (_, e_assuming_c') <- simplifyCase (c', e)
          if e_assuming_c' == e'
            then merge ((c :|| c', e) : as) bs
            else do
              (_, e'_assuming_c) <- simplifyCase (c, e')
              if e'_assuming_c == e
                then merge ((c :|| c', e') : as) bs
                else merge ((c', e') : (c, e) : as) bs

    -- Remove cases for which the predicate can be shown False.
    eliminateFalsifiableCases = filterM (fmap isUnknown . isFalse . fst)

rewriteWithoutRules :: IndexFn -> IndexFnM IndexFn
rewriteWithoutRules =
  convergeRename rewrite_
