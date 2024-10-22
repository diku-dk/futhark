module Futhark.Analysis.Proofs.Rule where

import Control.Monad (foldM, msum, (<=<))
import Data.List (subsequences, (\\))
import Futhark.Analysis.Proofs.IndexFn (IndexFn, casesToList)
import Futhark.Analysis.Proofs.Monad (IndexFnM)
import Futhark.Analysis.Proofs.IndexFnPlus (unifyIndexFnWith)
import Futhark.Analysis.Proofs.Symbol (Symbol)
import Futhark.Analysis.Proofs.SymbolPlus ()
import Futhark.Analysis.Proofs.Unify (Substitution, Unify (unify), unifies, renameAnd, unifies_, Replaceable (rep))
import Futhark.Analysis.Proofs.Util (allocateTerms)
import Futhark.SoP.SoP (SoP, numTerms, sopFromList, sopToList, (.+.))
import Futhark.Util.Pretty (Pretty)

data Rule a b m = Rule
  { name :: String,
    from :: a,
    to :: Substitution b -> m a,
    sideCondition :: Substitution b -> m Bool
  }

type RuleBook a b m = [Rule a b m]

class ApplyRule a b where
  applyRule :: Rule a b IndexFnM -> a -> IndexFnM a

instance ApplyRule (SoP Symbol) Symbol where
  applyRule rule sop
    | numTerms (from rule) <= numTerms sop = do
        let (subterms, contexts) = unzip . splits $ sopToList sop
        -- Get first valid subterm substitution. Recursively match context.
        subs <- mapM (matchPartition . sopFromList) subterms
        case msum $ zipWith (\x y -> (,y) <$> x) subs contexts of
          Just (s, ctx) -> (.+.) <$> applyRule rule (sopFromList ctx) <*> to rule s
          Nothing -> pure sop
    | otherwise = pure sop
    where
      -- Get all (at least k-subterms, remaining subterms).
      k = numTerms (from rule)
      splits xs = [(s, xs \\ s) | s <- subsequences xs, length s >= k]

      -- Pick first partition that matches.
      -- Unify subterms in `x` with subterms in `y` in all possible ways.
      matchPartition s =
        msum <$>
          mapM (check (sideCondition rule) <=< unifies) (allocateTerms (from rule) s)

instance ApplyRule Symbol Symbol where
  applyRule = applyRuleDefault

instance ApplyRule IndexFn Symbol where
  applyRule rule indexfn =
    renameAnd (unifyIndexFnWith matchCases) (from rule) indexfn
      >>= check (sideCondition rule)
      >>= maybe (pure indexfn) (to rule)
    where
      -- Unify cases as if applying a SoP rule, except that partial matches are disallowed.
      matchCases k cs1 cs2 = do
        let (p_xs, v_xs) = unzip $ casesToList cs1
        let (p_ys, v_ys) = unzip $ casesToList cs2
        s1 <- unifies_ k (zip p_xs p_ys)
        foldM
          ( \s (x, y) ->
              (s <>) <$> matchSoP k (rep s x) (rep s y)
          )
          s1
          (zip v_xs v_ys)
      -- TODO does msum here exclude potential matches?
      -- (That is, rather than try every possible fold above,
      -- we select the first valid always.)
      matchSoP k x y = msum $ map (unifies_ k) (allocateTerms x y)

check :: (Monad f) => (a -> f Bool) -> Maybe a -> f (Maybe a)
check _ Nothing = pure Nothing
check cond (Just s) = do
  b <- cond s
  pure $ if b then Just s else Nothing

applyRuleDefault :: (Unify b u, Pretty b) => Rule b u IndexFnM -> b -> IndexFnM b
applyRuleDefault rule x =
  unify (from rule) x
    >>= check (sideCondition rule)
    >>= maybe (pure x) (to rule)

applyRuleBook :: (ApplyRule a b) => IndexFnM (RuleBook a b IndexFnM) -> a -> IndexFnM a
applyRuleBook book x =
  book >>= foldM (flip applyRule) x
