module Futhark.Analysis.Proofs.Rule where

import Control.Monad (foldM, msum, (<=<))
import Data.List (subsequences, (\\))
import Futhark.Analysis.Proofs.IndexFn (IndexFn, IndexFnM)
import Futhark.Analysis.Proofs.IndexFnPlus ()
import Futhark.Analysis.Proofs.Symbol (Symbol)
import Futhark.Analysis.Proofs.SymbolPlus ()
import Futhark.Analysis.Proofs.Unify (Substitution, Unify (unify), unifies)
import Futhark.Analysis.Proofs.Util (partitions)
import Futhark.SoP.SoP (SoP, numTerms, sopFromList, sopToList, term2SoP, (.+.))

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
      matchPartition s =
        msum <$> mapM (check (sideCondition rule) <=< unifies) (combine s)

      -- Pair each term in from rule with subterms in sop.
      -- For example, h1 + h2 and x + y + z pairs as follows
      -- [[(h1, x), (h2, y+z)],
      --  [(h1, x+y), (h2, z)],
      --  [(h1, x+z), (h2, y)],
      --  ... permutations where h1 and h2 are switched
      -- ]
      combine s
        | k <= numTerms s = do
            let from_xs = sopToList (from rule)
            partition <- partitions k (sopToList s)
            pure $
              zipWith (\x ts -> (uncurry term2SoP x, sopFromList ts)) from_xs partition
        | otherwise = []

instance ApplyRule Symbol Symbol where
  applyRule = applyRuleDefault

instance ApplyRule IndexFn Symbol where
  applyRule = applyRuleDefault

check :: (Monad f) => (a -> f Bool) -> Maybe a -> f (Maybe a)
check _ Nothing = pure Nothing
check cond (Just s) = do
  b <- cond s
  pure $ if b then Just s else Nothing

applyRuleDefault :: (Unify b u) => Rule b u IndexFnM -> b -> IndexFnM b
applyRuleDefault rule x =
  unify (from rule) x
    >>= check (sideCondition rule)
    >>= maybe (pure x) (to rule)

applyRuleBook :: (ApplyRule a b) => IndexFnM (RuleBook a b IndexFnM) -> a -> IndexFnM a
applyRuleBook book x =
  book >>= foldM (flip applyRule) x
