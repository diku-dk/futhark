module Futhark.Analysis.Proofs.Rule
  ( applyRuleBook,
    rulesIndexFn,
    vacuous,
    RuleBook,
    Rule (..),
  )
where

import Control.Monad (foldM, msum, unless, (<=<))
import Data.List (subsequences, (\\))
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), Iterator (..), cases, casesToList)
import Futhark.Analysis.Proofs.IndexFnPlus (subIndexFn, unifyIndexFnWith)
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugPrettyM, debugT')
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.SymbolPlus (repVName, toSumOfSums)
import Futhark.Analysis.Proofs.Unify (Replaceable (rep), Substitution (mapping), Unify (unify), mkRep, renameAnd, sub, unifies, unifies_)
import Futhark.Analysis.Proofs.Util (allocateTerms)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, int2SoP, numTerms, sopFromList, sopToList, sopToLists, sym2SoP, (.+.), (~+~))
import Futhark.Util.Pretty (Pretty)
import Language.Futhark (VName)

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
        msum
          <$> mapM (check (sideCondition rule) <=< unifies) (allocateTerms (from rule) s)

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
        unless
          (length p_xs == length p_ys)
          (fail "Number of cases should match too.")
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

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------
vacuous :: (Monad m) => b -> m Bool
vacuous = const (pure True)

int :: Integer -> SoP Symbol
int = int2SoP

hole :: VName -> SoP Symbol
hole = sym2SoP . Hole

rulesIndexFn :: IndexFnM [Rule IndexFn Symbol IndexFnM]
rulesIndexFn = do
  i <- newVName "i"
  k <- newVName "k"
  n <- newVName "n"
  m <- newVName "m"
  b <- newVName "b"
  h1 <- newVName "h"
  h2 <- newVName "h"
  h3 <- newVName "h"
  pure
    [ Rule
        { name = "Rule 5 (carry)",
          -- y = ∀i ∈ [0, 1, ..., n - 1] .
          --    | i == b => e1
          --    | i /= b => y[i-1]
          -- _______________________________________________________________
          -- y = ∀i ∈ [0, 1, ..., n - 1] . {i->b}e1
          from =
            IndexFn
              { iterator = Forall i (Iota (hole n)),
                body =
                  cases
                    [ (hole i :== int 0, hole h1),
                      (hole i :/= int 0, sym2SoP Recurrence)
                    ]
              },
          -- Indexing variable i replaced by 0 in e1.
          to = \s ->
            subIndexFn s =<< do
              let i' = repVName (mapping s) i
              e1 <- sub s (hole h1)
              let e1_b = rep (mkRep i' (int 0)) e1
              pure $
                IndexFn
                  { iterator = Forall i (Iota (hole n)),
                    body = cases [(Bool True, e1_b)]
                  },
          sideCondition = vacuous
        },
      Rule
        { name = "Rule 5 (carry)",
          -- y = ∀i ∈ ⊎k=iota m [b, b+1, ..., b + n - 1] .
          --    | i == b => e1
          --    | i /= b => y[i-1]
          -- _______________________________________________________________
          -- y = ∀i ∈ ⊎k=iota m [b, b+1, ..., b + n - 1] . {i->b}e1
          --
          -- Note that b may depend on k.
          from =
            IndexFn
              { iterator = Forall i (Cat k (hole m) (hole b)),
                body =
                  cases
                    [ (hole i :== hole b, hole h1),
                      (hole i :/= hole b, sym2SoP Recurrence)
                    ]
              },
          to = \s ->
            subIndexFn s =<< do
              let i' = repVName (mapping s) i
              e1 <- sub s (hole h1)
              b' <- sub s (hole b)
              -- Use that i = b to remove any dependence on i.
              let e1_b = rep (mkRep i' b') e1
              pure $
                IndexFn
                  { iterator = Forall i (Cat k (hole m) (hole b)),
                    body = cases [(Bool True, e1_b)]
                  },
          sideCondition = vacuous
        },
      Rule
        { name = "Rule 5 (carry) (case order switched)",
          -- TODO deduplicate
          from =
            IndexFn
              { iterator = Forall i (Iota (hole n)),
                body =
                  cases
                    [ (hole i :/= int 0, sym2SoP Recurrence),
                      (hole i :== int 0, hole h1)
                    ]
              },
          -- Indexing variable i replaced by 0 in e1.
          to = \s ->
            subIndexFn s =<< do
              let i' = repVName (mapping s) i
              e1 <- sub s (hole h1)
              let e1_b = rep (mkRep i' (int 0)) e1
              pure $
                IndexFn
                  { iterator = Forall i (Iota (hole n)),
                    body = cases [(Bool True, e1_b)]
                  },
          sideCondition = \s -> do
            e1_symbols <- concatMap fst . sopToLists <$> sub s (Hole h1)
            pure $ Recurrence `notElem` e1_symbols
        },
      Rule
        { name = "Rule 5 (carry) (case order switched)",
          -- TODO deduplicate
          from =
            IndexFn
              { iterator = Forall i (Cat k (hole m) (hole b)),
                body =
                  cases
                    [ (hole i :/= hole b, sym2SoP Recurrence),
                      (hole i :== hole b, hole h1)
                    ]
              },
          to = \s ->
            subIndexFn s =<< do
              let i' = repVName (mapping s) i
              e1 <- sub s (hole h1)
              b' <- sub s (hole b)
              -- Use that i = b to remove any dependence on i.
              let e1_b = rep (mkRep i' b') e1
              pure $
                IndexFn
                  { iterator = Forall i (Cat k (hole m) (hole b)),
                    body = cases [(Bool True, e1_b)]
                  },
          sideCondition = \s -> do
            b_symbols <- concatMap fst . sopToLists <$> sub s (Hole b)
            e1_symbols <- concatMap fst . sopToLists <$> sub s (Hole h1)
            pure $ Recurrence `notElem` (b_symbols <> e1_symbols)
        },
      Rule
        { name = "Prefix sum",
          -- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
          --    | i == b => e1              (e1 may depend on i)
          --    | p_rec  => y[i-1] + e2     (e2 may depend on i)
          --
          -- If y is a valid index function p_rec can match any expression;
          -- we know that it will reduce to i > 0. (For example, p_rec = i /= b
          -- and i in [b, b+1, ...] implies that p_rec <=> i > b.)
          --
          -- e2 is a SoP with terms e2_0, ..., e2_l.
          -- _______________________________________________________________
          -- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
          --    e1{b/i} + (Σ_{j=b+1}^i e2_0{j/i}) + ... + (Σ_{j=b+1}^i e2_l{j/i})
          from =
            IndexFn
              { iterator = Forall i (Iota (hole n)),
                body =
                  cases
                    [ (hole i :== int 0, hole h1),
                      (Hole h3, Recurrence ~+~ Hole h2)
                    ]
              },
          to = \s -> do
            let b' = int 0
            let iter = repVName (mapping s) i
            e1 <- sub s (hole h1)
            let e1_b = rep (mkRep iter b') e1
            e2 <- sub s (hole h2)
            j <- newVName "j"
            let e2_j = rep (mkRep iter (sym2SoP $ Var j)) e2
            let e2_sum = toSumOfSums j (b' .+. int2SoP 1) (sym2SoP $ Var iter) e2_j
            subIndexFn s $
              IndexFn
                { iterator = Forall i (Iota (hole n)),
                  body = cases [(Bool True, e1_b .+. e2_sum)]
                },
          sideCondition = \s -> do
            e1_symbols <- concatMap fst . sopToLists <$> sub s (Hole h1)
            e2_symbols <- concatMap fst . sopToLists <$> sub s (Hole h2)
            pure $ Recurrence `notElem` (e1_symbols <> e2_symbols)
        },
      -- TODO deduplicate Iota/Cat rules
      Rule
        { name = "Prefix sum",
          from =
            IndexFn
              { iterator = Forall i (Cat k (hole m) (hole b)),
                body =
                  cases
                    [ (hole i :== hole b, hole h1),
                      (Hole h3, Recurrence ~+~ Hole h2)
                    ]
              },
          to = \s -> debugT' "prefix sum cat" $ do
            let i' = repVName (mapping s) i
            b' <- sub s (hole b)
            e1_b <- rep (mkRep i' b') <$> sub s (hole h1)
            debugPrettyM "e1_b" e1_b
            e2 <- sub s (hole h2)
            j <- newVName "j"
            let e2_j = rep (mkRep i' (sym2SoP $ Var j)) e2
            let e2_sum = toSumOfSums j (b' .+. int2SoP 1) (sym2SoP $ Var i') e2_j
            debugPrettyM "e2" e2
            debugPrettyM "e2_j" e2_j
            debugPrettyM "e2_sum" e2_sum
            subIndexFn s $
              IndexFn
                { iterator = Forall i (Cat k (hole m) (hole b)),
                  body = cases [(Bool True, e1_b .+. e2_sum)]
                },
          sideCondition = \s -> do
            e1_symbols <- concatMap fst . sopToLists <$> sub s (Hole h1)
            e2_symbols <- concatMap fst . sopToLists <$> sub s (Hole h2)
            pure $ Recurrence `notElem` (e1_symbols <> e2_symbols)
        },
      Rule
        { name = "Bool is Int",
          -- NOTE not needed in the paper because all values are ints there?
          from =
            IndexFn
              { iterator = Empty,
                body =
                  cases
                    [ (Hole h1, int2SoP 1),
                      (Hole h2, int2SoP 0)
                    ]
              },
          to = \s ->
            subIndexFn s $
              IndexFn
                { iterator = Empty,
                  body = cases [(Bool True, hole h1)]
                },
          sideCondition = vacuous
        },
      Rule
        { name = "Bool is Int (case order switched)",
          from =
            IndexFn
              { iterator = Empty,
                body =
                  cases
                    [ (Hole h1, int2SoP 0),
                      (Hole h2, int2SoP 1)
                    ]
              },
          to = \s ->
            subIndexFn s $
              IndexFn
                { iterator = Empty,
                  body = cases [(Bool True, hole h2)]
                },
          sideCondition = vacuous
        }
    ]
