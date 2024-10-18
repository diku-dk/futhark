module Futhark.Analysis.Proofs.Rewrite where

import Control.Monad (filterM, (<=<))
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), cases, casesToList, debugPrettyM)
import Futhark.Analysis.Proofs.IndexFnPlus (normalizeIndexFn, subIndexFn)
import Futhark.Analysis.Proofs.Query (addRelIterator, addRelSymbol, isFalse, isUnknown, rollbackAlgEnv, simplify)
import Futhark.Analysis.Proofs.Rule (Rule (..), applyRuleBook)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), toSumOfLinComb)
import Futhark.Analysis.Proofs.SymbolPlus (repVName)
import Futhark.Analysis.Proofs.Unify (Substitution (mapping), mkRep, renameSame, rep, sub)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, int2SoP, sopToLists, sym2SoP, (.*.), (.+.), (~+~))
import Language.Futhark (VName)

vacuous :: (Monad m) => b -> m Bool
vacuous = const (pure True)

int :: Integer -> SoP Symbol
int = int2SoP

sVar :: VName -> SoP Symbol
sVar = sym2SoP . Var

hole :: VName -> SoP Symbol
hole = sym2SoP . Hole

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
        addRelIterator it
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

scale :: VName -> Symbol -> SoP Symbol
scale c symbol = hole c .*. sym2SoP symbol

rulesIndexFn :: IndexFnM [Rule IndexFn Symbol IndexFnM]
rulesIndexFn = do
  i <- newVName "i"
  k <- newVName "k"
  n <- newVName "n"
  m <- newVName "m"
  b <- newVName "b"
  h1 <- newVName "h"
  h2 <- newVName "h"
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
                    [ (hole i :== int 0, hole h1),
                      (hole i :/= int 0, sym2SoP Recurrence)
                    ]
              },
          -- Indexing variable i replaced by b in e1.
          to = \s ->
            subIndexFn s =<< do
              let i' = repVName (mapping s) i
              e1 <- sub s (hole h1)
              b' <- sub s (hole b)
              let e1_b = rep (mkRep i' b') e1
              pure $
                IndexFn
                  { iterator = Forall i (Cat k (hole m) (hole b)),
                    body = cases [(Bool True, e1_b)]
                  },
          sideCondition = vacuous
        },
      Rule
        { name = "Prefix sum",
          -- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
          --    | i == b => e1              (e1 may depend on i)
          --    | i /= b => y[i-1] + e2     (e2 may depend on i)
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
                      (hole i :/= int 0, Recurrence ~+~ Hole h2)
                    ]
              },
          to = \s ->
            subIndexFn s =<< do
              let b' = int 0
              let iter = repVName (mapping s) i
              j <- newVName "j"
              e1 <- sub s (hole h1)
              let e1_b = rep (mkRep iter b') e1
              e2 <- sub s (hole h2)
              let e2_j = rep (mkRep iter (sym2SoP $ Var j)) e2
              let e2_sum = toSumOfLinComb j (b' .+. int2SoP 1) (sym2SoP $ Var iter) e2_j
              pure $
                IndexFn
                  { iterator = Forall i (Iota (hole n)),
                    body = cases [(Bool True, e1_b .+. e2_sum)]
                  },
          sideCondition = \s -> do
            e2_symbols <- concatMap fst . sopToLists <$> sub s (Hole h2)
            pure $ Recurrence `notElem` e2_symbols
        },
      -- TODO deduplicate Iota/Cat rules
      Rule
        { name = "Prefix sum",
          from =
            IndexFn
              { iterator = Forall i (Cat k (hole m) (hole b)),
                body =
                  cases
                    [ (hole i :== int 0, hole h1),
                      (hole i :/= int 0, Recurrence ~+~ Hole h2)
                    ]
              },
          to = \s ->
            subIndexFn s =<< do
              b' <- sub s (hole b)
              let iter = repVName (mapping s) i
              j <- newVName "j"
              e1 <- sub s (hole h1)
              let e1_b = rep (mkRep iter b') e1
              e2 <- sub s (hole h2)
              let e2_j = rep (mkRep iter (sym2SoP $ Var j)) e2
              let e2_sum = toSumOfLinComb j (b' .+. int2SoP 1) (sym2SoP $ Var iter) e2_j
              pure $
                IndexFn
                  { iterator = Forall i (Iota (hole n)),
                    body = cases [(Bool True, e1_b .+. e2_sum)]
                  },
          sideCondition = \s -> do
            e2_symbols <- concatMap fst . sopToLists <$> sub s (Hole h2)
            pure $ Recurrence `notElem` e2_symbols
        }
    ]
