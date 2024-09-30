module Futhark.Analysis.Proofs.Rewrite where

import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), cases)
import Futhark.Analysis.Proofs.IndexFnPlus (normalizeIndexFn, subIndexFn)
import Futhark.Analysis.Proofs.Refine (refineIndexFn, refineSymbol)
import Futhark.Analysis.Proofs.Rule (Rule (..), applyRuleBook)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, normalizeSymbol, applyLinCombRule)
import Futhark.Analysis.Proofs.SymbolPlus (getRenamedLinCombBoundVar, repVName)
import Futhark.Analysis.Proofs.Traversals (ASTMappable, ASTMapper (..), astMap)
import Futhark.Analysis.Proofs.Unify (Substitution (mapping), mkRep, rep, sub)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.FourierMotzkin (($<=$), ($==$), ($>$))
import Futhark.SoP.Monad (substEquivs)
import Futhark.SoP.SoP (SoP, int2SoP, sym2SoP, (.*.), (.+.), (.-.), sopToLists)
import Language.Futhark (VName)

vacuous :: (Monad m) => b -> m Bool
vacuous = const (pure True)

int :: Integer -> SoP Symbol
int = int2SoP

sVar :: VName -> SoP Symbol
sVar = sym2SoP . Var

hole :: VName -> SoP Symbol
hole = sym2SoP . Hole

(~+~) :: (Ord u) => u -> u -> SoP u
a ~+~ b = sym2SoP a .+. sym2SoP b

(~-~) :: (Ord u) => u -> u -> SoP u
a ~-~ b = sym2SoP a .-. sym2SoP b

converge :: (Eq a, Monad m) => (a -> m a) -> a -> m a
converge f x = converge_ (f x)
  where
    converge_ fx = do
      y <- fx
      z <- f y
      if y == z
        then pure y
        else converge_ (pure z)

rewriteGen :: (ASTMappable Symbol a) => a -> IndexFnM a
rewriteGen = astMap m
  where
    m :: ASTMapper Symbol IndexFnM =
      ASTMapper
        { mapOnSymbol = converge (fmap normalizeSymbol . refineSymbol),
          mapOnSoP = converge (applyRuleBook rulesSoP) <=< substEquivs
        }

class (Monad m) => Rewritable v m where
  rewrite :: v -> m v

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = rewriteGen

instance Rewritable Symbol IndexFnM where
  rewrite = rewriteGen

instance Rewritable IndexFn IndexFnM where
  rewrite =
    converge $
      refineIndexFn rewriteGen <=< normalizeIndexFn <=< applyRuleBook rulesIndexFn <=< rewriteGen

scale :: VName -> Symbol -> SoP Symbol
scale c symbol = hole c .*. sym2SoP symbol

rulesSoP :: IndexFnM [Rule (SoP Symbol) Symbol IndexFnM]
rulesSoP = do
  i <- newVName "i"
  -- TODO this hole meant for constants must come before other
  -- holes in order to match in rules. This is because
  -- only the first hole in a term is attempted to match against
  -- constants and the first hole will be the one with the lowest tag.
  -- Gotta fix that.
  c <- newVName "h"
  h1 <- newVName "h"
  h2 <- newVName "h"
  h3 <- newVName "h"
  h4 <- newVName "h"
  x1 <- newVName "x"
  y1 <- newVName "y"
  pure
    [ let lincomb = LinComb i (hole h1) (hole h2) (Hole h3)
       in Rule
            { name = "Extend sum lower bound",
              from =
                scale c lincomb .+. scale c (Hole h4),
              to = \s ->
                sub s $
                  scale c $
                    LinComb i (hole h1 .-. int 1) (hole h2) (Hole h3),
              sideCondition = \s -> do
                lb <- sub s (Hole h1)
                j <- fromJust <$> getRenamedLinCombBoundVar s lincomb
                x <- rep (mkRep j $ lb .-. int 1) <$> sub s (Hole h3)
                y <- sub s (Hole h4)
                x $==$ y
            },
      let lincomb = LinComb i (hole h1) (hole h2) (Hole h3)
       in Rule
            { name = "Extend sum upper bound",
              from =
                scale c lincomb .+. scale c (Hole h4),
              to = \s ->
                sub s $
                  scale c $
                    LinComb i (hole h1) (hole h2 .+. int 1) (Hole h3),
              sideCondition = \s -> do
                ub <- sub s (Hole h2)
                j <- fromJust <$> getRenamedLinCombBoundVar s lincomb
                x <- rep (mkRep j $ ub .+. int 1) <$> sub s (Hole h3)
                y <- sub s (Hole h4)
                x $==$ y
            },
      Rule
        { name = "Merge sum-subtractation",
          from =
            scale c (LinComb i (hole h1) (hole x1) (Hole h2))
              .-. scale c (LinComb i (hole h1) (hole y1) (Hole h2)),
          to = \s ->
            sub s $
              scale c $
                LinComb i (hole y1 .+. int 1) (hole x1) (Hole h2),
          sideCondition = \s -> do
            y' <- sub s (Hole y1)
            x' <- sub s (Hole x1)
            y' $<=$ x'
        },
      Rule
        { name = "[[¬x]] => 1 - [[x]]",
          from = sym2SoP $ Indicator (neg (Hole h1)),
          to = \s -> sub s $ int 1 .-. sym2SoP (Indicator (Hole h1)),
          sideCondition = vacuous
        },
      let lincomb = LinComb i (hole h1) (hole h2) (Hole h3)
       in Rule
            { name = "Replace sum over one element sequence by element",
              from = sym2SoP lincomb,
              to = \s -> do
                j <- fromJust <$> getRenamedLinCombBoundVar s lincomb
                idx <- sub s (Hole h1)
                rep (mkRep j idx) <$> sub s (Hole h3),
              sideCondition = \s -> do
                start <- sub s (Hole h1)
                end <- sub s (Hole h2)
                start $==$ end
            },
      Rule
        { name = "Replace sum over empty sequence by zero",
          from = sym2SoP $ LinComb i (hole h1) (hole h2) (Hole h3),
          to = const . pure $ int2SoP 0,
          sideCondition = \s -> do
            start <- sub s (Hole h1)
            end <- sub s (Hole h2)
            start $>$ end
        }
    ]

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
              let e2_sum = applyLinCombRule j (b' .+. int2SoP 1) (sym2SoP $ Var iter) e2_j
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
              let e2_sum = applyLinCombRule j (b' .+. int2SoP 1) (sym2SoP $ Var iter) e2_j
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
