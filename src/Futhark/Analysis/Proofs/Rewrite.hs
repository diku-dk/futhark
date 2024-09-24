module Futhark.Analysis.Proofs.Rewrite where

import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), cases)
import Futhark.Analysis.Proofs.IndexFnPlus (normalizeIndexFn, repVName, subIndexFn)
import Futhark.Analysis.Proofs.Refine (refineIndexFn, refineSymbol)
import Futhark.Analysis.Proofs.Rule (Rule (..), applyRuleBook)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), normalizeSymbol)
import Futhark.Analysis.Proofs.SymbolPlus (getRenamedLinCombBoundVar)
import Futhark.Analysis.Proofs.Traversals (ASTMappable, ASTMapper (..), astMap)
import Futhark.Analysis.Proofs.Unify (SubstitutionBuilder (..), rep, sub)
import Futhark.MonadFreshNames
import Futhark.SoP.FourierMotzkin (($<=$), ($==$), ($>$))
import Futhark.SoP.Monad (substEquivs)
import Futhark.SoP.SoP (SoP, int2SoP, sym2SoP, (.*.), (.+.), (.-.))
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
        { mapOnSymbol = converge (refineSymbol . normalizeSymbol),
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
      rewriteGen <=< applyRuleBook rulesIndexFn <=< normalizeIndexFn <=< refineIndexFn rewriteGen

scale :: VName -> Symbol -> SoP Symbol
scale c symbol = hole c .*. sym2SoP symbol

rulesSoP :: IndexFnM [Rule (SoP Symbol) Symbol IndexFnM]
rulesSoP = do
  i <- newVName "i"
  h1 <- newVName "h"
  h2 <- newVName "h"
  h3 <- newVName "h"
  x1 <- newVName "x"
  y1 <- newVName "y"
  c <- newVName "h"
  -- Recognizable holes for debugging.
  d1 <- newVName "d"
  d2 <- newVName "d"
  d3 <- newVName "d"
  d4 <- newVName "d"
  pure
    [ let lincomb = LinComb i (hole d1) (hole d2) (Hole d3)
       in Rule
            { name = "Extend sum lower bound",
              from =
                scale c lincomb .+. scale c (Hole d4),
              to = \s ->
                sub s $
                  scale c $
                    LinComb i (hole d1 .-. int 1) (hole d2) (Hole d3),
              sideCondition = \s -> do
                let lb = rep s (Hole d1)
                j <- fromJust <$> getRenamedLinCombBoundVar s lincomb
                x <- sub (mkSub j $ lb .-. int 1) $ rep s (Hole d3)
                y <- sub s (Hole d4)
                x $==$ y
            },
      let lincomb = LinComb i (hole d1) (hole d2) (Hole d3)
       in Rule
            { name = "Extend sum upper bound",
              from =
                scale c lincomb .+. scale c (Hole d4),
              to = \s ->
                sub s $
                  scale c $
                    LinComb i (hole d1) (hole d2 .+. int 1) (Hole d3),
              sideCondition = \s -> do
                let ub = rep s (Hole d2)
                j <- fromJust <$> getRenamedLinCombBoundVar s lincomb
                x <- sub (mkSub j $ ub .+. int 1) $ rep s (Hole d3)
                y <- sub s (Hole d4)
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
            let y' = rep s (Hole y1)
            let x' = rep s (Hole x1)
            y' $<=$ x'
        },
      Rule
        { name = "[[¬x]] => 1 - [[x]]",
          from = sym2SoP $ Indicator (Not (Hole h1)),
          to = \s -> sub s $ int 1 .-. sym2SoP (Indicator (Hole h1)),
          sideCondition = vacuous
        },
      let lincomb = LinComb i (hole h1) (hole h2) (Hole h3)
       in Rule
            { name = "Replace sum over one element sequence by element",
              from = sym2SoP lincomb,
              to = \s -> do
                j <- fromJust <$> getRenamedLinCombBoundVar s lincomb
                let idx = rep s (Hole h1)
                pure $ rep (mkSub j idx) $ rep s (Hole h3),
              sideCondition = \s -> do
                let start = rep s (Hole h1)
                let end = rep s (Hole h2)
                start $==$ end
            },
      Rule
        { name = "Replace sum over empty sequence by zero",
          from = sym2SoP $ LinComb i (hole h1) (hole h2) (Hole h3),
          to = const . pure $ int2SoP 0,
          sideCondition = \s -> do
            let start = rep s (Hole h1)
            let end = rep s (Hole h2)
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
          -- TODO add bound names (i) are not substituted test for Unify
          -- on index fns
          -- Indexing variable i replaced by 0 in e1.
          to = \s ->
            subIndexFn s =<< do
              let i' = repVName s i
              e1 <- sub s (hole h1)
              e1_b <- sub (mkSub i' (int 0)) e1
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
              let i' = repVName s i
              e1 <- sub s (hole h1)
              b' <- sub s (hole b)
              e1_b <- sub (mkSub i' b') e1
              pure $
                IndexFn
                  { iterator = Forall i (Cat k (hole m) (hole b)),
                    body = cases [(Bool True, e1_b)]
                  },
          sideCondition = vacuous
        }
    ]
