module Futhark.Analysis.Proofs.Rewrite where

import Control.Monad (foldM, msum, (<=<))
import Data.List (subsequences, (\\))
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), cases)
import Futhark.Analysis.Proofs.IndexFnPlus (repVName, subIndexFn, normalizeIndexFn)
import Futhark.Analysis.Proofs.Refine (refineSymbol)
import Futhark.Analysis.Proofs.Rule (Rule (..))
import Futhark.Analysis.Proofs.Symbol (Symbol (..), normalizeSymbol, getLinCombBoundVar)
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), astMap)
import Futhark.Analysis.Proofs.Unify (Hole, Replaceable, Substitution (vns), SubstitutionBuilder (..), Unify (unify), rep, sub, unifies, Renameable (renameWith))
import Futhark.Analysis.Proofs.Util (partitions)
import Futhark.MonadFreshNames
import Futhark.SoP.FourierMotzkin (($<=$), ($==$), ($>$))
import Futhark.SoP.Monad (substEquivs)
import Futhark.SoP.SoP (SoP, int2SoP, numTerms, sopFromList, sopToList, sym2SoP, term2SoP, (.*.), (.+.), (.-.))
import Language.Futhark (VName)
import Futhark.Util.Pretty (Pretty)
import Data.Maybe (fromJust)

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

match :: (Unify v u m) => Rule v u m -> v -> m (Maybe (Substitution u))
match rule x = unify (from rule) x >>= check (sideCondition rule)

check :: (Monad f) => (a -> f Bool) -> Maybe a -> f (Maybe a)
check _ Nothing = pure Nothing
check cond (Just s) = do
  b <- cond s
  pure $ if b then Just s else Nothing

class (Monad m) => Rewritable v m where
  rewrite :: v -> m v

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = astMap m <=< substEquivs
    where
      m =
        ASTMapper
          { mapOnSymbol = rewrite,
            mapOnSoP = converge $ \sop -> rulesSoP >>= foldM (flip matchSoP) sop
          }

instance Rewritable Symbol IndexFnM where
  rewrite = astMap m
    where
      m =
        ASTMapper
          { mapOnSoP = rewrite,
            mapOnSymbol = converge $ \x -> do
              rulesSymbol
                >>= foldM (flip match_) (normalizeSymbol x)
                >>= refineSymbol . normalizeSymbol
          }

      match_ rule symbol = do
        s :: Maybe (Substitution Symbol) <- case from rule of
          x :&& y -> matchCommutativeRule (:&&) x y
          x :|| y -> matchCommutativeRule (:||) x y
          _ -> match rule symbol
        maybe (pure symbol) (to rule) s
        where
          matchCommutativeRule op x y =
            msum <$> mapM (match rule) [x `op` y, y `op` x]

instance Rewritable IndexFn IndexFnM where
  rewrite =
    converge $
      astMap m <=< \indexfn -> rulesIndexFn >>= foldM (flip match_) indexfn >>= normalizeIndexFn
    where
      m :: ASTMapper Symbol IndexFnM =
        ASTMapper
          { mapOnSoP = rewrite,
            mapOnSymbol = rewrite
          }

      match_ rule fn = do
        -- debugM ("indexfn match " <> name rule <> ": " <> prettyString fn)
        match rule fn >>= maybe (pure fn) (to rule)

-- Apply SoP-rule with k terms to all matching k-subterms in a SoP.
-- For example, given rule `x + x => 2x` and SoP `a + b + c + a + b`,
-- it matches `a + a` and `b + b` and returns `2a + 2b + c`.
matchSoP ::
  ( Ord u,
    Replaceable u u,
    Unify u u IndexFnM,
    Pretty u,
    Hole u
  ) =>
  Rule (SoP u) u IndexFnM ->
  SoP u ->
  IndexFnM (SoP u)
matchSoP rule sop
  | numTerms (from rule) <= numTerms sop = do
      let (subterms, contexts) = unzip . splits $ sopToList sop
      -- Get first valid subterm substitution. Recursively match context.
      -- debugM ("matchSoP " <> name rule <> ": " <> prettyString sop)
      subs <- mapM (matchP rule . sopFromList) subterms
      case msum $ zipWith (\x y -> (,y) <$> x) subs contexts of
        Just (s, ctx) -> (.+.) <$> matchSoP rule (sopFromList ctx) <*> to rule s
        Nothing -> pure sop
  | otherwise = pure sop
  where
    -- Get all (k-subterms, remaining subterms).
    k = numTerms (from rule)
    splits xs = [(s, xs \\ s) | s <- subsequences xs, length s >= k]

    -- Pick first partition that matches.
    matchP rule sop =
      msum <$> mapM (check (sideCondition rule) <=< unifies) (combine rule sop)

combine :: (Ord u) => Rule (SoP u) u m -> SoP u -> [[(SoP u, SoP u)]]
combine rule sop
  | k <= numTerms sop = do
      -- Pair each term in from rule with subterms in sop.
      -- For example, h1 + h2 and x + y + z pairs as follows
      -- [[(h1, x), (h2, y+z)],
      --  [(h1, x+y), (h2, z)],
      --  [(h1, x+z), (h2, y)],
      --  ... permutations where h1 and h2 are switched
      -- ]
      partition <- partitions k ys
      pure $ zipWith (\x ts -> (uncurry term2SoP x, sopFromList ts)) xs partition
  | otherwise = []
  where
    k = numTerms (from rule)
    xs = sopToList (from rule)
    ys = sopToList sop

scale :: VName -> Symbol -> SoP Symbol
scale c symbol = hole c .*. sym2SoP symbol

rulesSoP :: IndexFnM [Rule (SoP Symbol) Symbol IndexFnM]
rulesSoP = do
  i <- newVName "i"
  h1 <- newVName "h"
  h2 <- newVName "h"
  h3 <- newVName "h"
  h4 <- newVName "h"
  x1 <- newVName "x"
  y1 <- newVName "y"
  c <- newVName "h"
  pure
    [ Rule
        { name = "Extend sum lower bound",
          from =
            scale c (LinComb i (hole h1) (hole h2) (Idx (Hole h3) (sVar i)))
              .+. scale c (Idx (Hole h3) (hole h4)),
          to = \s ->
            sub s $
              scale c $
                LinComb i (hole h4) (hole h2) (Idx (Hole h3) (sVar i)),
          sideCondition = \s -> do
            let lb = rep s (hole h1)
            let idx = rep s (hole h4)
            lb $==$ (idx .+. int 1)
        },
      Rule
        { name = "Extend sum upper bound",
          from =
            scale c (LinComb i (hole h1) (hole h2) (Idx (Hole h3) (sVar i)))
              .+. scale c (Idx (Hole h3) (hole h4)),
          to = \s ->
            sub s $
              scale c $
                LinComb i (hole h1) (hole h4) (Idx (Hole h3) (sVar i)),
          sideCondition = \s -> do
            let ub = rep s (hole h2)
            let idx = rep s (hole h4)
            ub $==$ (idx .-. int 1)
        },
      -- TODO Change indicator to be a property in order to not duplicate rules
      -- or have to introduce context holes?
      Rule
        { name = "Extend sum lower bound (indicator)",
          from =
            scale c (LinComb i (hole h1) (hole h2) (Indicator $ Idx (Hole h3) (sVar i)))
              .+. scale c (Indicator (Idx (Hole h3) (hole h4))),
          to = \s ->
            sub s $
              scale c $
                LinComb i (hole h4) (hole h2) (Indicator $ Idx (Hole h3) (sVar i)),
          sideCondition = \s -> do
            let lb = rep s (hole h1)
            let idx = rep s (hole h4)
            lb $==$ (idx .+. int 1)
        },
      Rule
        { name = "Extend sum upper bound (indicator)",
          from =
            scale c (LinComb i (hole h1) (hole h2) (Indicator $ Idx (Hole h3) (sVar i)))
              .+. scale c (Indicator (Idx (Hole h3) (hole h4))),
          to = \s ->
            sub s $
              scale c $
                LinComb i (hole h1) (hole h4) (Indicator $ Idx (Hole h3) (sVar i)),
          sideCondition = \s -> do
            let ub = rep s (hole h2)
            let idx = rep s (hole h4)
            ub $==$ (idx .-. int 1)
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
      Rule
        { name = "Replace sum over one element sequence by element",
          from = sym2SoP $ LinComb i (hole h1) (hole h2) (Hole h3),
          to = \s -> do
            j <- fromJust . getLinCombBoundVar <$>
               renameWith (vns s) (LinComb i (hole h1) (hole h2) (Hole h3))
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

-- TODO can all of these be handled by `normalize`? If so, remove.
rulesSymbol :: IndexFnM [Rule Symbol Symbol IndexFnM]
rulesSymbol = do
  pure
    []

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
