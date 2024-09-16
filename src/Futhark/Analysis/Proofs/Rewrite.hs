module Futhark.Analysis.Proofs.Rewrite
where

import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (SoP, sym2SoP, (.+.), int2SoP, (.-.), sopToList, sopFromList, numTerms, term2SoP)
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol (Symbol(..), normalizeSymbol, applyLinCombRule)
import Control.Monad (foldM, msum, (<=<))
import Futhark.SoP.FourierMotzkin (($<=$))
import Futhark.Analysis.Proofs.IndexFn (IndexFnM, IndexFn (..), cases, Iterator (..), Domain (..), subIndexFn, repVName)
import Data.List (subsequences, (\\))
import Futhark.Analysis.Proofs.Traversals (ASTMapper(..), astMap)
import Futhark.Analysis.Proofs.Refine (refineSymbol)
import Futhark.SoP.Monad (substEquivs)
import Data.Functor ((<&>))
import Language.Futhark (VName)
import qualified Data.Map as M
import Futhark.Analysis.Proofs.Util (partitions)

data Rule a b m = Rule {
    name :: String,
    from :: a,
    to :: Substitution b -> m a,
    sideCondition :: Substitution b -> m Bool
  }

vacuous :: Monad m => b -> m Bool
vacuous = const (pure True)

int :: Integer -> SoP Symbol
int = int2SoP

sVar :: VName -> SoP Symbol
sVar = sym2SoP . Var

hole :: VName -> SoP Symbol
hole = sym2SoP . Hole

(~+~) :: Ord u => u -> u -> SoP u
a ~+~ b = sym2SoP a .+. sym2SoP b

(~-~) :: Ord u => u -> u -> SoP u
a ~-~ b = sym2SoP a .-. sym2SoP b

converge :: (Eq a, Monad m) => (a -> m a) -> a -> m a
converge f x = do
  y <- f x
  y2 <- f y
  if y == y2 then pure y
  else converge f y

match :: Unify u v m => Rule u v m -> u -> m (Maybe (Substitution v))
match rule x = unify (from rule) x >>= check (sideCondition rule)

check :: Monad f => (a -> f Bool) -> Maybe a -> f (Maybe a)
check _ Nothing = pure Nothing
check cond (Just s) = do
  b <- cond s
  pure $ if b then Just s else Nothing

class Monad m => Rewritable u m where
  rewrite :: u -> m u

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = astMap m <=< substEquivs
    where
      m = ASTMapper
        { mapOnSymbol = rewrite,
          mapOnSoP = converge $ \sop -> rulesSoP >>= foldM (flip matchSoP) sop
        }

instance Rewritable Symbol IndexFnM where
  rewrite = astMap m
    where
      m = ASTMapper
        { mapOnSoP = rewrite,
          mapOnSymbol = converge $ \x -> do
            rulesSymbol
              >>= foldM (flip match_) (normalizeSymbol x)
              >>= refineSymbol . normalizeSymbol
              <&> normalizeSymbol
        }

      match_ rule symbol = do
          s :: Maybe (Substitution (SoP Symbol)) <- case from rule of
            x :&& y -> matchCommutativeRule (:&&) x y
            x :|| y -> matchCommutativeRule (:||) x y
            _ -> match rule symbol
          maybe (pure symbol) (to rule) s
          where
            matchCommutativeRule op x y =
              msum <$> mapM (match rule) [x `op` y, y `op` x]

instance Rewritable IndexFn IndexFnM where
  rewrite = converge $ astMap m <=< \indexfn -> rulesIndexFn >>= foldM (flip match_) indexfn
    where
      m :: ASTMapper Symbol IndexFnM = ASTMapper
        { mapOnSoP = rewrite,
          mapOnSymbol = rewrite
        }

      match_ rule fn = match rule fn >>= maybe (pure fn) (to rule)

-- Apply SoP-rule with k terms to all matching k-subterms in a SoP.
-- For example, given rule `x + x => 2x` and SoP `a + b + c + a + b`,
-- it matches `a + a` and `b + b` and returns `2a + 2b + c`.
matchSoP :: ( Replaceable u (SoP u)
            , Unify u (SoP u) m
            , Hole u
            , Ord u) => Rule (SoP u) (SoP u) m -> SoP u -> m (SoP u)
matchSoP rule sop
  | numTerms (from rule) <= numTerms sop = do
    let (subterms, contexts) = unzip . splits $ sopToList sop
    -- Get first valid subterm substitution. Recursively match context.
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

combine :: Ord u => Rule (SoP u) (SoP u) m -> SoP u -> [[(SoP u, SoP u)]]
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

rulesSoP :: IndexFnM [Rule (SoP Symbol) (SoP Symbol) IndexFnM]
rulesSoP = do
  i <- newVName "i"
  h1 <- newVName "h"
  h2 <- newVName "h"
  h3 <- newVName "h"
  x1 <- newVName "x"
  y1 <- newVName "y"
  pure
    [ Rule
        -- NOTE that this will also match, e.g.,
        --    x[a - 1337] + LinComb i (a - 1336) b x[i]
        -- since this unifies with
        --    x[h1] + LinComb i (h1+1) b x[i]
        -- by substituting h1 for a - 1337.
        { name = "Extend sum lower bound"
        , from = LinComb i (hole h1 .+. int 1) (hole h2) (Idx (Hole h3) (sVar i))
                   ~+~ Idx (Hole h3) (hole h1)
        , to = \s -> sub s $
                  LinComb i (hole h1) (hole h2) (Idx (Hole h3) (sVar i))
        , sideCondition = vacuous
        }
    , Rule
        -- NOTE This matches similarly to "Extend sum lower bound".
        { name = "Extend sum upper bound"
        , from = LinComb i (hole h1) (hole h2 .-. int 1) (Idx (Hole h3) (sVar i))
                   ~+~ Idx (Hole h3) (hole h2)
        , to = \s -> sub s $ LinComb i (hole h1) (hole h2) (Idx (Hole h3) (sVar i))
        , sideCondition = vacuous
        }
    , Rule
        { name = "Merge sum-subtractation"
        , from = LinComb i (hole h1) (hole x1) (Hole h2)
                   ~-~ LinComb i (hole h1) (hole y1) (Hole h2)
        , to = \s ->
           sub s $ LinComb i (hole y1 .+. int 1) (hole x1) (Hole h2)
        , sideCondition = \s -> do
            y' <- sub s (Hole y1)
            x' <- sub s (Hole x1)
            y' $<=$ x'
        }
    , Rule
        { name = "[[¬x]] => 1 - [[x]]"
        , from = sym2SoP $ Indicator (Not (Hole h1))
        , to = \s -> sub s $ int 1 .-. sym2SoP (Indicator (Hole h1))
        , sideCondition = vacuous
        }
    ]

-- TODO can all of these be handled by `normalize`? If so, remove.
rulesSymbol :: IndexFnM [Rule Symbol (SoP Symbol) IndexFnM]
rulesSymbol = do
  pure
    []
    -- [ Rule
    --     { name = ":&& identity"
    --     , from = Bool True :&& Var h1
    --     , to = \s -> pure . sop2Symbol . rep s $ Var h1
    --     }
    -- , Rule
    --     { name = ":&& annihilation"
    --     , from = Bool False :&& Var h1
    --     , to = \_ -> pure $ Bool False
    --     }
    -- , Rule
    --     { name = ":|| identity"
    --     , from = Bool False :|| Var h1
    --     , to = \s -> pure . sop2Symbol . rep s $ Var h1
    --     }
    -- , Rule
    --     { name = ":|| annihilation"
    --     , from = Bool True :|| Var h1
    --     , to = \_ -> pure $ Bool True
    --     }
    -- ]

rulesIndexFn :: IndexFnM [Rule IndexFn (SoP Symbol) IndexFnM]
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
        { name = "Rule 5 (carry)"
          -- y = ∀i ∈ [0, 1, ..., n - 1] .
          --    | i == b => e1
          --    | i /= b => y[i-1]
          -- _______________________________________________________________
          -- y = ∀i ∈ [0, 1, ..., n - 1] . {i->b}e1
        , from =
            IndexFn {
              iterator = Forall i (Iota (hole n)),
              body = cases [(hole i :== int 0, hole h1),
                            (hole i :/= int 0, sym2SoP Recurrence)]
            }
          -- TODO add bound names (i) are not substituted test for Unify
          -- on index fns
          -- Indexing variable i replaced by 0 in e1.
        , to = \s -> subIndexFn s =<< do
            let i' = repVName s i
            e1 <- sub s (hole h1)
            e1_b <- sub (M.singleton i' (int 0)) e1
            pure $ IndexFn {
              iterator = Forall i (Iota (hole n)),
              body = cases [(Bool True, e1_b)]
            }
        , sideCondition = vacuous
        }
    , Rule
        { name = "Rule 5 (carry)"
          -- y = ∀i ∈ ⊎k=iota m [b, b+1, ..., b + n - 1] .
          --    | i == b => e1
          --    | i /= b => y[i-1]
          -- _______________________________________________________________
          -- y = ∀i ∈ ⊎k=iota m [b, b+1, ..., b + n - 1] . {i->b}e1
          --
          -- Note that b may depend on k.
        , from =
            IndexFn {
              iterator = Forall i (Cat k (hole m) (hole b)),
              body = cases [(hole i :== int 0, hole h1),
                            (hole i :/= int 0, sym2SoP Recurrence)]
            }
          -- Indexing variable i replaced by b in e1.
        , to = \s -> subIndexFn s =<< do
            let i' = repVName s i
            e1 <- sub s (hole h1)
            b' <- sub s (hole b)
            e1_b <- sub (M.singleton i' b') e1
            pure $ IndexFn {
              iterator = Forall i (Cat k (hole m) (hole b)),
              body = cases [(Bool True, e1_b)]
            }
        , sideCondition = vacuous
        }
    , Rule
        { name = "Rule 4 (prefix sum)"
          -- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
          --    | i == b => e1              (e1 may depend on i)
          --    | i /= b => y[i-1] + e2     (e2 may depend on i)
          --
          -- e2 is a SoP with terms e2_0, ..., e2_l. Each term is a constant,
          -- an indexing statement or an indicator of an indexing statement.
          -- XXX is this condition necessary in the revised system? dont we just want to disallow additional Recurrences?
          -- _______________________________________________________________
          -- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
          --    e1{b/i} + (Σ_{j=b+1}^i e2_0{j/i}) + ... + (Σ_{j=b+1}^i e2_l{j/i})
        , from =
            IndexFn {
              iterator = Forall i (Iota (hole n)),
              body = cases [(hole i :== int 0, hole h1),
                            (hole i :/= int 0, Recurrence ~+~ Hole h2)]
            }
          -- XXX asdf i think I can get e2 on the correct form by making a LinComb
          -- and replacing on it? Otherwise extract "mkLinComb" into a visible function.
        , to = \s -> do
            let i' = repVName s i
            e1 <- sub s (Hole h1)
            e1_b <- sub (M.singleton i' (int 0)) e1
            e2 <- sub s (Hole h2)
            j <- newVName "j"
            e2_j <- sub (M.singleton i' (sVar j)) e2
            let e2_sum = applyLinCombRule j (int 1) (hole i) e2_j
            subIndexFn s $
              IndexFn {
                iterator = Forall i (Iota (hole n)),
                body = cases [(Bool True, e1_b .+. e2_sum)]
              }
        , sideCondition = vacuous
        }

    -- TODO this and "remove dead cases" would match anything and
    --      are better implemented without unification?
    -- , Rule
    --     { name = "Simplify rule 3 (all cases are constants)"
    --     , from =
    --         IndexFn {
    --           iterator = Forall i (Iota (hole n)),
    --           body = CHole h1
    --         }
    --     , to = undefined
    --     , sideCondition = undefined
    --     }
    -- simplifyRule3 :: IndexFn -> IndexFnM IndexFn
    -- simplifyRule3 v@(IndexFn _ (Cases ((Bool True, _) NE.:| []))) = pure v
    -- simplifyRule3 (IndexFn it (Cases cases))
    --   | Just sops <- mapM (justConstant . snd) cases = do
    --     let preds = NE.map fst cases
    --         sumOfIndicators =
    --           SoP.normalize . foldl1 (.+.) . NE.toList $
    --             NE.zipWith
    --               (\p x -> sym2SoP (Indicator p) .*. int2SoP x)
    --               preds
    --               sops
    --     tell ["Using simplification rule: integer-valued cases"]
    --     pure $ IndexFn it $ Cases (NE.singleton (Bool True, SoP2 sumOfIndicators))
    --   where
    --     justConstant (SoP2 sop) = SoP.justConstant sop
    --     justConstant _ = Nothing
    ]
