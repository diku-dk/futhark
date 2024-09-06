module Futhark.Analysis.Proofs.Rewrite
where

import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (SoP, sym2SoP, (.+.), int2SoP, (.-.), sopToList, sopFromList, numTerms)
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol (Symbol(..), normalizeSymbol)
import Control.Monad (foldM, msum, (<=<))
import Futhark.SoP.FourierMotzkin (($<=$))
import Futhark.Analysis.Proofs.IndexFn (IndexFnM, IndexFn (..), cases, Iterator (..), Domain (..), subIndexFn, repIteratorInBody)
import Data.List (subsequences, (\\))
import Futhark.Analysis.Proofs.Traversals (ASTMapper(..), astMap)
import Futhark.Analysis.Proofs.Refine (refineSymbol)
import Futhark.SoP.Monad (substEquivs)
import Data.Functor ((<&>))
import Language.Futhark (VName)

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

match :: Unify u v m => Rule u v m -> u -> m (Maybe (Substitution v))
match rule x = unify (from rule) x >>= checkSideCondition
  where
    checkSideCondition Nothing = pure Nothing
    checkSideCondition (Just s) = do
      b <- sideCondition rule s
      pure $ if b then Just s else Nothing

class Monad m => Rewritable u m where
  rewrite :: u -> m u

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = astMap m <=< substEquivs
    where
      m = ASTMapper
        { mapOnSymbol = rewrite,
          mapOnSoP = \sop -> rulesSoP >>= foldM (flip matchSoP) sop
        }

instance Rewritable Symbol IndexFnM where
  rewrite = astMap m
    where
      m = ASTMapper
        { mapOnSoP = rewrite,
          mapOnSymbol = \x -> do
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
  rewrite indexfn = rulesIndexFn >>= foldM (flip match_) indexfn
    where
      match_ rule fn = match rule fn >>= maybe (pure indexfn) (to rule)

-- Apply SoP-rule with k terms to all matching k-subterms in a SoP.
-- For example, given rule `x + x => 2x` and SoP `a + b + c + a + b`,
-- it matches `a + a` and `b + b` and returns `2a + 2b + c`.
matchSoP :: ( Replaceable u (SoP u)
            , Unify u (SoP u) m
            , Ord u) => Rule (SoP u) (SoP u) m -> SoP u -> m (SoP u)
matchSoP rule sop
  | numTerms (from rule) <= numTerms sop = do
    let (subterms, contexts) = unzip . combinations $ sopToList sop
    -- Get first valid subterm substitution. Recursively match context.
    subs <- mapM (match rule . sopFromList) subterms
    case msum $ zipWith (\x y -> (,y) <$> x) subs contexts of
      Just (s, ctx) -> (.+.) <$> matchSoP rule (sopFromList ctx) <*> to rule s
      Nothing -> pure sop
  | otherwise = pure sop
  where
    -- Get all (k-subterms, remaining subterms).
    k = numTerms (from rule)
    combinations xs = [(s, xs \\ s) | s <- subsequences xs, length s == k]

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
        { name = "Extend sum lower bound (1)"
        , from = LinComb i (hole h1 .+. int 1) (hole h2) (Hole h3)
                   ~+~ Idx (Hole h3) (hole h1)
        , to = \s -> sub s $ LinComb i (hole h1) (hole h2) (Hole h3)
        , sideCondition = vacuous
        }
    , Rule
        { name = "Extend sum lower bound (2)"
        , from = LinComb i (hole h1) (hole h2) (Hole h3)
                   ~+~ Idx (Hole h3) (hole h1 .-. int 1)
        , to = \s -> sub s $
                  LinComb i (hole h1 .-. int 1) (hole h2) (Hole h3)
        , sideCondition = vacuous
        }
    , Rule
        { name = "Extend sum upper bound (1)"
        , from = LinComb i (hole h1) (hole h2 .-. int 1) (Hole h3)
                   ~+~ Idx (Hole h3) (hole h2)
        , to = \s -> sub s $ LinComb i (hole h1) (hole h2) (Hole h3)
        , sideCondition = vacuous
        }
    , Rule
        { name = "Extend sum upper bound (2)"
        , from = LinComb i (hole h1) (hole h2) (Hole h3)
                   ~+~ Idx (Hole h3) (hole h2 .+. int 1)
        , to = \s -> sub s $
                  LinComb i (hole h1) (hole h2 .+. int 1) (Hole h3)
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
            rep s y' $<=$ rep s x'
        }
    , Rule
        { name = "[[¬x]] => 1 - [[x]]"
        , from = sym2SoP $ Indicator (Not (Hole h1))
        , to = \s -> sub s $ int 1 .-. sym2SoP (Indicator (Hole h1))
        , sideCondition = vacuous
        }
    ]
  where
    a ~+~ b = sym2SoP a .+. sym2SoP b
    a ~-~ b = sym2SoP a .-. sym2SoP b

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
  pure
    [ Rule
        { name = "Rule 5 (carry)",
          -- y = ∀i ∈ [0, 1, ..., n - 1] .
          --    | i == b => e1
          --    | i /= b => y[i-1]
          -- _______________________________________________________________
          -- y = ∀i ∈ [0, 1, ..., n - 1] . {i->b}e1
          from =
            IndexFn {
              iterator = Forall i (Iota (hole n)),
              body = cases [(hole i :== int 0, hole h1),
                            (hole i :/= int 0, sym2SoP Recurrence)]
            },
          -- TODO add bound names (i) are not substituted test for Unify
          -- on index fns
          -- Indexing variable i replaced by 0 in e1.
          to = \s -> repIteratorInBody (int 0) <$> (subIndexFn s $
            IndexFn {
              iterator = Forall i (Iota (hole n)),
              body = cases [(Bool True, hole h1)]
            }),
          sideCondition = vacuous
        }
    -- , Rule
    --     { name = "Rule 5 (carry)",
    --       -- y = ∀i ∈ ⊎k=iota m [b, b+1, ..., b + n - 1] .
    --       --    | i == b => e1
    --       --    | i /= b => y[i-1]
    --       -- _______________________________________________________________
    --       -- y = ∀i ∈ ⊎k=iota m [b, b+1, ..., b + n - 1] . {i->b}e1
    --       --
    --       -- Note that b may depend on k.
    --       from =
    --         IndexFn {
    --           iterator = Forall i (Cat k (hole m) (hole b)),
    --           body = cases [(hole i :== int 0, hole h1),
    --                         (hole i :/= int 0, sym2SoP Recurrence)]
    --         },
    --       -- Indexing variable i replaced by b in e1.
    --       to = \s -> repIteratorInBody <$> sub s (hole b) <*> (subIndexFn s $
    --         IndexFn {
    --           iterator = Forall i (Cat k (hole m) (hole b)),
    --           body = cases [(Bool True, hole h1)]
    --         }),
    --       sideCondition = vacuous
    --     }
    ]
    -- rewriteCarry (IndexFn it@(Forall i'' dom) (Cases cases))
    --   | (Var i :== b, c) :| [(Var i' :/= b', Recurrence)] <- cases,
    --     i == i',
    --     i == i'',
    --     b == b',
    --     b == domainSegStart dom,
    --     i `notMember` freeIn c = do
    --       tell ["Using carry rule"]
    --       pure $ IndexFn it (toCases c)

    -- TODO Simplify Rule 3 is maybe a good example of something
    --      where exact matching is easier than unification
    --      (that is, unification will match any index function here anyway).
    -- [ Rule
    --     { name = "Simplify rule 3",
    --       from = IndexFn (Hole iter) (hole h1),
    --       to = undefined
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

