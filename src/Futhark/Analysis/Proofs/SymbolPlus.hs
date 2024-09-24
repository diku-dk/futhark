{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.SymbolPlus where

import Data.Map qualified as M
import Data.Set qualified as S
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify (FreeVariables (fv), Hole (justHole), Renameable (rename_), Replaceable (rep), Substitution (..), SubstitutionBuilder (..), Unify (..), freshName, unifies_)
import Futhark.MonadFreshNames (MonadFreshNames)
import Futhark.SoP.SoP (sym2SoP)
import Language.Futhark (VName)

getRenamedLinCombBoundVar :: MonadFreshNames f => Substitution u -> Symbol -> f (Maybe VName)
getRenamedLinCombBoundVar s x = getLinCombBoundVar <$> rename_ (vns s) mempty x

instance FreeVariables Symbol where
  fv sym = case sym of
    Var vn -> fv vn
    Hole vn -> fv vn
    Idx xs i -> fv xs <> fv i
    LinComb i lb ub x -> fv lb <> fv ub <> fv x S.\\ S.singleton i
    Indicator x -> fv x
    Bool _ -> mempty
    Not x -> fv x
    x :< y -> fv x <> fv y
    x :<= y -> fv x <> fv y
    x :> y -> fv x <> fv y
    x :>= y -> fv x <> fv y
    x :== y -> fv x <> fv y
    x :/= y -> fv x <> fv y
    x :&& y -> fv x <> fv y
    x :|| y -> fv x <> fv y
    Recurrence -> mempty

instance Renameable Symbol where
  rename_ vns tau sym = case sym of
    Var x -> Var <$> rename_ vns tau x
    Hole x -> do
      x' <- rename_ vns tau x
      -- The Hole might be a quantifier index.
      pure $ if x' /= x then Var x' else Hole x'
    Idx xs i -> Idx <$> rename_ vns tau xs <*> rename_ vns tau i
    LinComb xn lb ub e -> do
      (xm, vns') <- freshName vns
      let tau' = M.insert xn xm tau
      LinComb xm <$> rename_ vns' tau' lb <*> rename_ vns' tau' ub <*> rename_ vns' tau' e
    Indicator x -> Indicator <$> rename_ vns tau x
    Bool x -> pure $ Bool x
    Not x -> Not <$> rename_ vns tau x
    x :< y -> f (:<) x y
    x :<= y -> f (:<=) x y
    x :> y -> f (:>) x y
    x :>= y -> f (:>=) x y
    x :== y -> f (:==) x y
    x :/= y -> f (:/=) x y
    x :&& y -> f (:&&) x y
    x :|| y -> f (:||) x y
    Recurrence -> pure Recurrence
    where
      f op x y = op <$> rename_ vns tau x <*> rename_ vns tau y

instance SubstitutionBuilder Symbol Symbol where
  addSub vn e s = s {sop = M.insert vn (sym2SoP e) $ sop s}

instance Replaceable Symbol Symbol where
  -- TODO flatten
  rep s symbol = case symbol of
    Var x -> M.findWithDefault (sym2SoP $ Var x) x (sop s)
    Hole x -> M.findWithDefault (sym2SoP $ Hole x) x (sop s)
    Idx xs i ->
      sym2SoP $ Idx (sop2Symbol $ rep s xs) (rep s i)
    LinComb i lb ub t ->
      -- NOTE we can avoid this rewrite here if we change the LinComb expression
      -- from Symbol to SoP Symbol.
      let s' = addSub i (Var i) s
       in applyLinCombRule i (rep s' lb) (rep s' ub) (rep s' t)
    Indicator e -> sym2SoP . Indicator . sop2Symbol $ rep s e
    Bool x -> sym2SoP $ Bool x
    Not x -> sym2SoP . Not . sop2Symbol $ rep s x
    x :< y -> f (:<) x y
    x :<= y -> f (:<=) x y
    x :> y -> f (:>) x y
    x :>= y -> f (:>=) x y
    x :== y -> f (:==) x y
    x :/= y -> f (:/=) x y
    x :&& y -> g (:&&) x y
    x :|| y -> g (:||) x y
    Recurrence -> sym2SoP Recurrence
    where
      f op x y = sym2SoP $ rep s x `op` rep s y
      g op x y = sym2SoP $ sop2Symbol (rep s x) `op` sop2Symbol (rep s y)

instance Hole Symbol where
  justHole (Hole x) = Just x
  justHole _ = Nothing

-- NOTE 2.b.iii says "if x occurs in some other equation",
-- but we don't have access to other equations here.
-- I reckon, we can always just do this substitution
-- at the call-site regardless.
-- Further, they keep (Var x := t) in the equations, but
-- that's relegated to the substitution here.
-- NOTE 3.a irrelevant here given that we are post type checking?
instance Unify Symbol Symbol where
  -- unify_ _ x y | trace ("\nunify_ " <> unwords (map prettyString [x, y])) False = undefined
  -- TODO I don't think we want exchange since unify is used to check whether
  --      the holes (FVs) in the first argument can be substituted to be
  --      syntactically identical to the second argument---not the other way
  --      around. (I.e., unify should not be commutative.)
  -- 1. Exchange.
  -- unify_ k t (Var x) | not (isVar t) =
  --   unify_ k (Var x) t
  --   where
  --     isVar (Var _) = True
  --     isVar _ = False
  -- 2.
  unify_ _ _ (Hole _) = error "Holes are not allowed in the second argument!"
  unify_ _ (Var x) t | Var x == t = pure mempty
  -- XXX Are the checks on bound vars redundant with holes?
  unify_ k (Hole x) t
    | x >= k = fail "2.b.i"
    | x `S.member` fvs || any (>= k) fvs = fail "2.b.ii"
    | otherwise = pure $ addSub x t mempty -- 2.b.iii. Variable elimination.
    where
      fvs = fv t
  -- 3.b
  unify_ k (LinComb _ a1 b1 e1) (LinComb _ a2 b2 e2) = do
    s1 <- unify_ k a1 a2
    s2 <- unify_ k (rep s1 b1) (rep s1 b2)
    s3 <- unify_ k (rep (s1 <> s2) e1) (rep (s1 <> s2) e2)
    pure $ s1 <> s2 <> s3
  unify_ k (Idx xs i) (Idx ys j) = do
    s <- unify_ k xs ys
    (s <>) <$> unify_ k (rep s i) (rep s j)
  unify_ k (Indicator x) (Indicator y) = unify_ k x y
  unify_ _ (Bool x) (Bool y) | x == y = pure mempty
  unify_ k (Not x) (Not y) = unify_ k x y
  unify_ _ Recurrence Recurrence = pure mempty
  unify_ k a b = case (a, b) of
    (x1 :< y1, x2 :< y2) -> unifies_ k [(x1, x2), (y1, y2)]
    (x1 :<= y1, x2 :<= y2) -> unifies_ k [(x1, x2), (y1, y2)]
    (x1 :> y1, x2 :> y2) -> unifies_ k [(x1, x2), (y1, y2)]
    (x1 :>= y1, x2 :>= y2) -> unifies_ k [(x1, x2), (y1, y2)]
    (x1 :== y1, x2 :== y2) -> unifies_ k [(x1, x2), (y1, y2)]
    (x1 :/= y1, x2 :/= y2) -> unifies_ k [(x1, x2), (y1, y2)]
    (x1 :&& y1, x2 :&& y2) -> unifies_ k [(x1, x2), (y1, y2)]
    (x1 :|| y1, x2 :|| y2) -> unifies_ k [(x1, x2), (y1, y2)]
    _ -> fail "Incompatible"
