{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Properties.SymbolPlus (toSumOfSums, repVName, repProperty) where

import Data.Map qualified as M
import Data.Set qualified as S
import Futhark.Analysis.Properties.Property (Property (..))
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.Unify (FreeVariables (fv), Hole (justHole), Renameable (rename_), Rep (..), Replacement, ReplacementBuilder (..), Unify (..), freshName, repPredicate, repTuple, unifies_)
import Futhark.SoP.SoP (SoP, int2SoP, scaleSoP, sopToLists, sym2SoP, (.+.), (.-.))
import Language.Futhark (VName)

-- Given iterator, lower bound, upper bound and a SoP, create
-- a linear combination of sums.
toSumOfSums :: VName -> SoP Symbol -> SoP Symbol -> SoP Symbol -> SoP Symbol
toSumOfSums i lb ub = foldl1 (.+.) . map (mkSum lb ub) . sopToLists
  where
    mkSum a b ([], c) =
      scaleSoP c (b .-. a .+. int2SoP 1)
    mkSum a b ([u], c) =
      scaleSoP c (sym2SoP $ Sum i a b u)
    mkSum _ _ _ =
      error "SoP is not a linear combination."

instance FreeVariables Symbol where
  fv sym = case sym of
    Var vn -> fv vn
    Hole vn -> fv vn
    Sum i lb ub x -> fv lb <> fv ub <> fv x S.\\ S.singleton i
    Apply f xs -> fv f <> mconcat (map fv xs)
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
    Prop p -> fv p

instance Renameable Symbol where
  rename_ vns tau sym = case sym of
    Var x -> Var <$> rename_ vns tau x
    Hole x -> do
      x' <- rename_ vns tau x
      -- The Hole might be a quantifier index.
      pure $ if x' /= x then Var x' else Hole x'
    Sum xn lb ub e -> do
      (xm, vns') <- freshName vns
      let tau' = M.insert xn xm tau
      Sum xm <$> rename_ vns' tau' lb <*> rename_ vns' tau' ub <*> rename_ vns' tau' e
    Apply f xs -> Apply <$> rename_ vns tau f <*> rename_ vns tau xs
    Bool x -> pure $ Bool x
    Not x -> neg <$> rename_ vns tau x
    x :< y -> g (:<) x y
    x :<= y -> g (:<=) x y
    x :> y -> g (:>) x y
    x :>= y -> g (:>=) x y
    x :== y -> g (:==) x y
    x :/= y -> g (:/=) x y
    x :&& y -> g (:&&) x y
    x :|| y -> g (:||) x y
    Recurrence -> pure Recurrence
    Prop p -> Prop <$> rename_ vns tau p
    where
      g op x y = op <$> rename_ vns tau x <*> rename_ vns tau y

instance ReplacementBuilder Symbol Symbol where
  addRep vn e = M.insert vn (sym2SoP e)

repVName :: Replacement Symbol -> VName -> VName
repVName s vn
  | Var i <- sop2Symbol $ rep s (Var vn) =
      i
repVName _ _ = error "repVName substitutes for non-VName."

instance Rep Symbol Symbol where
  rep s symbol = case symbol of
    Var x -> M.findWithDefault (sym2SoP $ Var x) x s
    Hole x -> M.findWithDefault (sym2SoP $ Hole x) x s
    Sum i lb ub t ->
      -- NOTE we can avoid this rewrite here if we change the Sum expression
      -- from Symbol to SoP Symbol.
      let s' = addRep i (Var i) s
       in toSumOfSums i (rep s' lb) (rep s' ub) (rep s' t)
    Apply f xs -> sym2SoP $ Apply (sop2Symbol $ rep s f) (map (rep s) xs)
    Bool x -> sym2SoP $ Bool x
    Not x -> sym2SoP . neg . sop2Symbol $ rep s x
    x :< y -> binop (:<) x y
    x :<= y -> binop (:<=) x y
    x :> y -> binop (:>) x y
    x :>= y -> binop (:>=) x y
    x :== y -> binop (:==) x y
    x :/= y -> binop (:/=) x y
    x :&& y -> binopS (:&&) x y
    x :|| y -> binopS (:||) x y
    Recurrence -> sym2SoP Recurrence
    Prop p -> sym2SoP $ Prop (repProperty s p)
    where
      binop op x y = sym2SoP $ rep s x `op` rep s y
      binopS op x y = sym2SoP $ sop2Symbol (rep s x) `op` sop2Symbol (rep s y)

  repSelf s symbol = sop2Symbol $ rep s symbol

repProperty :: Replacement Symbol -> Property Symbol -> Property Symbol
repProperty _ Boolean = Boolean
repProperty s (Disjoint x) = Disjoint $ S.map (repVName s) x
repProperty s (Monotonic x dir) = Monotonic (repVName s x) dir
repProperty s (Rng x rng) =
  Rng (repVName s x) (repTuple s rng)
repProperty s (Injective x (Just rcd)) =
  Injective (repVName s x) (Just $ repTuple s rcd)
repProperty s (Injective x Nothing) =
  Injective (repVName s x) Nothing
repProperty s (BijectiveRCD x rcd img) =
  BijectiveRCD (repVName s x) (repTuple s rcd) (repTuple s img)
repProperty s (FiltPartInv x pf pps) =
  FiltPartInv (repVName s x) (repPredicate s pf) (map (repPredicate s) pps)
repProperty s (FiltPart y x pf pps) =
  FiltPart (repVName s y) (repVName s x) (repPredicate s pf) (map (repPredicate s) pps)

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
  unify_ k (Var x) (Var y) = unify_ k x y
  -- XXX Are the checks on bound vars redundant with holes?
  unify_ k (Hole x) t
    | x >= k = error "2.b.i"
    | x `S.member` fvs || any (>= k) fvs = error "2.b.ii"
    | otherwise = pure $ addRep x t mempty -- 2.b.iii. Variable elimination.
    where
      fvs = fv t
  -- 3.b
  unify_ k (Sum _ a1 b1 e1) (Sum _ a2 b2 e2) = do
    s1 <- unify_ k a1 a2
    s2 <- unify_ k (rep s1 b1) (rep s1 b2)
    s3 <- unify_ k (rep (s1 <> s2) e1) (rep (s1 <> s2) e2)
    pure $ s1 <> s2 <> s3
  unify_ k (Apply f xs) (Apply g ys) = do
    s <- unifies_ k xs ys
    (s <>) <$> unify_ k (rep s f) (rep s g)
  unify_ _ (Bool x) (Bool y) | x == y = pure mempty
  unify_ k (Not x) (Not y) = unify_ k x y
  unify_ _ Recurrence Recurrence = pure mempty
  unify_ k (Prop p) (Prop q) = unify_ k p q
  unify_ k a b = case (a, b) of
    (x1 :< y1, x2 :< y2) -> unifies_ k [x1, y1] [x2, y2]
    (x1 :<= y1, x2 :<= y2) -> unifies_ k [x1, y1] [x2, y2]
    (x1 :> y1, x2 :> y2) -> unifies_ k [x1, y1] [x2, y2]
    (x1 :>= y1, x2 :>= y2) -> unifies_ k [x1, y1] [x2, y2]
    (x1 :== y1, x2 :== y2) -> unifies_ k [x1, y1] [x2, y2]
    (x1 :/= y1, x2 :/= y2) -> unifies_ k [x1, y1] [x2, y2]
    (x1 :&& y1, x2 :&& y2) -> unifies_ k [x1, y1] [x2, y2]
    (x1 :|| y1, x2 :|| y2) -> unifies_ k [x1, y1] [x2, y2]
    _ -> fail "Incompatible"
