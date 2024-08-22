module Futhark.Analysis.Proofs.Term
where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Language.Futhark (VName (VName))
import Futhark.Analysis.Proofs.Match (FreeVariables(fv), Renameable(rename_), Unify(..), Constraint(..), Nameable (..), SubstitutionBuilder (addSub), Replaceable (rep))
import Futhark.SoP.SoP (SoP, sym2SoP, Substitute(..), justSym, sopToLists, scaleSoP, (.-.), (.+.), int2SoP)
import Futhark.MonadFreshNames

data Term =
    Var VName
  | Sum
      VName        -- binder
      (SoP Term)   -- lower bound
      (SoP Term)   -- upper bound
      Term
  | Idx
      Term         -- array
      (SoP Term)   -- index
  | Recurrence
  deriving (Show, Eq, Ord)

instance FreeVariables Term where
  fv (Var vn) = S.singleton vn
  fv (Sum i lb ub e) = fv lb <> fv ub <> fv e S.\\ S.singleton i
  fv (Idx xs i) = fv xs <> fv i
  fv Recurrence = mempty

instance Substitute VName (SoP Term) Term where
  substitute _ = undefined

instance Nameable Term where
  mkName (VNameSource i) = (Var $ VName "x" i, VNameSource $ i + 1)

instance Renameable Term where
  rename_ tau (Var x) =
    pure $ Var $ M.findWithDefault x x tau
  rename_ tau (Idx xs i) =
    Idx <$> rename_ tau xs <*> rename_ tau i
  rename_ tau (Sum xn lb ub e) = do
    xm <- newNameFromString "i"
    let tau' = M.insert xn xm tau
    Sum xm <$> rename_ tau' lb <*> rename_ tau' ub <*> rename_ tau e
  rename_ _ Recurrence =
    pure Recurrence

instance SubstitutionBuilder Term Term where
  addSub vn e = M.insert vn (sym2SoP e)

sop2Term :: Ord u => SoP u -> u
sop2Term sop
  | Just t <- justSym sop = t
  | otherwise = error "sop2Term on something that is not a symbol"

instance Replaceable Term Term where
  -- TODO flatten
  rep s (Var x) = M.findWithDefault (sym2SoP $ Var x) x s
  rep s (Idx xs i) =
    sym2SoP $ Idx (sop2Term $ rep s xs) (rep s i)
  rep s (Sum i lb ub e) =
    -- NOTE we can avoid this rewrite here if we change the Sum expression
    -- from Term to SoP Term.
    let s' = addSub i (Var i) s
    in applySumRule i (rep s' lb) (rep s' ub) (rep s' e)
    where
      applySumRule j a b = foldl1 (.+.) . map (mkSum j a b) . sopToLists
      mkSum _ _ _ ([], c) =
        scaleSoP c (ub .-. lb .+. int2SoP 1)
      mkSum j a b ([t], c) = -- ([Sum i lb ub e], c)
        scaleSoP c (sym2SoP $ Sum j a b t)
      mkSum _ _ _ _ = error "Sum is not a linear combination."
  rep _ Recurrence = sym2SoP Recurrence

-- instance Replaceable (SoP Term) where
--   rep _ _ = undefined

-- NOTE 2.b.iii says "if x occurs in some other equation",
-- but we don't have access to other equations here.
-- I reckon, we can always just do this substitution
-- at the call-site regardless.
-- Further, they keep (Var x := t) in the equations, but
-- that's relegated to the substitution here.
-- NOTE 3.a irrelevant here given that we are post type checking?
instance (MonadFreshNames m, MonadFail m) => Unify Term Term m where
  -- 1. Exchange.
  unify_ k (t := Var x) | not (isVar t) =
    unify_ k (Var x := t)
    where
      isVar (Var _) = True
      isVar _ = False
  -- 2.a. Equation elimination.
  unify_ _ (Var x := t) | Var x == t =
      pure mempty
  -- 2.b.
  unify_ k (Var x := t) | Var x /= t && x >= k =
    fail "2.b.i"
  unify_ k (Var x := t)
    | Var x /= t =
    case fv t of
      fvs | x `S.member` fvs || any (>= k) fvs -> fail "2.b.ii"
      -- 2.b.iii. Variable elimination. 
      _ -> pure $ addSub x t mempty
  -- 3.b
  unify_ k (Sum _ a1 b1 e1 := Sum _ a2 b2 e2) = do
    s1 <- unify_ k (a1 := a2)
    s2 <- unify_ k (rep s1 b1 := rep s1 b2)
    s3 <- unify_ k (rep (s1 <> s2) e1 := rep (s1 <> s2) e2)
    pure $ s1 <> s2 <> s3
  unify_ k (Idx xs i := Idx ys j) = do
    s <- unify_ k (xs := ys)
    (s <>) <$> unify_ k (rep s i := rep s j)
  unify_ _ _ =
    pure mempty
