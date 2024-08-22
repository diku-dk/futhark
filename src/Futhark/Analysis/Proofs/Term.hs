module Futhark.Analysis.Proofs.Term
where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Language.Futhark (VName (VName))
import Futhark.Analysis.Proofs.Match (FreeVariables(fv), Renameable(rename_), Unify(..), Constraint(..), Nameable (..))
import Futhark.SoP.SoP (SoP, sym2SoP, Substitute(..))
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
      _ -> pure $ M.singleton x (sym2SoP t)
  -- 3.b
  unify_ k (Sum _ a1 b1 e1 := Sum _ a2 b2 e2) = do
    -- TODO unsure if substitute should just be replace?
    s1 <- unify_ k (a1 := a2)
    s2 <- unify_ k (substitute s1 (b1 := b2))
    s3 <- unify_ k (substitute (s1 <> s2) (e1 := e2))
    pure $ mconcat [s1, s2, s3]
  unify_ k (Idx xs i := Idx ys j) = do
    s1 <- unify_ k (xs := ys)
    s2 <- unify_ k (substitute s1 (i := j))
    pure $ s1 <> s2
  unify_ _ _ =
    pure mempty
