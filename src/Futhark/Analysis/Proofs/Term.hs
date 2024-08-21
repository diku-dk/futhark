module Futhark.Analysis.Proofs.Term
where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Language.Futhark (VName)
import Futhark.Analysis.Proofs.Match (FreeIn(freeIn), Renameable(rename), Unify(..), Constraint(..), isBoundVar)
import Futhark.SoP.SoP (SoP, sym2SoP, Substitute(..))

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

instance FreeIn Term where
  freeIn (Var vn) = S.singleton vn
  freeIn (Sum _ lb ub e) = freeIn lb <> freeIn ub <> freeIn e
  freeIn (Idx xs i) = freeIn xs <> freeIn i
  freeIn Recurrence = mempty

instance Substitute VName (SoP Term) Term where
  substitute _ _ = undefined

instance Renameable Term where
  rename _ = undefined

instance Unify Term Term where
  unify_ _ (e1 := e2) | e1 == e2 =
    Just mempty
  -- 1. Exchange.
  unify_ k (t := Var x) | not (isVar t) =
    unify_ k (Var x := t)
    where
      isVar (Var _) = True
      isVar _ = False
  -- 2.a. Equation elimination.
  unify_ _ (Var x := t) | Var x == t =
      Just mempty
  -- 2.b.
  unify_ k (Var x := t) | Var x /= t && isBoundVar k x =
      fail "2.b.i"
  unify_ k (Var x := t)
    | Var x /= t =
    let fvs = freeIn t
    in if x `S.member` fvs || any (isBoundVar k) fvs then fail "2.b.ii"
       else Just $ M.singleton x (sym2SoP t) -- 2.b.iii. Variable elimination.
       -- NOTE 2.b.iii says "if x occurs in some other equation",
       -- but we don't have access to other equations here.
       -- I reckon, we can always just do this substitution
       -- at the call-site regardless.
       -- Further, they keep (Var x := t) in the equations, but
       -- that's relegated to the substitution here.
  -- 3.a irrelevant here given that we are post type checking?
  -- 3.b
  unify_ k (Sum _ a1 b1 e1 := Sum _ a2 b2 e2) = do
    -- foldM (\s c -> do
    --          s' <- unify_ k (substitute s c)
    --          Just $ s <> s'
    --       ) mempty [a1 := a2, b1 := b2]
    sub1 <- unify_ k (a1 := a2)
    sub2 <- unify_ k (substitute sub1 (b1 := b2))
    sub3 <- unify_ k (substitute (sub1 <> sub2) (e1 := e2))
    Just $ mconcat [sub1, sub2, sub3]
  unify_ _ (Idx xs i := Idx ys j) =
    undefined
  unify_ _ _ = undefined
