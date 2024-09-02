module Futhark.Analysis.Proofs.Symbol
where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Language.Futhark (VName)
import Futhark.Analysis.Proofs.Unify (FreeVariables(fv), Renameable(rename_), Unify(..), SubstitutionBuilder (addSub), Replaceable (rep), repRel)
import Futhark.SoP.SoP (SoP, sym2SoP, justSym, sopToLists, scaleSoP, (.-.), (.+.), int2SoP, Rel (..))
import Futhark.MonadFreshNames
import Debug.Trace (trace)
import Futhark.Util.Pretty (Pretty, pretty, parens, brackets, (<+>), prettyString, enclose)

data Symbol =
    Var VName
  | Idx
      Symbol         -- array
      (SoP Symbol)   -- index
  | LinComb
      VName          -- binder
      (SoP Symbol)   -- lower bound
      (SoP Symbol)   -- upper bound
      Symbol
  | Indicator (Rel Symbol)
  | Recurrence
  deriving (Show, Eq, Ord)

instance Pretty Symbol where
  pretty (Var x) = pretty x
  pretty (Idx (Var x) i) = pretty x <> brackets (pretty i)
  pretty (Idx x i) = parens (pretty x) <> brackets (pretty i)
  pretty (LinComb i lb ub e) =
    "∑"
      <> pretty i <> "∈"
      <> parens (pretty lb <+> ".." <+> pretty ub)
      <> autoParens e
    where
      autoParens x@(Var _) = pretty x
      autoParens x = parens (pretty x)
  pretty (Indicator p) = iversonbrackets (pretty p)
    where
      iversonbrackets = enclose "⟦" "⟧"
  pretty Recurrence = "↺ "

instance FreeVariables Symbol where
  fv sym = case sym of
    Var vn -> fv vn
    Idx xs i -> fv xs <> fv i
    LinComb i lb ub x -> fv lb <> fv ub <> fv x S.\\ S.singleton i
    Indicator x -> fv x
    Recurrence -> mempty

-- instance Nameable Symbol where
--   mkName (VNameSource i) = (Var $ VName "x" i, VNameSource $ i + 1)

instance Renameable Symbol where
  rename_ tau sym = case sym of
    Var x -> Var <$> rename_ tau x
    Idx xs i -> Idx <$> rename_ tau xs <*> rename_ tau i
    LinComb xn lb ub e -> do
      xm <- newNameFromString "i"
      let tau' = M.insert xn xm tau
      LinComb xm <$> rename_ tau' lb <*> rename_ tau' ub <*> rename_ tau' e
    Indicator x -> Indicator <$> rename_ tau x
    Recurrence -> pure Recurrence

instance SubstitutionBuilder Symbol (SoP Symbol) where
  addSub vn e = M.insert vn (sym2SoP e)

sop2Symbol :: Ord u => SoP u -> u
sop2Symbol sop
  | Just t <- justSym sop = t
  | otherwise = error "sop2Symbol on something that is not a symbol"

instance Replaceable Symbol (SoP Symbol) where
  -- TODO flatten
  rep s (Var x) =
    let y = M.findWithDefault (sym2SoP $ Var x) x s
    in trace (if x `M.member` s then "rep <" <> prettyString x <> "," <> prettyString y <> ">" else "") y
  rep s (Idx xs i) =
    sym2SoP $ Idx (sop2Symbol $ rep s xs) (rep s i)
  rep s (LinComb i lb ub t) =
    -- NOTE we can avoid this rewrite here if we change the LinComb expression
    -- from Symbol to SoP Symbol.
    let s' = addSub i (Var i) s
    in applyLinCombRule (rep s' lb) (rep s' ub) (rep s' t)
    where
      applyLinCombRule a b = foldl1 (.+.) . map (mkLinComb a b) . sopToLists
      mkLinComb _ _ ([], c) =
        scaleSoP c (ub .-. lb .+. int2SoP 1)
      mkLinComb a b ([u], c) =
        scaleSoP c (sym2SoP $ LinComb i a b u)
      mkLinComb _ _ _ =
        error "Replacement is not a linear combination."
  rep s (Indicator e) = sym2SoP $ Indicator (repRel s e)
  rep _ Recurrence = sym2SoP Recurrence


-- NOTE 2.b.iii says "if x occurs in some other equation",
-- but we don't have access to other equations here.
-- I reckon, we can always just do this substitution
-- at the call-site regardless.
-- Further, they keep (Var x := t) in the equations, but
-- that's relegated to the substitution here.
-- NOTE 3.a irrelevant here given that we are post type checking?
instance MonadFreshNames m => Unify Symbol (SoP Symbol) m where
  -- unify_ _ x y | trace ("\nunify_ " <> unwords (map show [x, y])) False = undefined
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
  unify_ k (Var x) t
    | Var x == t = pure mempty -- 2.a. Equation (constraint) elimination.
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
  unify_ _ (LinComb {}) _ =
    fail "Incompatible"
  unify_ _ (Idx {}) _ =
    fail "Incompatible"
  unify_ _ _ _ =
    pure mempty
