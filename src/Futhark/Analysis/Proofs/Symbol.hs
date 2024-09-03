module Futhark.Analysis.Proofs.Symbol
where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Language.Futhark (VName)
import Futhark.Analysis.Proofs.Unify (FreeVariables(fv), Renameable(rename_), Unify(..), SubstitutionBuilder (addSub), Replaceable (rep), unifies)
import Futhark.SoP.SoP (SoP, sym2SoP, justSym, sopToLists, scaleSoP, (.-.), (.+.), int2SoP)
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
  | Indicator Symbol
  | Bool Bool
  | Not Symbol
  | SoP Symbol :< SoP Symbol
  | SoP Symbol :<= SoP Symbol
  | SoP Symbol :> SoP Symbol
  | SoP Symbol :>= SoP Symbol
  | SoP Symbol :== SoP Symbol
  | SoP Symbol :/= SoP Symbol
  | Symbol :&& Symbol
  | Symbol :|| Symbol
  | Recurrence
  deriving (Show, Eq, Ord)

toNNF :: Symbol -> Symbol
toNNF (Not (Not x)) = x
toNNF (Not (Bool True)) = Bool False
toNNF (Not (Bool False)) = Bool True
toNNF (Not (x :|| y)) = toNNF (Not x) :&& toNNF (Not y)
toNNF (Not (x :&& y)) = toNNF (Not x) :|| toNNF (Not y)
toNNF (Not (x :== y)) = x :/= y
toNNF (Not (x :< y)) = x :>= y
toNNF (Not (x :> y)) = x :<= y
toNNF (Not (x :/= y)) = x :== y
toNNF (Not (x :>= y)) = x :< y
toNNF (Not (x :<= y)) = x :> y
toNNF x = x

instance Pretty Symbol where
  pretty symbol = case symbol of
    (Var x) -> pretty x
    (Idx x i) -> autoParens x <> brackets (pretty i)
    (LinComb i lb ub e) ->
      "∑"
        <> pretty i <> "∈"
        <> parens (pretty lb <+> ".." <+> pretty ub)
        <> autoParens e
    Indicator p -> iversonbrackets (pretty p)
    Bool x -> pretty x
    Not x -> "¬" <> autoParens x
    x :< y -> prettyOp "<" x y
    x :<= y -> prettyOp "<=" x y
    x :> y -> prettyOp ">" x y
    x :>= y -> prettyOp ">=" x y
    x :== y -> prettyOp "==" x y
    x :/= y -> prettyOp "/=" x y
    x :&& y -> prettyOp "&&" x y
    x :|| y -> prettyOp "||" x y
    Recurrence -> "↺ "
    where
      autoParens x@(Var _) = pretty x
      autoParens x = parens (pretty x)
      iversonbrackets = enclose "⟦" "⟧"
      prettyOp s x y = pretty x <+> s <+> pretty y

instance FreeVariables Symbol where
  fv sym = case sym of
    Var vn -> fv vn
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
  rename_ tau sym = case sym of
    Var x -> Var <$> rename_ tau x
    Idx xs i -> Idx <$> rename_ tau xs <*> rename_ tau i
    LinComb xn lb ub e -> do
      xm <- newNameFromString "i"
      let tau' = M.insert xn xm tau
      LinComb xm <$> rename_ tau' lb <*> rename_ tau' ub <*> rename_ tau' e
    Indicator x -> Indicator <$> rename_ tau x
    Bool x -> pure $ Bool x
    Not x -> Not <$> rename_ tau x
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
      f op x y = op <$> rename_ tau x <*> rename_ tau y

instance SubstitutionBuilder Symbol (SoP Symbol) where
  addSub vn e = M.insert vn (sym2SoP e)

sop2Symbol :: Ord u => SoP u -> u
sop2Symbol sop
  | Just t <- justSym sop = t
  | otherwise = error "sop2Symbol on something that is not a symbol"

instance Replaceable Symbol (SoP Symbol) where
  -- TODO flatten
  rep s symbol = case symbol of
    Var x ->
      let y = M.findWithDefault (sym2SoP $ Var x) x s
      in trace (if x `M.member` s then "rep <" <> prettyString x <> "," <> prettyString y <> ">" else "") y
    Idx xs i ->
      sym2SoP $ Idx (sop2Symbol $ rep s xs) (rep s i)
    LinComb i lb ub t ->
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
  unify_ _ (Bool x) (Bool y) | x == y = pure mempty
  unify_ k (Not x) (Not y) = unify_ k x y
  unify_ _ Recurrence Recurrence = pure mempty
  unify_ k a b = case (a, b) of
    (x1 :< y1, x2 :< y2) -> unifies k [(x1, x2), (y1, y2)]
    (x1 :<= y1, x2 :<= y2) -> unifies k [(x1, x2), (y1, y2)]
    (x1 :> y1, x2 :> y2) -> unifies k [(x1, x2), (y1, y2)]
    (x1 :>= y1, x2 :>= y2) -> unifies k [(x1, x2), (y1, y2)]
    (x1 :== y1, x2 :== y2) -> unifies k [(x1, x2), (y1, y2)]
    (x1 :/= y1, x2 :/= y2) -> unifies k [(x1, x2), (y1, y2)]
    (x1 :&& y1, x2 :&& y2) -> unifies k [(x1, x2), (y1, y2)]
    (x1 :|| y1, x2 :|| y2) -> unifies k [(x1, x2), (y1, y2)]
    _ -> fail "Incompatible"
