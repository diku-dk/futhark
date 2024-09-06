module Futhark.Analysis.Proofs.Symbol
where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Language.Futhark (VName(VName), baseTag, baseName)
import Futhark.Analysis.Proofs.Unify (FreeVariables(fv), Renameable(rename_), Unify(..), SubstitutionBuilder (addSub), Replaceable (rep), unifies)
import Futhark.SoP.SoP (SoP, sym2SoP, justSym, sopToLists, scaleSoP, (.-.), (.+.), int2SoP)
import Futhark.MonadFreshNames
import Futhark.Util.Pretty (Pretty, pretty, parens, brackets, (<+>), enclose)

data Symbol =
    Var VName
  | Hole VName
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
  deriving (Show, Eq)

-- This is only defined manually to not compare on the indexing variable in
-- LinComb, which is subject to renaming. SoP terms are sorted, so renaming
-- may lead to rearranging of terms, which in turn makes testing fragile.
-- TODO come up with a more succint solution.
instance Ord Symbol where
  compare (Var x1) (Var x2) = compare x1 x2
  compare (Hole x1) (Hole x2) = compare x1 x2
  compare (Idx s1 i1) (Idx s2 i2) = compare (s1, i1) (s2, i2)
  compare (LinComb _ l1 u1 s1) (LinComb _ l2 u2 s2) = compare (l1, u1, s1) (l2, u2, s2)
  compare (Indicator s1) (Indicator s2) = compare s1 s2
  compare (Bool b1) (Bool b2) = compare b1 b2
  compare (Not s1) (Not s2) = compare s1 s2
  compare (s1 :< s2) (s3 :< s4) = compare (s1, s2) (s3, s4)
  compare (s1 :<= s2) (s3 :<= s4) = compare (s1, s2) (s3, s4)
  compare (s1 :> s2) (s3 :> s4) = compare (s1, s2) (s3, s4)
  compare (s1 :>= s2) (s3 :>= s4) = compare (s1, s2) (s3, s4)
  compare (s1 :== s2) (s3 :== s4) = compare (s1, s2) (s3, s4)
  compare (s1 :/= s2) (s3 :/= s4) = compare (s1, s2) (s3, s4)
  compare (s1 :&& s2) (s3 :&& s4) = compare (s1, s2) (s3, s4)
  compare (s1 :|| s2) (s3 :|| s4) = compare (s1, s2) (s3, s4)
  compare Recurrence Recurrence = EQ
  compare (Var {}) _ = LT
  compare (Hole {}) (Var {}) = GT
  compare (Hole {}) _ = LT
  compare (Idx {}) (Var {}) = GT
  compare (Idx {}) (Hole {}) = GT
  compare (Idx {}) _ = LT
  compare (LinComb {}) (Var {}) = GT
  compare (LinComb {}) (Hole {}) = GT
  compare (LinComb {}) (Idx {}) = GT
  compare (LinComb {}) _ = LT
  compare (Indicator {}) (Var {}) = GT
  compare (Indicator {}) (Hole {}) = GT
  compare (Indicator {}) (Idx {}) = GT
  compare (Indicator {}) (LinComb {}) = GT
  compare (Indicator {}) _ = LT
  compare (Bool {}) (Var {}) = GT
  compare (Bool {}) (Hole {}) = GT
  compare (Bool {}) (Idx {}) = GT
  compare (Bool {}) (LinComb {}) = GT
  compare (Bool {}) (Indicator {}) = GT
  compare (Bool {}) _ = LT
  compare (Not {}) (Var {}) = GT
  compare (Not {}) (Hole {}) = GT
  compare (Not {}) (Idx {}) = GT
  compare (Not {}) (LinComb {}) = GT
  compare (Not {}) (Indicator {}) = GT
  compare (Not {}) (Bool {}) = GT
  compare (Not {}) _ = LT
  compare (_ :< _) (Var {}) = GT
  compare (_ :< _) (Hole {}) = GT
  compare (_ :< _) (Idx {}) = GT
  compare (_ :< _) (LinComb {}) = GT
  compare (_ :< _) (Indicator {}) = GT
  compare (_ :< _) (Bool {}) = GT
  compare (_ :< _) (Not _) = GT
  compare (_ :< _) _ = LT
  compare (_ :<= _) (Var {}) = GT
  compare (_ :<= _) (Hole {}) = GT
  compare (_ :<= _) (Idx {}) = GT
  compare (_ :<= _) (LinComb {}) = GT
  compare (_ :<= _) (Indicator {}) = GT
  compare (_ :<= _) (Bool {}) = GT
  compare (_ :<= _) (Not _) = GT
  compare (_ :<= _) (_ :< _) = GT
  compare (_ :<= _) _ = LT
  compare (_ :> _) (Var {}) = GT
  compare (_ :> _) (Hole {}) = GT
  compare (_ :> _) (Idx {}) = GT
  compare (_ :> _) (LinComb {}) = GT
  compare (_ :> _) (Indicator {}) = GT
  compare (_ :> _) (Bool {}) = GT
  compare (_ :> _) (Not _) = GT
  compare (_ :> _) (_ :< _) = GT
  compare (_ :> _) (_ :<= _) = GT
  compare (_ :> _) _ = LT
  compare (_ :>= _) (Var {}) = GT
  compare (_ :>= _) (Hole {}) = GT
  compare (_ :>= _) (Idx {}) = GT
  compare (_ :>= _) (LinComb {}) = GT
  compare (_ :>= _) (Indicator {}) = GT
  compare (_ :>= _) (Bool {}) = GT
  compare (_ :>= _) (Not _) = GT
  compare (_ :>= _) (_ :< _) = GT
  compare (_ :>= _) (_ :<= _) = GT
  compare (_ :>= _) (_ :> _) = GT
  compare (_ :>= _) _ = LT
  compare (_ :== _) (Var {}) = GT
  compare (_ :== _) (Hole {}) = GT
  compare (_ :== _) (Idx {}) = GT
  compare (_ :== _) (LinComb {}) = GT
  compare (_ :== _) (Indicator {}) = GT
  compare (_ :== _) (Bool {}) = GT
  compare (_ :== _) (Not _) = GT
  compare (_ :== _) (_ :< _) = GT
  compare (_ :== _) (_ :<= _) = GT
  compare (_ :== _) (_ :> _) = GT
  compare (_ :== _) (_ :>= _) = GT
  compare (_ :== _) _ = LT
  compare (_ :/= _) (Var {}) = GT
  compare (_ :/= _) (Hole {}) = GT
  compare (_ :/= _) (Idx {}) = GT
  compare (_ :/= _) (LinComb {}) = GT
  compare (_ :/= _) (Indicator {}) = GT
  compare (_ :/= _) (Bool {}) = GT
  compare (_ :/= _) (Not _) = GT
  compare (_ :/= _) (_ :< _) = GT
  compare (_ :/= _) (_ :<= _) = GT
  compare (_ :/= _) (_ :> _) = GT
  compare (_ :/= _) (_ :>= _) = GT
  compare (_ :/= _) (_ :== _) = GT
  compare (_ :/= _) _ = LT
  compare (_ :&& _) (Var {}) = GT
  compare (_ :&& _) (Hole {}) = GT
  compare (_ :&& _) (Idx {}) = GT
  compare (_ :&& _) (LinComb {}) = GT
  compare (_ :&& _) (Indicator {}) = GT
  compare (_ :&& _) (Bool {}) = GT
  compare (_ :&& _) (Not _) = GT
  compare (_ :&& _) (_ :< _) = GT
  compare (_ :&& _) (_ :<= _) = GT
  compare (_ :&& _) (_ :> _) = GT
  compare (_ :&& _) (_ :>= _) = GT
  compare (_ :&& _) (_ :== _) = GT
  compare (_ :&& _) (_ :/= _) = GT
  compare (_ :&& _) _ = LT
  compare (_ :|| _) (Var {}) = GT
  compare (_ :|| _) (Hole {}) = GT
  compare (_ :|| _) (Idx {}) = GT
  compare (_ :|| _) (LinComb {}) = GT
  compare (_ :|| _) (Indicator {}) = GT
  compare (_ :|| _) (Bool {}) = GT
  compare (_ :|| _) (Not _) = GT
  compare (_ :|| _) (_ :< _) = GT
  compare (_ :|| _) (_ :<= _) = GT
  compare (_ :|| _) (_ :> _) = GT
  compare (_ :|| _) (_ :>= _) = GT
  compare (_ :|| _) (_ :== _) = GT
  compare (_ :|| _) (_ :/= _) = GT
  compare (_ :|| _) (_ :&& _) = GT
  compare (_ :|| _) _ = LT
  compare Recurrence _ = GT

instance Pretty Symbol where
  pretty symbol = case symbol of
    (Var x) -> pretty x
    (Hole x) -> pretty (VName "h" (baseTag x))
    (Idx x i) -> autoParens x <> brackets (pretty i)
    (LinComb i lb ub e) ->
      "∑"
        <> pretty i <> "∈"
        <> parens (pretty lb <+> ".." <+> pretty ub)
        <> " " <> autoParens e
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
      autoParens x@(Hole _) = pretty x
      autoParens x = parens (pretty x)
      iversonbrackets = enclose "⟦" "⟧"
      prettyOp s x y = pretty x <+> s <+> pretty y

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
  rename_ tau sym = case sym of
    Var x -> Var <$> rename_ tau x
    Hole x -> Hole <$> rename_ tau x -- The Hole might be a quantifier index.
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
    Var x -> M.findWithDefault (sym2SoP $ Var x) x s
    Hole x -> M.findWithDefault (sym2SoP $ Hole x) x s
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
  unify_ _ (Var x) t | Var x == t = pure mempty
  unify_ k (Hole x) t
    | Hole x == t = fail "Holes are not allowed in the second argument!"
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
