module Futhark.Analysis.Proofs.Symbol where

import Futhark.Analysis.Proofs.Util (prettyHole, prettyName)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, scaleSoP, sopToLists, sym2SoP, (.+.), (.-.))
import Futhark.Util.Pretty (Pretty, brackets, enclose, parens, pretty, (<+>))
import Language.Futhark (VName)

data Symbol
  = Var VName
  | Hole VName
  | Idx
      Symbol -- array
      (SoP Symbol) -- index
  | LinComb
      VName -- binder
      (SoP Symbol) -- lower bound
      (SoP Symbol) -- upper bound
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

data Property
  deriving (Show, Eq, Ord)

sop2Symbol :: (Ord u) => SoP u -> u
sop2Symbol sop
  | Just t <- justSym sop = t
  | otherwise = error "sop2Symbol on something that is not a symbol"

applyLinCombRule :: VName -> SoP Symbol -> SoP Symbol -> SoP Symbol -> SoP Symbol
applyLinCombRule i a b = foldl1 (.+.) . map (mkLinComb i a b) . sopToLists

mkLinComb :: VName -> SoP Symbol -> SoP Symbol -> ([Symbol], Integer) -> SoP Symbol
mkLinComb _ a b ([], c) =
  scaleSoP c (b .-. a .+. int2SoP 1)
mkLinComb i a b ([u], c) =
  scaleSoP c (sym2SoP $ LinComb i a b u)
mkLinComb _ _ _ _ =
  error "Replacement is not a linear combination."

-- TODO Normalize only normalizes Boolean expressions.
--      Use a Boolean representation that is normalized by construction.
normalizeSymbol :: Symbol -> Symbol
normalizeSymbol symbol = case toNNF symbol of
  (Not x) -> toNNF (Not x)
  (x :&& y) ->
    case (x, y) of
      (Bool True, b) -> b -- Identity.
      (a, Bool True) -> a
      (Bool False, _) -> Bool False -- Annihilation.
      (_, Bool False) -> Bool False
      (a, b) | a == b -> a -- Idempotence.
      (a, b) | a == toNNF (Not b) -> Bool False -- A contradiction.
      (a, b) -> a :&& b
  (x :|| y) -> do
    case (x, y) of
      (Bool False, b) -> b -- Identity.
      (a, Bool False) -> a
      (Bool True, _) -> Bool True -- Annihilation.
      (_, Bool True) -> Bool True
      (a, b) | a == b -> a -- Idempotence.
      (a, b) | a == toNNF (Not b) -> Bool True -- A tautology.
      (a, b) -> a :|| b
  v -> v
  where
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
    (Var x) -> prettyName x
    (Hole x) -> prettyHole x
    (Idx x i) -> autoParens x <> brackets (pretty i)
    (LinComb i lb ub e) ->
      "∑"
        <> prettyName i
        <> "∈"
        <> parens (pretty lb <+> ".." <+> pretty ub)
        <> " "
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
      autoParens x@(Hole _) = pretty x
      autoParens x = parens (pretty x)
      iversonbrackets = enclose "⟦" "⟧"
      prettyOp s x y = pretty x <+> s <+> pretty y
