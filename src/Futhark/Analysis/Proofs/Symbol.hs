module Futhark.Analysis.Proofs.Symbol where

import Futhark.Analysis.Proofs.Util (prettyHole, prettyName)
import Futhark.SoP.SoP (SoP, justSym)
import Futhark.Util.Pretty (Pretty, apply, brackets, enclose, parens, pretty, (<+>))
import Language.Futhark (VName)

data Symbol
  = Var VName
  | Hole VName
  | Idx
      Symbol -- array
      (SoP Symbol) -- index
  | Sum
      VName -- binder
      (SoP Symbol) -- lower bound
      (SoP Symbol) -- upper bound
      Symbol
  | Apply Symbol [SoP Symbol]
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

infixr 4 :<
infixr 4 :<=
infixr 4 :>
infixr 4 :>=
infixr 4 :==
infixr 4 :/=
infixr 3 :&&
infixr 2 :||

isBoolean :: Symbol -> Bool
isBoolean (Bool {}) = True
isBoolean (Not {}) = True
isBoolean (_ :< _) = True
isBoolean (_ :<= _) = True
isBoolean (_ :> _) = True
isBoolean (_ :>= _) = True
isBoolean (_ :== _) = True
isBoolean (_ :/= _) = True
isBoolean _ = False

sop2Symbol :: (Ord u) => SoP u -> u
sop2Symbol sop
  | Just t <- justSym sop = t
  | otherwise = error "sop2Symbol on non-symbol"

getSumBoundVar :: Symbol -> Maybe VName
getSumBoundVar (Sum i _ _ _) = Just i
getSumBoundVar _ = Nothing

toDNF :: Symbol -> Symbol
toDNF (a :&& (b :|| c)) = toDNF (a :&& b) :|| toDNF (a :&& c)
toDNF ((a :|| b) :&& c) = toDNF (a :&& c) :|| toDNF (b :&& c)
toDNF (a :|| b) = toDNF a :|| toDNF b
toDNF p = p

toCNF :: Symbol -> Symbol
toCNF p = neg $ toDNF (neg p)

neg :: Symbol -> Symbol
neg (Not x) = x
neg (x :|| y) = neg x :&& neg y
neg (x :&& y) = neg x :|| neg y
neg (Bool True) = Bool False
neg (Bool False) = Bool True
neg (x :== y) = x :/= y
neg (x :< y) = x :>= y
neg (x :> y) = x :<= y
neg (x :/= y) = x :== y
neg (x :>= y) = x :< y
neg (x :<= y) = x :> y
neg x = Not x

-- TODO Normalize only normalizes Boolean expressions.
--      Use a Boolean representation that is normalized by construction.
normalizeSymbol :: Symbol -> Symbol
normalizeSymbol symbol = case toCNF symbol of
  (Not x) -> neg x
  (x :&& y) ->
    case (x, y) of
      (Bool True, b) -> b -- Identity.
      (a, Bool True) -> a
      (Bool False, _) -> Bool False -- Annihilation.
      (_, Bool False) -> Bool False
      (a, b) | a == b -> a -- Idempotence.
      (a, b) | a == neg b -> Bool False -- A contradiction.
      (a, b) -> a :&& b
  (x :|| y) -> do
    case (x, y) of
      (Bool False, b) -> b -- Identity.
      (a, Bool False) -> a
      (Bool True, _) -> Bool True -- Annihilation.
      (_, Bool True) -> Bool True
      (a, b) | a == b -> a -- Idempotence.
      (a, b) | a == neg b -> Bool True -- A tautology.
      (a, b) -> a :|| b
  v -> v

instance Pretty Symbol where
  pretty symbol = case symbol of
    (Var x) -> prettyName x
    (Hole x) -> prettyHole x
    (Idx x i) -> autoParens x <> brackets (pretty i)
    (Sum i lb ub e) ->
      "∑"
        <> prettyName i
        <> "∈"
        <> parens (pretty lb <+> ".." <+> pretty ub)
        <> " "
        <> autoParens e
    Apply f xs -> pretty f <> apply (map pretty xs)
    Bool x -> pretty x
    Not x -> "¬" <> autoParens x
    x :< y -> prettyOp "<" x y
    x :<= y -> prettyOp "≤" x y
    x :> y -> prettyOp ">" x y
    x :>= y -> prettyOp "≥" x y
    x :== y -> prettyOp "=" x y
    x :/= y -> prettyOp "≠" x y
    x :&& y -> prettyOp "∧" x y
    x :|| y -> prettyOp "∨" x y
    Recurrence -> "↺ "
    where
      autoParens x@(Var _) = pretty x
      autoParens x@(Hole _) = pretty x
      autoParens x = parens (pretty x)
      iversonbrackets = enclose "⟦" "⟧"
      prettyOp s x y = pretty x <+> s <+> pretty y
