module Futhark.Analysis.Proofs.Symbol where

import Futhark.Analysis.Proofs.Util (prettyHole, prettyName)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, scaleSoP, sopToLists, sym2SoP, (.+.), (.-.))
import Futhark.Util.Pretty (Pretty, apply, brackets, enclose, parens, pretty, (<+>))
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

getLinCombBoundVar :: Symbol -> Maybe VName
getLinCombBoundVar (LinComb i _ _ _) = Just i
getLinCombBoundVar _ = Nothing

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
    Apply f xs -> pretty f <> apply (map pretty xs)
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
