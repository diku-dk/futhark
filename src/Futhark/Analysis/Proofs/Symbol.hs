module Futhark.Analysis.Proofs.Symbol where

import Futhark.Analysis.Proofs.Util (prettyHole, prettyName)
import Futhark.SoP.SoP (SoP, justConstant, justSym)
import Futhark.Util.Pretty (Pretty, apply, brackets, commasep, parens, pretty, prettyString, (<+>), softline)
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
  | Apply Symbol [SoP Symbol] -- First argument is Var or Hole.
  | Tuple [SoP Symbol]
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
isBoolean (_ :&& _) = True
isBoolean (_ :|| _) = True
isBoolean _ = False

sop2Symbol :: SoP Symbol -> Symbol
sop2Symbol sop
  | Just t <- justSym sop = t
  | Just 1 <- justConstant sop = Bool True
  | Just 0 <- justConstant sop = Bool False
  | otherwise = error $ "sop2Symbol on non-symbol: " <> prettyString sop

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
    Tuple xs -> parens (commasep $ map pretty xs)
    Bool x -> pretty x
    Not x -> "¬" <> autoParens x
    x :< y -> prettyOp "<" x y
    x :<= y -> prettyOp "≤" x y
    x :> y -> prettyOp ">" x y
    x :>= y -> prettyOp "≥" x y
    x :== y -> prettyOp "=" x y
    x :/= y -> prettyOp "≠" x y
    x :&& y -> autoParens x <+> "^" <+> autoParens y
    x :|| y -> prettyOp "∨" x y
    Recurrence -> "↺ "
    where
      autoParens x@(Var _) = pretty x
      autoParens x@(Hole _) = pretty x
      autoParens x = parens (pretty x)
      prettyOp s x y = pretty x <> softline <> s <+> pretty y
