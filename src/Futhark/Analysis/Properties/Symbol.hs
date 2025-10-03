module Futhark.Analysis.Properties.Symbol
  ( Symbol (..),
    FromSoP (..),
    isBoolean,
    sop2Symbol,
    toDNF,
    toCNF,
    neg,
    conjToList,
  )
where

import Futhark.Analysis.Properties.Util (prettyHole, prettyName)
import Futhark.SoP.SoP (SoP, justConstant, justSym, justAffine)
import Futhark.Util.Pretty (Pretty, apply, parens, pretty, prettyString, softline, (<+>))
import Language.Futhark (VName)
import Futhark.Analysis.Properties.Property (Property)

data Symbol
  = Var VName
  | Hole VName
  | Sum
      VName -- binder
      (SoP Symbol) -- lower bound
      (SoP Symbol) -- upper bound
      Symbol
  | Apply Symbol [SoP Symbol] -- First argument is Var or Hole.
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
  | Pow Integer (SoP Symbol)
  | Recurrence
  | -- Properties are used only in index functions for pre-/post-conditions.
    Prop (Property Symbol)
  | Assume Symbol
  deriving (Show, Eq, Ord)

infixr 4 :<

infixr 4 :<=

infixr 4 :>

infixr 4 :>=

infixr 4 :==

infixr 4 :/=

infixr 3 :&&

infixr 2 :||

class FromSoP u where
  fromSoP :: SoP u -> u

instance FromSoP Symbol where
  -- WARNING: partial function.
  fromSoP = sop2Symbol

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
  | Just (-1, x, 1) <- justAffine sop = Not x
  | otherwise = error $ "sop2Symbol on non-symbol: " <> prettyString sop

toDNF :: Symbol -> Symbol
toDNF (a :&& (b :|| c)) = toDNF (a :&& b) :|| toDNF (a :&& c)
toDNF ((a :|| b) :&& c) = toDNF (a :&& c) :|| toDNF (b :&& c)
toDNF (a :|| b) = toDNF a :|| toDNF b
toDNF p = p

toCNF :: Symbol -> Symbol
toCNF p = neg $ toDNF (neg p)

conjToList :: Symbol -> [Symbol]
conjToList (a :&& b) = conjToList a <> conjToList b
conjToList x = [x]

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
    -- (Sum i lb ub e)
    --   | Idx (Var x) j <- e,
    --     Just (Var j') <- justSym j,
    --     j' == i ->
    --       -- Make sum slices extra pretty.
    --       "∑"
    --         <> pretty (Var x)
    --         <> brackets (pretty lb <+> ":" <+> pretty ub)
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
    x :&& y -> autoParens x <+> "^" <+> autoParens y
    x :|| y -> prettyOp "∨" x y
    Pow i y -> (pretty i) <> "**" <> parens (pretty y)
    Recurrence -> "↺ "
    Prop p -> pretty p
    Assume p -> "Assume" <+> autoParens p
    where
      autoParens x@(Var _) = pretty x
      autoParens x@(Hole _) = pretty x
      autoParens x = parens (pretty x)
      prettyOp s x y = pretty x <> softline <> s <+> pretty y
