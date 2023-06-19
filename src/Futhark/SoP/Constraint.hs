module Futhark.SoP.Constraint
  ( Constraint (..),
    orC,
    andC,
  )
where

import Futhark.SoP.Convert
import Futhark.SoP.SoP
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Prop

-- class ToConstraint u e where
--  toConstraint :: e -> Constraint u
--
-- instance ToConstraint VName Exp where
--  toConstraint (E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
--    | E.baseTag (E.qualLeaf op) <= maxIntrinsicTag,
--      name <- E.baseString $ E.qualLeaf op,
--      Just bop <- find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
--        (_, x) <- toSoPNum e_x
--        (_, y) <- toSoPNum e_y
--        (1,)
--          <$> case bop of
--            E.Equal -> pure $ x .-. y
--            E.Less -> pure $ y .-. (x .+. int2SoP 1)
--            E.Leq -> pure $ y .-. x
--            E.Greater -> pure $ x .-. (y .+. int2SoP 1)
--            E.Geq -> pure $ x .-. y
--            bop -> error $ "toSoPCmp: " <> prettyString bop

data Constraint u
  = (:<:) (SoP u) (SoP u)
  | (:<=:) (SoP u) (SoP u)
  | (:>:) (SoP u) (SoP u)
  | (:>=:) (SoP u) (SoP u)
  | (:==:) (SoP u) (SoP u)
  | (:/=:) (SoP u) (SoP u)
  | (:&&:) (Constraint u) (Constraint u)
  | (:||:) (Constraint u) (Constraint u)
  deriving (Eq, Ord, Show)

infixr 4 :<:

infixr 4 :<=:

infixr 4 :>:

infixr 4 :>=:

infixr 4 :==:

infixr 4 :/=:

infixr 3 :&&:

infixr 2 :||:

andC :: [Constraint u] -> Constraint u
andC = foldr1 (:&&:)

orC :: [Constraint u] -> Constraint u
orC = foldr1 (:||:)

instance Pretty u => Pretty (Constraint u) where
  pretty c =
    case c of
      x :<: y -> op "<" x y
      x :<=: y -> op "<=" x y
      x :>: y -> op ">" x y
      x :>=: y -> op ">=" x y
      x :==: y -> op "==" x y
      x :/=: y -> op "/=" x y
      x :&&: y -> op "&&" x y
      x :||: y -> op "||" x y
    where
      op s x y = pretty x <+> s <+> pretty y
