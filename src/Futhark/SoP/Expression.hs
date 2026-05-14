{-# LANGUAGE DataKinds #-}

module Futhark.SoP.Expression
  ( Expression (..),
    processExps,
  )
where

import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.PrimExp
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Prop

class Expression e where
  -- -- | Is this 'PrimType' not integral?
  -- notIntType :: PrimType -> Bool

  -- | Is this expression @mod@?
  moduloIsh :: e -> Maybe (e, e)

  -- -- | Is this 'PrimExp' @<@?
  -- lthishType :: CmpOp -> Maybe IntType

  -- -- | Is this 'PrimExp' @<=@?
  -- leqishType :: CmpOp -> Maybe IntType

  -- | Rewrite a mod expression into division.
  divInsteadOfMod :: e -> e

  -- | Algebraically manipulates an 'e' into a set of equality
  -- and inequality constraints.
  processExp :: e -> (Set (e == 0), Set (e >= 0))

processExps :: (Ord e, Expression e, Foldable t) => t e -> (Set (e == 0), Set (e >= 0))
processExps = foldMap processExp

instance Expression Exp where
  moduloIsh (E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
    | E.baseTag (E.qualLeaf op) <= maxIntrinsicTag,
      name <- E.baseString $ E.qualLeaf op,
      Just bop <- find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp],
      E.Mod <- bop =
        Just (e_x, e_y)
  moduloIsh _ = Nothing

instance (Ord u) => Expression (PrimExp u) where
  moduloIsh :: PrimExp u -> Maybe (PrimExp u, PrimExp u)
  moduloIsh (BinOpExp (SMod _ _) pe1 pe2) = Just (pe1, pe2)
  moduloIsh (BinOpExp (UMod _ _) pe1 pe2) = Just (pe1, pe2)
  moduloIsh _ = Nothing

  processExp :: PrimExp u -> (Set (PrimExp u == 0), Set (PrimExp u >= 0))
  processExp = undefined
