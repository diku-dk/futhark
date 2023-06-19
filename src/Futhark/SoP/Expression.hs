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

instance Ord u => Expression (PrimExp u) where
  moduloIsh :: PrimExp u -> Maybe (PrimExp u, PrimExp u)
  moduloIsh (BinOpExp (SMod _ _) pe1 pe2) = Just (pe1, pe2)
  moduloIsh (BinOpExp (UMod _ _) pe1 pe2) = Just (pe1, pe2)
  moduloIsh _ = Nothing

  processExp :: PrimExp u -> (Set (PrimExp u == 0), Set (PrimExp u >= 0))
  processExp (CmpOpExp (CmpEq ptp) x y)
    -- x = y => x - y = 0
    | IntType {} <- ptp =
        (S.singleton (x ~-~ y), mempty)
  processExp (CmpOpExp lessop x y)
    -- x < y => x + 1 <= y => y >= x + 1 => y - (x+1) >= 0
    | Just itp <- lthishType lessop =
        let pe = y ~-~ (x ~+~ ValueExp (IntValue $ intValue itp (1 :: Integer)))
         in (mempty, S.singleton pe)
    -- x <= y => y >= x => y - x >= 0
    | Just _ <- leqishType lessop =
        (mempty, S.singleton $ y ~-~ x)
    where
      -- Is this 'PrimExp' @<@?
      lthishType :: CmpOp -> Maybe IntType
      lthishType (CmpSlt itp) = Just itp
      lthishType (CmpUlt itp) = Just itp
      lthishType _ = Nothing

      -- Is this 'PrimExp' @<=@?
      leqishType :: CmpOp -> Maybe IntType
      leqishType (CmpUle itp) = Just itp
      leqishType (CmpSle itp) = Just itp
      leqishType _ = Nothing
  processExp (BinOpExp LogAnd x y) =
    processExps [x, y]
  processExp (CmpOpExp CmpEq {} pe1 pe2) =
    case (pe1, pe2) of
      -- (x && y) == True => x && y
      (BinOpExp LogAnd _ _, ValueExp (BoolValue True)) ->
        processExp pe1
      -- True == (x && y) => x && y
      (ValueExp (BoolValue True), BinOpExp LogAnd _ _) ->
        processExp pe2
      -- (x || y) == False => !x && !y
      (BinOpExp LogOr x y, ValueExp (BoolValue False)) ->
        processExps [UnOpExp Not x, UnOpExp Not y]
      -- False == (x || y) => !x && !y
      (ValueExp (BoolValue False), BinOpExp LogOr x y) ->
        processExps [UnOpExp Not x, UnOpExp Not y]
      _ -> mempty
  processExp (UnOpExp Not pe) =
    case pe of
      -- !(!x) => x
      UnOpExp Not x ->
        processExp x
      -- !(x < y) => y <= x
      CmpOpExp (CmpSlt itp) x y ->
        processExp $ CmpOpExp (CmpSle itp) y x
      -- !(x <= y) => y < x
      CmpOpExp (CmpSle itp) x y ->
        processExp $ CmpOpExp (CmpSlt itp) y x
      -- !(x < y) => y <= x
      CmpOpExp (CmpUlt itp) x y ->
        processExp $ CmpOpExp (CmpUle itp) y x
      -- !(x <= y) => y < x
      CmpOpExp (CmpUle itp) x y ->
        processExp $ CmpOpExp (CmpUlt itp) y x
      _ -> mempty
  processExp _ = mempty
