{-# LANGUAGE DataKinds #-}

-- | Basic 'PrimExp' functions.
module Futhark.SoP.PrimExp
  ( notIntType,
    divideIsh,
    moduloIsh,
    divInsteadOfMod,
    processPE,
    processPEs,
  )
where

import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.PrimExp
import Futhark.SoP.Monad

-- | Is this 'PrimType' not integral?
notIntType :: PrimType -> Bool
notIntType (IntType _) = False
notIntType _ = True

-- | Is this 'BinOp' division?
divideIsh :: BinOp -> Bool
divideIsh (UDiv _ _) = True
divideIsh (UDivUp _ _) = True
divideIsh (SDiv _ _) = True
divideIsh (SDivUp _ _) = True
divideIsh (FDiv _) = True
divideIsh _ = False

-- | Is this 'PrimExp' @mod@?
moduloIsh :: PrimExp u -> Maybe (PrimExp u, PrimExp u)
moduloIsh (BinOpExp (SMod _ _) pe1 pe2) = Just (pe1, pe2)
moduloIsh (BinOpExp (UMod _ _) pe1 pe2) = Just (pe1, pe2)
moduloIsh _ = Nothing

-- | Is this 'PrimExp' @<@?
lthishType :: CmpOp -> Maybe IntType
lthishType (CmpSlt itp) = Just itp
lthishType (CmpUlt itp) = Just itp
lthishType _ = Nothing

-- | Is this 'PrimExp' @<=@?
leqishType :: CmpOp -> Maybe IntType
leqishType (CmpUle itp) = Just itp
leqishType (CmpSle itp) = Just itp
leqishType _ = Nothing

-- | Rewrite a mod expression into division.
divInsteadOfMod :: Show u => PrimExp u -> PrimExp u
divInsteadOfMod (BinOpExp (UMod itp saf) pe1 pe2) =
  BinOpExp (UDiv itp saf) pe1 pe2
divInsteadOfMod (BinOpExp (SMod itp saf) pe1 pe2) =
  BinOpExp (SDiv itp saf) pe1 pe2
divInsteadOfMod pe = error ("Impossible case reached in divInsteadOfMod!" ++ show pe)

-- | Algebraically manipualtes a 'PrimExp' into a set of equality
-- and inequality constraints.
processPE :: (Ord u, Nameable u) => PrimExp u -> (Set (PrimExp u == 0), Set (PrimExp u >= 0))
processPE (CmpOpExp (CmpEq ptp) x y)
  -- x = y => x - y = 0
  | IntType {} <- ptp =
      (S.singleton (x ~-~ y), mempty)
processPE (CmpOpExp lessop x y)
  -- x < y => x + 1 <= y => y >= x + 1 => y - (x+1) >= 0
  | Just itp <- lthishType lessop =
      let pe = y ~-~ (x ~+~ ValueExp (IntValue $ intValue itp (1 :: Integer)))
       in (mempty, S.singleton pe)
  -- x <= y => y >= x => y - x >= 0
  | Just _ <- leqishType lessop =
      (mempty, S.singleton $ y ~-~ x)
processPE (BinOpExp LogAnd x y) =
  processPEs [x, y]
processPE (CmpOpExp CmpEq {} pe1 pe2) =
  case (pe1, pe2) of
    -- (x && y) == True => x && y
    (BinOpExp LogAnd _ _, ValueExp (BoolValue True)) ->
      processPE pe1
    -- True == (x && y) => x && y
    (ValueExp (BoolValue True), BinOpExp LogAnd _ _) ->
      processPE pe2
    -- (x || y) == False => !x && !y
    (BinOpExp LogOr x y, ValueExp (BoolValue False)) ->
      processPEs [UnOpExp Not x, UnOpExp Not y]
    -- False == (x || y) => !x && !y
    (ValueExp (BoolValue False), BinOpExp LogOr x y) ->
      processPEs [UnOpExp Not x, UnOpExp Not y]
    _ -> mempty
processPE (UnOpExp Not pe) =
  case pe of
    -- !(!x) => x
    UnOpExp Not x ->
      processPE x
    -- !(x < y) => y <= x
    CmpOpExp (CmpSlt itp) x y ->
      processPE $ CmpOpExp (CmpSle itp) y x
    -- !(x <= y) => y < x
    CmpOpExp (CmpSle itp) x y ->
      processPE $ CmpOpExp (CmpSlt itp) y x
    -- !(x < y) => y <= x
    CmpOpExp (CmpUlt itp) x y ->
      processPE $ CmpOpExp (CmpUle itp) y x
    -- !(x <= y) => y < x
    CmpOpExp (CmpUle itp) x y ->
      processPE $ CmpOpExp (CmpUlt itp) y x
    _ -> mempty
processPE _ = mempty

-- | Process multiple `PrimExp`s at once.
processPEs ::
  (Ord u, Nameable u, Foldable t) =>
  t (PrimExp u) ->
  (Set (PrimExp u == 0), Set (PrimExp u >= 0))
processPEs = foldMap processPE
