{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Futhark.Analysis.AlgSimplify2
  ( Prod (..),
    SofP,
    simplify,
  )
where

import Data.Bits (xor)
import Data.Function ((&))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Futhark.Analysis.PrimExp
import Futhark.IR.Syntax.Core
import Futhark.Util.Pretty
import GHC.Generics
import Prelude hiding (negate)

type Exp = PrimExp VName

data Prod = Prod
  { negated :: Bool,
    atoms :: [Exp]
  }
  deriving (Show, Eq, Ord)

instance Pretty Prod where
  ppr (Prod b exps) =
    (if b then "-" else "") <> ppr exps

type SofP = [Prod]

sumOfProducts :: Exp -> SofP
sumOfProducts (BinOpExp (Add Int64 OverflowUndef) e1 e2) =
  sumOfProducts e1 <> sumOfProducts e2
sumOfProducts (BinOpExp (Sub Int64 OverflowUndef) (ValueExp (IntValue (Int64Value 0))) e) =
  map negate $ sumOfProducts e
sumOfProducts (BinOpExp (Sub Int64 OverflowUndef) e1 e2) =
  sumOfProducts e1 <> map negate (sumOfProducts e2)
sumOfProducts (BinOpExp (Mul Int64 OverflowUndef) e1 e2) =
  sumOfProducts e1 `mult` sumOfProducts e2
sumOfProducts e = [Prod False [e]]

mult :: SofP -> SofP -> SofP
mult xs ys = [Prod (b `xor` b') (x <> y) | Prod b x <- xs, Prod b' y <- ys]

negate :: Prod -> Prod
negate p = p {negated = not $ negated p}

sumToExp :: SofP -> Exp
sumToExp [] =
  ValueExp $ IntValue $ Int64Value 0
sumToExp [x] = prodToExp x
sumToExp (x : xs) = foldl (BinOpExp $ Add Int64 OverflowUndef) (prodToExp x) $ map prodToExp xs

prodToExp :: Prod -> Exp
prodToExp (Prod _ []) = ValueExp $ IntValue $ Int64Value 0
prodToExp (Prod True atoms) =
  foldl (BinOpExp $ Mul Int64 OverflowUndef) (ValueExp $ IntValue $ Int64Value $ -1) atoms
prodToExp (Prod False (atom : atoms)) =
  foldl (BinOpExp $ Mul Int64 OverflowUndef) atom atoms

simplify :: Exp -> Exp
simplify = sumToExp . removeNegations . sumOfProducts

removeNegations :: SofP -> SofP
removeNegations [] = []
removeNegations (t : ts) =
  case break (== negate t) ts of
    (start, _ : rest) -> removeNegations $ start <> rest
    _ -> t : removeNegations ts
