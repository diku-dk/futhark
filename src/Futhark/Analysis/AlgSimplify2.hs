{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Futhark.Analysis.AlgSimplify2
  ( Prod (..),
    SofP,
    simplify0,
    simplify,
    simplify',
    sumOfProducts,
    sumToExp,
    prodToExp,
  )
where

import Data.Bits (xor)
import Data.Function ((&))
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Futhark.Analysis.PrimExp
import Futhark.IR.Syntax.Core
import Futhark.Util
import Futhark.Util.Pretty
import GHC.Generics
import Prelude hiding (negate)

type Exp = PrimExp VName

type TExp = TPrimExp Int64 VName

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
sumOfProducts = map sortProduct . sumOfProducts'

sortProduct :: Prod -> Prod
sortProduct (Prod n as) = Prod n $ sort as

sumOfProducts' :: Exp -> SofP
sumOfProducts' (BinOpExp (Add Int64 _) e1 e2) =
  sumOfProducts' e1 <> sumOfProducts' e2
sumOfProducts' (BinOpExp (Sub Int64 _) (ValueExp (IntValue (Int64Value 0))) e) =
  map negate $ sumOfProducts' e
sumOfProducts' (BinOpExp (Sub Int64 _) e1 e2) =
  sumOfProducts' e1 <> map negate (sumOfProducts' e2)
sumOfProducts' (BinOpExp (Mul Int64 _) e1 e2) =
  sumOfProducts' e1 `mult` sumOfProducts' e2
sumOfProducts' e = [Prod False [e]]

mult :: SofP -> SofP -> SofP
mult xs ys = [Prod (b `xor` b') (x <> y) | Prod b x <- xs, Prod b' y <- ys]

negate :: Prod -> Prod
negate p = p {negated = not $ negated p}

sumToExp :: SofP -> Exp
sumToExp [] = val 0
sumToExp [x] = prodToExp x
sumToExp (x : xs) =
  foldl (BinOpExp $ Add Int64 OverflowUndef) (prodToExp x) $
    map prodToExp xs

prodToExp :: Prod -> Exp
prodToExp (Prod _ []) = val 1
prodToExp (Prod True atoms) =
  foldl (BinOpExp $ Mul Int64 OverflowUndef) (val (-1)) atoms
prodToExp (Prod False (atom : atoms)) =
  foldl (BinOpExp $ Mul Int64 OverflowUndef) atom atoms

simplify0 :: Exp -> SofP
simplify0 = fixPoint (mapMaybe applyZero . map removeOnes . removeNegations) . sumOfProducts

simplify :: Exp -> Exp
simplify = sumToExp . simplify0

simplify' :: TExp -> TExp
simplify' = TPrimExp . simplify . untyped

applyZero :: Prod -> Maybe Prod
applyZero p@(Prod neg atoms)
  | any (== val 0) atoms = Nothing
  | otherwise = Just p

removeOnes :: Prod -> Prod
removeOnes (Prod neg atoms) =
  Prod neg $ filter (/= val 1) atoms

removeNegations :: SofP -> SofP
removeNegations [] = []
removeNegations (t : ts) =
  case break (== negate t) ts of
    (start, _ : rest) -> removeNegations $ start <> rest
    _ -> t : removeNegations ts

val :: Int64 -> Exp
val = ValueExp . IntValue . Int64Value
