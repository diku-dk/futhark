{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Futhark.Analysis.AlgSimplify2
  ( Prod (..),
    SofP,
    simplify0,
    simplify,
    simplify',
    simplifySofP,
    sumOfProducts,
    sumToExp,
    prodToExp,
    add,
    sub,
    negate,
    isMultipleOf,
    removeLessThans,
    lessThanish,
  )
where

import Data.Bits (xor)
import Data.Function ((&))
import Data.List (intersect, partition, sort, (\\))
import Data.Maybe (mapMaybe)
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Prop.Names
import Futhark.IR.Syntax.Core
import Futhark.Util
import Futhark.Util.Pretty
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
sumOfProducts' (ValueExp (IntValue (Int64Value i))) =
  [Prod (i < 0) [ValueExp $ IntValue $ Int64Value $ abs i]]
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
prodToExp (Prod True [ValueExp (IntValue (Int64Value i))]) = ValueExp $ IntValue $ Int64Value (- i)
prodToExp (Prod True as) =
  foldl (BinOpExp $ Mul Int64 OverflowUndef) (val (-1)) as
prodToExp (Prod False (a : as)) =
  foldl (BinOpExp $ Mul Int64 OverflowUndef) a as

simplifySofP :: SofP -> SofP
simplifySofP = fixPoint (mapMaybe (applyZero . removeOnes) . constFoldValueExps . removeNegations)

simplify0 :: Exp -> SofP
simplify0 = simplifySofP . sumOfProducts

simplify :: Exp -> Exp
simplify = constFoldPrimExp . sumToExp . simplify0

simplify' :: TExp -> TExp
simplify' = TPrimExp . simplify . untyped

applyZero :: Prod -> Maybe Prod
applyZero p@(Prod neg as)
  | val 0 `elem` as = Nothing
  | otherwise = Just p

removeOnes :: Prod -> Prod
removeOnes (Prod neg as) =
  let as' = filter (/= val 1) as
   in Prod neg $ if null as' then [ValueExp $ IntValue $ Int64Value 1] else as'

removeNegations :: SofP -> SofP
removeNegations [] = []
removeNegations (t : ts) =
  case break (== negate t) ts of
    (start, _ : rest) -> removeNegations $ start <> rest
    _ -> t : removeNegations ts

constFoldValueExps :: SofP -> SofP
constFoldValueExps prods =
  let (value_exps, others) = partition (all isPrimValue . atoms) prods
      value_exps' = sumOfProducts $ constFoldPrimExp $ sumToExp value_exps
   in value_exps' <> others

isPrimValue :: Exp -> Bool
isPrimValue (ValueExp _) = True
isPrimValue _ = False

val :: Int64 -> Exp
val = ValueExp . IntValue . Int64Value

add :: SofP -> SofP -> SofP
add ps1 ps2 = simplifySofP $ ps1 <> ps2

sub :: SofP -> SofP -> SofP
sub ps1 ps2 = add ps1 $ map negate ps2

isMultipleOf :: Prod -> [Exp] -> Bool
isMultipleOf (Prod _ as) term =
  let quotient = as \\ term
   in sort (quotient <> term) == sort as

-- | Given a list of 'Names' that we know are non-negative (>= 0), determine
-- whether we can say for sure that the given 'AlgSimplify2.SofP' is
-- non-negative. Conservatively returns 'False' if there is any doubt.
--
-- TODO: We need to expand this to be able to handle cases such as @i*n + g < (i
-- + 1) * n@, if it is known that @g < n@, eg. from a 'SegSpace' or a loop form.
nonNegativeish :: Names -> SofP -> Bool
nonNegativeish non_negatives = all (nonNegativeishProd non_negatives)

nonNegativeishProd :: Names -> Prod -> Bool
nonNegativeishProd _ (Prod True _) = False
nonNegativeishProd non_negatives (Prod False as) =
  all (nonNegativeishExp non_negatives) as

nonNegativeishExp :: Names -> PrimExp VName -> Bool
nonNegativeishExp _ (ValueExp v) = not $ negativeIsh v
nonNegativeishExp non_negatives (LeafExp vname _) = vname `nameIn` non_negatives
nonNegativeishExp _ _ = False

-- | Is e1 symbolically less than or equal to e2?
lessThanOrEqualish :: [(VName, PrimExp VName)] -> Names -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> Bool
lessThanOrEqualish less_thans0 non_negatives e1 e2 =
  case e2 - e1 & untyped & simplify0 of
    [] -> True
    simplified ->
      nonNegativeish non_negatives $
        fixPoint (`removeLessThans` less_thans) $ simplified
  where
    less_thans =
      concatMap
        (\(i, bound) -> [(Var i, bound), (Constant $ IntValue $ Int64Value 0, bound)])
        less_thans0

lessThanish :: [(VName, PrimExp VName)] -> Names -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> Bool
lessThanish less_thans non_negatives e1 e2 =
  lessThanOrEqualish less_thans non_negatives (e1 + 1) e2

removeLessThans :: SofP -> [(SubExp, PrimExp VName)] -> SofP
removeLessThans =
  foldl
    ( \sofp (i, bound) ->
        let to_remove =
              simplifySofP $
                Prod True [primExpFromSubExp (IntType Int64) i] :
                simplify0 bound
         in case to_remove `intersect` sofp of
              to_remove' | to_remove' == to_remove -> sofp \\ to_remove
              _ -> sofp
    )
