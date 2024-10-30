module Futhark.Analysis.AlgSimplify
  ( Prod (..),
    SofP,
    simplify0,
    simplify,
    simplify',
    simplifySofP,
    simplifySofP',
    sumOfProducts,
    sumToExp,
    prodToExp,
    add,
    sub,
    negate,
    isMultipleOf,
    maybeDivide,
    removeLessThans,
    lessThanish,
    compareComplexity,
  )
where

import Data.Bits (xor)
import Data.Function ((&))
import Data.List (findIndex, intersect, partition, sort, (\\))
import Data.Maybe (mapMaybe)
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Prop.Names
import Futhark.IR.Syntax.Core (SubExp (..), VName)
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
prodToExp (Prod True [ValueExp (IntValue (Int64Value i))]) = ValueExp $ IntValue $ Int64Value (-i)
prodToExp (Prod True as) =
  foldl (BinOpExp $ Mul Int64 OverflowUndef) (val (-1)) as
prodToExp (Prod False (a : as)) =
  foldl (BinOpExp $ Mul Int64 OverflowUndef) a as

simplifySofP :: SofP -> SofP
simplifySofP =
  -- TODO: Maybe 'constFoldValueExps' is not necessary after adding scaleConsts
  fixPoint (mapMaybe (applyZero . removeOnes) . scaleConsts . constFoldValueExps . removeNegations)

simplifySofP' :: SofP -> SofP
simplifySofP' = fixPoint (mapMaybe (applyZero . removeOnes) . scaleConsts . removeNegations)

simplify0 :: Exp -> SofP
simplify0 = simplifySofP . sumOfProducts

simplify :: Exp -> Exp
simplify = constFoldPrimExp . sumToExp . simplify0

simplify' :: TExp -> TExp
simplify' = TPrimExp . simplify . untyped

applyZero :: Prod -> Maybe Prod
applyZero p@(Prod _ as)
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

intFromExp :: Exp -> Maybe Int64
intFromExp (ValueExp (IntValue x)) = Just $ valueIntegral x
intFromExp _ = Nothing

-- | Given @-[2, x]@ returns @(-2, [x])@
prodToScale :: Prod -> (Int64, [Exp])
prodToScale (Prod b exps) =
  let (scalars, exps') = partitionMaybe intFromExp exps
   in if b
        then (-(product scalars), exps')
        else (product scalars, exps')

-- | Given @(-2, [x])@ returns @-[1, 2, x]@
scaleToProd :: (Int64, [Exp]) -> Prod
scaleToProd (i, exps) =
  Prod (i < 0) $ ValueExp (IntValue $ Int64Value $ abs i) : exps

-- | Given @[[2, x], -[x]]@ returns @[[x]]@
scaleConsts :: SofP -> SofP
scaleConsts =
  helper [] . map prodToScale
  where
    helper :: [Prod] -> [(Int64, [Exp])] -> [Prod]
    helper acc [] = reverse acc
    helper acc ((scale, exps) : rest) =
      case flip focusNth rest =<< findIndex ((==) exps . snd) rest of
        Nothing -> helper (scaleToProd (scale, exps) : acc) rest
        Just (before, (scale', _), after) ->
          helper acc $ (scale + scale', exps) : (before <> after)

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

maybeDivide :: Prod -> Prod -> Maybe Prod
maybeDivide dividend divisor
  | Prod dividend_b dividend_factors <- dividend,
    Prod divisor_b divisor_factors <- divisor,
    quotient <- dividend_factors \\ divisor_factors,
    sort (quotient <> divisor_factors) == sort dividend_factors =
    Just $ Prod (dividend_b `xor` divisor_b) quotient
  | (dividend_scale, dividend_rest) <- prodToScale dividend,
    (divisor_scale, divisor_rest) <- prodToScale divisor,
    dividend_scale `mod` divisor_scale == 0,
    null $ divisor_rest \\ dividend_rest =
    Just $
      Prod
        (signum (dividend_scale `div` divisor_scale) < 0)
        ( ValueExp (IntValue $ Int64Value $ dividend_scale `div` divisor_scale) :
          (dividend_rest \\ divisor_rest)
        )
  | otherwise = Nothing

-- | Given a list of 'Names' that we know are non-negative (>= 0), determine
-- whether we can say for sure that the given 'AlgSimplify.SofP' is
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
        fixPoint (`removeLessThans` less_thans) simplified
  where
    less_thans =
      concatMap
        (\(i, bound) -> [(Var i, bound), (Constant $ IntValue $ Int64Value 0, bound)])
        less_thans0

lessThanish :: [(VName, PrimExp VName)] -> Names -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> Bool
lessThanish less_thans non_negatives e1 =
  lessThanOrEqualish less_thans non_negatives (e1 + 1)

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

compareComplexity :: SofP -> SofP -> Ordering
compareComplexity xs0 ys0 =
  case length xs0 `compare` length ys0 of
    EQ -> helper xs0 ys0
    c -> c
  where
    helper [] [] = EQ
    helper [] _ = LT
    helper _ [] = GT
    helper (px : xs) (py : ys) =
      case (prodToScale px, prodToScale py) of
        ((ix, []), (iy, [])) -> case ix `compare` iy of
          EQ -> helper xs ys
          c -> c
        ((_, []), (_, _)) -> LT
        ((_, _), (_, [])) -> GT
        ((_, x), (_, y)) -> case length x `compare` length y of
          EQ -> helper xs ys
          c -> c
