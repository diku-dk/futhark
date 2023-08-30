{-# LANGUAGE OverloadedStrings #-}

module Futhark.IR.Mem.Interval
  ( Interval (..),
    distributeOffset,
    expandOffset,
    intervalOverlap,
    selfOverlap,
    primBool,
    intervalPairs,
    justLeafExp,
  )
where

import Data.Function (on)
import Data.List (maximumBy, minimumBy, (\\))
import Futhark.Analysis.AlgSimplify qualified as AlgSimplify
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Prop
import Futhark.IR.Syntax hiding (Result)
import Futhark.Util

data Interval = Interval
  { lowerBound :: TPrimExp Int64 VName,
    numElements :: TPrimExp Int64 VName,
    stride :: TPrimExp Int64 VName
  }
  deriving (Show, Eq)

instance FreeIn Interval where
  freeIn' (Interval lb ne st) = freeIn' lb <> freeIn' ne <> freeIn' st

distributeOffset :: (MonadFail m) => AlgSimplify.SofP -> [Interval] -> m [Interval]
distributeOffset [] interval = pure interval
distributeOffset offset [] = fail $ "Cannot distribute offset " <> show offset <> " across empty interval"
distributeOffset offset [Interval lb ne 1] = pure [Interval (lb + TPrimExp (AlgSimplify.sumToExp offset)) ne 1]
distributeOffset offset (Interval lb ne st0 : is)
  | st <- AlgSimplify.Prod False [untyped st0],
    Just (before, quotient, after) <- focusMaybe (`AlgSimplify.maybeDivide` st) offset =
      distributeOffset (before <> after) $
        Interval (lb + TPrimExp (AlgSimplify.sumToExp [quotient])) ne st0 : is
  | [st] <- AlgSimplify.simplify0 $ untyped st0,
    Just (before, quotient, after) <- focusMaybe (`AlgSimplify.maybeDivide` st) offset =
      distributeOffset (before <> after) $
        Interval (lb + TPrimExp (AlgSimplify.sumToExp [quotient])) ne st0 : is
  | otherwise = do
      rest <- distributeOffset offset is
      pure $ Interval lb ne st0 : rest

findMostComplexTerm :: AlgSimplify.SofP -> (AlgSimplify.Prod, AlgSimplify.SofP)
findMostComplexTerm prods =
  let max_prod = maximumBy (compare `on` (length . AlgSimplify.atoms)) prods
   in (max_prod, prods \\ [max_prod])

findClosestStride :: [PrimExp VName] -> [Interval] -> (PrimExp VName, [PrimExp VName])
findClosestStride offset_term is =
  let strides = map (untyped . stride) is
      p =
        minimumBy
          ( compare
              `on` ( termDifferenceLength
                       . minimumBy (compare `on` \s -> length (offset_term \\ AlgSimplify.atoms s))
                       . AlgSimplify.simplify0
                   )
          )
          strides
   in ( p,
        (offset_term \\) $
          AlgSimplify.atoms $
            minimumBy (compare `on` \s -> length (offset_term \\ AlgSimplify.atoms s)) $
              AlgSimplify.simplify0 p
      )
  where
    termDifferenceLength (AlgSimplify.Prod _ xs) = length (offset_term \\ xs)

expandOffset :: AlgSimplify.SofP -> [Interval] -> Maybe AlgSimplify.SofP
expandOffset [] _ = Nothing
expandOffset offset i1
  | (AlgSimplify.Prod b term_to_add, offset_rest) <- findMostComplexTerm offset, -- Find gnb
    (closest_stride, first_term_divisor) <- findClosestStride term_to_add i1, -- find (nb-b, g)
    target <- [AlgSimplify.Prod b $ closest_stride : first_term_divisor], -- g(nb-b)
    diff <- AlgSimplify.sumOfProducts $ AlgSimplify.sumToExp $ AlgSimplify.Prod b term_to_add : map AlgSimplify.negate target, -- gnb - gnb + gb = gnb - g(nb-b)
    replacement <- target <> diff -- gnb = g(nb-b) + gnb - gnb + gb
    =
      Just (replacement <> offset_rest)

intervalOverlap :: [(VName, PrimExp VName)] -> Names -> Interval -> Interval -> Bool
intervalOverlap less_thans non_negatives (Interval lb1 ne1 st1) (Interval lb2 ne2 st2)
  | st1 == st2,
    AlgSimplify.lessThanish less_thans non_negatives lb1 lb2,
    AlgSimplify.lessThanish less_thans non_negatives (lb1 + ne1 - 1) lb2 =
      False
  | st1 == st2,
    AlgSimplify.lessThanish less_thans non_negatives lb2 lb1,
    AlgSimplify.lessThanish less_thans non_negatives (lb2 + ne2 - 1) lb1 =
      False
  | otherwise = True

primBool :: TPrimExp Bool VName -> Maybe Bool
primBool p
  | Just (BoolValue b) <- evalPrimExp (const Nothing) $ untyped p = Just b
  | otherwise = Nothing

intervalPairs :: [Interval] -> [Interval] -> [(Interval, Interval)]
intervalPairs = intervalPairs' []
  where
    intervalPairs' :: [(Interval, Interval)] -> [Interval] -> [Interval] -> [(Interval, Interval)]
    intervalPairs' acc [] [] = reverse acc
    intervalPairs' acc (i@(Interval lb _ st) : is) [] = intervalPairs' ((i, Interval lb 1 st) : acc) is []
    intervalPairs' acc [] (i@(Interval lb _ st) : is) = intervalPairs' ((Interval lb 1 st, i) : acc) [] is
    intervalPairs' acc (i1@(Interval lb1 _ st1) : is1) (i2@(Interval lb2 _ st2) : is2)
      | st1 == st2 = intervalPairs' ((i1, i2) : acc) is1 is2
      | otherwise =
          let res1 = intervalPairs' ((i1, Interval lb1 1 st1) : acc) is1 (i2 : is2)
              res2 = intervalPairs' ((Interval lb2 1 st2, i2) : acc) (i1 : is1) is2
           in if length res1 <= length res2
                then res1
                else res2

-- | Returns true if the intervals are self-overlapping, meaning that for a
-- given dimension d, the stride of d is larger than the aggregate spans of the
-- lower dimensions.
selfOverlap :: scope -> asserts -> [(VName, PrimExp VName)] -> [PrimExp VName] -> [Interval] -> Maybe Interval
selfOverlap _ _ _ _ [_] = Nothing
selfOverlap _ _ less_thans non_negatives' is
  | Just non_negatives <- namesFromList <$> mapM justLeafExp non_negatives' =
      -- TODO: Do we need to do something clever using some ranges of known values?
      let selfOverlap' acc (x : xs) =
            let interval_span = (lowerBound x + numElements x - 1) * stride x
                res = AlgSimplify.lessThanish less_thans non_negatives (AlgSimplify.simplify' acc) (AlgSimplify.simplify' $ stride x)
             in if res then selfOverlap' (acc + interval_span) xs else Just x
          selfOverlap' _ [] = Nothing
       in selfOverlap' 0 $ reverse is
selfOverlap _ _ _ _ (x : _) = Just x
selfOverlap _ _ _ _ [] = Nothing

justLeafExp :: PrimExp VName -> Maybe VName
justLeafExp (LeafExp v _) = Just v
justLeafExp _ = Nothing
