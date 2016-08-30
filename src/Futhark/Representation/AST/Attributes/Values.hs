{-# LANGUAGE RankNTypes #-}
-- | Queries and operations on values.  Useful for the interpreter and
-- constant folding.
module Futhark.Representation.AST.Attributes.Values
       (
         valueType
       , valueShape

         -- * Frobbing arrays
       , permuteArray
       , rotateArray
       , concatArrays
       , splitArray
       , flatSlice
       )
       where

import Data.Array hiding (indices)
import Data.List

import Prelude

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Attributes.Rearrange
import Futhark.Util (chunk)

-- | Return the type of the given value.
valueType :: Value -> Type
valueType (PrimVal v) =
  Prim $ primValueType v
valueType (ArrayVal _ et shape) =
  Array et (Shape $ map constant shape) NoUniqueness

-- | Return a list of the sizes of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.  If an array has @n@ dimensions, the result is always a list
-- of @n@ elements.
valueShape :: Value -> [Int]
valueShape (ArrayVal _ _ shape) = shape
valueShape _ = []

-- | Permute the dimensions of an array value.  If the given value is
-- not an array, it is returned unchanged.  The length of the
-- permutation must be equal to the rank of the value.
permuteArray :: [Int] -> Value -> Value
permuteArray perm (ArrayVal inarr et oldshape) =
  let newshape = move oldshape
      idx is shape = sum (zipWith (*) is (map product $ drop 1 (tails shape)))
  in ArrayVal (listArray (bounds inarr)
               [ inarr ! idx (invmove is) oldshape
               | is <- map reverse $ picks $ reverse newshape ])
     et newshape
  where move = rearrangeShape perm
        invmove = rearrangeShape $ rearrangeInverse perm
        picks [] = []
        picks [n] = map (:[]) [0..n-1]
        picks (n:ns) = [ i:is | is <- picks ns, i <- [0..n-1] ]
permuteArray _ v = v

-- | Rotate the elements of an array as per the 'Rotate' BasicOp.
rotateArray :: [Int] -> Value -> Value
rotateArray ks (ArrayVal inarr et shape) =
  ArrayVal (listArray (bounds inarr) $ rotate ks shape $ elems inarr) et shape
rotateArray _ v = v

rotate :: [Int] -> [Int] -> [a] -> [a]
rotate (k:ks) (d:ds) xs =
  -- (0) Split xs into rows.
  -- (1) Recursively rotate every row.
  -- (2) Then rotate the order of rows.
  let rows = chunk (product ds) xs
      xs_rotated = map (rotate ks ds) rows
      new_rows
        | k > 0 = drop k xs_rotated ++ take k xs_rotated
        | otherwise = drop (d+k) xs_rotated ++ take (d+k) xs_rotated
  in concat new_rows
rotate _ _ xs = xs

-- | Concatenate two arrays as per the 'Concat' BasicOp.
concatArrays :: Int -> Value -> Value -> Value
concatArrays i (ArrayVal arr1 et shape1) (ArrayVal arr2 _ shape2) =
  ArrayVal (listArray (0,product shape3-1) $ concatenate xcs xs ycs ys) et shape3
  where xcs = product $ drop i shape1
        xs = elems arr1
        ycs = product $ drop i shape2
        ys = elems arr2
        shape3 = zipWith3 update shape1 shape2 [0..]
        update x y j | i == j    = x + y
                     | otherwise = x
concatArrays _ x _ = x

concatenate :: Int -> [a] -> Int -> [a] -> [a]
concatenate xcs xs ycs ys =
  let xs' = chunk xcs xs
      ys' = chunk ycs ys
  in concat $ zipWith (++) xs' ys'

splitArray :: Int -> [Int] -> Value -> [Value]
splitArray i splits (ArrayVal arr et shape) =
  [ ArrayVal (listArray (0, product splitshape-1) splitarr) et splitshape
  | (splitarr, splitshape) <- split i splits shape (elems arr) ]
splitArray _ _ v = [v]

split :: Int -> [Int] -> [Int] -> [a] -> [([a],[Int])]
split 0 ss (_:ds) xs =
  let rows = chunk (product ds) xs
      mkSplit n m = (concat $ take (m-n) $ drop n rows,
                     (m-n) : ds)
  in zipWith mkSplit (scanl (+) 0 ss) (scanl1 (+) ss)
split i ss (d:ds) xs =
  let rows = chunk (product ds) xs
  in case map (split (i-1) ss ds) rows of
    [] -> []
    r:rs -> let (splits, shapes) = unzip $ foldl combine r rs
            in zip splits $ map (d:) shapes
  where combine :: [([a],[Int])] -> [([a],[Int])] -> [([a],[Int])]
        combine splits_and_shapes1 splits_and_shapes2 =
          let (splits1, shapes1) = unzip splits_and_shapes1
              (splits2, _) = unzip splits_and_shapes2
          in zip (zipWith (++) splits1 splits2) shapes1
split _ _ ds xs = [(xs, ds)]

-- | The row-major flat indices of the given slice in an array of the
-- given shape.
flatSlice :: Slice Int -> [Int] -> [Int]
flatSlice slice shape = indices slice shape [0..product shape-1]

indices :: Slice Int -> [Int] -> [a] -> [a]
indices (DimFix i:is) (_:ds) xs =
  let rows = chunk (product ds) xs
  in case drop i rows of
       [] -> error "Values indices: out of bounds"
       xs':_ -> indices is ds xs'
indices (DimSlice i n:is) (_:ds) xs =
  let rows = chunk (product ds) xs
      sliced_rows = take n $ drop i rows
  in concatMap (indices is ds) sliced_rows
indices _ _ xs = xs
