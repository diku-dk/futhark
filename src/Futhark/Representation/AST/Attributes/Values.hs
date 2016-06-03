{-# LANGUAGE RankNTypes #-}
-- | Queries and operations on values.  Useful for the interpreter and
-- constant folding.
module Futhark.Representation.AST.Attributes.Values
       (
         valueType
       , valueShape

         -- * Rearranging
       , permuteArray
       , rotateArray
       )
       where

import Data.Array
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

-- | Rotate the elements of an array as per the 'Rotate' PrimOp.
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
        | k > 0 = drop (d - k) xs_rotated ++ take (d - k) xs_rotated
        | otherwise = drop (-k) xs_rotated ++ take (-k) xs_rotated
  in concat new_rows
rotate _ _ xs = xs
