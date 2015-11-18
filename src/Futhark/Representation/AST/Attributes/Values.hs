-- | Queries and operations on values.  Useful for the interpreter and
-- constant folding.
module Futhark.Representation.AST.Attributes.Values
       (
         valueType
       , valueShape
       , valueSize
       , IsValue (..)
       , intconst

         -- * Rearranging
       , permuteArray
       , stripeArray
       , unstripeArray

         -- * Miscellaneous
       , arrayString
       )
       where

import Data.Array
import Data.List

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Attributes.Rearrange
import Futhark.Representation.AST.Attributes.Stripe

-- | Return the type of the given value.
valueType :: Value -> Type
valueType (BasicVal v) =
  Basic $ basicValueType v
valueType (ArrayVal _ et shape) =
  Array et (Shape $ map (Constant . IntVal . fromIntegral) shape) Nonunique

-- | Return the size of the first dimension of an array, or zero for
-- non-arrays.
valueSize :: Value -> Int
valueSize t = case valueShape t of
                []  -> 0
                n:_ -> n

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

-- | Stripe the elements of an array value.  If the given value is
-- not an array, it is returned unchanged.
stripeArray :: Int -> Value -> Value
stripeArray stride (ArrayVal inarr et shape) =
  ArrayVal (listArray (0, upper_bound)
            [ inarr ! i | i <- stripeIndices n stride ])
  et shape
  where upper_bound = snd $ bounds inarr
        n = upper_bound + 1
stripeArray _ v = v

-- | Inversely stripe the elements of an array value.  If the given value is
-- not an array, it is returned unchanged.
unstripeArray :: Int -> Value -> Value
unstripeArray stride (ArrayVal inarr et shape) =
  ArrayVal (listArray (0, upper_bound)
            [ inarr ! i | i <- stripeIndicesInverse n stride ])
  et shape
  where upper_bound = snd $ bounds inarr
        n = upper_bound + 1
unstripeArray _ v = v

-- | If the given value is a nonempty array containing only
-- characters, return the corresponding 'String', otherwise return
-- 'Nothing'.
arrayString :: Value -> Maybe String
arrayString (ArrayVal arr _ _)
  | c:cs <- elems arr = mapM asChar $ c:cs
  where asChar (CharVal c) = Just c
        asChar _           = Nothing
arrayString _ = Nothing
