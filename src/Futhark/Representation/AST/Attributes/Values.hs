module Futhark.Representation.AST.Attributes.Values
       (
         valueType
       , valueShape
       , valueSize
       , IsValue (..)
       , intconst

         -- * Rearranging
       , permuteShape
       , permuteArray
       , permuteInverse
       , permuteReach
       , permuteCompose
       , transposeIndex

         -- * Miscellaneous
       , arrayString
       )
       where

import Data.Array
import Data.List
import Data.Ord

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Constants

-- | Return the type of the given value.
valueType :: Value -> Type
valueType (BasicVal v) =
  Basic $ basicValueType v
valueType (ArrayVal _ et shape) =
  Array et (Shape $ map (Constant . IntVal) shape) Nonunique

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

-- | Calculate the given permutation of the list.  It is an error if
-- the permutation goes out of bounds.
permuteShape :: [Int] -> [a] -> [a]
permuteShape perm l = map (l!!) perm

permuteArray :: [Int] -> Value -> Value
permuteArray perm (ArrayVal inarr et oldshape) =
  let newshape = move oldshape
      idx is shape = sum (zipWith (*) is (map product $ drop 1 (tails shape)))
  in ArrayVal (listArray (bounds inarr)
               [ inarr ! idx (invmove is) oldshape
               | is <- map reverse $ picks $ reverse newshape ])
     et newshape
  where move = permuteShape perm
        invmove = permuteShape $ permuteInverse perm
        picks [] = []
        picks [n] = map (:[]) [0..n-1]
        picks (n:ns) = [ i:is | is <- picks ns, i <- [0..n-1] ]
permuteArray _ v = v

-- | Produce the inverse permutation.
permuteInverse :: [Int] -> [Int]
permuteInverse perm = map snd $ sortBy (comparing fst) $ zip perm [0..]

-- | Return the first dimension not affected by the permutation.  For
-- example, the permutation @[1,0,2]@ would return @2@.
permuteReach :: [Int] -> Int
permuteReach perm = case dropWhile (uncurry (/=)) $ zip (tails perm) (tails [0..n-1]) of
                      []          -> n + 1
                      (perm',_):_ -> n - length perm'
  where n = length perm

-- | Compose two permutations, with the second given permutation being
-- applied first.
permuteCompose :: [Int] -> [Int] -> [Int]
permuteCompose = permuteShape

-- | If @l@ is an index into the array @a@, then @transposeIndex k n
-- l@ is an index to the same element in the array @transposeArray k n
-- a@.
transposeIndex :: Int -> Int -> [a] -> [a]
transposeIndex k n l
  | k + n >= length l =
    let n' = ((k + n) `mod` length l)-k
    in transposeIndex k n' l
  | n < 0,
    (pre,needle:end) <- splitAt k l,
    (beg,mid) <- splitAt (length pre+n) pre =
    beg ++ [needle] ++ mid ++ end
  | (beg,needle:post) <- splitAt k l,
    (mid,end) <- splitAt n post =
    beg ++ mid ++ [needle] ++ end
  | otherwise = l

-- | If the given value is a nonempty array containing only
-- characters, return the corresponding 'String', otherwise return
-- 'Nothing'.
arrayString :: Value -> Maybe String
arrayString (ArrayVal arr _ _)
  | c:cs <- elems arr = mapM asChar $ c:cs
  where asChar (CharVal c) = Just c
        asChar _           = Nothing
arrayString _ = Nothing
