module Futhark.Representation.AST.Attributes.Rearrange
       ( rearrangeShape
       , rearrangeInverse
       , rearrangeReach
       , rearrangeCompose
       , isPermutationOf
       , transposeIndex
       ) where

import Data.List
import Data.Ord

import Futhark.Util

-- | Calculate the given permutation of the list.  It is an error if
-- the permutation goes out of bounds.
rearrangeShape :: [Int] -> [a] -> [a]
rearrangeShape perm l = map pick perm
  where pick i
          | 0 <= i, i < n = l!!i
          | otherwise =
              error $ show perm ++ " is not a valid permutation for input."
        n = length l

-- | Produce the inverse permutation.
rearrangeInverse :: [Int] -> [Int]
rearrangeInverse perm = map snd $ sortBy (comparing fst) $ zip perm [0..]

-- | Return the first dimension not affected by the permutation.  For
-- example, the permutation @[1,0,2]@ would return @2@.
rearrangeReach :: [Int] -> Int
rearrangeReach perm = case dropWhile (uncurry (/=)) $ zip (tails perm) (tails [0..n-1]) of
                      []          -> n + 1
                      (perm',_):_ -> n - length perm'
  where n = length perm

-- | Compose two permutations, with the second given permutation being
-- applied first.
rearrangeCompose :: [Int] -> [Int] -> [Int]
rearrangeCompose = rearrangeShape

-- | Check whether the first list is a permutation of the second, and
-- if so, return the permutation.  This will also find identity
-- permutations (i.e. the lists are the same) The implementation is
-- naive and slow.
isPermutationOf :: Eq a => [a] -> [a] -> Maybe [Int]
isPermutationOf l1 l2 =
  case mapAccumLM (pick 0) (map Just l2) l1 of
    Just (l2', perm)
      | all (==Nothing) l2' -> Just perm
    _                       -> Nothing
  where pick :: Eq a => Int -> [Maybe a] -> a -> Maybe ([Maybe a], Int)
        pick _ [] _ = Nothing
        pick i (x:xs) y
          | Just y == x = Just (Nothing : xs, i)
          | otherwise = do
              (xs', v) <- pick (i+1) xs y
              return (x : xs', v)

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
