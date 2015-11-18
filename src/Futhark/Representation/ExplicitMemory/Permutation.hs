{-# LANGUAGE GADTs, DataKinds, KindSignatures, ScopedTypeVariables, StandaloneDeriving #-}
-- | A well-typed representation of permutations.
--
-- For a 'Vector' of size @n@, one could represent a permutation as
-- simply a list @[Ordinal n]@, but this might not necessarily be a
-- valid permutation, as there may be duplicate or missing indices.
-- This module solves that problem by representing a permutation as a
-- sequence of 'Swap's of the elements at two indices.  The use of
-- 'Ordinal's guarentees that the swaps are in bounds.
module Futhark.Representation.ExplicitMemory.Permutation
       (
         Swap (..)
       , Permutation (..)
       , apply
       , invert
       )
       where

import Data.Type.Natural
import Data.Vector.Sized
import Futhark.Util.Pretty

-- | The value 'i :<->: j' represents the action of swapping the
-- elements at index 'i' and 'i'.
data Swap :: Nat -> * where
  (:<->:) :: Index n -> Index n -> Swap n

infixr 5 :<->:

instance Show (Swap n) where
  show (n :<->: m) = show n ++ " :<->: " ++ show m

instance Eq (Swap n) where
  (x1 :<->: y1) == (x2 :<->: y2) =
    (x1 == x2 && y1 == y2) ||
    (y1 == x2 && y2 == x1)

-- | A permutation is conceptually a sequence of 'Swap's.  It may even
-- be infinite - although it's not clear what this means.  More
-- importantly, the 'Permutation' may not be the shortest possible
-- permutation.  You are advised not to use this representation for
-- very large arrays.
--
-- @s :>>: p@ applies first @s@, then @p@.
data Permutation :: Nat -> * where
  Identity :: Permutation n
  (:>>:) :: Swap n -> Permutation n -> Permutation n

deriving instance (Eq (Permutation n))

infixr 4 :>>:

instance Show (Permutation n) where
  show Identity = "Identity"
  show (s :>>: perm) = show s ++ " :>>: " ++ show perm

instance Pretty (Permutation n) where
  ppr origperm = text "<" <> commasep (ppr' origperm) <> text ">"
    where ppr' (swap :>>: perm') =
            text (show swap) : ppr' perm'
          ppr' Identity =
            []

-- | Apply a permutation to a vector, yielding a new vector.
apply :: forall a n.Permutation n -> Vector a n -> Vector a n
apply Identity vec = vec
apply (from :<->: to :>>: perm) vec =
  apply perm $ update 0 vec
  where from' = ordToInt from
        to'   = ordToInt to
        fromV = vec %!! from
        toV   = vec %!! to
        -- The only way to update elements of a vector seems to be to
        -- traverse it.
        update :: Int -> Vector a m -> Vector a m
        update _ Nil = Nil
        update i (x :- xs) | i == from' = toV   :- update (i+1) xs
                           | i == to'   = fromV :- update (i+1) xs
                           | otherwise  = x     :- update (i+1) xs

-- | Invert a permutation.
invert :: Permutation n -> Permutation n
invert Identity = Identity
invert (swap :>>: perm) =
  swap `snoc` invert perm

snoc :: Swap n -> Permutation n -> Permutation n
snoc swap Identity          = swap  :>>: Identity
snoc swap (swap2 :>>: perm) = swap2 :>>: swap `snoc` perm
