module Futhark.Util.BiMap
  ( BiMap,
    insert,
    lookupRight,
    lookupLeft,
  )
where

import Data.Map qualified as M

data BiMap a b = BiMap (M.Map a b) (M.Map b a)
  deriving (Eq, Ord, Show)

instance (Ord a, Ord b) => Monoid (BiMap a b) where
  mempty = BiMap mempty mempty

instance (Ord a, Ord b) => Semigroup (BiMap a b) where
  BiMap r1 l1 <> BiMap r2 l2 = BiMap (r1 <> r2) (l1 <> l2)

insert :: (Ord a, Ord b) => a -> b -> BiMap a b -> BiMap a b
insert l r (BiMap mr ml) = BiMap (M.insert l r mr) (M.insert r l ml)

lookupRight :: (Ord a) => a -> BiMap a b -> Maybe b
lookupRight l (BiMap mr _) = M.lookup l mr

lookupLeft :: (Ord b) => b -> BiMap a b -> Maybe a
lookupLeft r (BiMap _ ml) = M.lookup r ml
