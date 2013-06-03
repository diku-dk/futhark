module L0.Util ( all2 )
  where

-- | Like 'all', but with two lists.  The lists need not have the same
-- length for the predicate to be true.
all2 :: (a -> b -> Bool) -> [a] -> [b] -> Bool
all2 p xs ys = all id $ zipWith p xs ys
