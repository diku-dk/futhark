-- | Non-Futhark-specific utilities.  If you find yourself writing
-- general functions on generic data structures, consider putting them
-- here.
--
-- Sometimes it is also preferable to copy a small function rather
-- than introducing a large dependency.  In this case, make sure to
-- note where you got it from (and make sure that the license is
-- compatible).
module Futhark.Util
       (mapAccumLM,
        chunk,
        mapEither,
        splitAt3
        )
       where

import Data.Either

-- | Like 'mapAccumL', but monadic.
mapAccumLM :: Monad m =>
              (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
  (acc', x') <- f acc x
  (acc'', xs') <- mapAccumLM f acc' xs
  return (acc'', x':xs')

-- | @chunk n a@ splits @a@ into @n@-size-chunks.  If the length of
-- @a@ is not divisible by @n@, the last chunk will have fewer than
-- @n@ elements (but it will never be empty).
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
  let (bef,aft) = splitAt n xs
  in bef : chunk n aft

-- | A combination of 'map' and 'partitionEithers'.
mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither f l = partitionEithers $ map f l

-- | Like 'splitAt', but produces three lists.
splitAt3 :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt3 n m l =
  let (xs, l') = splitAt n l
      (ys, zs) = splitAt m l'
  in (xs, ys, zs)
