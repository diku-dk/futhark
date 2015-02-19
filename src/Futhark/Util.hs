module Futhark.Util
       (mapAccumLM)
       where

-- | Like 'mapAccumL', but monadic.
mapAccumLM :: Monad m =>
              (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
  (acc', x') <- f acc x
  (acc'', xs') <- mapAccumLM f acc' xs
  return (acc'', x':xs')
