module Futhark.Util
       (mapAccumLM,
        chunk)
       where

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
