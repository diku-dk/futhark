module Futhark.Util.NDArray
  ( NDArray,
    fromList,
    shape,
    size,
    rank,
    (!),
    elems,
    flatten,
    mapWithIndex,
    mapMWithIndex,
    mapMWithIndex_,
  )
where

import Control.Monad (zipWithM, zipWithM_)
import Data.Array qualified as A

data NDArray a = NDArray [Int] (A.Array Int a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

fromList :: [Int] -> [a] -> NDArray a
fromList s l = NDArray s $ A.array (0, length l - 1) (zip [0 ..] l)

shape :: NDArray a -> [Int]
shape (NDArray s _) = s

size :: NDArray a -> Int
size = product . shape

rank :: NDArray a -> Int
rank = length . shape

(!) :: NDArray a -> [Int] -> a
(!) (NDArray s a) idx =
  let i = sum $ zipWith (*) (reverse idx) $ scanl (*) 1 s
   in a A.! i

elems :: NDArray a -> [a]
elems (NDArray _ a) = A.elems a

flatten :: NDArray a -> A.Array Int a
flatten (NDArray _ a) = a

indexOf :: [Int] -> Int -> [Int]
indexOf (d : ds) i = (i `mod` d) : indexOf ds (i `div` d)
indexOf [] _ = []

mapWithIndex :: ([Int] -> a -> b) -> NDArray a -> NDArray b
mapWithIndex f nd =
  fromList (shape nd) $ zipWith f (map (indexOf $ shape nd) [0 .. size nd]) $ elems nd

mapMWithIndex :: (Monad m) => ([Int] -> a -> m b) -> NDArray a -> m (NDArray b)
mapMWithIndex f nd =
  fromList (shape nd) <$> zipWithM f (map (indexOf $ shape nd) [0 .. size nd]) (elems nd)

mapMWithIndex_ :: (Monad m) => ([Int] -> a -> m b) -> NDArray a -> m ()
mapMWithIndex_ f nd =
  zipWithM_ f (map (indexOf $ shape nd) [0 .. size nd]) (elems nd)
