module Futhark.SoP.Util
  ( anyM,
    allM,
    ifM,
    toMS,
    localS,
  )
where

import Control.Monad.State
import Data.Foldable
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb mt mf = do
  b <- mb
  if b then mt else mf

anyM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
anyM p = foldr (\a b -> ifM (p a) (pure True) b) (pure False)

allM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
allM p = foldr (\a b -> ifM (p a) b (pure False)) (pure True)

toMS :: (Ord a, Foldable t) => t a -> MultiSet a
toMS = MS.fromList . Data.Foldable.toList

localS :: Monad m => StateT s m a -> StateT s m a
localS m = do
  env <- get
  a <- m
  put env
  pure a
