{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Futhark.SoP.Util
  ( anyM,
    allM,
    ifM,
    toMS,
    localS,
    type (>=),
    type (==),
    (^&&),
    (^||),
    andM,
    orM,
    asumM,
  )
where

import Control.Applicative
import Control.Monad.State
import Data.Foldable
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import GHC.TypeLits (Natural)

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mb mt mf = do
  b <- mb
  if b then mt else mf

(^&&) :: (Monad m) => m Bool -> m Bool -> m Bool
x ^&& y = ifM x y (pure False)

infixr 3 ^&&

(^||) :: (Monad m) => m Bool -> m Bool -> m Bool
x ^|| y = ifM x (pure True) y

infixr 2 ^||

andM :: (Monad m, Foldable t) => t (m Bool) -> m Bool
andM = allM id

orM :: (Monad m, Foldable t) => t (m Bool) -> m Bool
orM = anyM id

anyM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
anyM p = foldr (\a b -> ifM (p a) (pure True) b) (pure False)

allM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
allM p = foldr (\a b -> ifM (p a) b (pure False)) (pure True)

toMS :: (Ord a, Foldable t) => t a -> MultiSet a
toMS = MS.fromList . Data.Foldable.toList

localS :: (MonadState s m) => (s -> s) -> m a -> m a
localS f m = do
  env <- get
  modify f
  a <- m
  put env
  pure a

-- | A type label to indicate @a >= 0@.
type a >= (b :: Natural) = a

-- | A type label to indicate @a = 0@.
type a == (b :: Natural) = a

asumM :: (Monad m, Traversable t, Alternative f) => t (m (f a)) -> m (f a)
asumM = (fmap asum) . sequence
