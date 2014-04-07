{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.HOTrans.TryFusion
  ( TryFusion
  , tryFusion
  , liftMaybe
  , liftNeedNames
  , liftMaybeNeedNames
  )
  where

import Control.Applicative
import Control.Monad.State

import Futhark.InternalRep
import Futhark.NeedNames
import Futhark.MonadFreshNames

newtype TryFusion a = TryFusion (StateT VNameSource Maybe a)
  deriving (Functor, Applicative, Alternative,
            Monad, MonadState (NameSource VName))

instance MonadFreshNames TryFusion where
  getNameSource = get
  putNameSource = put

tryFusion :: MonadFreshNames m => TryFusion a -> m (Maybe a)
tryFusion (TryFusion m) = do
  src <- getNameSource
  case runStateT m src of
    Just (x, src') -> do
      putNameSource src'
      return $ Just x
    Nothing -> return Nothing

liftMaybe :: Maybe a -> TryFusion a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x

liftNeedNames :: NeedNames a -> TryFusion a
liftNeedNames = provideNames

liftMaybeNeedNames :: NeedNames (Maybe a) -> TryFusion a
liftMaybeNeedNames m = do x <- liftNeedNames m
                          liftMaybe x
