{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Futhark.Optimise.Fusion.TryFusion
  ( TryFusion
  , tryFusion
  , liftMaybe
  , liftNeedNames
  , liftMaybeNeedNames
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Prelude

import Futhark.Representation.SOACS
import Futhark.NeedNames
import Futhark.MonadFreshNames

newtype TryFusion a = TryFusion (ReaderT (TypeEnv (NameType SOACS))
                                 (StateT VNameSource Maybe)
                                 a)
  deriving (Functor, Applicative, Alternative, Monad,
            MonadReader (TypeEnv (NameType SOACS)),
            MonadFreshNames,
            HasTypeEnv (NameType SOACS),
            LocalTypeEnv (NameType SOACS))

tryFusion :: MonadFreshNames m =>
             TryFusion a -> TypeEnv (NameType SOACS) -> m (Maybe a)
tryFusion (TryFusion m) types = modifyNameSource $ \src ->
  case runStateT (runReaderT m types) src of
    Just (x, src') -> (Just x, src')
    Nothing        -> (Nothing, src)

liftMaybe :: Maybe a -> TryFusion a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x

liftNeedNames :: NeedNames a -> TryFusion a
liftNeedNames = provideNames

liftMaybeNeedNames :: NeedNames (Maybe a) -> TryFusion a
liftMaybeNeedNames m = do x <- liftNeedNames m
                          liftMaybe x
