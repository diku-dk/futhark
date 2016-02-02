{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Futhark.Optimise.Fusion.TryFusion
  ( TryFusion
  , tryFusion
  , fmapaybe
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Prelude

import Futhark.Representation.SOACS
import Futhark.MonadFreshNames

newtype TryFusion a = TryFusion (ReaderT (Scope SOACS)
                                 (StateT VNameSource Maybe)
                                 a)
  deriving (Functor, Applicative, Alternative, Monad,
            MonadFreshNames,
            HasScope SOACS,
            LocalScope SOACS)

tryFusion :: MonadFreshNames m =>
             TryFusion a -> Scope SOACS -> m (Maybe a)
tryFusion (TryFusion m) types = modifyNameSource $ \src ->
  case runStateT (runReaderT m types) src of
    Just (x, src') -> (Just x, src')
    Nothing        -> (Nothing, src)

fmapaybe :: Maybe a -> TryFusion a
fmapaybe Nothing = fail "Nothing"
fmapaybe (Just x) = return x
