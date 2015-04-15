{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.Basic
import Futhark.NeedNames
import Futhark.MonadFreshNames

newtype TryFusion a = TryFusion (ReaderT TypeEnv
                                 (StateT VNameSource Maybe)
                                 a)
  deriving (Functor, Applicative, Alternative, Monad,
            MonadReader TypeEnv,
            MonadState (NameSource VName))

instance MonadFreshNames TryFusion where
  getNameSource = get
  putNameSource = put

instance HasTypeEnv TryFusion where
  lookupType name =
    maybe notFound return =<< asks (HM.lookup name)
    where notFound =
            fail $ "Variable " ++ pretty name ++ " not found in symbol table"
  askTypeEnv = ask

tryFusion :: MonadFreshNames m =>
             TryFusion a -> TypeEnv -> m (Maybe a)
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
