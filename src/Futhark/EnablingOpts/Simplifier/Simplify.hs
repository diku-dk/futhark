{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module exports 'Simplify', the monad used for writing
-- simplification rules.
module Futhark.EnablingOpts.Simplifier.Simplify
  ( Simplify
  , simplify
  , cannotSimplify
  , liftMaybe
  , liftNeedNames
  , liftMaybeNeedNames
  )

where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Futhark.InternalRep
import Futhark.NeedNames
import Futhark.MonadFreshNames
import Futhark.Binder

newtype Simplify a = Simplify (MaybeT Binder a)
  deriving (Functor, Applicative,
            Monad, MonadState (NameSource VName))

instance MonadFreshNames Simplify where
  getNameSource = get
  putNameSource = put

instance MonadBinder Simplify where
  addBinding                   = Simplify . lift . addBinding
  collectBindings (Simplify m) = Simplify $ MaybeT $ do
    (x, bnds) <- collectBindings $ runMaybeT m
    case x of Nothing -> return Nothing
              Just x' -> return $ Just (x', bnds)

instance Alternative Simplify where
  empty = Simplify $ MaybeT $ return Nothing
  Simplify m1 <|> Simplify m2 = Simplify $ do
    (x, bnds) <- lift $ collectBindings $ runMaybeT m1
    case x of Nothing -> m2
              Just x' -> do lift $ mapM_ addBinding bnds
                            return x'

simplify :: MonadFreshNames m => Simplify [Binding] -> m (Maybe [Binding])
simplify (Simplify m) = modifyNameSource $ \src ->
  case runBinderWithNameSource (runMaybeT m) src of
    ((Just bnds, extrabnds), src') -> (Just $ extrabnds ++ bnds, src')
    ((Nothing, _), _)              -> (Nothing, src)

cannotSimplify :: Simplify a
cannotSimplify = fail "Cannot simplify"

liftMaybe :: Maybe a -> Simplify a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x

liftNeedNames :: NeedNames a -> Simplify a
liftNeedNames = provideNames

liftMaybeNeedNames :: NeedNames (Maybe a) -> Simplify a
liftMaybeNeedNames m = do x <- liftNeedNames m
                          liftMaybe x
