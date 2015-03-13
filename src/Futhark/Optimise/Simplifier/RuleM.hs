{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
-- | This module exports 'RuleM', the monad used for writing
-- simplification rules.
module Futhark.Optimise.Simplifier.RuleM
  ( RuleM
  , simplify
  , cannotSimplify
  , liftMaybe
  )

where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder

newtype RuleM m a = RuleM (MaybeT m a)
  deriving (Functor, Applicative, Monad)

instance MonadFreshNames m => MonadFreshNames (RuleM m) where
  getNameSource = RuleM . lift $ getNameSource
  putNameSource = RuleM . lift . putNameSource

instance (Monad m, HasTypeEnv m) => HasTypeEnv (RuleM m) where
  lookupTypeM = RuleM . lift . lookupTypeM
  askTypeEnv  = RuleM . lift $ askTypeEnv

instance MonadBinder m => MonadBinder (RuleM m) where
  type Lore (RuleM m) = Lore m
  mkLetM pat e = RuleM $ lift $ mkLetM pat e
  mkLetNamesM names e = RuleM $ lift $ mkLetNamesM names e
  mkBodyM bnds res = RuleM $ lift $ mkBodyM bnds res

  addBinding                = RuleM . lift . addBinding
  collectBindings (RuleM m) = RuleM $ MaybeT $ do
    (x, bnds) <- collectBindings $ runMaybeT m
    case x of Nothing -> return Nothing
              Just x' -> return $ Just (x', bnds)

instance MonadBinder m => Alternative (RuleM m) where
  empty = RuleM $ MaybeT $ return Nothing
  RuleM m1 <|> RuleM m2 = RuleM $ do
    (x, bnds) <- lift $ collectBindings $ runMaybeT m1
    case x of Nothing -> m2
              Just x' -> do lift $ mapM_ addBinding bnds
                            return x'
simplify :: MonadBinder m =>
            RuleM m a
         -> m (Maybe (a, [Binding (Lore m)]))
simplify (RuleM m) = do
  (x, bnds) <- collectBindings $ runMaybeT m
  case x of
    Just x' -> return $ Just (x', bnds)
    Nothing -> return Nothing

cannotSimplify :: Monad m => RuleM m a
cannotSimplify = fail "Cannot simplify"

liftMaybe :: Monad m => Maybe a -> RuleM m a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x
