{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
-- | This module exports 'Simplify', the monad used for writing
-- simplification rules.
module Futhark.Optimise.Simplifier.Simplify
  ( Simplify
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

newtype Simplify m a = Simplify (MaybeT m a)
  deriving (Functor, Applicative, Monad)

instance MonadFreshNames m => MonadFreshNames (Simplify m) where
  getNameSource = Simplify . lift $ getNameSource
  putNameSource = Simplify . lift . putNameSource

instance BindableM m => BindableM (Simplify m) where
  type Lore (Simplify m) = Lore m
  mkLetM pat e = Simplify $ lift $ mkLetM pat e
  mkLetNamesM names e = Simplify $ lift $ mkLetNamesM names e
  mkBodyM bnds res = Simplify $ lift $ mkBodyM bnds res

instance MonadBinder m => MonadBinder (Simplify m) where
  addBinding                   = Simplify . lift . addBinding
  collectBindings (Simplify m) = Simplify $ MaybeT $ do
    (x, bnds) <- collectBindings $ runMaybeT m
    case x of Nothing -> return Nothing
              Just x' -> return $ Just (x', bnds)

instance MonadBinder m => Alternative (Simplify m) where
  empty = Simplify $ MaybeT $ return Nothing
  Simplify m1 <|> Simplify m2 = Simplify $ do
    (x, bnds) <- lift $ collectBindings $ runMaybeT m1
    case x of Nothing -> m2
              Just x' -> do lift $ mapM_ addBinding bnds
                            return x'
simplify :: MonadBinder m =>
            Simplify m a
         -> m (Maybe (a, [Binding (Lore m)]))
simplify (Simplify m) = do
  (x, bnds) <- collectBindings $ runMaybeT m
  case x of
    Just x' -> return $ Just (x', bnds)
    Nothing -> return Nothing

cannotSimplify :: Monad m => Simplify m a
cannotSimplify = fail "Cannot simplify"

liftMaybe :: Monad m => Maybe a -> Simplify m a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x
