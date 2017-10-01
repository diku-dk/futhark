{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
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

-- | The monad in which simplification rules are evaluated.
newtype RuleM m a = RuleM (MaybeT m a)
  deriving (Functor, Applicative, Monad)

instance MonadFreshNames m => MonadFreshNames (RuleM m) where
  getNameSource = RuleM . lift $ getNameSource
  putNameSource = RuleM . lift . putNameSource

instance (Monad m, HasScope t m) => HasScope t (RuleM m) where
  lookupType = RuleM . lift . lookupType
  askScope  = RuleM . lift $ askScope

instance (Monad m, LocalScope t m) => LocalScope t (RuleM m) where
  localScope types (RuleM m) = RuleM $ do
    x <- lift $ localScope types $ runMaybeT m
    MaybeT $ return x

instance MonadBinder m => MonadBinder (RuleM m) where
  type Lore (RuleM m) = Lore m
  mkExpAttrM pat e = RuleM $ lift $ mkExpAttrM pat e
  mkLetNamesM names e = RuleM $ lift $ mkLetNamesM names e
  mkBodyM bnds res = RuleM $ lift $ mkBodyM bnds res

  addStm                = RuleM . lift . addStm
  collectStms (RuleM m) = RuleM $ MaybeT $ do
    (x, bnds) <- collectStms $ runMaybeT m
    case x of Nothing -> return Nothing
              Just x' -> return $ Just (x', bnds)
  certifying cs (RuleM m) = RuleM $ MaybeT $
    certifying cs $ runMaybeT m

instance MonadBinder m => Alternative (RuleM m) where
  empty = RuleM $ MaybeT $ return Nothing
  RuleM m1 <|> RuleM m2 = RuleM $ do
    (x, bnds) <- lift $ collectStms $ runMaybeT m1
    case x of Nothing -> m2
              Just x' -> do lift $ mapM_ addStm bnds
                            return x'

-- | Execute a 'RuleM' action.  If succesful, returns the result and a
-- list of new bindings.  Even if the action fail, there may still be
-- a monadic effect - particularly, the name source may have been
-- modified.
simplify :: MonadBinder m =>
            RuleM m a
         -> m (Maybe (a, [Stm (Lore m)]))
simplify (RuleM m) = do
  (x, bnds) <- collectStms $ runMaybeT m
  case x of
    Just x' -> return $ Just (x', bnds)
    Nothing -> return Nothing

cannotSimplify :: Monad m => RuleM m a
cannotSimplify = fail "Cannot simplify"

liftMaybe :: Monad m => Maybe a -> RuleM m a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x
