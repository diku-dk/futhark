{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
-- | This module defines a convenience monad/typeclass for creating
-- normalised programs.
module Futhark.Binder
  ( -- * A concrete @MonadBinder@ monad.
    BinderT
  , runBinderT
  , BinderOps (..)
  , bindableMkExpAttrB
  , bindableMkBodyB
  , bindableMkLetNamesB
  , Binder
  , runBinder
  , runBinder_
  , joinBinder
  , runBodyBinder
  -- * Non-class interface
  , addBinderStms
  , collectBinderStms
  , certifyingBinder
  -- * The 'MonadBinder' typeclass
  , module Futhark.Binder.Class
  )
where

import Control.Arrow (second)
import Control.Monad.Writer
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Error.Class
import qualified Data.Map.Strict as M
import qualified Control.Monad.Fail as Fail

import Futhark.Binder.Class
import Futhark.Representation.AST

class Attributes lore => BinderOps lore where
  mkExpAttrB :: (MonadBinder m, Lore m ~ lore) =>
                Pattern lore -> Exp lore -> m (ExpAttr lore)
  mkBodyB :: (MonadBinder m, Lore m ~ lore) =>
             Stms lore -> Result -> m (Body lore)
  mkLetNamesB :: (MonadBinder m, Lore m ~ lore) =>
                 [VName] -> Exp lore -> m (Stm lore)

bindableMkExpAttrB :: (MonadBinder m, Bindable (Lore m)) =>
                      Pattern (Lore m) -> Exp (Lore m) -> m (ExpAttr (Lore m))
bindableMkExpAttrB pat e = return $ mkExpAttr pat e

bindableMkBodyB :: (MonadBinder m, Bindable (Lore m)) =>
                   Stms (Lore m) -> Result -> m (Body (Lore m))
bindableMkBodyB stms res = return $ mkBody stms res

bindableMkLetNamesB :: (MonadBinder m, Bindable (Lore m)) =>
                       [VName] -> Exp (Lore m) -> m (Stm (Lore m))
bindableMkLetNamesB = mkLetNames

newtype BinderT lore m a = BinderT (StateT (Stms lore, Scope lore) m a)
  deriving (Functor, Monad, Applicative)

instance MonadTrans (BinderT lore) where
  lift = BinderT . lift

instance Monad m => Fail.MonadFail (BinderT lore m) where
  fail = error . ("BinderT.fail: "++)

type Binder lore = BinderT lore (State VNameSource)

instance MonadFreshNames m => MonadFreshNames (BinderT lore m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (Attributes lore, Monad m) =>
         HasScope lore (BinderT lore m) where
  lookupType name = do
    t <- BinderT $ gets $ M.lookup name . snd
    case t of
      Nothing -> error $ "BinderT.lookupType: unknown variable " ++ pretty name
      Just t' -> return $ typeOf t'
  askScope = BinderT $ gets snd

instance (Attributes lore, Monad m) =>
         LocalScope lore (BinderT lore m) where
  localScope types (BinderT m) = BinderT $ do
    modify $ second (M.union types)
    x <- m
    modify $ second (`M.difference` types)
    return x

instance (Attributes lore, MonadFreshNames m, BinderOps lore) =>
         MonadBinder (BinderT lore m) where
  type Lore (BinderT lore m) = lore
  mkExpAttrM = mkExpAttrB
  mkBodyM = mkBodyB
  mkLetNamesM = mkLetNamesB

  addStms     = addBinderStms
  collectStms = collectBinderStms

  certifying = certifyingBinder

runBinderT :: MonadFreshNames m =>
              BinderT lore m a
           -> Scope lore
           -> m (a, Stms lore)
runBinderT (BinderT m) scope = do
  (x, (stms, _)) <- runStateT m (mempty, scope)
  return (x, stms)

runBinder :: (MonadFreshNames m,
              HasScope somelore m, SameScope somelore lore) =>
              Binder lore a
           -> m (a, Stms lore)
runBinder m = do
  types <- askScope
  modifyNameSource $ runState $ runBinderT m $ castScope types

-- | Like 'runBinder', but throw away the result and just return the
-- added bindings.
runBinder_ :: (MonadFreshNames m,
               HasScope somelore m, SameScope somelore lore) =>
              Binder lore a
           -> m (Stms lore)
runBinder_ = fmap snd . runBinder

-- | As 'runBinder', but uses 'addStm' to add the returned
-- bindings to the surrounding monad.
joinBinder :: MonadBinder m => Binder (Lore m) a -> m a
joinBinder m = do (x, bnds) <- runBinder m
                  addStms bnds
                  return x

runBodyBinder :: (Bindable lore, MonadFreshNames m,
                  HasScope somelore m, SameScope somelore lore) =>
                 Binder lore (Body lore) -> m (Body lore)
runBodyBinder = fmap (uncurry $ flip insertStms) . runBinder

addBinderStms :: Monad m =>
                 Stms lore -> BinderT lore m ()
addBinderStms stms = BinderT $
  modify $ \(cur_stms,scope) -> (cur_stms<>stms,
                                 scope `M.union` scopeOf stms)

collectBinderStms :: Monad m =>
                     BinderT lore m a
                  -> BinderT lore m (a, Stms lore)
collectBinderStms m = do
  (old_stms, old_scope) <- BinderT get
  BinderT $ put (mempty, old_scope)
  x <- m
  (new_stms, _) <- BinderT get
  BinderT $ put (old_stms, old_scope)
  return (x, new_stms)

certifyingBinder :: (MonadFreshNames m, BinderOps lore) =>
                    Certificates -> BinderT lore m a
                 -> BinderT lore m a
certifyingBinder cs m = do
  (x, stms) <- collectStms m
  addStms $ certify cs <$> stms
  return x

-- Utility instance defintions for MTL classes.  These require
-- UndecidableInstances, but save on typing elsewhere.

mapInner :: Monad m =>
            (m (a, (Stms lore, Scope lore))
             -> m (b, (Stms lore, Scope lore)))
         -> BinderT lore m a -> BinderT lore m b
mapInner f (BinderT m) = BinderT $ do
  s <- get
  (x, s') <- lift $ f $ runStateT m s
  put s'
  return x

instance MonadReader r m => MonadReader r (BinderT lore m) where
  ask = BinderT $ lift ask
  local f = mapInner $ local f

instance MonadState s m => MonadState s (BinderT lore m) where
  get = BinderT $ lift get
  put = BinderT . lift . put

instance MonadWriter w m => MonadWriter w (BinderT lore m) where
  tell = BinderT . lift . tell
  pass = mapInner $ \m -> pass $ do
    ((x, f), s) <- m
    return ((x, s), f)
  listen = mapInner $ \m -> do
    ((x, s), y) <- listen m
    return ((x, y), s)

instance MonadError e m => MonadError e (BinderT lore m) where
  throwError = lift . throwError
  catchError (BinderT m) f =
    BinderT $ catchError m $ unBinder . f
    where unBinder (BinderT m') = m'
