{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
-- | This module defines a convenience monad/typeclass for creating
-- normalised programs.
module Futhark.Binder
  ( -- * A concrete @MonadBinder@ monad.
    BinderT
  , runBinderT
  , Binder
  , runBinder
  , runBinder_
  , joinBinder
  , runBodyBinder
  , runBinderEmptyEnv
  -- * Non-class interface
  , addBinderBinding
  , collectBinderBindings
  -- * The 'MonadBinder' typeclass
  , module Futhark.Binder.Class
  )
where

import qualified Data.DList as DL
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error.Class
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Binder.Class
import Futhark.Representation.AST
import Futhark.MonadFreshNames

newtype BinderT lore m a = BinderT (StateT
                                    (Scope lore)
                                    (WriterT
                                     (DL.DList (Binding lore))
                                     m)
                                    a)
  deriving (Functor, Monad, Applicative,
            MonadWriter (DL.DList (Binding lore)))
-- Cannot add MonadState instance, because it would conflict with the
-- utility instances.

instance MonadTrans (BinderT lore) where
  lift = BinderT . lift . lift

type Binder lore = BinderT lore (State VNameSource)

instance MonadFreshNames m => MonadFreshNames (BinderT lore m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (Attributes lore, Applicative m, Monad m) =>
         HasScope lore (BinderT lore m) where
  lookupType name = do
    t <- BinderT $ gets $ HM.lookup name
    case t of
      Nothing -> fail $ "BinderT.lookupType: unknown variable " ++ pretty name
      Just t' -> return $ typeOf t'
  askScope = BinderT get

instance (Attributes lore, Applicative m, Monad m) =>
         LocalScope lore (BinderT lore m) where
  localScope types (BinderT m) = BinderT $ do
    modify (`HM.union` types)
    x <- m
    modify (`HM.difference` types)
    return x

instance (Attributes lore, Bindable lore, MonadFreshNames m) =>
         MonadBinder (BinderT lore m) where
  type Lore (BinderT lore m) = lore
  mkBodyM bnds res = return $ mkBody bnds res
  mkLetM pat e =
    return $ mkLet
    (map asPair $ patternContextElements pat)
    (map asPair $ patternValueElements pat)
    e
    where asPair patElem = (patElemIdent patElem, patElemBindage patElem)
  mkLetNamesM = mkLetNames

  addBinding      = addBinderBinding
  collectBindings = collectBinderBindings

runBinderT :: Monad m =>
              BinderT lore m a
           -> Scope lore
           -> m (a, [Binding lore])
runBinderT (BinderT m) types = do
  (x, bnds) <- runWriterT $ evalStateT m types
  return (x, DL.toList bnds)

runBinder :: (MonadFreshNames m, HasScope somelore m, SameScope somelore lore) =>
              Binder lore a
           -> m (a, [Binding lore])
runBinder m = do
  types <- askScope
  modifyNameSource $ runState $ runBinderT m $ castScope types

-- | Like 'runBinder', but throw away the result and just return the
-- added bindings.
runBinder_ :: (MonadFreshNames m, HasScope somelore m, SameScope somelore lore) =>
              Binder lore a
           -> m [Binding lore]
runBinder_ = fmap snd . runBinder

-- | As 'runBinder', but uses 'addBinding' to add the returned
-- bindings to the surrounding monad.
joinBinder :: MonadBinder m =>
              Binder (Lore m) a
           -> m a
joinBinder m = do (x, bnds) <- runBinder m
                  mapM_ addBinding bnds
                  return x

runBodyBinder :: (Bindable lore, MonadFreshNames m,
                  HasScope somelore m, SameScope somelore lore) =>
                 Binder lore (Body lore) -> m (Body lore)
runBodyBinder = fmap (uncurry $ flip insertBindings) . runBinder

runBinderEmptyEnv :: MonadFreshNames m =>
                     Binder lore a -> m (a, [Binding lore])
runBinderEmptyEnv m =
  modifyNameSource $ runState $ runBinderT m mempty

addBinderBinding :: (Annotations lore, Monad m) =>
                    Binding lore -> BinderT lore m ()
addBinderBinding binding = do
  tell $ DL.singleton binding
  BinderT $ modify (`HM.union` scopeOf binding)

collectBinderBindings :: (Annotations lore, Monad m) =>
                         BinderT lore m a
                      -> BinderT lore m (a, [Binding lore])
collectBinderBindings m = pass $ do
  (x, bnds) <- listen m
  let bnds' = DL.toList bnds
  BinderT $ modify (`HM.difference` scopeOf bnds')
  return ((x, bnds'), const DL.empty)

-- Utility instance defintions for MTL classes.  These require
-- UndecidableInstances, but save on typing elsewhere.
instance MonadReader r m => MonadReader r (BinderT lore m) where
  ask = BinderT ask
  local f (BinderT m) = BinderT $ local f m

instance MonadState s m => MonadState s (BinderT lore m) where
  get = BinderT $ lift get
  put = BinderT . lift . put

instance MonadError e m => MonadError e (BinderT lore m) where
  throwError = lift . throwError
  catchError (BinderT m) f =
    BinderT $ catchError m $ unBinder . f
    where unBinder (BinderT m') = m'
