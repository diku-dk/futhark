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
  , addBinderStm
  , collectBinderStms
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
import qualified Data.Map.Strict as M

import Prelude

import Futhark.Binder.Class
import Futhark.Representation.AST
import Futhark.MonadFreshNames

newtype BinderT lore m a = BinderT (StateT
                                    (Scope lore)
                                    (WriterT
                                     (DL.DList (Stm lore))
                                     m)
                                    a)
  deriving (Functor, Monad, Applicative,
            MonadWriter (DL.DList (Stm lore)))
-- Cannot add MonadState instance, because it would conflict with the
-- utility instances.

instance MonadTrans (BinderT lore) where
  lift = BinderT . lift . lift

type Binder lore = BinderT lore (State VNameSource)

instance MonadFreshNames m => MonadFreshNames (BinderT lore m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (Attributes lore, Monad m) =>
         HasScope lore (BinderT lore m) where
  lookupType name = do
    t <- BinderT $ gets $ M.lookup name
    case t of
      Nothing -> fail $ "BinderT.lookupType: unknown variable " ++ pretty name
      Just t' -> return $ typeOf t'
  askScope = BinderT get

instance (Attributes lore, Monad m) =>
         LocalScope lore (BinderT lore m) where
  localScope types (BinderT m) = BinderT $ do
    modify (`M.union` types)
    x <- m
    modify (`M.difference` types)
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

  addStm      = addBinderStm
  collectStms = collectBinderStms

runBinderT :: Monad m =>
              BinderT lore m a
           -> Scope lore
           -> m (a, [Stm lore])
runBinderT (BinderT m) types = do
  (x, bnds) <- runWriterT $ evalStateT m types
  return (x, DL.toList bnds)

runBinder :: (MonadFreshNames m, HasScope somelore m, SameScope somelore lore) =>
              Binder lore a
           -> m (a, [Stm lore])
runBinder m = do
  types <- askScope
  modifyNameSource $ runState $ runBinderT m $ castScope types

-- | Like 'runBinder', but throw away the result and just return the
-- added bindings.
runBinder_ :: (MonadFreshNames m, HasScope somelore m, SameScope somelore lore) =>
              Binder lore a
           -> m [Stm lore]
runBinder_ = fmap snd . runBinder

-- | As 'runBinder', but uses 'addStm' to add the returned
-- bindings to the surrounding monad.
joinBinder :: MonadBinder m =>
              Binder (Lore m) a
           -> m a
joinBinder m = do (x, bnds) <- runBinder m
                  mapM_ addStm bnds
                  return x

runBodyBinder :: (Bindable lore, MonadFreshNames m,
                  HasScope somelore m, SameScope somelore lore) =>
                 Binder lore (Body lore) -> m (Body lore)
runBodyBinder = fmap (uncurry $ flip insertStms) . runBinder

runBinderEmptyEnv :: MonadFreshNames m =>
                     Binder lore a -> m (a, [Stm lore])
runBinderEmptyEnv m =
  modifyNameSource $ runState $ runBinderT m mempty

addBinderStm :: Monad m =>
                Stm lore -> BinderT lore m ()
addBinderStm binding = do
  tell $ DL.singleton binding
  BinderT $ modify (`M.union` scopeOf binding)

collectBinderStms :: Monad m =>
                     BinderT lore m a
                  -> BinderT lore m (a, [Stm lore])
collectBinderStms m = pass $ do
  (x, bnds) <- listen m
  let bnds' = DL.toList bnds
  BinderT $ modify (`M.difference` scopeOf bnds')
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
