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
  , runBinderEmptyEnv
  -- * Non-class interface
  , addBinderStm
  , collectBinderStms
  , certifyingBinder
  -- * The 'MonadBinder' typeclass
  , module Futhark.Binder.Class
  )
where

import qualified Data.DList as DL
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.Error.Class
import qualified Data.Map.Strict as M

import Prelude

import Futhark.Binder.Class
import Futhark.Representation.AST
import Futhark.MonadFreshNames

class BinderOps lore where
  mkExpAttrB :: (MonadBinder m, Lore m ~ lore) =>
                Pattern lore -> Exp lore -> m (ExpAttr lore)
  mkBodyB :: (MonadBinder m, Lore m ~ lore) =>
             [Stm lore] -> Result -> m (Body lore)
  mkLetNamesB :: (MonadBinder m, Lore m ~ lore) =>
                 [(VName,Bindage)] -> Exp lore -> m (Stm lore)

bindableMkExpAttrB :: (MonadBinder m, Bindable (Lore m)) =>
                      Pattern (Lore m) -> Exp (Lore m) -> m (ExpAttr (Lore m))
bindableMkExpAttrB pat e = return $ mkExpAttr pat e

bindableMkBodyB :: (MonadBinder m, Bindable (Lore m)) =>
                   [Stm (Lore m)] -> Result -> m (Body (Lore m))
bindableMkBodyB stms res = return $ mkBody stms res

bindableMkLetNamesB :: (MonadBinder m, Bindable (Lore m)) =>
                       [(VName,Bindage)] -> Exp (Lore m) -> m (Stm (Lore m))
bindableMkLetNamesB = mkLetNames

newtype BinderT lore m a = BinderT (RWST
                                     ()
                                     (DL.DList (Stm lore))
                                     (Scope lore)
                                      m
                                    a)
  deriving (Functor, Monad, Applicative,
            MonadWriter (DL.DList (Stm lore)))
-- Cannot add MonadState instance, because it would
-- conflict with the utility instances.

instance MonadTrans (BinderT lore) where
  lift = BinderT . lift

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

instance (Attributes lore, MonadFreshNames m, BinderOps lore) =>
         MonadBinder (BinderT lore m) where
  type Lore (BinderT lore m) = lore
  mkExpAttrM = mkExpAttrB
  mkBodyM = mkBodyB
  mkLetNamesM = mkLetNamesB

  addStm      = addBinderStm
  collectStms = collectBinderStms

  certifying = certifyingBinder

runBinderT :: (MonadFreshNames m, BinderOps lore) =>
              BinderT lore m a
           -> Scope lore
           -> m (a, [Stm lore])
runBinderT (BinderT m) types = do
  (x, bnds) <- evalRWST m () types
  return (x, DL.toList bnds)

runBinder :: (MonadFreshNames m,
              HasScope somelore m, SameScope somelore lore,
              BinderOps lore) =>
              Binder lore a
           -> m (a, [Stm lore])
runBinder m = do
  types <- askScope
  modifyNameSource $ runState $ runBinderT m $ castScope types

-- | Like 'runBinder', but throw away the result and just return the
-- added bindings.
runBinder_ :: (MonadFreshNames m,
               HasScope somelore m, SameScope somelore lore,
               BinderOps lore) =>
              Binder lore a
           -> m [Stm lore]
runBinder_ = fmap snd . runBinder

-- | As 'runBinder', but uses 'addStm' to add the returned
-- bindings to the surrounding monad.
joinBinder :: (MonadBinder m, BinderOps (Lore m)) =>
              Binder (Lore m) a
           -> m a
joinBinder m = do (x, bnds) <- runBinder m
                  mapM_ addStm bnds
                  return x

runBodyBinder :: (Bindable lore, BinderOps lore, MonadFreshNames m,
                  HasScope somelore m, SameScope somelore lore) =>
                 Binder lore (Body lore) -> m (Body lore)
runBodyBinder = fmap (uncurry $ flip insertStms) . runBinder

runBinderEmptyEnv :: (MonadFreshNames m, BinderOps lore) =>
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

certifyingBinder :: Monad m =>
                    Certificates -> BinderT lore m a
                 -> BinderT lore m a
certifyingBinder = censor . fmap . certify

-- Utility instance defintions for MTL classes.  These require
-- UndecidableInstances, but save on typing elsewhere.
instance MonadReader r m => MonadReader r (BinderT lore m) where
  ask = BinderT $ lift ask
  local f (BinderT m) = BinderT $ do
    r <- ask
    s <- get
    (x, s', w) <- lift $ local f $ runRWST m r s
    put s'
    tell w
    return x

instance MonadState s m => MonadState s (BinderT lore m) where
  get = BinderT $ lift get
  put = BinderT . lift . put

instance MonadError e m => MonadError e (BinderT lore m) where
  throwError = lift . throwError
  catchError (BinderT m) f =
    BinderT $ catchError m $ unBinder . f
    where unBinder (BinderT m') = m'
