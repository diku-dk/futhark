{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-- | This module defines a convenience monad/typeclass for creating
-- normalised programs.
module Futhark.Binder
  ( -- * A concrete @MonadBinder@ monad.
    BinderT
  , runBinderT
  , Binder
  , runBinder
  , runBodyBinder
  , runBinderEmptyEnv
  , bindingIdentTypes
  -- * Non-class interface
  , addBinderBinding
  , collectBinderBindings
  -- * The 'MonadBinder' typeclass
  , module Futhark.Binder.Class
  )
where

import qualified Data.DList as DL
import Control.Applicative
import Control.Arrow ((&&&))
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
                                    TypeEnv
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

instance MonadFreshNames m => HasTypeEnv (BinderT lore m) where
  lookupType name = do
    t <- BinderT $ gets $ HM.lookup name
    case t of
      Nothing -> fail $ "BinderT.lookupType: unknown variable " ++ pretty name
      Just t' -> return t'
  askTypeEnv = BinderT get

instance (Proper lore, Bindable lore, MonadFreshNames m) =>
         MonadBinder (BinderT lore m) where
  type Lore (BinderT lore m) = lore
  mkBodyM bnds res = return $ mkBody bnds res
  mkLetM pat e = return $ mkLet pat' e
    where pat' = [ (patElemIdent patElem, patElemBindage patElem)
                 | patElem <- patternElements pat ]
  mkLetNamesM = mkLetNames

  addBinding      = addBinderBinding
  collectBindings = collectBinderBindings

runBinderT :: Monad m =>
              BinderT lore m a
           -> TypeEnv
           -> m (a, [Binding lore])
runBinderT (BinderT m) types = do
  (x, bnds) <- runWriterT $ evalStateT m types
  return (x, DL.toList bnds)

runBinder :: (MonadFreshNames m, Bindable lore, HasTypeEnv m) =>
              Binder lore a
           -> m (a, [Binding lore])
runBinder m = do
  types <- askTypeEnv
  modifyNameSource $ runState $ runBinderT m types

runBodyBinder :: (Bindable lore, MonadFreshNames m, HasTypeEnv m) =>
                 Binder lore (Body lore) -> m (Body lore)
runBodyBinder = liftM (uncurry $ flip insertBindings) . runBinder

runBinderEmptyEnv :: MonadFreshNames m =>
                     Binder lore a -> m (a, [Binding lore])
runBinderEmptyEnv m =
  modifyNameSource $ runState $ runBinderT m mempty

addBinderBinding :: Monad m =>
                    Binding lore -> BinderT lore m ()
addBinderBinding binding = do
  tell $ DL.singleton binding
  BinderT $ modify (`HM.union` typeEnvFromBindings [binding])

collectBinderBindings :: Monad m =>
                         BinderT lore m a
                      -> BinderT lore m (a, [Binding lore])
collectBinderBindings m = pass $ do
  (x, bnds) <- listen m
  let bnds' = DL.toList bnds
  BinderT $ modify (`HM.difference` typeEnvFromBindings bnds')
  return ((x, bnds'), const DL.empty)

-- | Add the names and types from the given list of 'Ident's to the
-- type environment while executing the given action.  This is used to
-- deal with function parameters and loop merge variables.
bindingIdentTypes :: Monad m =>
                     [Ident] -> BinderT lore m a
                  -> BinderT lore m a
bindingIdentTypes idents (BinderT m) = BinderT $ do
  modify (`HM.union` types)
  x <- m
  modify (`HM.difference` types)
  return x
  where types = HM.fromList $ map (identName &&& identType) idents

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
