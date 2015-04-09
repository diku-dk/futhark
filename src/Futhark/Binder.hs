{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-- | This module defines a convenience monad/typeclass for creating
-- normalised programs.
module Futhark.Binder
  ( -- * A concrete @MonadBinder@ monad.
    BinderT
  , runBinderT
  , Binder
  , runBinder
  , runBinder'
  , runBinder''
  , runBinderWithNameSource
  -- * Writer convenience interface
  , addBindingWriter
  , collectBindingsWriter
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

addBindingWriter :: (MonadFreshNames m, Applicative m, MonadWriter (DL.DList (Binding lore)) m) =>
                    Binding lore -> m ()
addBindingWriter = tell . DL.singleton

collectBindingsWriter :: (MonadFreshNames m,
                          Applicative m,
                          MonadWriter (DL.DList (Binding lore)) m) =>
                         m a -> m (a, [Binding lore])
collectBindingsWriter m = pass $ do
                            (x, bnds) <- listen m
                            return ((x, DL.toList bnds), const DL.empty)

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
      Nothing -> fail $ "Unknown variable " ++ pretty name
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

  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

runBinderT :: Monad m =>
              BinderT lore m a
           -> m (a, [Binding lore])
runBinderT (BinderT m) = do (x, bnds) <- runWriterT $ evalStateT m HM.empty
                            return (x, DL.toList bnds)

runBinder :: (Bindable lore, MonadFreshNames m) =>
             Binder lore (Body lore) -> m (Body lore)
runBinder m = do
  (b, f) <- runBinder' m
  return $ f b

runBinder' :: (MonadFreshNames m, Bindable lore) =>
              Binder lore a -> m (a, Body lore -> Body lore)
runBinder' m = do
  (x, bnds) <- runBinder'' m
  return (x, insertBindings bnds)

runBinder'' :: MonadFreshNames m => Binder lore a -> m (a, [Binding lore])
runBinder'' = modifyNameSource . runBinderWithNameSource

runBinderWithNameSource :: Binder lore a -> VNameSource
                        -> ((a, [Binding lore]), VNameSource)
runBinderWithNameSource = runState . runBinderT

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
