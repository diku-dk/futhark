{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
-- | This module defines a convenience monad/typeclass for creating
-- normalised programs.
module Futhark.Binder
  ( MonadBinder(..)
  , bodyBindings
  , insertBindingsM
  , letBind
  , letWithBind
  , loopBind
  , bodyBind
  -- * A concrete @Binder@ monad.
  , Binder
  , runBinder
  , runBinder'
  , runBinder''
  , runBinderWithNameSource
  -- * Writer convenience interface
  , addBindingWriter
  , collectBindingsWriter
  )
where

import qualified Data.DList as DL
import Data.Loc
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

import Futhark.Representation.Basic
import Futhark.MonadFreshNames

class (MonadFreshNames m, Applicative m, Monad m) => MonadBinder m where
  addBinding      :: Binding -> m ()
  collectBindings :: m a -> m (a, [Binding])

letBind :: MonadBinder m => [Ident] -> Exp -> m ()
letBind pat e =
  addBinding $ Let pat mempty e

letWithBind :: MonadBinder m =>
               Certificates -> Ident -> Ident -> [SubExp] -> SubExp -> m ()
letWithBind cs dest src idxs ve =
  addBinding $ Let [dest] mempty $ Update cs src idxs ve $ srclocOf src

loopBind :: MonadBinder m => [(Ident, SubExp)] -> Ident -> SubExp -> Body -> m ()
loopBind merge i bound loopbody =
  addBinding $ Let mergepat mempty $
               DoLoop mergepat merge i bound loopbody $ srclocOf i
  where mergepat = map fst merge

bodyBind :: MonadBinder m => Body -> m [SubExp]
bodyBind (Body bnds (Result _ es _)) = do
  mapM_ addBinding bnds
  return es

-- | Evaluate the action, producing a body, then wrap it in all the
-- bindings it created using 'addBinding'.
insertBindingsM :: MonadBinder m => m Body -> m Body
insertBindingsM m = do
  (e,bnds) <- collectBindings m
  return $ insertBindings bnds e

addBindingWriter :: (MonadFreshNames m, Applicative m, MonadWriter (DL.DList Binding) m) =>
                    Binding -> m ()
addBindingWriter = tell . DL.singleton

collectBindingsWriter :: (MonadFreshNames m, Applicative m, MonadWriter (DL.DList Binding) m) =>
                         m a -> m (a, [Binding])
collectBindingsWriter m = pass $ do
                            (x, bnds) <- listen m
                            return ((x, DL.toList bnds), const DL.empty)

newtype Binder a = TransformM (WriterT (DL.DList Binding) (State VNameSource) a)
  deriving (Functor, Monad, Applicative,
            MonadWriter (DL.DList Binding), MonadState VNameSource)

instance MonadFreshNames Binder where
  getNameSource = get
  putNameSource = put

instance MonadBinder Binder where
  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

runBinder :: MonadFreshNames m => Binder Body -> m Body
runBinder m = do
  (b, f) <- runBinder' m
  return $ f b

runBinder' :: MonadFreshNames m => Binder a -> m (a, Body -> Body)
runBinder' m = do
  (x, bnds) <- runBinder'' m
  return (x, insertBindings bnds)

runBinder'' :: MonadFreshNames m => Binder a -> m (a, [Binding])
runBinder'' = modifyNameSource . runBinderWithNameSource

runBinderWithNameSource :: Binder a -> VNameSource -> ((a, [Binding]), VNameSource)
runBinderWithNameSource (TransformM m) src =
  let ((x,bnds),src') = runState (runWriterT m) src
  in ((x, DL.toList bnds), src')
