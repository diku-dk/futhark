{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines a convenience monad/typeclass for creating
-- normalised programs.  The fundamental building block is 'BinderT'
-- and its execution functions, but it is usually easier to use
-- 'Binder'.
--
-- See "Futhark.Construct" for a high-level description.
module Futhark.Binder
  ( -- * A concrete @MonadBinder@ monad.
    BinderT,
    runBinderT,
    runBinderT_,
    runBinderT',
    runBinderT'_,
    BinderOps (..),
    Binder,
    runBinder,
    runBinder_,
    runBodyBinder,

    -- * The 'MonadBinder' typeclass
    module Futhark.Binder.Class,
  )
where

import Control.Arrow (second)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import Futhark.Binder.Class
import Futhark.IR

-- | A 'BinderT' (and by extension, a 'Binder') is only an instance of
-- 'MonadBinder' for lores that implement this type class, which
-- contains methods for constructing statements.
class ASTLore lore => BinderOps lore where
  mkExpDecB ::
    (MonadBinder m, Lore m ~ lore) =>
    Pattern lore ->
    Exp lore ->
    m (ExpDec lore)
  mkBodyB ::
    (MonadBinder m, Lore m ~ lore) =>
    Stms lore ->
    Result ->
    m (Body lore)
  mkLetNamesB ::
    (MonadBinder m, Lore m ~ lore) =>
    [VName] ->
    Exp lore ->
    m (Stm lore)

  default mkExpDecB ::
    (MonadBinder m, Bindable lore) =>
    Pattern lore ->
    Exp lore ->
    m (ExpDec lore)
  mkExpDecB pat e = return $ mkExpDec pat e

  default mkBodyB ::
    (MonadBinder m, Bindable lore) =>
    Stms lore ->
    Result ->
    m (Body lore)
  mkBodyB stms res = return $ mkBody stms res

  default mkLetNamesB ::
    (MonadBinder m, Lore m ~ lore, Bindable lore) =>
    [VName] ->
    Exp lore ->
    m (Stm lore)
  mkLetNamesB = mkLetNames

-- | A monad transformer that tracks statements and provides a
-- 'MonadBinder' instance, assuming that the underlying monad provides
-- a name source.  In almost all cases, this is what you will use for
-- constructing statements (possibly as part of a larger monad stack).
-- If you find yourself needing to implement 'MonadBinder' from
-- scratch, then it is likely that you are making a mistake.
newtype BinderT lore m a = BinderT (StateT (Stms lore, Scope lore) m a)
  deriving (Functor, Monad, Applicative)

instance MonadTrans (BinderT lore) where
  lift = BinderT . lift

-- | The most commonly used binder monad.
type Binder lore = BinderT lore (State VNameSource)

instance MonadFreshNames m => MonadFreshNames (BinderT lore m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (ASTLore lore, Monad m) =>
  HasScope lore (BinderT lore m)
  where
  lookupType name = do
    t <- BinderT $ gets $ M.lookup name . snd
    case t of
      Nothing -> error $ "BinderT.lookupType: unknown variable " ++ pretty name
      Just t' -> return $ typeOf t'
  askScope = BinderT $ gets snd

instance
  (ASTLore lore, Monad m) =>
  LocalScope lore (BinderT lore m)
  where
  localScope types (BinderT m) = BinderT $ do
    modify $ second (M.union types)
    x <- m
    modify $ second (`M.difference` types)
    return x

instance
  (ASTLore lore, MonadFreshNames m, BinderOps lore) =>
  MonadBinder (BinderT lore m)
  where
  type Lore (BinderT lore m) = lore
  mkExpDecM = mkExpDecB
  mkBodyM = mkBodyB
  mkLetNamesM = mkLetNamesB

  addStms stms =
    BinderT $
      modify $ \(cur_stms, scope) ->
        (cur_stms <> stms, scope `M.union` scopeOf stms)

  collectStms m = do
    (old_stms, old_scope) <- BinderT get
    BinderT $ put (mempty, old_scope)
    x <- m
    (new_stms, _) <- BinderT get
    BinderT $ put (old_stms, old_scope)
    return (x, new_stms)

-- | Run a binder action given an initial scope, returning a value and
-- the statements added ('addStm') during the action.
runBinderT ::
  MonadFreshNames m =>
  BinderT lore m a ->
  Scope lore ->
  m (a, Stms lore)
runBinderT (BinderT m) scope = do
  (x, (stms, _)) <- runStateT m (mempty, scope)
  return (x, stms)

-- | Like 'runBinderT', but return only the statements.
runBinderT_ ::
  MonadFreshNames m =>
  BinderT lore m () ->
  Scope lore ->
  m (Stms lore)
runBinderT_ m = fmap snd . runBinderT m

-- | Like 'runBinderT', but get the initial scope from the current
-- monad.
runBinderT' ::
  (MonadFreshNames m, HasScope somelore m, SameScope somelore lore) =>
  BinderT lore m a ->
  m (a, Stms lore)
runBinderT' m = do
  scope <- askScope
  runBinderT m $ castScope scope

-- | Like 'runBinderT_', but get the initial scope from the current
-- monad.
runBinderT'_ ::
  (MonadFreshNames m, HasScope somelore m, SameScope somelore lore) =>
  BinderT lore m a ->
  m (Stms lore)
runBinderT'_ = fmap snd . runBinderT'

-- | Run a binder action, returning a value and the statements added
-- ('addStm') during the action.  Assumes that the current monad
-- provides initial scope and name source.
runBinder ::
  ( MonadFreshNames m,
    HasScope somelore m,
    SameScope somelore lore
  ) =>
  Binder lore a ->
  m (a, Stms lore)
runBinder m = do
  types <- askScope
  modifyNameSource $ runState $ runBinderT m $ castScope types

-- | Like 'runBinder', but throw away the result and just return the
-- added statements.
runBinder_ ::
  ( MonadFreshNames m,
    HasScope somelore m,
    SameScope somelore lore
  ) =>
  Binder lore a ->
  m (Stms lore)
runBinder_ = fmap snd . runBinder

-- | Run a binder that produces a t'Body', and prefix that t'Body' by
-- the statements produced during execution of the action.
runBodyBinder ::
  ( Bindable lore,
    MonadFreshNames m,
    HasScope somelore m,
    SameScope somelore lore
  ) =>
  Binder lore (Body lore) ->
  m (Body lore)
runBodyBinder = fmap (uncurry $ flip insertStms) . runBinder

-- Utility instance defintions for MTL classes.  These require
-- UndecidableInstances, but save on typing elsewhere.

mapInner ::
  Monad m =>
  ( m (a, (Stms lore, Scope lore)) ->
    m (b, (Stms lore, Scope lore))
  ) ->
  BinderT lore m a ->
  BinderT lore m b
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
    where
      unBinder (BinderT m') = m'
