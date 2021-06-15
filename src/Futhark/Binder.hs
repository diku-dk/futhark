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
-- 'MonadBinder' for representations that implement this type class,
-- which contains methods for constructing statements.
class ASTRep rep => BinderOps rep where
  mkExpDecB ::
    (MonadBinder m, Rep m ~ rep) =>
    Pattern rep ->
    Exp rep ->
    m (ExpDec rep)
  mkBodyB ::
    (MonadBinder m, Rep m ~ rep) =>
    Stms rep ->
    Result ->
    m (Body rep)
  mkLetNamesB ::
    (MonadBinder m, Rep m ~ rep) =>
    [VName] ->
    Exp rep ->
    m (Stm rep)

  default mkExpDecB ::
    (MonadBinder m, Bindable rep) =>
    Pattern rep ->
    Exp rep ->
    m (ExpDec rep)
  mkExpDecB pat e = return $ mkExpDec pat e

  default mkBodyB ::
    (MonadBinder m, Bindable rep) =>
    Stms rep ->
    Result ->
    m (Body rep)
  mkBodyB stms res = return $ mkBody stms res

  default mkLetNamesB ::
    (MonadBinder m, Rep m ~ rep, Bindable rep) =>
    [VName] ->
    Exp rep ->
    m (Stm rep)
  mkLetNamesB = mkLetNames

-- | A monad transformer that tracks statements and provides a
-- 'MonadBinder' instance, assuming that the underlying monad provides
-- a name source.  In almost all cases, this is what you will use for
-- constructing statements (possibly as part of a larger monad stack).
-- If you find yourself needing to implement 'MonadBinder' from
-- scratch, then it is likely that you are making a mistake.
newtype BinderT rep m a = BinderT (StateT (Stms rep, Scope rep) m a)
  deriving (Functor, Monad, Applicative)

instance MonadTrans (BinderT rep) where
  lift = BinderT . lift

-- | The most commonly used binder monad.
type Binder rep = BinderT rep (State VNameSource)

instance MonadFreshNames m => MonadFreshNames (BinderT rep m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (ASTRep rep, Monad m) =>
  HasScope rep (BinderT rep m)
  where
  lookupType name = do
    t <- BinderT $ gets $ M.lookup name . snd
    case t of
      Nothing -> error $ "BinderT.lookupType: unknown variable " ++ pretty name
      Just t' -> return $ typeOf t'
  askScope = BinderT $ gets snd

instance
  (ASTRep rep, Monad m) =>
  LocalScope rep (BinderT rep m)
  where
  localScope types (BinderT m) = BinderT $ do
    modify $ second (M.union types)
    x <- m
    modify $ second (`M.difference` types)
    return x

instance
  (ASTRep rep, MonadFreshNames m, BinderOps rep) =>
  MonadBinder (BinderT rep m)
  where
  type Rep (BinderT rep m) = rep
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
  BinderT rep m a ->
  Scope rep ->
  m (a, Stms rep)
runBinderT (BinderT m) scope = do
  (x, (stms, _)) <- runStateT m (mempty, scope)
  return (x, stms)

-- | Like 'runBinderT', but return only the statements.
runBinderT_ ::
  MonadFreshNames m =>
  BinderT rep m () ->
  Scope rep ->
  m (Stms rep)
runBinderT_ m = fmap snd . runBinderT m

-- | Like 'runBinderT', but get the initial scope from the current
-- monad.
runBinderT' ::
  (MonadFreshNames m, HasScope somerep m, SameScope somerep rep) =>
  BinderT rep m a ->
  m (a, Stms rep)
runBinderT' m = do
  scope <- askScope
  runBinderT m $ castScope scope

-- | Like 'runBinderT_', but get the initial scope from the current
-- monad.
runBinderT'_ ::
  (MonadFreshNames m, HasScope somerep m, SameScope somerep rep) =>
  BinderT rep m a ->
  m (Stms rep)
runBinderT'_ = fmap snd . runBinderT'

-- | Run a binder action, returning a value and the statements added
-- ('addStm') during the action.  Assumes that the current monad
-- provides initial scope and name source.
runBinder ::
  ( MonadFreshNames m,
    HasScope somerep m,
    SameScope somerep rep
  ) =>
  Binder rep a ->
  m (a, Stms rep)
runBinder m = do
  types <- askScope
  modifyNameSource $ runState $ runBinderT m $ castScope types

-- | Like 'runBinder', but throw away the result and just return the
-- added statements.
runBinder_ ::
  ( MonadFreshNames m,
    HasScope somerep m,
    SameScope somerep rep
  ) =>
  Binder rep a ->
  m (Stms rep)
runBinder_ = fmap snd . runBinder

-- | Run a binder that produces a t'Body', and prefix that t'Body' by
-- the statements produced during execution of the action.
runBodyBinder ::
  ( Bindable rep,
    MonadFreshNames m,
    HasScope somerep m,
    SameScope somerep rep
  ) =>
  Binder rep (Body rep) ->
  m (Body rep)
runBodyBinder = fmap (uncurry $ flip insertStms) . runBinder

-- Utility instance defintions for MTL classes.  These require
-- UndecidableInstances, but save on typing elsewhere.

mapInner ::
  Monad m =>
  ( m (a, (Stms rep, Scope rep)) ->
    m (b, (Stms rep, Scope rep))
  ) ->
  BinderT rep m a ->
  BinderT rep m b
mapInner f (BinderT m) = BinderT $ do
  s <- get
  (x, s') <- lift $ f $ runStateT m s
  put s'
  return x

instance MonadReader r m => MonadReader r (BinderT rep m) where
  ask = BinderT $ lift ask
  local f = mapInner $ local f

instance MonadState s m => MonadState s (BinderT rep m) where
  get = BinderT $ lift get
  put = BinderT . lift . put

instance MonadWriter w m => MonadWriter w (BinderT rep m) where
  tell = BinderT . lift . tell
  pass = mapInner $ \m -> pass $ do
    ((x, f), s) <- m
    return ((x, s), f)
  listen = mapInner $ \m -> do
    ((x, s), y) <- listen m
    return ((x, y), s)

instance MonadError e m => MonadError e (BinderT rep m) where
  throwError = lift . throwError
  catchError (BinderT m) f =
    BinderT $ catchError m $ unBinder . f
    where
      unBinder (BinderT m') = m'
