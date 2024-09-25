{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines a convenience monad/typeclass for building
-- ASTs.  The fundamental building block is 'BuilderT' and its
-- execution functions, but it is usually easier to use 'Builder'.
--
-- See "Futhark.Construct" for a high-level description.
module Futhark.Builder
  ( -- * A concrete @MonadBuilder@ monad.
    BuilderT,
    runBuilderT,
    runBuilderT_,
    runBuilderT',
    runBuilderT'_,
    BuilderOps (..),
    Builder,
    runBuilder,
    runBuilder_,
    runBodyBuilder,
    runLambdaBuilder,

    -- * The 'MonadBuilder' typeclass
    module Futhark.Builder.Class,
  )
where

import Control.Arrow (second)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Map.Strict qualified as M
import Futhark.Builder.Class
import Futhark.IR

-- | A 'BuilderT' (and by extension, a 'Builder') is only an instance of
-- 'MonadBuilder' for representations that implement this type class,
-- which contains methods for constructing statements.
class (ASTRep rep) => BuilderOps rep where
  mkExpDecB ::
    (MonadBuilder m, Rep m ~ rep) =>
    Pat (LetDec rep) ->
    Exp rep ->
    m (ExpDec rep)
  mkBodyB ::
    (MonadBuilder m, Rep m ~ rep) =>
    Stms rep ->
    Result ->
    m (Body rep)
  mkLetNamesB ::
    (MonadBuilder m, Rep m ~ rep) =>
    [VName] ->
    Exp rep ->
    m (Stm rep)

  default mkExpDecB ::
    (MonadBuilder m, Buildable rep) =>
    Pat (LetDec rep) ->
    Exp rep ->
    m (ExpDec rep)
  mkExpDecB pat e = pure $ mkExpDec pat e

  default mkBodyB ::
    (MonadBuilder m, Buildable rep) =>
    Stms rep ->
    Result ->
    m (Body rep)
  mkBodyB stms res = pure $ mkBody stms res

  default mkLetNamesB ::
    (MonadBuilder m, Rep m ~ rep, Buildable rep) =>
    [VName] ->
    Exp rep ->
    m (Stm rep)
  mkLetNamesB = mkLetNames

-- | A monad transformer that tracks statements and provides a
-- 'MonadBuilder' instance, assuming that the underlying monad provides
-- a name source.  In almost all cases, this is what you will use for
-- constructing statements (possibly as part of a larger monad stack).
-- If you find yourself needing to implement 'MonadBuilder' from
-- scratch, then it is likely that you are making a mistake.
newtype BuilderT rep m a = BuilderT (StateT (Stms rep, Scope rep) m a)
  deriving (Functor, Monad, Applicative)

instance MonadTrans (BuilderT rep) where
  lift = BuilderT . lift

-- | The most commonly used builder monad.
type Builder rep = BuilderT rep (State VNameSource)

instance (MonadFreshNames m) => MonadFreshNames (BuilderT rep m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (ASTRep rep, Monad m) => HasScope rep (BuilderT rep m) where
  lookupType name = do
    t <- BuilderT $ gets $ M.lookup name . snd
    case t of
      Nothing -> do
        known <- BuilderT $ gets $ M.keys . snd
        error . unlines $
          [ "BuilderT.lookupType: unknown variable " ++ prettyString name,
            "Known variables: ",
            unwords $ map prettyString known
          ]
      Just t' -> pure $ typeOf t'
  askScope = BuilderT $ gets snd

instance (ASTRep rep, Monad m) => LocalScope rep (BuilderT rep m) where
  localScope types (BuilderT m) = BuilderT $ do
    modify $ second (M.union types)
    x <- m
    modify $ second (`M.difference` types)
    pure x

instance
  (MonadFreshNames m, BuilderOps rep) =>
  MonadBuilder (BuilderT rep m)
  where
  type Rep (BuilderT rep m) = rep
  mkExpDecM = mkExpDecB
  mkBodyM = mkBodyB
  mkLetNamesM = mkLetNamesB

  addStms stms =
    BuilderT $
      modify $ \(cur_stms, scope) ->
        (cur_stms <> stms, scope `M.union` scopeOf stms)

  collectStms m = do
    (old_stms, old_scope) <- BuilderT get
    BuilderT $ put (mempty, old_scope)
    x <- m
    (new_stms, _) <- BuilderT get
    BuilderT $ put (old_stms, old_scope)
    pure (x, new_stms)

-- | Run a builder action given an initial scope, returning a value and
-- the statements added ('addStm') during the action.
runBuilderT ::
  (MonadFreshNames m) =>
  BuilderT rep m a ->
  Scope rep ->
  m (a, Stms rep)
runBuilderT (BuilderT m) scope = do
  (x, (stms, _)) <- runStateT m (mempty, scope)
  pure (x, stms)

-- | Like 'runBuilderT', but return only the statements.
runBuilderT_ ::
  (MonadFreshNames m) =>
  BuilderT rep m () ->
  Scope rep ->
  m (Stms rep)
runBuilderT_ m = fmap snd . runBuilderT m

-- | Like 'runBuilderT', but get the initial scope from the current
-- monad.
runBuilderT' ::
  (MonadFreshNames m, HasScope somerep m, SameScope somerep rep) =>
  BuilderT rep m a ->
  m (a, Stms rep)
runBuilderT' m = do
  scope <- askScope
  runBuilderT m $ castScope scope

-- | Like 'runBuilderT_', but get the initial scope from the current
-- monad.
runBuilderT'_ ::
  (MonadFreshNames m, HasScope somerep m, SameScope somerep rep) =>
  BuilderT rep m a ->
  m (Stms rep)
runBuilderT'_ = fmap snd . runBuilderT'

-- | Run a builder action, returning a value and the statements added
-- ('addStm') during the action.  Assumes that the current monad
-- provides initial scope and name source.
runBuilder ::
  (MonadFreshNames m, HasScope somerep m, SameScope somerep rep) =>
  Builder rep a ->
  m (a, Stms rep)
runBuilder m = do
  types <- askScope
  modifyNameSource $ runState $ runBuilderT m $ castScope types

-- | Like 'runBuilder', but throw away the result and just return the
-- added statements.
runBuilder_ ::
  (MonadFreshNames m, HasScope somerep m, SameScope somerep rep) =>
  Builder rep a ->
  m (Stms rep)
runBuilder_ = fmap snd . runBuilder

-- | Run a builder that produces a 'Result' and construct a body that
-- contains that result alongside the statements produced during the
-- builder.
runBodyBuilder ::
  ( Buildable rep,
    MonadFreshNames m,
    HasScope somerep m,
    SameScope somerep rep
  ) =>
  Builder rep Result ->
  m (Body rep)
runBodyBuilder =
  fmap (uncurry $ flip insertStms) . runBuilder . fmap (mkBody mempty)

-- | Given lambda parameters, Run a builder action that produces the
-- statements and returns the 'Result' of the lambda body.
runLambdaBuilder ::
  ( Buildable rep,
    MonadFreshNames m,
    HasScope somerep m,
    SameScope somerep rep
  ) =>
  [LParam rep] ->
  Builder rep Result ->
  m (Lambda rep)
runLambdaBuilder params m = do
  ((res, ret), stms) <- runBuilder . localScope (scopeOfLParams params) $ do
    res <- m
    ret <- mapM subExpResType res
    pure (res, ret)
  pure $ Lambda params ret $ mkBody stms res

-- Utility instance defintions for MTL classes.  These require
-- UndecidableInstances, but save on typing elsewhere.

mapInner ::
  (Monad m) =>
  ( m (a, (Stms rep, Scope rep)) ->
    m (b, (Stms rep, Scope rep))
  ) ->
  BuilderT rep m a ->
  BuilderT rep m b
mapInner f (BuilderT m) = BuilderT $ do
  s <- get
  (x, s') <- lift $ f $ runStateT m s
  put s'
  pure x

instance (MonadReader r m) => MonadReader r (BuilderT rep m) where
  ask = BuilderT $ lift ask
  local f = mapInner $ local f

instance (MonadState s m) => MonadState s (BuilderT rep m) where
  get = BuilderT $ lift get
  put = BuilderT . lift . put

instance (MonadWriter w m) => MonadWriter w (BuilderT rep m) where
  tell = BuilderT . lift . tell
  pass = mapInner $ \m -> pass $ do
    ((x, f), s) <- m
    pure ((x, s), f)
  listen = mapInner $ \m -> do
    ((x, s), y) <- listen m
    pure ((x, y), s)

instance (MonadError e m) => MonadError e (BuilderT rep m) where
  throwError = lift . throwError
  catchError (BuilderT m) f =
    BuilderT $ catchError m $ unBuilder . f
    where
      unBuilder (BuilderT m') = m'
