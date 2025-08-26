{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a monadic facility similar (and built on top
-- of) "Futhark.FreshNames".  The removes the need for a (small) amount of
-- boilerplate, at the cost of using some GHC extensions.  The idea is
-- that if your compiler pass runs in a monad that is an instance of
-- 'MonadFreshNames', you can automatically use the name generation
-- functions exported by this module.
module Futhark.MonadFreshNames
  ( MonadFreshNames (..),
    modifyNameSource,
    newName,
    newVName,
    newIdent,
    newIdent',
    newParam,
    module Futhark.FreshNames,
  )
where

import Control.Monad.Except
import Control.Monad.RWS.Lazy qualified
import Control.Monad.RWS.Strict qualified
import Control.Monad.Reader
import Control.Monad.State.Lazy qualified
import Control.Monad.State.Strict qualified
import Control.Monad.Trans.Maybe qualified
import Control.Monad.Writer.Lazy qualified
import Control.Monad.Writer.Strict qualified
import Futhark.FreshNames hiding (newName)
import Futhark.FreshNames qualified as FreshNames
import Futhark.IR.Syntax

-- | A monad that stores a name source.  The following is a good
-- instance for a monad in which the only state is a @NameSource vn@:
--
-- @
--  instance MonadFreshNames vn MyMonad where
--    getNameSource = get
--    putNameSource = put
-- @
class (Monad m) => MonadFreshNames m where
  getNameSource :: m VNameSource
  putNameSource :: VNameSource -> m ()

instance (Monad im) => MonadFreshNames (Control.Monad.State.Lazy.StateT VNameSource im) where
  getNameSource = Control.Monad.State.Lazy.get
  putNameSource = Control.Monad.State.Lazy.put

instance (Monad im) => MonadFreshNames (Control.Monad.State.Strict.StateT VNameSource im) where
  getNameSource = Control.Monad.State.Strict.get
  putNameSource = Control.Monad.State.Strict.put

instance
  (Monad im, Monoid w) =>
  MonadFreshNames (Control.Monad.RWS.Lazy.RWST r w VNameSource im)
  where
  getNameSource = Control.Monad.RWS.Lazy.get
  putNameSource = Control.Monad.RWS.Lazy.put

instance
  (Monad im, Monoid w) =>
  MonadFreshNames (Control.Monad.RWS.Strict.RWST r w VNameSource im)
  where
  getNameSource = Control.Monad.RWS.Strict.get
  putNameSource = Control.Monad.RWS.Strict.put

-- | Run a computation needing a fresh name source and returning a new
-- one, using 'getNameSource' and 'putNameSource' before and after the
-- computation.
modifyNameSource :: (MonadFreshNames m) => (VNameSource -> (a, VNameSource)) -> m a
modifyNameSource m = do
  src <- getNameSource
  let (x, src') = m src
  src' `seq` putNameSource src'
  pure x

-- | Produce a fresh name, using the given name as a template.
newName :: (MonadFreshNames m) => VName -> m VName
newName = modifyNameSource . flip FreshNames.newName

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: (MonadFreshNames m) => Name -> m VName
newVName s = newName $ VName s 0

-- | Produce a fresh 'Ident', using the given name as a template.
newIdent ::
  (MonadFreshNames m) => Name -> Type -> m Ident
newIdent s t = Ident <$> newVName s <*> pure t

-- | Produce a fresh 'Ident', using the given 'Ident' as a template,
-- but possibly modifying the name.
newIdent' ::
  (MonadFreshNames m) =>
  (Name -> Name) ->
  Ident ->
  m Ident
newIdent' f ident =
  newIdent
    (f $ baseName $ identName ident)
    (identType ident)

-- | Produce a fresh 'Param', using the given name as a template.
newParam ::
  (MonadFreshNames m) =>
  Name ->
  dec ->
  m (Param dec)
newParam s t = Param mempty <$> newVName s <*> pure t

-- Utility instance defintions for MTL classes.  This requires
-- UndecidableInstances, but saves on typing elsewhere.

instance (MonadFreshNames m) => MonadFreshNames (ReaderT s m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (MonadFreshNames m, Monoid s) =>
  MonadFreshNames (Control.Monad.Writer.Lazy.WriterT s m)
  where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (MonadFreshNames m, Monoid s) =>
  MonadFreshNames (Control.Monad.Writer.Strict.WriterT s m)
  where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (MonadFreshNames m) =>
  MonadFreshNames (Control.Monad.Trans.Maybe.MaybeT m)
  where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance
  (MonadFreshNames m) =>
  MonadFreshNames (ExceptT e m)
  where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource
