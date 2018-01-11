{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- | This module provides a monadic facility similar (and built on top
-- of) "Futhark.FreshNames".  The removes the need for a (small) amount of
-- boilerplate, at the cost of using some GHC extensions.  The idea is
-- that if your compiler pass runs in a monad that is an instance of
-- 'MonadFreshNames', you can automatically use the name generation
-- functions exported by this module.
module Futhark.MonadFreshNames
  ( MonadFreshNames (..)
  , modifyNameSource
  , newName
  , newNameFromString
  , newID
  , newIDFromString
  , newVName
  , newVName'
  , newIdent
  , newIdent'
  , newIdents
  , newParam
  , newParam'
  , module Futhark.FreshNames
  ) where

import Control.Monad.Except
import qualified Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict
import qualified Control.Monad.Writer.Lazy
import qualified Control.Monad.Writer.Strict
import qualified Control.Monad.RWS.Lazy
import qualified Control.Monad.RWS.Strict
import qualified Control.Monad.Trans.Maybe
import Control.Monad.Reader

import Futhark.Representation.AST.Syntax
import qualified Futhark.FreshNames as FreshNames
import Futhark.FreshNames hiding (newName, newVName)

-- | A monad that stores a name source.  The following is a good
-- instance for a monad in which the only state is a @NameSource vn@:
--
-- @
--  instance MonadFreshNames vn MyMonad where
--    getNameSource = get
--    putNameSource = put
-- @
class (Applicative m, Monad m) => MonadFreshNames m where
  getNameSource :: m VNameSource
  putNameSource :: VNameSource -> m ()

instance (Applicative im, Monad im) => MonadFreshNames (Control.Monad.State.Lazy.StateT VNameSource im) where
  getNameSource = Control.Monad.State.Lazy.get
  putNameSource = Control.Monad.State.Lazy.put

instance (Applicative im, Monad im) => MonadFreshNames (Control.Monad.State.Strict.StateT VNameSource im) where
  getNameSource = Control.Monad.State.Strict.get
  putNameSource = Control.Monad.State.Strict.put

instance (Applicative im, Monad im, Monoid w) =>
         MonadFreshNames (Control.Monad.RWS.Lazy.RWST r w VNameSource im) where
  getNameSource = Control.Monad.RWS.Lazy.get
  putNameSource = Control.Monad.RWS.Lazy.put

instance (Applicative im, Monad im, Monoid w) =>
         MonadFreshNames (Control.Monad.RWS.Strict.RWST r w VNameSource im) where
  getNameSource = Control.Monad.RWS.Strict.get
  putNameSource = Control.Monad.RWS.Strict.put

-- | Run a computation needing a fresh name source and returning a new
-- one, using 'getNameSource' and 'putNameSource' before and after the
-- computation.
modifyNameSource :: MonadFreshNames m => (VNameSource -> (a, VNameSource)) -> m a
modifyNameSource m = do src <- getNameSource
                        let (x,src') = m src
                        putNameSource src'
                        return x

-- | Produce a fresh name, using the given name as a template.
newName :: MonadFreshNames m => VName -> m VName
newName = modifyNameSource . flip FreshNames.newName

-- | As @newName@, but takes a 'String' for the name template.
newNameFromString :: MonadFreshNames m => String -> m VName
newNameFromString s = newName $ VName (nameFromString s) 0

-- | Produce a fresh 'ID', using the given base name as a template.
newID :: MonadFreshNames m => Name -> m VName
newID s = newName $ VName s 0

-- | As 'newID', but takes a 'String' for the name template.
newIDFromString :: MonadFreshNames m => String -> m VName
newIDFromString = newID . nameFromString

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: MonadFreshNames m => String -> m VName
newVName = newID . nameFromString

-- | Produce a fresh 'VName', using the given name as a template, but
-- possibly appending something more..
newVName' :: MonadFreshNames m => (String -> String) -> String -> m VName
newVName' f = newID . nameFromString . f

-- | Produce a fresh 'Ident', using the given name as a template.
newIdent :: MonadFreshNames m =>
            String -> Type -> m Ident
newIdent s t = do
  s' <- newID $ nameFromString s
  return $ Ident s' t

-- | Produce a fresh 'Ident', using the given 'Ident' as a template,
-- but possibly modifying the name.
newIdent' :: MonadFreshNames m =>
             (String -> String)
          -> Ident -> m Ident
newIdent' f ident =
  newIdent (f $ nameToString $ baseName $ identName ident)
           (identType ident)

-- | Produce several 'Ident's, using the given name as a template,
-- based on a list of types.
newIdents :: MonadFreshNames m =>
             String -> [Type] -> m [Ident]
newIdents = mapM . newIdent

-- | Produce a fresh 'Param', using the given name as a template.
newParam :: MonadFreshNames m =>
            String -> attr -> m (Param attr)
newParam s t = do
  s' <- newID $ nameFromString s
  return $ Param s' t

-- | Produce a fresh 'Param', using the given 'Param' as a template,
-- but possibly modifying the name.
newParam' :: MonadFreshNames m =>
             (String -> String)
          -> Param attr -> m (Param attr)
newParam' f param =
  newParam (f $ nameToString $ baseName $ paramName param)
           (paramAttr param)

-- Utility instance defintions for MTL classes.  This requires
-- UndecidableInstances, but saves on typing elsewhere.

instance MonadFreshNames m => MonadFreshNames (ReaderT s m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (MonadFreshNames m, Monoid s) =>
         MonadFreshNames (Control.Monad.Writer.Lazy.WriterT s m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (MonadFreshNames m, Monoid s) =>
         MonadFreshNames (Control.Monad.Writer.Strict.WriterT s m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance MonadFreshNames m =>
         MonadFreshNames (Control.Monad.Trans.Maybe.MaybeT m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance MonadFreshNames m =>
         MonadFreshNames (ExceptT e m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource
