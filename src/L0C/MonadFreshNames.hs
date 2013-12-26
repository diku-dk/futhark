{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
-- | This module provides a monadic facility similar (and built on top
-- of) "L0C.FreshNames".  The removes the need for a (small) amount of
-- boilerplate, at the cost of using some GHC extensions.  The idea is
-- that if your compiler pass runs in a monad that is an instance of
-- 'MonadFreshNames', you can automatically use the name generation
-- functions exported by this module.
module L0C.MonadFreshNames
  ( MonadFreshNames (..)
  , newName
  , newNameFromString
  , newID
  , newIDFromString
  , newVName
  , newIdent
  , module L0C.FreshNames
  ) where

import Control.Applicative

import qualified Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict

import Data.Loc

import L0C.L0
import qualified L0C.FreshNames as FreshNames
import L0C.FreshNames hiding (newName, newID, newVName)

-- | A monad that stores a name source.  The following is a good
-- instance for a monad in which the only state is a @NameSource vn@:
--
-- @
--  instance MonadFreshNames vn MyMonad where
--    getNameSource = get
--    putNameSource = put
-- @
class (Applicative m, Monad m, VarName vn)  => MonadFreshNames vn m where
  getNameSource :: m (NameSource vn)
  putNameSource :: NameSource vn -> m ()

instance (Applicative im, VarName vn, Monad im) => MonadFreshNames vn (Control.Monad.State.Lazy.StateT (NameSource vn) im) where
  getNameSource = Control.Monad.State.Lazy.get
  putNameSource = Control.Monad.State.Lazy.put

instance (Applicative im, VarName vn, Monad im) => MonadFreshNames vn (Control.Monad.State.Strict.StateT (NameSource vn) im) where
  getNameSource = Control.Monad.State.Strict.get
  putNameSource = Control.Monad.State.Strict.put

-- | Produce a fresh name, using the given name as a template.
newName :: MonadFreshNames vn m => vn -> m vn
newName s = do src <- getNameSource
               let (s', src') = FreshNames.newName src s
               putNameSource src'
               return s'

-- | As @newName@, but takes a 'String' for the name template.
newNameFromString :: MonadFreshNames vn m => String -> m vn
newNameFromString s = newName $ varName s Nothing

-- | Produce a fresh 'ID', using the given base name as a template.
newID :: MonadFreshNames (ID vn) m => vn -> m (ID vn)
newID s = newName $ ID (s, 0)

-- | As 'newID', but takes a 'String' for the name template.
newIDFromString :: VarName vn => MonadFreshNames (ID vn) m => String -> m (ID vn)
newIDFromString s = newID $ varName s Nothing

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: MonadFreshNames VName m => String -> m VName
newVName = newID . nameFromString

-- | Produce a fresh 'Ident', using the given name as a template.
newIdent :: (VarName vn, MonadFreshNames (ID vn) m) =>
            String -> ty (ID vn) -> SrcLoc -> m (IdentBase ty (ID vn))
newIdent s t loc = do
  s' <- newID $ varName s Nothing
  return $ Ident s' t loc
