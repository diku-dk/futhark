{-# LANGUAGE FlexibleInstances #-}
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
  , newIdent'
  , newIdents
  , module L0C.FreshNames
  ) where

import Control.Applicative
import qualified Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict

import Data.Loc

import L0C.InternalRep
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
class (Applicative m, Monad m) => MonadFreshNames m where
  getNameSource :: m VNameSource
  putNameSource :: VNameSource -> m ()

instance (Applicative im, Monad im) => MonadFreshNames (Control.Monad.State.Lazy.StateT VNameSource im) where
  getNameSource = Control.Monad.State.Lazy.get
  putNameSource = Control.Monad.State.Lazy.put

instance (Applicative im, Monad im) => MonadFreshNames (Control.Monad.State.Strict.StateT VNameSource im) where
  getNameSource = Control.Monad.State.Strict.get
  putNameSource = Control.Monad.State.Strict.put

-- | Produce a fresh name, using the given name as a template.
newName :: MonadFreshNames m => VName -> m VName
newName s = do src <- getNameSource
               let (s', src') = FreshNames.newName src s
               putNameSource src'
               return s'

-- | As @newName@, but takes a 'String' for the name template.
newNameFromString :: MonadFreshNames m => String -> m VName
newNameFromString s = newName $ varName s Nothing

-- | Produce a fresh 'ID', using the given base name as a template.
newID :: MonadFreshNames m => Name -> m VName
newID s = newName $ ID (s, 0)

-- | As 'newID', but takes a 'String' for the name template.
newIDFromString :: MonadFreshNames m => String -> m VName
newIDFromString s = newID $ varName s Nothing

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: MonadFreshNames m => String -> m VName
newVName = newID . nameFromString

-- | Produce a fresh 'Ident', using the given name as a template.
newIdent :: MonadFreshNames m =>
            String -> TypeBase als shape -> SrcLoc -> m (IdentBase als shape)
newIdent s t loc = do
  s' <- newID $ varName s Nothing
  return $ Ident s' t loc

-- | Produce a fresh 'Ident', using the given 'Ident' as a template,
-- but possibly modifying the name.
newIdent' :: MonadFreshNames m =>
             (String -> String)
          -> IdentBase als shape -> m (IdentBase als shape)
newIdent' f ident =
  newIdent (f $ nameToString $ baseName $ identName ident)
           (identType ident) $
           srclocOf ident

-- | Produce several 'Ident's, using the given name as a template,
-- based on a list of types.
newIdents :: MonadFreshNames m =>
             String -> [TypeBase als shape] -> SrcLoc -> m [IdentBase als shape]
newIdents s ts loc =
  mapM (\t -> newIdent s t loc) ts
