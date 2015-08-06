{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
-- | This module defines the concept of a type environment as a
-- mapping from variable names to 'Type's.  Convenience facilities are
-- also provided to communicate that some monad or applicative functor
-- maintains type information.
module Futhark.Representation.AST.Attributes.TypeEnv
       ( HasTypeEnv (..)
       , LocalTypeEnv (..)
       , TypeEnv
         -- * Extended type environment
       , ExtendedTypeEnv
       , extendedTypeEnv
       ) where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.AST.Syntax.Core

-- | A type environment is a mapping from variable names to types.
type TypeEnv = HM.HashMap VName Type

-- | The class of applicative functors (or more common in practice:
-- monads) that permit the lookup of variable types.  A default method
-- for 'lookupType' exists, which is sufficient (if not always
-- maximally efficient, and using 'error' to fail) when 'askTypeEnv'
-- is defined.
class Applicative m => HasTypeEnv m where
  -- | Return the type of the given variable, or fail if it is not in
  -- the type environment.
  lookupType :: VName -> m Type
  lookupType name =
    HM.lookupDefault notFound name <$> askTypeEnv
    where notFound =
            error $ "TypeEnv.lookupType: Name " ++ textual name ++
            " not found in type environment."

  -- | Return the type environment contained in the applicative
  -- functor.
  askTypeEnv :: m TypeEnv

instance (Applicative m, Monad m) => HasTypeEnv (ReaderT TypeEnv m) where
  askTypeEnv = ask

-- | The class of monads that not only provide a 'TypeEnv', but also
-- the ability to locally extend it.  A 'Reader' containing a
-- 'TypeEnv' is the prototypical example of such a monad.
class (HasTypeEnv m, Monad m) => LocalTypeEnv m where
  -- | Run a computation with an extended type environment.  Note that
  -- this is intended to *add* to the current type environment, it
  -- does not replace it.
  localTypeEnv :: TypeEnv -> m a -> m a

instance (Applicative m, Monad m) => LocalTypeEnv (ReaderT TypeEnv m) where
  localTypeEnv = local . HM.union

-- | A monad transformer that carries around an extended 'TypeEnv'.
-- Its 'lookupType' method will first look in the extended 'TypeEnv',
-- and then use the 'lookupType' method of the underlying monad.
newtype ExtendedTypeEnv m a = ExtendedTypeEnv (ReaderT TypeEnv m a)
                            deriving (Functor, Applicative, Monad,
                                      MonadReader TypeEnv)

instance (HasTypeEnv m, Monad m) => HasTypeEnv (ExtendedTypeEnv m) where
  lookupType name = do
    res <- asks $ HM.lookup name
    maybe (ExtendedTypeEnv $ lift $ lookupType name) return res
  askTypeEnv = HM.union <$> ask <*> ExtendedTypeEnv (lift askTypeEnv)

-- | Run a computation in the extended type environment.
extendedTypeEnv :: ExtendedTypeEnv m a -> TypeEnv -> m a
extendedTypeEnv (ExtendedTypeEnv m) = runReaderT m
