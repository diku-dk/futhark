{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Futhark.Representation.AST.Attributes.TypeEnv
       ( HasTypeEnv (..)
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

type TypeEnv = HM.HashMap VName Type

class Applicative m => HasTypeEnv m where
  lookupType :: VName -> m Type
  lookupType name =
    fromMaybe notFound <$> HM.lookup name <$> askTypeEnv
    where notFound =
            error $ "TypeEnv.lookupType: Name " ++ textual name ++
            " not found in type environment."

  askTypeEnv :: m TypeEnv

instance (Applicative m, Monad m) => HasTypeEnv (ReaderT TypeEnv m) where
  askTypeEnv = ask

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

-- Run a computation in the extended type environment.
extendedTypeEnv :: ExtendedTypeEnv m a -> TypeEnv -> m a
extendedTypeEnv (ExtendedTypeEnv m) = runReaderT m
