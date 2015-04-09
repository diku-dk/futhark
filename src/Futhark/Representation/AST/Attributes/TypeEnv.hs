{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Futhark.Representation.AST.Attributes.TypeEnv
       ( HasTypeEnv (..)
       , TypeEnv
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
  lookupTypeM :: VName -> m Type
  lookupTypeM name =
    maybe notFound id <$> HM.lookup name <$> askTypeEnv
    where notFound = error $ "lookupTypeM: Name " ++ textual name ++
                     " not found in type environment."

  askTypeEnv :: m TypeEnv

instance (Applicative m, Monad m) => HasTypeEnv (ReaderT TypeEnv m) where
  askTypeEnv = ask
