{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
-- | This module defines the concept of a type environment as a
-- mapping from variable names to 'Type's.  Convenience facilities are
-- also provided to communicate that some monad or applicative functor
-- maintains type information.
module Futhark.Representation.AST.Attributes.TypeEnv
       ( HasTypeEnv (..)
       , NameType (..)
       , LocalTypeEnv (..)
       , TypeEnv
         -- * Extended type environment
       , ExtendedTypeEnv
       , extendedTypeEnv
       ) where

import Control.Applicative
import Control.Monad.Reader
import qualified Control.Monad.RWS.Strict
import qualified Control.Monad.RWS.Lazy
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.AST.Annotations
import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.Attributes.Types
import Futhark.Transform.Substitute

-- | How some name in scope was bound.
data NameType lore = LetType (LetAttr lore)
                   | FParamType (FParamAttr lore)
                   | LParamType (LParamAttr lore)
                   | IndexType

instance Annotations lore => Typed (NameType lore) where
  typeOf (LetType attr) = typeOf attr
  typeOf (FParamType attr) = typeOf attr
  typeOf (LParamType attr) = typeOf attr
  typeOf IndexType = Basic Int

instance Substitutable lore => Substitute (NameType lore) where
  substituteNames subst (LetType attr) =
    LetType $ substituteNames subst attr
  substituteNames subst (FParamType attr) =
    FParamType $ substituteNames subst attr
  substituteNames subst (LParamType attr) =
    LParamType $ substituteNames subst attr
  substituteNames _ IndexType =
    IndexType

-- | A type environment is a mapping from variable names to something
-- that contains a type.
type TypeEnv t = HM.HashMap VName t

-- | The class of applicative functors (or more common in practice:
-- monads) that permit the lookup of variable types.  A default method
-- for 'lookupType' exists, which is sufficient (if not always
-- maximally efficient, and using 'error' to fail) when 'askTypeEnv'
-- is defined.
class (Applicative m, Typed t) => HasTypeEnv t m | m -> t where
  -- | Return the type of the given variable, or fail if it is not in
  -- the type environment.
  lookupType :: VName -> m Type
  lookupType = liftA typeOf . lookupInfo

  -- | Return the info of the given variable, or fail if it is not in
  -- the type environment.
  lookupInfo :: VName -> m t
  lookupInfo name =
    asksTypeEnv (HM.lookupDefault notFound name)
    where notFound =
            error $ "TypeEnv.lookupInfo: Name " ++ textual name ++
            " not found in type environment."

  -- | Return the type environment contained in the applicative
  -- functor.
  askTypeEnv :: m (TypeEnv t)

  -- | Return the result of applying some function to the type
  -- environment.
  asksTypeEnv :: (TypeEnv t -> a) -> m a
  asksTypeEnv f = f <$> askTypeEnv

instance (Applicative m, Monad m, Typed t) =>
         HasTypeEnv t (ReaderT (TypeEnv t) m) where
  askTypeEnv = ask

instance (Applicative m, Monad m, Monoid w, Typed t) =>
         HasTypeEnv t (Control.Monad.RWS.Strict.RWST (TypeEnv t) w s m) where
  askTypeEnv = ask

instance (Applicative m, Monad m, Monoid w, Typed t) =>
         HasTypeEnv t (Control.Monad.RWS.Lazy.RWST (TypeEnv t) w s m) where
  askTypeEnv = ask

-- | The class of monads that not only provide a 'TypeEnv', but also
-- the ability to locally extend it.  A 'Reader' containing a
-- 'TypeEnv' is the prototypical example of such a monad.
class (HasTypeEnv t m, Monad m) => LocalTypeEnv t m where
  -- | Run a computation with an extended type environment.  Note that
  -- this is intended to *add* to the current type environment, it
  -- does not replace it.
  localTypeEnv :: TypeEnv t -> m a -> m a

instance (Applicative m, Monad m, Typed t) =>
         LocalTypeEnv t (ReaderT (TypeEnv t) m) where
  localTypeEnv = local . HM.union

instance (Applicative m, Monad m, Monoid w, Typed t) =>
         LocalTypeEnv t (Control.Monad.RWS.Strict.RWST (TypeEnv t) w s m) where
  localTypeEnv = local . HM.union

instance (Applicative m, Monad m, Monoid w, Typed t) =>
         LocalTypeEnv t (Control.Monad.RWS.Lazy.RWST (TypeEnv t) w s m) where
  localTypeEnv = local . HM.union

-- | A monad transformer that carries around an extended 'TypeEnv'.
-- Its 'lookupType' method will first look in the extended 'TypeEnv',
-- and then use the 'lookupType' method of the underlying monad.
newtype ExtendedTypeEnv lore m a = ExtendedTypeEnv (ReaderT (TypeEnv (NameType lore)) m a)
                            deriving (Functor, Applicative, Monad,
                                      MonadReader (TypeEnv (NameType lore)))

instance (HasTypeEnv (NameType lore) m, Monad m) =>
         HasTypeEnv (NameType lore) (ExtendedTypeEnv lore m) where
  lookupType name = do
    res <- asks $ fmap typeOf . HM.lookup name
    maybe (ExtendedTypeEnv $ lift $ lookupType name) return res
  askTypeEnv = HM.union <$> ask <*> ExtendedTypeEnv (lift askTypeEnv)

-- | Run a computation in the extended type environment.
extendedTypeEnv :: Annotations lore => ExtendedTypeEnv lore m a -> TypeEnv (NameType lore) -> m a
extendedTypeEnv (ExtendedTypeEnv m) = runReaderT m
