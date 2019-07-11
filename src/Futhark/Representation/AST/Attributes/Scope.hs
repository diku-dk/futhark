{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | This module defines the concept of a type environment as a
-- mapping from variable names to 'Type's.  Convenience facilities are
-- also provided to communicate that some monad or applicative functor
-- maintains type information.
module Futhark.Representation.AST.Attributes.Scope
       ( HasScope (..)
       , NameInfo (..)
       , LocalScope (..)
       , Scope
       , Scoped(..)
       , inScopeOf
       , scopeOfLParams
       , scopeOfFParams
       , scopeOfPattern
       , scopeOfPatElem

       , SameScope
       , castScope
       , castNameInfo

         -- * Extended type environment
       , ExtendedScope
       , extendedScope
       ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.RWS.Strict
import qualified Control.Monad.RWS.Lazy
import Data.Foldable
import qualified Data.Map.Strict as M

import Futhark.Representation.AST.Annotations
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Pretty ()

-- | How some name in scope was bound.
data NameInfo lore = LetInfo (LetAttr lore)
                   | FParamInfo (FParamAttr lore)
                   | LParamInfo (LParamAttr lore)
                   | IndexInfo IntType

deriving instance Annotations lore => Show (NameInfo lore)

instance Annotations lore => Typed (NameInfo lore) where
  typeOf (LetInfo attr) = typeOf attr
  typeOf (FParamInfo attr) = typeOf attr
  typeOf (LParamInfo attr) = typeOf attr
  typeOf (IndexInfo it) = Prim $ IntType it

-- | A scope is a mapping from variable names to information about
-- that name.
type Scope lore = M.Map VName (NameInfo lore)

-- | The class of applicative functors (or more common in practice:
-- monads) that permit the lookup of variable types.  A default method
-- for 'lookupType' exists, which is sufficient (if not always
-- maximally efficient, and using 'error' to fail) when 'askScope'
-- is defined.
class (Applicative m, Annotations lore) => HasScope lore m | m -> lore where
  -- | Return the type of the given variable, or fail if it is not in
  -- the type environment.
  lookupType :: VName -> m Type
  lookupType = fmap typeOf . lookupInfo

  -- | Return the info of the given variable, or fail if it is not in
  -- the type environment.
  lookupInfo :: VName -> m (NameInfo lore)
  lookupInfo name =
    asksScope (M.findWithDefault notFound name)
    where notFound =
            error $ "Scope.lookupInfo: Name " ++ pretty name ++
            " not found in type environment."

  -- | Return the type environment contained in the applicative
  -- functor.
  askScope :: m (Scope lore)

  -- | Return the result of applying some function to the type
  -- environment.
  asksScope :: (Scope lore -> a) -> m a
  asksScope f = f <$> askScope

instance (Applicative m, Monad m, Annotations lore) =>
         HasScope lore (ReaderT (Scope lore) m) where
  askScope = ask

instance (Monad m, HasScope lore m) => HasScope lore (ExceptT e m) where
  askScope = lift askScope

instance (Applicative m, Monad m, Monoid w, Annotations lore) =>
         HasScope lore (Control.Monad.RWS.Strict.RWST (Scope lore) w s m) where
  askScope = ask

instance (Applicative m, Monad m, Monoid w, Annotations lore) =>
         HasScope lore (Control.Monad.RWS.Lazy.RWST (Scope lore) w s m) where
  askScope = ask

-- | The class of monads that not only provide a 'Scope', but also
-- the ability to locally extend it.  A 'Reader' containing a
-- 'Scope' is the prototypical example of such a monad.
class (HasScope lore m, Monad m) => LocalScope lore m where
  -- | Run a computation with an extended type environment.  Note that
  -- this is intended to *add* to the current type environment, it
  -- does not replace it.
  localScope :: Scope lore -> m a -> m a

instance (Monad m, LocalScope lore m) => LocalScope lore (ExceptT e m) where
  localScope = mapExceptT . localScope

instance (Applicative m, Monad m, Annotations lore) =>
         LocalScope lore (ReaderT (Scope lore) m) where
  localScope = local . M.union

instance (Applicative m, Monad m, Monoid w, Annotations lore) =>
         LocalScope lore (Control.Monad.RWS.Strict.RWST (Scope lore) w s m) where
  localScope = local . M.union

instance (Applicative m, Monad m, Monoid w, Annotations lore) =>
         LocalScope lore (Control.Monad.RWS.Lazy.RWST (Scope lore) w s m) where
  localScope = local . M.union

-- | The class of things that can provide a scope.  There is no
-- overarching rule for what this means.  For a 'Stm', it is the
-- corresponding pattern.  For a 'Lambda', is is the parameters
-- (including index).
class Scoped lore a | a -> lore where
  scopeOf :: a -> Scope lore

inScopeOf :: (Scoped lore a, LocalScope lore m) => a -> m b -> m b
inScopeOf = localScope . scopeOf

instance Scoped lore a => Scoped lore [a] where
  scopeOf = mconcat . map scopeOf

instance Scoped lore (Stms lore) where
  scopeOf = fold . fmap scopeOf

instance Scoped lore (Stm lore) where
  scopeOf = scopeOfPattern . stmPattern

instance Scoped lore (FunDef lore) where
  scopeOf = scopeOfFParams . funDefParams

instance Scoped lore (VName, NameInfo lore) where
  scopeOf = uncurry M.singleton

instance Scoped lore (LoopForm lore) where
  scopeOf (WhileLoop _) = mempty
  scopeOf (ForLoop i it _ xs) =
    M.insert i (IndexInfo it) $ scopeOfLParams (map fst xs)

scopeOfPattern :: LetAttr lore ~ attr => PatternT attr -> Scope lore
scopeOfPattern =
  mconcat . map scopeOfPatElem . patternElements

scopeOfPatElem :: LetAttr lore ~ attr => PatElemT attr -> Scope lore
scopeOfPatElem (PatElem name attr) = M.singleton name $ LetInfo attr

scopeOfLParams :: LParamAttr lore ~ attr =>
                  [ParamT attr] -> Scope lore
scopeOfLParams = M.fromList . map f
  where f param = (paramName param, LParamInfo $ paramAttr param)

scopeOfFParams :: FParamAttr lore ~ attr =>
                  [ParamT attr] -> Scope lore
scopeOfFParams = M.fromList . map f
  where f param = (paramName param, FParamInfo $ paramAttr param)

instance Scoped lore (Lambda lore) where
  scopeOf lam = scopeOfLParams $ lambdaParams lam

type SameScope lore1 lore2 = (LetAttr lore1 ~ LetAttr lore2,
                              FParamAttr lore1 ~ FParamAttr lore2,
                              LParamAttr lore1 ~ LParamAttr lore2)

-- | If two scopes are really the same, then you can convert one to
-- the other.
castScope :: SameScope fromlore tolore =>
             Scope fromlore -> Scope tolore
castScope = M.map castNameInfo

castNameInfo :: SameScope fromlore tolore =>
                NameInfo fromlore -> NameInfo tolore
castNameInfo (LetInfo attr) = LetInfo attr
castNameInfo (FParamInfo attr) = FParamInfo attr
castNameInfo (LParamInfo attr) = LParamInfo attr
castNameInfo (IndexInfo it) = IndexInfo it

-- | A monad transformer that carries around an extended 'Scope'.
-- Its 'lookupType' method will first look in the extended 'Scope',
-- and then use the 'lookupType' method of the underlying monad.
newtype ExtendedScope lore m a = ExtendedScope (ReaderT (Scope lore) m a)
                            deriving (Functor, Applicative, Monad,
                                      MonadReader (Scope lore))

instance (HasScope lore m, Monad m) =>
         HasScope lore (ExtendedScope lore m) where
  lookupType name = do
    res <- asks $ fmap typeOf . M.lookup name
    maybe (ExtendedScope $ lift $ lookupType name) return res
  askScope = asks M.union <*> ExtendedScope (lift askScope)

-- | Run a computation in the extended type environment.
extendedScope :: ExtendedScope lore m a
              -> Scope lore
              -> m a
extendedScope (ExtendedScope m) = runReaderT m
