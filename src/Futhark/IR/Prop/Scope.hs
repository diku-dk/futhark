{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
-- | The core Futhark AST does not contain type information when we
-- use a variable.  Therefore, most transformations expect to be able
-- to access some kind of symbol table that maps names to their types.
--
-- This module defines the concept of a type environment as a mapping
-- from variable names to 'NameInfo's.  Convenience facilities are
-- also provided to communicate that some monad or applicative functor
-- maintains type information.
module Futhark.IR.Prop.Scope
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

         -- * Extended type environment
       , ExtendedScope
       , extendedScope
       ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.RWS.Strict
import qualified Control.Monad.RWS.Lazy
import qualified Data.Map.Strict as M

import Futhark.IR.Decorations
import Futhark.IR.Syntax
import Futhark.IR.Prop.Types
import Futhark.IR.Prop.Patterns
import Futhark.IR.Pretty ()

-- | How some name in scope was bound.
data NameInfo lore = LetName (LetDec lore)
                   | FParamName (FParamInfo lore)
                   | LParamName (LParamInfo lore)
                   | IndexName IntType

deriving instance Decorations lore => Show (NameInfo lore)

instance Decorations lore => Typed (NameInfo lore) where
  typeOf (LetName dec) = typeOf dec
  typeOf (FParamName dec) = typeOf dec
  typeOf (LParamName dec) = typeOf dec
  typeOf (IndexName it) = Prim $ IntType it

-- | A scope is a mapping from variable names to information about
-- that name.
type Scope lore = M.Map VName (NameInfo lore)

-- | The class of applicative functors (or more common in practice:
-- monads) that permit the lookup of variable types.  A default method
-- for 'lookupType' exists, which is sufficient (if not always
-- maximally efficient, and using 'error' to fail) when 'askScope'
-- is defined.
class (Applicative m, Decorations lore) => HasScope lore m | m -> lore where
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

instance (Applicative m, Monad m, Decorations lore) =>
         HasScope lore (ReaderT (Scope lore) m) where
  askScope = ask

instance (Monad m, HasScope lore m) => HasScope lore (ExceptT e m) where
  askScope = lift askScope

instance (Applicative m, Monad m, Monoid w, Decorations lore) =>
         HasScope lore (Control.Monad.RWS.Strict.RWST (Scope lore) w s m) where
  askScope = ask

instance (Applicative m, Monad m, Monoid w, Decorations lore) =>
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

instance (Applicative m, Monad m, Decorations lore) =>
         LocalScope lore (ReaderT (Scope lore) m) where
  localScope = local . M.union

instance (Applicative m, Monad m, Monoid w, Decorations lore) =>
         LocalScope lore (Control.Monad.RWS.Strict.RWST (Scope lore) w s m) where
  localScope = local . M.union

instance (Applicative m, Monad m, Monoid w, Decorations lore) =>
         LocalScope lore (Control.Monad.RWS.Lazy.RWST (Scope lore) w s m) where
  localScope = local . M.union

-- | The class of things that can provide a scope.  There is no
-- overarching rule for what this means.  For a 'Stm', it is the
-- corresponding pattern.  For a t'Lambda', is is the parameters.
class Scoped lore a | a -> lore where
  scopeOf :: a -> Scope lore

-- | Extend the monadic scope with the 'scopeOf' the given value.
inScopeOf :: (Scoped lore a, LocalScope lore m) => a -> m b -> m b
inScopeOf = localScope . scopeOf

instance Scoped lore a => Scoped lore [a] where
  scopeOf = mconcat . map scopeOf

instance Scoped lore (Stms lore) where
  scopeOf = foldMap scopeOf

instance Scoped lore (Stm lore) where
  scopeOf = scopeOfPattern . stmPattern

instance Scoped lore (FunDef lore) where
  scopeOf = scopeOfFParams . funDefParams

instance Scoped lore (VName, NameInfo lore) where
  scopeOf = uncurry M.singleton

instance Scoped lore (LoopForm lore) where
  scopeOf (WhileLoop _) = mempty
  scopeOf (ForLoop i it _ xs) =
    M.insert i (IndexName it) $ scopeOfLParams (map fst xs)

-- | The scope of a pattern.
scopeOfPattern :: LetDec lore ~ dec => PatternT dec -> Scope lore
scopeOfPattern =
  mconcat . map scopeOfPatElem . patternElements

-- | The scope of a pattern element.
scopeOfPatElem :: LetDec lore ~ dec => PatElemT dec -> Scope lore
scopeOfPatElem (PatElem name dec) = M.singleton name $ LetName dec

-- | The scope of some lambda parameters.
scopeOfLParams :: LParamInfo lore ~ dec =>
                  [Param dec] -> Scope lore
scopeOfLParams = M.fromList . map f
  where f param = (paramName param, LParamName $ paramDec param)

-- | The scope of some function or loop parameters.
scopeOfFParams :: FParamInfo lore ~ dec =>
                  [Param dec] -> Scope lore
scopeOfFParams = M.fromList . map f
  where f param = (paramName param, FParamName $ paramDec param)

instance Scoped lore (Lambda lore) where
  scopeOf lam = scopeOfLParams $ lambdaParams lam

-- | A constraint that indicates two lores have the same 'NameInfo'
-- representation.
type SameScope lore1 lore2 = (LetDec lore1 ~ LetDec lore2,
                              FParamInfo lore1 ~ FParamInfo lore2,
                              LParamInfo lore1 ~ LParamInfo lore2)

-- | If two scopes are really the same, then you can convert one to
-- the other.
castScope :: SameScope fromlore tolore =>
             Scope fromlore -> Scope tolore
castScope = M.map castNameInfo

castNameInfo :: SameScope fromlore tolore =>
                NameInfo fromlore -> NameInfo tolore
castNameInfo (LetName dec) = LetName dec
castNameInfo (FParamName dec) = FParamName dec
castNameInfo (LParamName dec) = LParamName dec
castNameInfo (IndexName it) = IndexName it

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
