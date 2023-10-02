{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The core Futhark AST does not contain type information when we
-- use a variable.  Therefore, most transformations expect to be able
-- to access some kind of symbol table that maps names to their types.
--
-- This module defines the concept of a type environment as a mapping
-- from variable names to 'NameInfo's.  Convenience facilities are
-- also provided to communicate that some monad or applicative functor
-- maintains type information.
--
-- A simple example of a monad that maintains such as environment is
-- 'Reader'.  Indeed, 'HasScope' and 'LocalScope' instances for this
-- monad are already defined.
module Futhark.IR.Prop.Scope
  ( HasScope (..),
    NameInfo (..),
    LocalScope (..),
    Scope,
    Scoped (..),
    inScopeOf,
    scopeOfLParams,
    scopeOfFParams,
    scopeOfLoopForm,
    scopeOfPat,
    scopeOfPatElem,
    SameScope,
    castScope,

    -- * Extended type environment
    ExtendedScope,
    extendedScope,
  )
where

import Control.Monad.Except
import Control.Monad.RWS.Lazy qualified
import Control.Monad.RWS.Strict qualified
import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Futhark.IR.Pretty ()
import Futhark.IR.Prop.Types
import Futhark.IR.Rep
import Futhark.IR.Syntax

-- | How some name in scope was bound.
data NameInfo rep
  = LetName (LetDec rep)
  | FParamName (FParamInfo rep)
  | LParamName (LParamInfo rep)
  | IndexName IntType

deriving instance (RepTypes rep) => Show (NameInfo rep)

instance (RepTypes rep) => Typed (NameInfo rep) where
  typeOf (LetName dec) = typeOf dec
  typeOf (FParamName dec) = typeOf dec
  typeOf (LParamName dec) = typeOf dec
  typeOf (IndexName it) = Prim $ IntType it

-- | A scope is a mapping from variable names to information about
-- that name.
type Scope rep = M.Map VName (NameInfo rep)

-- | The class of applicative functors (or more common in practice:
-- monads) that permit the lookup of variable types.  A default method
-- for 'lookupType' exists, which is sufficient (if not always
-- maximally efficient, and using 'error' to fail) when 'askScope'
-- is defined.
class (Applicative m, RepTypes rep) => HasScope rep m | m -> rep where
  -- | Return the type of the given variable, or fail if it is not in
  -- the type environment.
  lookupType :: VName -> m Type
  lookupType = fmap typeOf . lookupInfo

  -- | Return the info of the given variable, or fail if it is not in
  -- the type environment.
  lookupInfo :: VName -> m (NameInfo rep)
  lookupInfo name =
    asksScope (M.findWithDefault notFound name)
    where
      notFound =
        error $
          "Scope.lookupInfo: Name "
            ++ prettyString name
            ++ " not found in type environment."

  -- | Return the type environment contained in the applicative
  -- functor.
  askScope :: m (Scope rep)

  -- | Return the result of applying some function to the type
  -- environment.
  asksScope :: (Scope rep -> a) -> m a
  asksScope f = f <$> askScope

instance
  (Monad m, RepTypes rep) =>
  HasScope rep (ReaderT (Scope rep) m)
  where
  askScope = ask

instance (Monad m, HasScope rep m) => HasScope rep (ExceptT e m) where
  askScope = lift askScope

instance
  (Monad m, Monoid w, RepTypes rep) =>
  HasScope rep (Control.Monad.RWS.Strict.RWST (Scope rep) w s m)
  where
  askScope = ask

instance
  (Monad m, Monoid w, RepTypes rep) =>
  HasScope rep (Control.Monad.RWS.Lazy.RWST (Scope rep) w s m)
  where
  askScope = ask

-- | The class of monads that not only provide a 'Scope', but also
-- the ability to locally extend it.  A 'Reader' containing a
-- 'Scope' is the prototypical example of such a monad.
class (HasScope rep m, Monad m) => LocalScope rep m where
  -- | Run a computation with an extended type environment.  Note that
  -- this is intended to *add* to the current type environment, it
  -- does not replace it.
  localScope :: Scope rep -> m a -> m a

instance (LocalScope rep m) => LocalScope rep (ExceptT e m) where
  localScope = mapExceptT . localScope

instance
  (Monad m, RepTypes rep) =>
  LocalScope rep (ReaderT (Scope rep) m)
  where
  localScope = local . M.union

instance
  (Monad m, Monoid w, RepTypes rep) =>
  LocalScope rep (Control.Monad.RWS.Strict.RWST (Scope rep) w s m)
  where
  localScope = local . M.union

instance
  (Monad m, Monoid w, RepTypes rep) =>
  LocalScope rep (Control.Monad.RWS.Lazy.RWST (Scope rep) w s m)
  where
  localScope = local . M.union

-- | The class of things that can provide a scope.  There is no
-- overarching rule for what this means.  For a 'Stm', it is the
-- corresponding pattern.  For a t'Lambda', is is the parameters.
class Scoped rep a | a -> rep where
  scopeOf :: a -> Scope rep

-- | Extend the monadic scope with the 'scopeOf' the given value.
inScopeOf :: (Scoped rep a, LocalScope rep m) => a -> m b -> m b
inScopeOf = localScope . scopeOf

instance (Scoped rep a) => Scoped rep [a] where
  scopeOf = mconcat . map scopeOf

instance Scoped rep (Stms rep) where
  scopeOf = foldMap scopeOf

instance Scoped rep (Stm rep) where
  scopeOf = scopeOfPat . stmPat

instance Scoped rep (FunDef rep) where
  scopeOf = scopeOfFParams . funDefParams

instance Scoped rep (VName, NameInfo rep) where
  scopeOf = uncurry M.singleton

-- | The scope of a loop form.
scopeOfLoopForm :: LoopForm -> Scope rep
scopeOfLoopForm (WhileLoop _) = mempty
scopeOfLoopForm (ForLoop i it _) = M.singleton i $ IndexName it

-- | The scope of a pattern.
scopeOfPat :: (LetDec rep ~ dec) => Pat dec -> Scope rep
scopeOfPat =
  mconcat . map scopeOfPatElem . patElems

-- | The scope of a pattern element.
scopeOfPatElem :: (LetDec rep ~ dec) => PatElem dec -> Scope rep
scopeOfPatElem (PatElem name dec) = M.singleton name $ LetName dec

-- | The scope of some lambda parameters.
scopeOfLParams ::
  (LParamInfo rep ~ dec) =>
  [Param dec] ->
  Scope rep
scopeOfLParams = M.fromList . map f
  where
    f param = (paramName param, LParamName $ paramDec param)

-- | The scope of some function or loop parameters.
scopeOfFParams ::
  (FParamInfo rep ~ dec) =>
  [Param dec] ->
  Scope rep
scopeOfFParams = M.fromList . map f
  where
    f param = (paramName param, FParamName $ paramDec param)

instance Scoped rep (Lambda rep) where
  scopeOf lam = scopeOfLParams $ lambdaParams lam

-- | A constraint that indicates two representations have the same 'NameInfo'
-- representation.
type SameScope rep1 rep2 =
  ( LetDec rep1 ~ LetDec rep2,
    FParamInfo rep1 ~ FParamInfo rep2,
    LParamInfo rep1 ~ LParamInfo rep2
  )

-- | If two scopes are really the same, then you can convert one to
-- the other.
castScope ::
  (SameScope fromrep torep) =>
  Scope fromrep ->
  Scope torep
castScope = M.map castNameInfo

castNameInfo ::
  (SameScope fromrep torep) =>
  NameInfo fromrep ->
  NameInfo torep
castNameInfo (LetName dec) = LetName dec
castNameInfo (FParamName dec) = FParamName dec
castNameInfo (LParamName dec) = LParamName dec
castNameInfo (IndexName it) = IndexName it

-- | A monad transformer that carries around an extended 'Scope'.
-- Its 'lookupType' method will first look in the extended 'Scope',
-- and then use the 'lookupType' method of the underlying monad.
newtype ExtendedScope rep m a = ExtendedScope (ReaderT (Scope rep) m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Scope rep)
    )

instance
  (HasScope rep m, Monad m) =>
  HasScope rep (ExtendedScope rep m)
  where
  lookupType name = do
    res <- asks $ fmap typeOf . M.lookup name
    maybe (ExtendedScope $ lift $ lookupType name) pure res
  askScope = asks M.union <*> ExtendedScope (lift askScope)

-- | Run a computation in the extended type environment.
extendedScope ::
  ExtendedScope rep m a ->
  Scope rep ->
  m a
extendedScope (ExtendedScope m) = runReaderT m
