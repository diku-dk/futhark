-- | The notion of scope used during term checking, and other small
-- definitions shared between the unsized term checker
-- ("Language.Futhark.TypeChecker.Terms.Unsized") and the sized one
-- ("Language.Futhark.TypeChecker.Terms"). The two checkers represent
-- sizes differently, so the definitions here are parameterised over
-- the size representation, and the functions take a function for
-- converting the sizes of module-level types.
module Language.Futhark.TypeChecker.Terms.Scope
  ( ValBinding (..),
    TermScope (..),
    Inferred (..),
    envToTermScope,
    envToTermScopeNoVals,
    lookupOuterVal,
    initialTermScope,
    lookupQualNameEnv,
    typeParamIdent,
  )
where

import Data.Map.Strict qualified as M
import Data.Maybe
import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Monad qualified as TypeM

-- | What a bound value stands for. Note that although modules are in the same
-- name space, they are not value bindings.
data ValBinding dim
  = BoundV [TypeParam] (TypeBase dim NoUniqueness)
  | OverloadedF [PrimType] [Maybe PrimType] (Maybe PrimType)
  | EqualityF
  | -- | A function currently being checked, in scope of its own body so that it
    -- may be used recursively. Each occurrence adopts the type recorded for it
    -- by the unsized type checker, with fresh sizes - just like a call of a
    -- monomorphic function, and which allows "size-polymorphic recursion" (but
    -- not general polymorphic recursion).
    RecursiveV
  deriving (Show)

-- | The lexical scope information.
data TermScope dim = TermScope
  { scopeVtable :: M.Map VName (ValBinding dim),
    scopeTypeTable :: M.Map VName TypeBinding,
    scopeModTable :: M.Map VName Mod
  }
  deriving (Show)

instance Semigroup (TermScope dim) where
  TermScope vt1 tt1 mt1 <> TermScope vt2 tt2 mt2 =
    TermScope (vt2 `M.union` vt1) (tt2 `M.union` tt1) (mt1 `M.union` mt2)

-- | During type checking a pattern, we might find an explicit ascription. These
-- contain complete type information (although they must of course still be
-- checked against what remains of the pattern).
data Inferred t
  = NoneInferred
  | Ascribed t
  deriving (Show)

instance Functor Inferred where
  fmap _ NoneInferred = NoneInferred
  fmap f (Ascribed t) = Ascribed (f t)

-- | Create a scope from a module-level environment.
envToTermScope ::
  (StructType -> TypeBase dim NoUniqueness) ->
  Env ->
  TermScope dim
envToTermScope onType env =
  TermScope
    { scopeVtable = vtable,
      scopeTypeTable = envTypeTable env,
      scopeModTable = envModTable env
    }
  where
    vtable = M.map valBinding $ envVtable env
    valBinding (TypeM.BoundV tps v) = BoundV tps $ onType v

-- | Like 'envToTermScope', but omits the (potentially very large)
-- value table. Its bindings are instead looked up on demand with
-- 'lookupOuterVal'. This avoids transforming the entire value table of
-- the outer environment - which for a top-level definition includes
-- the whole prelude - once for every binding we check.
envToTermScopeNoVals :: Env -> TermScope dim
envToTermScopeNoVals env =
  TermScope
    { scopeVtable = mempty,
      scopeTypeTable = envTypeTable env,
      scopeModTable = envModTable env
    }

-- | Look up a single value binding in an 'Env' and convert it, using
-- the given size conversion. The fallback for names not found in the
-- (value-free) term scope built by 'envToTermScopeNoVals'.
lookupOuterVal ::
  (StructType -> TypeBase dim NoUniqueness) ->
  Env ->
  VName ->
  Maybe (ValBinding dim)
lookupOuterVal onType env v =
  convert <$> M.lookup v (envVtable env)
  where
    convert (TypeM.BoundV tps t) = BoundV tps $ onType t

-- | The initial scope, containing the intrinsics.
initialTermScope ::
  (StructType -> TypeBase dim NoUniqueness) ->
  TermScope dim
initialTermScope onType =
  TermScope
    { scopeVtable = initialVtable,
      scopeTypeTable = mempty,
      scopeModTable = mempty
    }
  where
    initialVtable = M.fromList $ mapMaybe addIntrinsicF $ M.toList intrinsics

    prim = Scalar . Prim
    arrow x y = Scalar $ Arrow mempty Unnamed Observe x y

    addIntrinsicF (name, IntrinsicMonoFun pts t) =
      Just (name, BoundV [] $ onType $ arrow pts' $ RetType [] $ prim t)
      where
        pts' = case pts of
          [pt] -> prim pt
          _ -> Scalar $ tupleRecord $ map prim pts
    addIntrinsicF (name, IntrinsicOverloadedFun ts pts rts) =
      Just (name, OverloadedF ts pts rts)
    addIntrinsicF (name, IntrinsicPolyFun tvs pts rt) =
      Just
        ( name,
          BoundV tvs $ onType $ foldFunType pts rt
        )
    addIntrinsicF (name, IntrinsicEquality) =
      Just (name, EqualityF)
    addIntrinsicF _ = Nothing

-- | Find the scope corresponding to the qualifiers of the given
-- name. Fails with 'error' if the qualifiers do not name a module,
-- as this means the program should not have made it through earlier
-- checks.
lookupQualNameEnv ::
  (StructType -> TypeBase dim NoUniqueness) ->
  TermScope dim ->
  QualName VName ->
  TermScope dim
lookupQualNameEnv _ scope (QualName [q] _)
  | isIntrinsic q = scope -- Magical intrinsic module.
lookupQualNameEnv onType scope qn@(QualName quals _) = descend scope quals
  where
    descend s [] = s
    descend s (q : qs)
      | Just (ModEnv q_env) <- M.lookup q $ scopeModTable s =
          descend (envToTermScope onType q_env) qs
      | otherwise =
          error $ "lookupQualNameEnv " <> show qn

-- | An identifier corresponding to a type parameter, for size
-- parameters, which also exist as terms.
typeParamIdent :: TypeParam -> Maybe (Ident StructType)
typeParamIdent (TypeParamDim v loc) =
  Just $ Ident v (Info $ Scalar $ Prim $ Signed Int64) loc
typeParamIdent _ = Nothing
