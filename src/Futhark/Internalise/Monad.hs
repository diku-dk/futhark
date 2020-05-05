{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Futhark.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , throwError
  , VarSubstitutions
  , InternaliseEnv (..)
  , Closure
  , FunInfo

  , substitutingVars
  , lookupSubst
  , addFunDef

  , lookupFunction
  , lookupFunction'
  , lookupConst
  , allConsts

  , bindFunction
  , bindConstant

  , localConstsScope

  , asserting
  , assertingOne

  -- * Type Handling
  , InternaliseTypeM
  , liftInternaliseM
  , runInternaliseTypeM
  , lookupDim
  , withDims
  , DimTable

    -- * Convenient reexports
  , module Futhark.Tools
  )
  where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import qualified Data.Map.Strict as M

import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Util (takeLast)

-- | Extra parameters to pass when calling this function.  This
-- corresponds to the closure of a locally defined function.
type Closure = [VName]

type FunInfo = (Name, Closure,
                [VName], [DeclType],
                [FParam],
                [(SubExp,Type)] -> Maybe [DeclExtType])

type FunTable = M.Map VName FunInfo

-- | A mapping from external variable names to the corresponding
-- internalised subexpressions.
type VarSubstitutions = M.Map VName [SubExp]

data InternaliseEnv = InternaliseEnv {
    envSubsts :: VarSubstitutions
  , envDoBoundsChecks :: Bool
  , envSafe :: Bool
  }

data InternaliseState = InternaliseState {
    stateNameSource :: VNameSource
  , stateFunTable :: FunTable
  , stateConstSubsts :: VarSubstitutions
  , stateConstScope :: Scope SOACS
  , stateConsts :: Names
  }

data InternaliseResult = InternaliseResult (Stms SOACS) [FunDef SOACS]

instance Semigroup InternaliseResult where
  InternaliseResult xs1 ys1 <> InternaliseResult xs2 ys2 =
    InternaliseResult (xs1<>xs2) (ys1<>ys2)

instance Monoid InternaliseResult where
  mempty = InternaliseResult mempty mempty

newtype InternaliseM  a = InternaliseM (BinderT SOACS
                                        (RWS
                                         InternaliseEnv
                                         InternaliseResult
                                         InternaliseState)
                                        a)
  deriving (Functor, Applicative, Monad,
            MonadReader InternaliseEnv,
            MonadState InternaliseState,
            MonadFreshNames,
            HasScope SOACS,
            LocalScope SOACS)

instance (Monoid w, Monad m) => MonadFreshNames (RWST r w InternaliseState m) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s { stateNameSource = src }

instance MonadBinder InternaliseM where
  type Lore InternaliseM = SOACS
  mkExpAttrM pat e = InternaliseM $ mkExpAttrM pat e
  mkBodyM bnds res = InternaliseM $ mkBodyM bnds res
  mkLetNamesM pat e = InternaliseM $ mkLetNamesM pat e

  addStms = InternaliseM . addStms
  collectStms (InternaliseM m) = InternaliseM $ collectStms m
  certifying cs (InternaliseM m) = InternaliseM $ certifying cs m

runInternaliseM :: MonadFreshNames m =>
                   Bool -> InternaliseM ()
                -> m (Stms SOACS, [FunDef SOACS])
runInternaliseM safe (InternaliseM m) =
  modifyNameSource $ \src ->
  let ((_, consts), s, InternaliseResult _ funs) =
        runRWS (runBinderT m mempty) newEnv (newState src)
  in ((consts, funs), stateNameSource s)
  where newEnv = InternaliseEnv {
                   envSubsts = mempty
                 , envDoBoundsChecks = True
                 , envSafe = safe
                 }
        newState src =
          InternaliseState { stateNameSource = src
                           , stateFunTable = mempty
                           , stateConstSubsts = mempty
                           , stateConsts = mempty
                           , stateConstScope = mempty
                           }

substitutingVars :: VarSubstitutions -> InternaliseM a -> InternaliseM a
substitutingVars substs = local $ \env -> env { envSubsts = substs <> envSubsts env }

lookupSubst :: VName -> InternaliseM (Maybe [SubExp])
lookupSubst v = do
  env_substs <- asks $ M.lookup v . envSubsts
  const_substs <- gets $ M.lookup v . stateConstSubsts
  return $ env_substs `mplus` const_substs

-- | Add a function definition to the program being constructed.
addFunDef :: FunDef SOACS -> InternaliseM ()
addFunDef fd =
  InternaliseM $ lift $ tell $ InternaliseResult mempty [fd]

lookupFunction' :: VName -> InternaliseM (Maybe FunInfo)
lookupFunction' fname = gets $ M.lookup fname . stateFunTable

lookupFunction :: VName -> InternaliseM FunInfo
lookupFunction fname = maybe bad return =<< lookupFunction' fname
  where bad = error $ "Internalise.lookupFunction: Function '" ++ pretty fname ++ "' not found."

lookupConst :: VName -> InternaliseM (Maybe [SubExp])
lookupConst fname = gets $ M.lookup fname . stateConstSubsts

allConsts :: InternaliseM Names
allConsts = gets stateConsts

bindFunction :: VName -> FunDef SOACS -> FunInfo -> InternaliseM ()
bindFunction fname fd info = do
  addFunDef fd
  modify $ \s -> s { stateFunTable = M.insert fname info $ stateFunTable s }

bindConstant :: VName -> FunDef SOACS -> InternaliseM ()
bindConstant cname fd = do
  let stms = bodyStms $ funDefBody fd
      substs = takeLast (length (funDefRetType fd)) $
               bodyResult $ funDefBody fd
      const_names = namesFromList $ M.keys $ scopeOf stms
  addStms stms
  modify $ \s ->
    s { stateConstSubsts = M.insert cname substs $ stateConstSubsts s
      , stateConstScope = scopeOf stms <> stateConstScope s
      , stateConsts = const_names <> stateConsts s
      }

localConstsScope :: InternaliseM a -> InternaliseM a
localConstsScope m = do
  scope <- gets stateConstScope
  localScope scope m

-- | Execute the given action if 'envDoBoundsChecks' is true, otherwise
-- just return an empty list.
asserting :: InternaliseM Certificates
          -> InternaliseM Certificates
asserting m = do
  doBoundsChecks <- asks envDoBoundsChecks
  if doBoundsChecks
  then m
  else return mempty

-- | Execute the given action if 'envDoBoundsChecks' is true, otherwise
-- just return an empty list.
assertingOne :: InternaliseM VName
             -> InternaliseM Certificates
assertingOne m = asserting $ Certificates . pure <$> m

type DimTable = M.Map VName ExtSize

newtype TypeEnv = TypeEnv { typeEnvDims  :: DimTable }

type TypeState = Int

newtype InternaliseTypeM a =
  InternaliseTypeM (ReaderT TypeEnv (StateT TypeState InternaliseM) a)
  deriving (Functor, Applicative, Monad,
            MonadReader TypeEnv,
            MonadState TypeState)

liftInternaliseM :: InternaliseM a -> InternaliseTypeM a
liftInternaliseM = InternaliseTypeM . lift . lift

runInternaliseTypeM :: InternaliseTypeM a
                    -> InternaliseM a
runInternaliseTypeM (InternaliseTypeM m) =
  evalStateT (runReaderT m (TypeEnv mempty)) 0

withDims :: DimTable -> InternaliseTypeM a -> InternaliseTypeM a
withDims dtable = local $ \env -> env { typeEnvDims = dtable <> typeEnvDims env }

lookupDim :: VName -> InternaliseTypeM (Maybe ExtSize)
lookupDim name = M.lookup name <$> asks typeEnvDims
