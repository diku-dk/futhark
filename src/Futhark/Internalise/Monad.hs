{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Futhark.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , throwError
  , VarSubstitutions
  , InternaliseEnv (..)
  , ConstParams
  , Closure
  , FunInfo, ConstInfo

  , substitutingVars
  , addFunction

  , lookupFunction
  , lookupFunction'
  , lookupConst

  , bindFunction
  , bindConstant

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
import qualified Control.Monad.Fail as Fail
import qualified Data.Map.Strict as M

import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools

type ConstParams = [(Name,VName)]

-- | Extra parameters to pass when calling this function.  This
-- corresponds to the closure of a locally defined function.
type Closure = [VName]

type FunInfo = (Name, ConstParams, Closure,
                [VName], [DeclType],
                [FParam],
                [(SubExp,Type)] -> Maybe [DeclExtType])

type FunTable = M.Map VName FunInfo

type ConstInfo = (Name, ConstParams,
                  [(SubExp,Type)] -> Maybe [DeclExtType],
                  [SubExp] -> InternaliseM ())

type ConstTable = M.Map VName ConstInfo

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
  , stateConstTable :: ConstTable
  }

newtype InternaliseResult = InternaliseResult [FunDef SOACS]
  deriving (Semigroup, Monoid)

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

instance Fail.MonadFail InternaliseM where
  fail = error . ("InternaliseM: "++)

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
                -> m [FunDef SOACS]
runInternaliseM safe (InternaliseM m) =
  modifyNameSource $ \src ->
  let (_, s, InternaliseResult funs) =
        runRWS (runBinderT m mempty) newEnv (newState src)
  in (funs, stateNameSource s)
  where newEnv = InternaliseEnv {
                   envSubsts = mempty
                 , envDoBoundsChecks = True
                 , envSafe = safe
                 }
        newState src =
          InternaliseState { stateNameSource = src
                           , stateFunTable = mempty
                           , stateConstTable = mempty
                           }

substitutingVars :: VarSubstitutions -> InternaliseM a -> InternaliseM a
substitutingVars substs = local $ \env -> env { envSubsts = substs <> envSubsts env }

-- | Add a function definition to the program being constructed.
addFunction :: FunDef SOACS -> InternaliseM ()
addFunction = InternaliseM . lift . tell . InternaliseResult . pure

lookupFunction' :: VName -> InternaliseM (Maybe FunInfo)
lookupFunction' fname = gets $ M.lookup fname . stateFunTable

lookupFunction :: VName -> InternaliseM FunInfo
lookupFunction fname = maybe bad return =<< lookupFunction' fname
  where bad = fail $ "Internalise.lookupFunction: Function '" ++ pretty fname ++ "' not found."

lookupConst :: VName -> InternaliseM (Maybe ConstInfo)
lookupConst fname = gets $ M.lookup fname . stateConstTable

bindFunction :: VName -> FunInfo -> InternaliseM ()
bindFunction fname info =
  modify $ \s -> s { stateFunTable = M.insert fname info $ stateFunTable s }

bindConstant :: VName -> ConstInfo -> InternaliseM ()
bindConstant cname info =
  modify $ \s -> s { stateConstTable = M.insert cname info $ stateConstTable s }

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

type TypeState = (Int, ConstParams)

newtype InternaliseTypeM a =
  InternaliseTypeM (ReaderT TypeEnv (StateT TypeState InternaliseM) a)
  deriving (Functor, Applicative, Monad,
            MonadReader TypeEnv,
            MonadState TypeState)

liftInternaliseM :: InternaliseM a -> InternaliseTypeM a
liftInternaliseM = InternaliseTypeM . lift . lift

runInternaliseTypeM :: InternaliseTypeM a
                    -> InternaliseM (a, ConstParams)
runInternaliseTypeM (InternaliseTypeM m) = do
  let new_env = TypeEnv mempty
      new_state = (0, mempty)
  (x, (_, cm)) <- runStateT (runReaderT m new_env) new_state
  return (x, cm)

withDims :: DimTable -> InternaliseTypeM a -> InternaliseTypeM a
withDims dtable = local $ \env -> env { typeEnvDims = dtable <> typeEnvDims env }

lookupDim :: VName -> InternaliseTypeM (Maybe ExtSize)
lookupDim name = M.lookup name <$> asks typeEnvDims
