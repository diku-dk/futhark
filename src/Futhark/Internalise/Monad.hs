{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Futhark.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , throwError
  , VarSubstitutions
  , InternaliseEnv (..)
  , ConstParams
  , Closure
  , FunInfo

  , substitutingVars
  , addFunction

  , lookupFunction
  , lookupFunction'

  , bindingFunction

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
import qualified Data.Semigroup as Sem

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

-- | A mapping from external variable names to the corresponding
-- internalised subexpressions.
type VarSubstitutions = M.Map VName [SubExp]

data InternaliseEnv = InternaliseEnv {
    envSubsts :: VarSubstitutions
  , envDoBoundsChecks :: Bool
  , envFunTable :: FunTable
  }

newtype InternaliseState =
  InternaliseState { stateNameSource :: VNameSource }

newtype InternaliseResult = InternaliseResult [FunDef]
  deriving (Sem.Semigroup, Monoid)

newtype InternaliseM  a = InternaliseM (BinderT SOACS
                                        (RWST
                                         InternaliseEnv
                                         InternaliseResult
                                         InternaliseState
                                         (Except String))
                                        a)
  deriving (Functor, Applicative, Monad,
            MonadReader InternaliseEnv,
            MonadState InternaliseState,
            MonadFreshNames,
            MonadError String,
            HasScope SOACS,
            LocalScope SOACS)

instance (Monoid w, Monad m) => MonadFreshNames (RWST r w InternaliseState m) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s { stateNameSource = src }

instance Fail.MonadFail InternaliseM where
  fail = InternaliseM . throwError

instance MonadBinder InternaliseM where
  type Lore InternaliseM = SOACS
  mkExpAttrM pat e = InternaliseM $ mkExpAttrM pat e
  mkBodyM bnds res = InternaliseM $ mkBodyM bnds res
  mkLetNamesM pat e = InternaliseM $ mkLetNamesM pat e

  addStms = InternaliseM . addStms
  collectStms (InternaliseM m) = InternaliseM $ collectStms m
  certifying cs (InternaliseM m) = InternaliseM $ certifying cs m

runInternaliseM :: MonadFreshNames m =>
                   InternaliseM ()
                -> m (Either String [FunDef])
runInternaliseM (InternaliseM m) =
  modifyNameSource $ \src -> do
  let onError e             = (Left e, src)
      onSuccess (funs,src') = (Right funs, src')
  either onError onSuccess $ runExcept $ do
    (_, s, InternaliseResult funs) <- runRWST (runBinderT m mempty) newEnv (newState src)
    return (funs, stateNameSource s)
  where newEnv = InternaliseEnv {
                   envSubsts = mempty
                 , envDoBoundsChecks = True
                 , envFunTable = mempty
                 }
        newState src =
          InternaliseState { stateNameSource = src }

substitutingVars :: VarSubstitutions -> InternaliseM a -> InternaliseM a
substitutingVars substs = local $ \env -> env { envSubsts = substs <> envSubsts env }

-- | Add a function definition to the program being constructed.
addFunction :: FunDef -> InternaliseM ()
addFunction = InternaliseM . lift . tell . InternaliseResult . pure

lookupFunction' :: VName -> InternaliseM (Maybe FunInfo)
lookupFunction' fname = asks $ M.lookup fname . envFunTable

lookupFunction :: VName -> InternaliseM FunInfo
lookupFunction fname = maybe bad return =<< lookupFunction' fname
  where bad = fail $ "Internalise.lookupFunction: Function '" ++ pretty fname ++ "' not found."

bindingFunction :: VName -> FunInfo
                -> InternaliseM a -> InternaliseM a
bindingFunction fname info =
  local (\env -> env { envFunTable = M.insert fname info $ envFunTable env })

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
            MonadState TypeState,
            MonadError String)

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
