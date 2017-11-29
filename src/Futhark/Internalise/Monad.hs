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
  , SpecArgs
  , SpecParams
  , TypeEntry

  , substitutingVars
  , addFunction

  , maybeSpecialiseEarly
  , lookupFunction
  , lookupFunction'

  , bindingIdentTypes
  , bindingParamTypes
  , noteFunction
  , noteType
  , notingTypes
  , notedTypes

  , asserting
  , assertingOne

  -- * Type Handling
  , InternaliseTypeM
  , liftInternaliseM
  , runInternaliseTypeM
  , lookupTypeVar
  , lookupTypeVar'
  , lookupDim
  , withTypes
  , withDims
  , DimTable
  , TypeTable

    -- * Convenient reexports
  , module Futhark.Tools
  )
  where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS hiding (mapM)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

import qualified Language.Futhark as E
import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools

import Prelude hiding (mapM)

type ConstParams = [(Name,VName)]

-- | Extra parameters to pass when calling this function.  This
-- corresponds to the closure of a locally defined function.
type Closure = [VName]

-- | The type arguments to a polymorhic function.
type SpecArgs = ([E.TypeBase () ()], SpecParams)

-- | The type internalise arguments to a polymorphic function.
type SpecParams = [TypeBase Rank NoUniqueness]

data FunBinding = FunBinding
  { polymorphicSpecialisations :: M.Map SpecParams FunInfo
    -- ^ Already generated specialised functions.
  , polymorphicSpecialise :: SpecArgs -> InternaliseM FunInfo
    -- ^ Generate a new specialisation.
  }

type FunInfo = (Name, ConstParams, Closure,
                [VName], [DeclType],
                [FParam],
                [(SubExp,Type)] -> Maybe [DeclExtType])

type FunTable = M.Map VName FunBinding

type TypeEntry = ([E.TypeParam], E.StructType)

type TypeTable = M.Map VName TypeEntry

-- | A mapping from external variable names to the corresponding
-- internalised subexpressions.
type VarSubstitutions = M.Map VName [SubExp]

data InternaliseEnv = InternaliseEnv {
    envSubsts :: VarSubstitutions
  , envDoBoundsChecks :: Bool
  }

data InternaliseState =
  InternaliseState { stateFtable :: FunTable
                   , stateTtable :: TypeTable
                   , stateNameSource :: VNameSource
                   }

newtype InternaliseResult = InternaliseResult [FunDef]
  deriving (Monoid)

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

instance MonadBinder InternaliseM where
  type Lore InternaliseM = SOACS
  mkExpAttrM pat e = InternaliseM $ mkExpAttrM pat e
  mkBodyM bnds res = InternaliseM $ mkBodyM bnds res
  mkLetNamesM pat e = InternaliseM $ mkLetNamesM pat e

  addStm =
    InternaliseM . addStm
  collectStms (InternaliseM m) =
    InternaliseM $ collectStms m
  certifying cs (InternaliseM m) =
    InternaliseM $ certifying cs m

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
                 }
        newState src =
          InternaliseState { stateFtable = mempty
                           , stateTtable = mempty
                           , stateNameSource = src
                           }

substitutingVars :: VarSubstitutions -> InternaliseM a -> InternaliseM a
substitutingVars substs = local $ \env -> env { envSubsts = substs <> envSubsts env }

-- | Add a function definition to the program being constructed.
addFunction :: FunDef -> InternaliseM ()
addFunction = InternaliseM . lift . tell . InternaliseResult . pure

lookupFunction' :: VName -> InternaliseM (Maybe FunInfo)
lookupFunction' fname =
  gets $ M.lookup [] <=< fmap polymorphicSpecialisations . M.lookup fname . stateFtable

lookupFunction :: VName -> SpecArgs -> InternaliseM FunInfo
lookupFunction fname types@(_, i_types) = do
  fb <- maybe bad return =<< gets (M.lookup fname . stateFtable)
  case M.lookup i_types $ polymorphicSpecialisations fb of
    Just info -> return info
    Nothing -> do
      info <- local (\env -> env { envDoBoundsChecks = True }) $
              polymorphicSpecialise fb types
      let fb' = fb { polymorphicSpecialisations =
                       M.insert i_types info $ polymorphicSpecialisations fb }
      modify $ \s -> s { stateFtable = M.insert fname fb' $ stateFtable s }
      return info
  where bad =
          fail $ "Internalise.lookupFunction: Function '" ++ pretty fname ++ "' not found."

-- Generate monomorphic specialisation early as a HACK to support
-- recursive functions.
maybeSpecialiseEarly :: VName -> Name -> [FParam] -> [DeclExtType] -> InternaliseM ()
maybeSpecialiseEarly fname fname' params rettype = do
  let info = (fname', mempty, mempty,
              mempty, map declTypeOf params,
              params,
              applyRetType rettype params)
      fb = FunBinding (M.singleton (map (rankShaped . paramType) params) info)
           (\ts -> fail $ "Cannot have polymorphic recursive function. " ++ show ts)
  modify $ \s -> s { stateFtable = M.insert fname fb $ stateFtable s }

bindingIdentTypes :: [Ident] -> InternaliseM a
                  -> InternaliseM a
bindingIdentTypes idents (InternaliseM m) =
  InternaliseM $ localScope (typeScopeFromIdents idents) m

typeScopeFromIdents :: [Ident] -> Scope SOACS
typeScopeFromIdents = M.fromList . map assoc
  where assoc ident = (identName ident, LetInfo $ identType ident)

bindingParamTypes :: [LParam] -> InternaliseM a
                  -> InternaliseM a
bindingParamTypes = bindingIdentTypes . map paramIdent

noteFunction :: VName -> (SpecArgs -> InternaliseM FunInfo) -> InternaliseM ()
noteFunction fname generate =
  modify $ \s -> s { stateFtable = M.singleton fname entry <> stateFtable s }
  where entry = FunBinding mempty generate

noteType :: VName -> TypeEntry -> InternaliseM ()
noteType name entry =
  modify $ \s -> s { stateTtable = M.insert name entry $ stateTtable s }

-- | Expand the type table, run an action, and restore the old type
-- table.  Any calls to 'noteType' during the action will be lost.
notingTypes :: [(VName, TypeEntry)] -> InternaliseM a -> InternaliseM a
notingTypes types m = do
  old <- gets stateTtable
  mapM_ (uncurry noteType) types
  x <- m
  modify $ \s -> s { stateTtable = old }
  return x

notedTypes :: InternaliseM TypeTable
notedTypes = gets stateTtable

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

data TypeEnv = TypeEnv { typeEnvTypes :: TypeTable
                       , typeEnvDims  :: DimTable
                       }

type TypeState = (Int, M.Map VName Int, ConstParams)

newtype InternaliseTypeM a =
  InternaliseTypeM (ReaderT TypeEnv (StateT TypeState InternaliseM) a)
  deriving (Functor, Applicative, Monad,
            MonadReader TypeEnv,
            MonadState TypeState,
            MonadError String)

liftInternaliseM :: InternaliseM a -> InternaliseTypeM a
liftInternaliseM = InternaliseTypeM . lift . lift

runInternaliseTypeM :: S.Set VName
                    -> InternaliseTypeM a
                    -> InternaliseM (a, M.Map VName Int, ConstParams)
runInternaliseTypeM bound (InternaliseTypeM m) = do
  types <- gets stateTtable
  let bound' = M.fromList $ zip (S.toList bound) [0..]
      dims = M.map Ext bound'
      new_env = TypeEnv types dims
      new_state = (S.size bound, bound', mempty)
  (x, (_, substs, cm)) <- runStateT (runReaderT m new_env) new_state
  return (x, substs, cm)

withTypes :: TypeTable -> InternaliseTypeM a -> InternaliseTypeM a
withTypes ttable = local $ \env -> env { typeEnvTypes = ttable <> typeEnvTypes env }

withDims :: DimTable -> InternaliseTypeM a -> InternaliseTypeM a
withDims dtable = local $ \env -> env { typeEnvDims = dtable <> typeEnvDims env }

lookupDim :: VName -> InternaliseTypeM (Maybe ExtSize)
lookupDim name = M.lookup name <$> asks typeEnvDims

lookupTypeVar' :: VName -> InternaliseTypeM (Maybe TypeEntry)
lookupTypeVar' tname = M.lookup tname <$> asks typeEnvTypes

lookupTypeVar :: VName -> InternaliseTypeM TypeEntry
lookupTypeVar tname = do
  t <- lookupTypeVar' tname
  case t of Nothing -> fail $ "Internalise.lookupTypeVar: Type '" ++ pretty tname ++ "' not found"
            Just x -> return x
