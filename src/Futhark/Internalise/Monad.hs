{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Futhark.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , throwError
  , FunTable
  , TypeTable
  , VarSubstitutions
  , DecSubstitutions
  , InternaliseEnv(..)
  , ConstParams
  , FunBinding (..)
  , lookupFunction
  , lookupTypeVar
  , lookupFunctor
  , lookupSubst
  , newOrExistingSubst

  , bindingIdentTypes
  , bindingParamTypes
  , bindingFunctions
  , bindingTypes
  , bindingFunctor
  , bindingType
  , withDecSubsts
  , generatingFunctor

    -- * Convenient reexports
  , module Futhark.Tools
  )
  where

import Control.Applicative
import Control.Monad.Except hiding (mapM)
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import qualified Data.HashMap.Lazy as HM
import qualified Data.DList as DL
import Data.List

import qualified Language.Futhark as E
import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools

import Prelude hiding (mapM)

type ConstParams = [(Name,VName)]

data FunBinding = FunBinding
                  { internalFun :: (Name, ConstParams, [VName], [DeclType],
                                    [(SubExp,Type)] -> Maybe ExtRetType)
                  , externalFun :: (E.StructType, [E.StructType])
                  }

type FunTable = HM.HashMap VName FunBinding

type TypeTable = HM.HashMap VName [TypeBase Rank NoUniqueness]

-- | A mapping from external variable names to the corresponding
-- internalised subexpressions.
type VarSubstitutions = HM.HashMap VName [SubExp]

-- | Mapping from original top-level names to new top-level names.
type DecSubstitutions = HM.HashMap VName VName

data InternaliseEnv = InternaliseEnv {
    envSubsts :: VarSubstitutions
  , envDecSubsts :: DecSubstitutions
  , envFtable :: FunTable
  , envTtable :: TypeTable
  , envFunctorTable :: HM.HashMap VName E.ModExp
  , envDoBoundsChecks :: Bool
  , envGeneratingFunctor :: Bool
  }

newtype InternaliseM  a = InternaliseM (BinderT SOACS
                                        (ReaderT InternaliseEnv
                                         (StateT VNameSource
                                          (Except String)))
                                        a)
  deriving (Functor, Applicative, Monad,
            MonadWriter (DL.DList Stm),
            MonadReader InternaliseEnv,
            MonadState VNameSource,
            MonadError String,
            HasScope SOACS,
            LocalScope SOACS)

instance MonadFreshNames InternaliseM where
  getNameSource = get
  putNameSource = put

instance MonadBinder InternaliseM where
  type Lore InternaliseM = SOACS
  mkLetM pat e = InternaliseM $ mkLetM pat e
  mkBodyM bnds res = InternaliseM $ mkBodyM bnds res
  mkLetNamesM pat e = InternaliseM $ mkLetNamesM pat e

  addStm =
    InternaliseM . addStm
  collectStms (InternaliseM m) =
    InternaliseM $ collectStms m

runInternaliseM :: MonadFreshNames m =>
                   FunTable -> InternaliseM a
                -> m (Either String a)
runInternaliseM ftable (InternaliseM m) =
  modifyNameSource $ \src ->
  let onError e                 = (Left e, src)
      onSuccess ((prog,_),src') = (Right prog, src')
  in either onError onSuccess $ runExcept $
     runStateT (runReaderT (runBinderT m mempty) newEnv) src
  where newEnv = InternaliseEnv {
                   envSubsts = mempty
                 , envFtable = ftable
                 , envTtable = mempty
                 , envFunctorTable = mempty
                 , envDecSubsts = mempty
                 , envDoBoundsChecks = True
                 , envGeneratingFunctor = False
                 }

lookupFunction :: VName -> InternaliseM FunBinding
lookupFunction fname = do
  fun <- HM.lookup fname <$> asks envFtable
  case fun of Nothing   -> fail $ "Internalise.lookupFunction: Function '" ++ pretty fname ++ "' not found"
              Just fun' -> return fun'

lookupTypeVar :: VName -> InternaliseM [TypeBase Rank NoUniqueness]
lookupTypeVar tname = do
  t <- HM.lookup tname <$> asks envTtable
  case t of Nothing -> fail $ "Internalise.lookupTypeVar: Type '" ++ pretty tname ++ "' not found"
            Just t' -> return t'

lookupFunctor :: VName -> InternaliseM E.ModExp
lookupFunctor mname = do
  maybe_me <- asks $ HM.lookup mname . envFunctorTable
  case maybe_me of
    Nothing -> fail $ "Internalise.lookupFunctor: Functor '" ++
               pretty mname ++ "' not found"
    Just me -> return me

-- | Substitution for any variable or defined name.  Used for functor
-- application.  Never pick apart QualNames directly in the
-- internaliser - use this function instead.  If there is no
-- substitution, the name is just returned.
lookupSubst :: E.QualName VName -> InternaliseM VName
lookupSubst (E.QualName _ name) = do
  r <- asks $ HM.lookup name . envDecSubsts
  case r of
    Just v -> return v
    _      -> return name

-- | Like lookupSubst, but creates a fresh name if inside a functor
-- and a substitution does not already exist.
newOrExistingSubst :: VName -> InternaliseM VName
newOrExistingSubst name = do
  in_functor <- asks envGeneratingFunctor
  r <- asks $ HM.lookup name . envDecSubsts
  case r of
    Just v               -> return v
    Nothing | in_functor -> newName name
            | otherwise  -> return name

bindingIdentTypes :: [Ident] -> InternaliseM a
                  -> InternaliseM a
bindingIdentTypes idents (InternaliseM m) =
  InternaliseM $ localScope (typeEnvFromIdents idents) m

typeEnvFromIdents :: [Ident] -> Scope SOACS
typeEnvFromIdents = HM.fromList . map assoc
  where assoc ident = (identName ident, LetInfo $ identType ident)

bindingParamTypes :: [LParam] -> InternaliseM a
                  -> InternaliseM a
bindingParamTypes = bindingIdentTypes . map paramIdent

bindingFunctions :: FunTable -> InternaliseM a -> InternaliseM a
bindingFunctions ftable_expansion =
  local $ \env -> env { envFtable = ftable_expansion <> envFtable env }

bindingTypes :: TypeTable -> InternaliseM a -> InternaliseM a
bindingTypes ttable_expansion =
  local $ \env -> env { envTtable = ttable_expansion <> envTtable env }

bindingFunctor :: VName -> E.ModExp -> InternaliseM a -> InternaliseM a
bindingFunctor name me =
  local $ \env -> env { envFunctorTable = HM.insert name me $ envFunctorTable env }

bindingType :: VName -> [TypeBase Rank NoUniqueness] -> InternaliseM a -> InternaliseM a
bindingType name t =
  local $ \env -> env { envTtable = HM.insert name t $ envTtable env }

withDecSubsts :: HM.HashMap VName VName -> InternaliseM a -> InternaliseM a
withDecSubsts substs =
  local $ \env -> env { envDecSubsts = substs <> envDecSubsts env }

generatingFunctor :: InternaliseM a -> InternaliseM a
generatingFunctor =
  local $ \env -> env { envGeneratingFunctor = True }
