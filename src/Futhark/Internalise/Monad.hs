{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Futhark.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , throwError
  , FunTable
  , TypeTable
  , VarSubstitutions
  , DecSubstitutions
  , InternaliseEnv (..)
  , ConstParams
  , Closure
  , FunBinding (..)

  , addFunction

  , lookupFunction
  , lookupFunction'
  , lookupTypeVar
  , lookupFunctor
  , lookupSubst
  , newOrExistingSubst

  , bindingIdentTypes
  , bindingParamTypes
  , noteFunctions
  , noteFunctor
  , noteType
  , noteDecSubsts
  , generatingFunctor

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

import qualified Data.HashMap.Lazy as HM
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

data FunBinding = FunBinding
                  { internalFun :: (Name, ConstParams, Closure,
                                    [VName], [DeclType],
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
  , envDoBoundsChecks :: Bool
  , envGeneratingFunctor :: Bool
  , envFunctorSubsts :: DecSubstitutions
  }

data InternaliseState =
  InternaliseState { stateDecSubsts :: DecSubstitutions
                   , stateFtable :: FunTable
                   , stateTtable :: TypeTable
                   , stateFunctorTable :: HM.HashMap VName E.ModExp
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
  mkLetM pat e = InternaliseM $ mkLetM pat e
  mkBodyM bnds res = InternaliseM $ mkBodyM bnds res
  mkLetNamesM pat e = InternaliseM $ mkLetNamesM pat e

  addStm =
    InternaliseM . addStm
  collectStms (InternaliseM m) =
    InternaliseM $ collectStms m

runInternaliseM :: MonadFreshNames m =>
                   FunTable -> InternaliseM ()
                -> m (Either String [FunDef])
runInternaliseM ftable (InternaliseM m) =
  modifyNameSource $ \src -> do
  let onError e             = (Left e, src)
      onSuccess (funs,src') = (Right funs, src')
  either onError onSuccess $ runExcept $ do
    (_, s, InternaliseResult funs) <- runRWST (runBinderT m mempty) newEnv (newState src)
    return (funs, stateNameSource s)
  where newEnv = InternaliseEnv {
                   envSubsts = mempty
                 , envDoBoundsChecks = True
                 , envGeneratingFunctor = False
                 , envFunctorSubsts = mempty
                 }
        newState src =
          InternaliseState { stateFtable = ftable
                           , stateTtable = mempty
                           , stateFunctorTable = mempty
                           , stateDecSubsts = mempty
                           , stateNameSource = src
                           }

-- | Add a function definition to the program being constructed.
addFunction :: FunDef -> InternaliseM ()
addFunction = InternaliseM . lift . tell . InternaliseResult . pure

lookupFunction' :: VName -> InternaliseM (Maybe FunBinding)
lookupFunction' fname = do
  ftable <- gets stateFtable
  case HM.lookup fname ftable of
    Nothing   -> return Nothing
    Just fun' -> return $ Just fun'

lookupFunction :: VName -> InternaliseM FunBinding
lookupFunction fname =
  maybe bad return =<< lookupFunction' fname
  where bad = fail $
              "Internalise.lookupFunction: Function '" ++ pretty fname ++ "' not found."

lookupTypeVar :: VName -> InternaliseM [TypeBase Rank NoUniqueness]
lookupTypeVar tname = do
  t <- gets $ HM.lookup tname. stateTtable
  case t of Nothing -> fail $ "Internalise.lookupTypeVar: Type '" ++ pretty tname ++ "' not found"
            Just t' -> return t'

lookupFunctor :: VName -> InternaliseM E.ModExp
lookupFunctor mname = do
  maybe_me <- gets $ HM.lookup mname . stateFunctorTable
  case maybe_me of
    Nothing -> fail $ "Internalise.lookupFunctor: Functor '" ++
               pretty mname ++ "' not found"
    Just me -> return me

allSubsts :: InternaliseM DecSubstitutions
allSubsts = HM.union <$> asks envFunctorSubsts <*> gets stateDecSubsts

-- | Substitution for any variable or defined name.  Used for functor
-- application.  Never pick apart QualNames directly in the
-- internaliser - use this function instead.  If there is no
-- substitution, the name is just returned.
lookupSubst :: E.QualName VName -> InternaliseM VName
lookupSubst (E.QualName _ name) = do
  r <- HM.lookup name <$> allSubsts
  case r of
    Just v | v /= name -> lookupSubst $ E.qualName v
           | otherwise -> return v
    _      -> return name

-- | Like lookupSubst, but creates a fresh name if inside a functor
-- and a substitution does not already exist.
newOrExistingSubst :: VName -> InternaliseM VName
newOrExistingSubst name = do
  in_functor <- asks envGeneratingFunctor
  r <- HM.lookup name <$> allSubsts
  case r of
    Just v | v /= name -> lookupSubst $ E.qualName v
           | otherwise -> return v
    Nothing | in_functor -> do x <- newName name
                               noteDecSubsts $ HM.singleton name x
                               return x
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

noteFunctions :: FunTable -> InternaliseM ()
noteFunctions ftable_expansion =
  modify $ \s -> s { stateFtable = ftable_expansion <> stateFtable s }

noteFunctor :: VName -> E.ModExp -> InternaliseM ()
noteFunctor name me =
  modify $ \s -> s { stateFunctorTable = HM.insert name me $ stateFunctorTable s }

noteType :: VName -> [TypeBase Rank NoUniqueness] -> InternaliseM ()
noteType name t =
  modify $ \s -> s { stateTtable = HM.insert name t $ stateTtable s }

setDecSubsts :: HM.HashMap VName VName -> InternaliseM ()
setDecSubsts substs = modify $ \s -> s { stateDecSubsts = substs }

noteDecSubsts :: HM.HashMap VName VName -> InternaliseM ()
noteDecSubsts substs = do
  cur_substs <- allSubsts
  -- Some substitutions of these names may already exist.
  let substs' = HM.map (forward cur_substs) substs
  modify $ \s ->
    s { stateDecSubsts = substs' `HM.union` stateDecSubsts s
      }
  where forward old_substs v = fromMaybe v $ HM.lookup v old_substs

generatingFunctor :: HM.HashMap VName VName
                  -> HM.HashMap VName VName
                  -> InternaliseM a -> InternaliseM a
generatingFunctor p_substs b_substs m = do
  -- Some substitutions of these names may already exist.  Also, we
  -- ensure we have fresh names for everything in the functor, except
  -- for those names that are already unique from the applications.
  in_functor <- asks envGeneratingFunctor
  func_substs <- asks envFunctorSubsts
  cur_substs <- allSubsts

  let frob (k, v)
        | Just v' <- HM.lookup k func_substs = Just (v', v)
        | otherwise                          = Nothing
  extra_substs <- if in_functor
                  then return $ HM.fromList $ mapMaybe frob $ HM.toList b_substs
                  else return mempty
  let recs = [extra_substs,
              b_substs,
              HM.map forward p_substs]
      nexts = extra_substs `HM.union` b_substs
      update env =
        env { envGeneratingFunctor = True
            , envFunctorSubsts = HM.unions recs `HM.union`
                                 envFunctorSubsts env
            }
      forward v = fromMaybe v $ HM.lookup v cur_substs
  old_dec_substs <- gets stateDecSubsts
  local update m <* setDecSubsts (nexts `HM.union` old_dec_substs)
