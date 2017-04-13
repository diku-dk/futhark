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
  , ModBinding (..)

  , addFunction

  , lookupFunction
  , lookupFunction'
  , lookupTypeVar
  , lookupMod
  , lookupSubst
  , newOrExistingSubst

  , bindingIdentTypes
  , bindingParamTypes
  , noteFunctions
  , noteMod
  , noteType
  , noteDecSubsts
  , generatingFunctor
  , withDecSubstitutions

  , asserting
  , assertingOne

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
                                    [FParam],
                                    [(SubExp,Type)] -> Maybe ExtRetType)
                  , externalFun :: (E.StructType, [E.StructType])
                  }

type FunTable = M.Map VName FunBinding

data ModBinding = ModBinding DecSubstitutions E.ModExp
                deriving (Show)

type TypeTable = M.Map VName [TypeBase Rank NoUniqueness]

-- | A mapping from external variable names to the corresponding
-- internalised subexpressions.
type VarSubstitutions = M.Map VName [SubExp]

-- | Mapping from original top-level names to new top-level names.
type DecSubstitutions = M.Map VName VName

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
                   , stateModTable :: M.Map VName ModBinding
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
                           , stateModTable = mempty
                           , stateDecSubsts = mempty
                           , stateNameSource = src
                           }

-- | Add a function definition to the program being constructed.
addFunction :: FunDef -> InternaliseM ()
addFunction = InternaliseM . lift . tell . InternaliseResult . pure

lookupFunction' :: VName -> InternaliseM (Maybe FunBinding)
lookupFunction' fname = gets $ M.lookup fname . stateFtable

lookupFunction :: VName -> InternaliseM FunBinding
lookupFunction fname =
  maybe bad return =<< lookupFunction' fname
  where bad = fail $
              "Internalise.lookupFunction: Function '" ++ pretty fname ++ "' not found."

lookupTypeVar :: VName -> InternaliseM [TypeBase Rank NoUniqueness]
lookupTypeVar tname = do
  t <- gets $ M.lookup tname. stateTtable
  case t of Nothing -> fail $ "Internalise.lookupTypeVar: Type '" ++ pretty tname ++ "' not found"
            Just t' -> return t'

lookupMod :: VName -> InternaliseM ModBinding
lookupMod mname = do
  maybe_me <- gets $ M.lookup mname . stateModTable
  case maybe_me of
    Nothing -> fail $ "Internalise.lookupMod: Module '" ++
               pretty mname ++ "' not found"
    Just me -> return me

allSubsts :: InternaliseM DecSubstitutions
allSubsts = M.union <$> asks envFunctorSubsts <*> gets stateDecSubsts

-- | Substitution for any variable or defined name.  Used for functor
-- application.  Never pick apart QualNames directly in the
-- internaliser - use this function instead.  If there is no
-- substitution, the name is just returned.
lookupSubst :: E.QualName VName -> InternaliseM VName
lookupSubst (E.QualName _ name) = do
  r <- M.lookup name <$> allSubsts
  case r of
    Just v | v /= name -> lookupSubst $ E.qualName v
           | otherwise -> return v
    _      -> return name

-- | Like lookupSubst, but creates a fresh name if inside a functor
-- and a substitution does not already exist.
newOrExistingSubst :: VName -> InternaliseM VName
newOrExistingSubst name = do
  in_functor <- asks envGeneratingFunctor
  r <- M.lookup name <$> allSubsts
  case r of
    Just v | v /= name -> lookupSubst $ E.qualName v
           | otherwise -> return v
    Nothing | in_functor -> do x <- newName name
                               noteDecSubsts $ M.singleton name x
                               return x
            | otherwise  -> return name

bindingIdentTypes :: [Ident] -> InternaliseM a
                  -> InternaliseM a
bindingIdentTypes idents (InternaliseM m) =
  InternaliseM $ localScope (typeEnvFromIdents idents) m

typeEnvFromIdents :: [Ident] -> Scope SOACS
typeEnvFromIdents = M.fromList . map assoc
  where assoc ident = (identName ident, LetInfo $ identType ident)

bindingParamTypes :: [LParam] -> InternaliseM a
                  -> InternaliseM a
bindingParamTypes = bindingIdentTypes . map paramIdent

noteFunctions :: FunTable -> InternaliseM ()
noteFunctions ftable_expansion =
  modify $ \s -> s { stateFtable = ftable_expansion <> stateFtable s }

noteMod :: VName -> DecSubstitutions -> E.ModExp -> InternaliseM ()
noteMod name substs me =
  modify $ \s -> s { stateModTable = M.insert name (ModBinding substs me) $ stateModTable s }

noteType :: VName -> [TypeBase Rank NoUniqueness] -> InternaliseM ()
noteType name t =
  modify $ \s -> s { stateTtable = M.insert name t $ stateTtable s }

setDecSubsts :: M.Map VName VName -> InternaliseM ()
setDecSubsts substs = modify $ \s -> s { stateDecSubsts = substs }

noteDecSubsts :: M.Map VName VName -> InternaliseM ()
noteDecSubsts substs = do
  cur_substs <- allSubsts
  -- Some substitutions of these names may already exist.
  let also_substs = M.fromList $ mapMaybe (keyHasExisting cur_substs) $ M.toList substs
  modify $ \s ->
    s { stateDecSubsts = also_substs `M.union` substs `M.union` stateDecSubsts s
      }
  where keyHasExisting cur_substs (k,v) = do
          v' <- M.lookup k cur_substs
          keyHasExisting cur_substs (v,v') `mplus` return (v,v')

generatingFunctor :: M.Map VName VName
                  -> M.Map VName VName
                  -> InternaliseM a -> InternaliseM a
generatingFunctor p_substs b_substs m = do
  -- Some substitutions of these names may already exist.  Also, we
  -- ensure we have fresh names for everything in the functor, except
  -- for those names that are already unique from the applications.
  in_functor <- asks envGeneratingFunctor
  cur_substs <- allSubsts

  let frob (k, v)
        | Just v' <- M.lookup k cur_substs, v' /= v =
            frob (v', v) `mplus` Just (v', v)
        | otherwise =
            Nothing

      extra_substs = if in_functor
                     then M.fromList $ mapMaybe frob $ M.toList b_substs
                     else mempty
      forwards = M.fromList $ mapMaybe frob $ M.toList p_substs

      recs = [extra_substs,
              forwards,
              p_substs,
              b_substs]
      update env =
        env { envGeneratingFunctor = True
            , envFunctorSubsts = M.unions recs `M.union`
                                 envFunctorSubsts env
            }
  old_dec_substs <- gets stateDecSubsts
  x <- local update m
  -- Some of the dec substs may be relevant for the result of the
  -- functor.  A relevant substitution is one that affects an element
  -- of b_subst.
  new_dec_substs <- gets stateDecSubsts
  let b_elems = M.elems b_substs
      new_relevant k _ = k `elem` b_elems
      nexts = M.filterWithKey new_relevant new_dec_substs
              `M.union` extra_substs `M.union` b_substs
  setDecSubsts (nexts `M.union` old_dec_substs)
  return x

withDecSubstitutions :: DecSubstitutions
                     -> InternaliseM a -> InternaliseM a
withDecSubstitutions p_substs m = do
  let update env =
        env { envFunctorSubsts = p_substs `M.union` envFunctorSubsts env }
  local update m


-- | Execute the given action if 'envDoBoundsChecks' is true, otherwise
-- just return an empty list.
asserting :: InternaliseM Certificates
          -> InternaliseM Certificates
asserting m = do
  doBoundsChecks <- asks envDoBoundsChecks
  if doBoundsChecks
  then m
  else return []

-- | Execute the given action if 'envDoBoundsChecks' is true, otherwise
-- just return an empty list.
assertingOne :: InternaliseM VName
             -> InternaliseM Certificates
assertingOne m = asserting $ fmap pure m
