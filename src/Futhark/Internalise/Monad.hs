{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Internalise.Monad
  ( InternaliseM,
    runInternaliseM,
    throwError,
    VarSubsts,
    InternaliseEnv (..),
    FunInfo,
    substitutingVars,
    lookupSubst,
    addFunDef,
    lookupFunction,
    lookupFunction',
    lookupConst,
    bindFunction,
    bindConstant,
    localConstsScope,
    assert,

    -- * Convenient reexports
    module Futhark.Tools,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools

type FunInfo =
  ( [VName],
    [DeclType],
    [FParam SOACS],
    [(SubExp, Type)] -> Maybe [DeclExtType]
  )

type FunTable = M.Map VName FunInfo

-- | A mapping from external variable names to the corresponding
-- internalised subexpressions.
type VarSubsts = M.Map VName [SubExp]

data InternaliseEnv = InternaliseEnv
  { envSubsts :: VarSubsts,
    envDoBoundsChecks :: Bool,
    envSafe :: Bool,
    envAttrs :: Attrs
  }

data InternaliseState = InternaliseState
  { stateNameSource :: VNameSource,
    stateFunTable :: FunTable,
    stateConstSubsts :: VarSubsts,
    stateConstScope :: Scope SOACS,
    stateFuns :: [FunDef SOACS]
  }

newtype InternaliseM a
  = InternaliseM
      (BuilderT SOACS (ReaderT InternaliseEnv (State InternaliseState)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader InternaliseEnv,
      MonadState InternaliseState,
      MonadFreshNames,
      HasScope SOACS,
      LocalScope SOACS
    )

instance MonadFreshNames (State InternaliseState) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s {stateNameSource = src}

instance MonadBuilder InternaliseM where
  type Rep InternaliseM = SOACS
  mkExpDecM pat e = InternaliseM $ mkExpDecM pat e
  mkBodyM stms res = InternaliseM $ mkBodyM stms res
  mkLetNamesM pat e = InternaliseM $ mkLetNamesM pat e

  addStms = InternaliseM . addStms
  collectStms (InternaliseM m) = InternaliseM $ collectStms m

runInternaliseM ::
  MonadFreshNames m =>
  Bool ->
  InternaliseM () ->
  m (Stms SOACS, [FunDef SOACS])
runInternaliseM safe (InternaliseM m) =
  modifyNameSource $ \src ->
    let ((_, consts), s) =
          runState (runReaderT (runBuilderT m mempty) newEnv) (newState src)
     in ((consts, reverse $ stateFuns s), stateNameSource s)
  where
    newEnv =
      InternaliseEnv
        { envSubsts = mempty,
          envDoBoundsChecks = True,
          envSafe = safe,
          envAttrs = mempty
        }
    newState src =
      InternaliseState
        { stateNameSource = src,
          stateFunTable = mempty,
          stateConstSubsts = mempty,
          stateConstScope = mempty,
          stateFuns = mempty
        }

substitutingVars :: VarSubsts -> InternaliseM a -> InternaliseM a
substitutingVars substs = local $ \env -> env {envSubsts = substs <> envSubsts env}

lookupSubst :: VName -> InternaliseM (Maybe [SubExp])
lookupSubst v = do
  env_substs <- asks $ M.lookup v . envSubsts
  const_substs <- gets $ M.lookup v . stateConstSubsts
  pure $ env_substs `mplus` const_substs

-- | Add a function definition to the program being constructed.
addFunDef :: FunDef SOACS -> InternaliseM ()
addFunDef fd = modify $ \s -> s {stateFuns = fd : stateFuns s}

lookupFunction' :: VName -> InternaliseM (Maybe FunInfo)
lookupFunction' fname = gets $ M.lookup fname . stateFunTable

lookupFunction :: VName -> InternaliseM FunInfo
lookupFunction fname = maybe bad pure =<< lookupFunction' fname
  where
    bad = error $ "Internalise.lookupFunction: Function '" ++ pretty fname ++ "' not found."

lookupConst :: VName -> InternaliseM (Maybe [SubExp])
lookupConst fname = gets $ M.lookup fname . stateConstSubsts

bindFunction :: VName -> FunDef SOACS -> FunInfo -> InternaliseM ()
bindFunction fname fd info = do
  addFunDef fd
  modify $ \s -> s {stateFunTable = M.insert fname info $ stateFunTable s}

bindConstant :: VName -> FunDef SOACS -> InternaliseM ()
bindConstant cname fd = do
  let stms = bodyStms $ funDefBody fd
      substs =
        drop (length (shapeContext (funDefRetType fd))) $
          map resSubExp $ bodyResult $ funDefBody fd
  addStms stms
  modify $ \s ->
    s
      { stateConstSubsts = M.insert cname substs $ stateConstSubsts s,
        stateConstScope = scopeOf stms <> stateConstScope s
      }

localConstsScope :: InternaliseM a -> InternaliseM a
localConstsScope m = do
  scope <- gets stateConstScope
  localScope scope m

-- | Construct an 'Assert' statement, but taking attributes into
-- account.  Always use this function, and never construct 'Assert'
-- directly in the internaliser!
assert ::
  String ->
  SubExp ->
  ErrorMsg SubExp ->
  SrcLoc ->
  InternaliseM Certs
assert desc se msg loc = assertingOne $ do
  attrs <- asks $ attrsForAssert . envAttrs
  attributing attrs $
    letExp desc $
      BasicOp $ Assert se msg (loc, mempty)

-- | Execute the given action if 'envDoBoundsChecks' is true, otherwise
-- just return an empty list.
asserting ::
  InternaliseM Certs ->
  InternaliseM Certs
asserting m = do
  doBoundsChecks <- asks envDoBoundsChecks
  if doBoundsChecks
    then m
    else pure mempty

-- | Execute the given action if 'envDoBoundsChecks' is true, otherwise
-- just return an empty list.
assertingOne ::
  InternaliseM VName ->
  InternaliseM Certs
assertingOne m = asserting $ Certs . pure <$> m
