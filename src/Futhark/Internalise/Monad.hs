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
    addOpaques,
    addFunDef,
    lookupFunction,
    lookupConst,
    bindFunction,
    bindConstant,
    assert,
    locating,

    -- * Convenient reexports
    module Futhark.Tools,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import Data.Map.Strict qualified as M
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools

type FunInfo =
  ( [VName],
    [DeclType],
    [FParam SOACS],
    [(SubExp, Type)] -> Maybe [(DeclExtType, RetAls)]
  )

type FunTable = M.Map VName FunInfo

-- | A mapping from external variable names to the corresponding
-- internalised subexpressions.
type VarSubsts = M.Map VName [SubExp]

data InternaliseEnv = InternaliseEnv
  { envSubsts :: VarSubsts,
    envDoBoundsChecks :: Bool,
    envSafe :: Bool,
    envAttrs :: Attrs,
    envLoc :: Loc
  }

data InternaliseState = InternaliseState
  { stateNameSource :: VNameSource,
    stateFunTable :: FunTable,
    stateConstSubsts :: VarSubsts,
    stateFuns :: [FunDef SOACS],
    stateTypes :: OpaqueTypes
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
      HasScope SOACS
    )

-- Internalisation has to deal with the risk of multiple binding of
-- the same variable (although always of the same type) in the
-- program; in particular this might imply shadowing a constant.  The
-- LocalScope instance for BuilderT does not handle this properly (and
-- doing so would make it slower).  So we remove already-known
-- variables before passing the scope on.
instance LocalScope SOACS InternaliseM where
  localScope scope (InternaliseM m) = do
    old_scope <- askScope
    InternaliseM $ localScope (scope `M.difference` old_scope) m

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
  (MonadFreshNames m) =>
  Bool ->
  InternaliseM () ->
  m (OpaqueTypes, Stms SOACS, [FunDef SOACS])
runInternaliseM safe (InternaliseM m) =
  modifyNameSource $ \src ->
    let ((_, consts), s) =
          runState (runReaderT (runBuilderT m mempty) newEnv) (newState src)
     in ( (stateTypes s, consts, reverse $ stateFuns s),
          stateNameSource s
        )
  where
    newEnv =
      InternaliseEnv
        { envSubsts = mempty,
          envDoBoundsChecks = True,
          envSafe = safe,
          envAttrs = mempty,
          envLoc = mempty
        }
    newState src =
      InternaliseState
        { stateNameSource = src,
          stateFunTable = mempty,
          stateConstSubsts = mempty,
          stateFuns = mempty,
          stateTypes = mempty
        }

substitutingVars :: VarSubsts -> InternaliseM a -> InternaliseM a
substitutingVars substs = local $ \env -> env {envSubsts = substs <> envSubsts env}

lookupSubst :: VName -> InternaliseM (Maybe [SubExp])
lookupSubst v = do
  env_substs <- asks $ M.lookup v . envSubsts
  const_substs <- gets $ M.lookup v . stateConstSubsts
  pure $ env_substs `mplus` const_substs

-- | Add opaque types.  If the types are already known, they will not
-- be added.
addOpaques :: OpaqueTypes -> InternaliseM ()
addOpaques ts@(OpaqueTypes ts') = modify $ \s ->
  -- TODO: handle this better (#1960)
  case find (knownButDifferent (stateTypes s)) ts' of
    Just (x, _) -> error $ "addOpaques: multiple incompatible definitions of type " <> nameToString x
    Nothing -> s {stateTypes = stateTypes s <> ts}
  where
    knownButDifferent (OpaqueTypes old_ts) (v, def) =
      any (\(v_old, v_def) -> v == v_old && def /= v_def) old_ts

-- | Add a function definition to the program being constructed.
addFunDef :: FunDef SOACS -> InternaliseM ()
addFunDef fd = modify $ \s -> s {stateFuns = fd : stateFuns s}

lookupFunction :: VName -> InternaliseM FunInfo
lookupFunction fname = maybe bad pure =<< gets (M.lookup fname . stateFunTable)
  where
    bad = error $ "Internalise.lookupFunction: Function '" ++ prettyString fname ++ "' not found."

lookupConst :: VName -> InternaliseM (Maybe [SubExp])
lookupConst fname = do
  is_var <- asksScope (fname `M.member`)
  fname_subst <- lookupSubst fname
  case (is_var, fname_subst) of
    (_, Just ses) -> pure $ Just ses
    (True, _) -> pure $ Just [Var fname]
    _ -> pure Nothing

bindFunction :: VName -> FunDef SOACS -> FunInfo -> InternaliseM ()
bindFunction fname fd info = do
  addFunDef fd
  modify $ \s -> s {stateFunTable = M.insert fname info $ stateFunTable s}

bindConstant :: VName -> FunDef SOACS -> InternaliseM ()
bindConstant cname fd = do
  addStms $ bodyStms $ funDefBody fd

  case map resSubExp . bodyResult . funDefBody $ fd of
    [se] -> do
      letBindNames [cname] $ BasicOp $ SubExp se
    ses -> do
      let substs =
            drop (length (shapeContext (map fst (funDefRetType fd)))) ses
      modify $ \s ->
        s
          { stateConstSubsts = M.insert cname substs $ stateConstSubsts s
          }

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
  attributing attrs $ letExp desc $ BasicOp $ Assert se msg (loc, mempty)

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

-- | Attach the provided location to all statements produced during execution of
-- this action.
locating :: (Located a) => a -> InternaliseM b -> InternaliseM b
locating a
  | loc == mempty = id
  | otherwise = censorStms $ fmap onStm
  where
    loc = locOf a
    onStm (Let pat aux e) = Let pat (aux {stmAuxLoc = Provenance loc}) e
