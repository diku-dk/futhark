{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Properties.Monad
  ( VEnv,
    IndexFnM (..),
    EArgs,
    ApplyEffect,
    runIndexFnM,
    insertIndexFn,
    insertTopLevel,
    clearAlgEnv,
    whenDebug,
    debugM,
    debugT,
    debugT',
    debugPrettyM,
    debugPrintAlgEnv,
    debugOn,
    withDebug,
    withoutDebug,
    rollbackAlgEnv,
    getIndexFns,
    getTopLevelIndexFns,
    prettyStr,
    insertTopLevelDef,
    getTopLevelDefs,
    printM,
    getAlgEnv,
    printTrace,
    getII,
    insertII,
    lookupIndexFn,
    printAlgEnv,
    addInvAlias,
    getInvAlias,
    setOutputNames,
    getOutputNames,
    whenBoundsChecking,
    withoutBoundsChecks,
    lookupUninterpreted,
    lookupConcat,
    insertConcat,
    insertTopLevel1,
    getTopLevel1,
  )
where

import Control.Monad (when)
import Control.Monad.RWS.Strict
import Data.Map qualified as M
import Debug.Trace (traceM)
import Futhark.Analysis.Properties.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.Property
import Futhark.Analysis.Properties.Symbol
import Futhark.MonadFreshNames
import Futhark.SoP.Expression (Expression (..))
import Futhark.SoP.Monad
import Futhark.Util (isEnvVarAtLeast)
import Futhark.Util.Pretty (Pretty, docStringW, pretty, prettyString)
import Language.Futhark (VName)
import Language.Futhark qualified as E
import qualified Data.List.NonEmpty as NE

type EArgs = NE.NonEmpty (E.Info (Maybe E.VName), E.Exp)
type ApplyEffect = [VName] -> IndexFnM ()

data VEnv = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Algebra.Symbol Symbol (Property Algebra.Symbol),
    indexfns :: M.Map VName [IndexFn],
    toplevel :: M.Map VName ([E.Pat E.ParamType], [IndexFn], E.TypeExp E.Exp VName),
    toplevel1 :: M.Map VName (E.SrcLoc -> EArgs -> IndexFnM (ApplyEffect, [IndexFn])),
    defs :: M.Map VName ([E.Pat E.ParamType], E.Exp),
    ii :: M.Map Domain (VName, IndexFn),
    invalias :: M.Map VName VName,
    uninterpreted :: M.Map E.Exp VName,
    outputNames :: [VName],
    concats :: M.Map VName [IndexFn],
    checkBounds :: Bool,
    debug :: Bool
  }

newtype IndexFnM a = IndexFnM (RWS () () VEnv a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames,
      MonadState VEnv
    )

runIndexFnM :: IndexFnM a -> VNameSource -> (a, M.Map VName [IndexFn])
runIndexFnM (IndexFnM m) vns = getRes $ runRWS m () s
  where
    getRes (x, env, _) = (x, indexfns env)
    s = VEnv vns mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty True False

instance (Monoid w) => MonadFreshNames (RWS r w VEnv) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

instance MonadSoP Algebra.Symbol Symbol (Property Algebra.Symbol) IndexFnM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  getProperties = gets (properties . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}
  findSymLEq0 = Algebra.findSymbolLEq0

-- TODO Remove Expression constraint from MonadSoP, if these are unused.
-- See, for example, RefineEquivs.
instance Expression Symbol where
  moduloIsh _ = Nothing
  divInsteadOfMod = undefined
  processExp = undefined

getIndexFns :: IndexFnM (M.Map VName [IndexFn])
getIndexFns = gets indexfns

getTopLevel1 :: IndexFnM (M.Map VName (E.SrcLoc -> EArgs -> IndexFnM (ApplyEffect, [IndexFn])))
getTopLevel1 = gets toplevel1

insertTopLevel1 :: E.VName -> (E.SrcLoc -> EArgs -> IndexFnM (ApplyEffect, [IndexFn])) -> IndexFnM ()
insertTopLevel1 vn x =
  modify $ \env -> env {toplevel1 = M.insert vn x $ toplevel1 env}

getTopLevelIndexFns :: IndexFnM (M.Map VName ([E.Pat E.ParamType], [IndexFn], E.TypeExp E.Exp VName))
getTopLevelIndexFns = gets toplevel

getTopLevelDefs :: IndexFnM (M.Map VName ([E.Pat E.ParamType], E.Exp))
getTopLevelDefs = gets defs

getAlgEnv :: IndexFnM (AlgEnv Algebra.Symbol Symbol (Property Algebra.Symbol))
getAlgEnv = gets algenv

getII :: IndexFnM (M.Map Domain (VName, IndexFn))
getII = gets ii

lookupIndexFn :: VName -> IndexFnM (Maybe [IndexFn])
lookupIndexFn vn = M.lookup vn <$> getIndexFns

insertIndexFn :: E.VName -> [IndexFn] -> IndexFnM ()
insertIndexFn x v =
  modify $ \env -> env {indexfns = M.insert x v $ indexfns env}

insertTopLevel :: E.VName -> ([E.Pat E.ParamType], [IndexFn], E.TypeExp E.Exp VName) -> IndexFnM ()
insertTopLevel vn (args, fns, te) =
  modify $
    \env -> env {toplevel = M.insert vn (args, fns, te) $ toplevel env}

insertTopLevelDef :: E.VName -> ([E.Pat E.ParamType], E.Exp) -> IndexFnM ()
insertTopLevelDef vn (args, e) =
  modify $
    \env -> env {defs = M.insert vn (args, e) $ defs env}

insertII :: Domain -> (VName, IndexFn) -> IndexFnM ()
insertII dom (vn, f) = do
  modify $ \env -> env {ii = M.insert dom (vn, f) $ ii env}

lookupConcat :: MonadState VEnv f => VName -> f (Maybe [IndexFn])
lookupConcat vn = M.lookup vn <$> gets concats

insertConcat :: MonadState VEnv m => VName -> [IndexFn] -> m ()
insertConcat vn fs =
  modify $ \env -> env {concats = M.insert vn fs $ concats env}

clearAlgEnv :: IndexFnM ()
clearAlgEnv =
  modify $ \env -> env {algenv = mempty}

rollbackAlgEnv :: IndexFnM a -> IndexFnM a
rollbackAlgEnv computation = do
  alg <- gets algenv
  res <- computation
  modify (\env -> env {algenv = alg})
  pure res

-- Maps vn to vn' and vn' to vn.
addInvAlias :: VName -> VName -> IndexFnM ()
addInvAlias vn vn' =
  modify $
    \env -> env {invalias = M.insert vn' vn $ M.insert vn vn' $ invalias env}

getInvAlias :: VName -> IndexFnM (Maybe VName)
getInvAlias vn = (M.!? vn) <$> gets invalias

setOutputNames :: MonadState VEnv m => [VName] -> m ()
setOutputNames vns =
  modify $ \env -> env {outputNames = vns}

getOutputNames :: MonadState VEnv m => m [VName]
getOutputNames = gets outputNames

whenBoundsChecking :: MonadState VEnv m => m () -> m ()
whenBoundsChecking m = do
  c <- gets checkBounds
  when c m

withoutBoundsChecks :: IndexFnM a -> IndexFnM a
withoutBoundsChecks m = do
  modify $ \env -> env {checkBounds = False}
  res <- m
  modify $ \env -> env {checkBounds = True}
  pure res

lookupUninterpreted :: (MonadState VEnv m, MonadFreshNames m) => E.Exp -> m VName
lookupUninterpreted e = do
  unint <- gets uninterpreted
  case M.lookup e unint of
    Just vn -> pure vn
    Nothing -> do
      vn <- newVName $ prettyStr e
      modify $ \env -> env {uninterpreted = M.insert e vn $ uninterpreted env}
      pure vn

--------------------------------------------------------------
-- Utilities
--------------------------------------------------------------
whenDebug :: IndexFnM () -> IndexFnM ()
whenDebug x = do
  debug <- gets debug
  when debug x

debugM :: String -> IndexFnM ()
debugM x = do
  whenDebug $ traceM x

debugT :: (Show a) => String -> IndexFnM a -> IndexFnM a
debugT msg m = do
  a <- m
  debugM (msg <> ": " <> show a)
  pure a

debugT' :: (Pretty a) => String -> IndexFnM a -> IndexFnM a
debugT' msg m = do
  a <- m
  debugM (msg <> ": " <> prettyString a)
  pure a

debugPrettyM :: (Pretty a) => String -> a -> IndexFnM ()
debugPrettyM msg x = do
  whenDebug $ traceM $ docStringW 110 $ "🪲 " <> pretty msg <> " " <> pretty x

prettyStr :: (Pretty a) => a -> String
prettyStr x = docStringW 110 (pretty x)

debugPrintAlgEnv :: IndexFnM ()
debugPrintAlgEnv = do
  algenv <- gets algenv
  debugPrettyM "" algenv

debugOn :: IndexFnM ()
debugOn = do
  modify (\s -> s {debug = True})

withDebug :: IndexFnM b -> IndexFnM b
withDebug f = do
  debugOn
  f

withoutDebug :: IndexFnM b -> IndexFnM b
withoutDebug f = do
  toggle <- gets debug
  modify (\s -> s {debug = False})
  x <- f
  modify (\s -> s {debug = toggle})
  pure x

printM :: (Monad m) => Int -> String -> m ()
printM level
  | isEnvVarAtLeast "FUTHARK_INDEXFN" level = traceM
  | otherwise = const $ pure ()

printTrace :: (Monad m, Pretty b) => Int -> String -> m b -> m b
printTrace level msg m = do
  a <- m
  printM level (msg <> " " <> prettyString a)
  pure a

printAlgEnv :: Int -> IndexFnM ()
printAlgEnv level = do
  printM level . prettyStr =<< getAlgEnv
