module Futhark.Analysis.Proofs.Monad where

import Control.Monad (when)
import Control.Monad.RWS.Strict
import Data.Map qualified as M
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.Symbol
import Futhark.MonadFreshNames
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))
import Futhark.Util.Pretty (Pretty, docString, pretty)
import Language.Futhark (VName)
import Language.Futhark qualified as E
import Futhark.SoP.Expression (Expression)
import Futhark.Analysis.Proofs.IndexFn
import qualified Futhark.Analysis.Proofs.AlgebraPC.Algebra as Algebra

data IndexFnProperty
  = Blah
  deriving (Show, Eq, Ord)

data VEnv = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Algebra.Symbol Symbol Algebra.Property,
    indexfns :: M.Map VName IndexFn,
    toplevel :: M.Map VName ([VName], [Maybe VName], IndexFn),
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

instance (Monoid w) => MonadFreshNames (RWS r w VEnv) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

instance Expression Symbol where
  -- TODO Is this constraint on MonadSoP needed?

instance MonadSoP Algebra.Symbol Symbol Algebra.Property IndexFnM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  getProperties = gets (properties . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}
  findSymLEq0 = Algebra.findSymbolLEq0

runIndexFnM :: IndexFnM a -> VNameSource -> (a, M.Map VName IndexFn)
runIndexFnM (IndexFnM m) vns = getRes $ runRWS m () s
  where
    getRes (x, env, _) = (x, indexfns env)
    s = VEnv vns mempty mempty mempty False

insertIndexFn :: E.VName -> IndexFn -> IndexFnM ()
insertIndexFn x v =
  modify $ \env -> env {indexfns = M.insert x v $ indexfns env}

-- insertTopLevel :: E.VName -> ([E.Pat], IndexFn) -> IndexFnM ()
-- insertTopLevel vn (args, ixfn) =
--   modify $
--     \env -> env {toplevel = M.insert vn (args, ixfn) $ toplevel env}

clearAlgEnv :: IndexFnM ()
clearAlgEnv =
  modify $ \env -> env {algenv = mempty}

--------------------------------------------------------------
-- Utilities
--------------------------------------------------------------
whenDebug :: IndexFnM () -> IndexFnM ()
whenDebug x = do
  debug <- gets debug
  when debug x

debugM :: String -> IndexFnM ()
debugM x = do
  whenDebug $ traceM $ "🪲 " <> x

debugPrettyM :: (Pretty a) => String -> a -> IndexFnM ()
debugPrettyM msg x = do
  whenDebug $ traceM $ docString $ "🪲 " <> pretty msg <> " " <> pretty x

withDebug :: b -> IndexFnM b
withDebug f = do
  modify (\s -> s {debug = True})
  pure f

debugOn :: IndexFnM ()
debugOn = do
  modify (\s -> s {debug = True})
