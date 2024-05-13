module Futhark.Analysis.View.Monad where

import Futhark.Analysis.View.Representation
import Futhark.MonadFreshNames
import Futhark.SoP.Monad
import Language.Futhark qualified as E
import Control.Monad.RWS.Strict hiding (Sum)
import Data.Map.Strict qualified as M
import Futhark.Analysis.Refinement.Latex (LaTeX)

type Log = LaTeX

data VEnv = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Term E.Exp,
    indexfns :: IndexFns,
    toplevel :: M.Map String ([E.Pat], IndexFn)
  }

-- The IndexFn monad keeps a source of fresh names and writes indexfns.
newtype IndexFnM a = IndexFnM (RWS () [Log] VEnv a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames,
      MonadState VEnv,
      MonadWriter [Log]
    )

instance (Monoid w) => MonadFreshNames (RWS r w VEnv) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

instance MonadSoP Term E.Exp IndexFnM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

execIndexFnM :: IndexFnM a -> VNameSource -> (IndexFns, [Log])
execIndexFnM (IndexFnM m) vns = getRes $ execRWS m () s
  where
    getRes (env, w) = (indexfns env, w)
    s = VEnv vns mempty mempty mempty

insertIndexFn :: E.VName -> IndexFn -> IndexFnM ()
insertIndexFn x v =
  modify $ \env -> env {indexfns = M.insert x v $ indexfns env}

insertTopLevel :: E.VName -> ([E.Pat], IndexFn) -> IndexFnM ()
insertTopLevel vn (args, ixfn) =
  modify $
    \env -> env {toplevel = M.insert (E.baseString vn) (args, ixfn) $ toplevel env}

clearAlgEnv :: IndexFnM ()
clearAlgEnv =
  modify $ \env -> env {algenv = mempty}
