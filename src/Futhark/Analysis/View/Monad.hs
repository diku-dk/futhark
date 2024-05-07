module Futhark.Analysis.View.Monad where

import Futhark.Analysis.View.Representation
import Futhark.MonadFreshNames
import Futhark.SoP.Monad
import Language.Futhark qualified as E
import Control.Monad.RWS.Strict hiding (Sum)
import Data.Map.Strict qualified as M
import Futhark.Analysis.Refinement.Latex (LaTeX)
import Prelude hiding (log)

type Log = LaTeX

data VEnv = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Term E.Exp,
    indexfns :: IndexFns
  }

-- The IndexFn monad keeps a source of fresh names and writes indexfns.
newtype IndexFnM a = IndexFnM (RWS () [Log] VEnv a)
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

instance MonadSoP Term E.Exp IndexFnM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

execIndexFnM :: IndexFnM a -> VNameSource -> (IndexFns, [Log])
execIndexFnM (IndexFnM m) vns = getRes $ execRWS m () s
  where
    getRes (env, log) = (indexfns env, log)
    s = VEnv vns mempty mempty

insertIndexFn :: E.VName -> IndexFn -> IndexFnM ()
insertIndexFn x v =
  modify $ \env -> env {indexfns = M.insert x v $ indexfns env}
