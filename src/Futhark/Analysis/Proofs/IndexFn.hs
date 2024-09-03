{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Proofs.IndexFn
where

import qualified Data.Map as M
import Language.Futhark (VName)
import Language.Futhark qualified as E
import qualified Data.List.NonEmpty as NE
import Futhark.Analysis.Proofs.Symbol
import Futhark.SoP.SoP (SoP)
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (mkName))
import Futhark.MonadFreshNames
import Control.Monad.RWS.Strict
-- import Futhark.SoP.SoP (SoP, int2SoP, (.-.))
-- import Futhark.Analysis.Proofs.Unify (Replaceable(..))

data Domain = Iota (SoP Symbol) -- [0, ..., n-1]
            | Cat            -- Catenate_{k=1}^{m-1} [b_{k-1}, ..., b_k)
                VName        -- k
                (SoP Symbol) -- m
                (SoP Symbol) -- b
  deriving Show

data Iterator = Forall VName Domain
              | Empty
  deriving Show

newtype Cases a b = Cases (NE.NonEmpty (a, b))
  deriving (Show, Eq, Ord)

data IndexFn = IndexFn
  { iterator :: Iterator,
    value :: Cases Symbol (SoP Symbol)
  }
  deriving Show

data VEnv = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Symbol E.Exp,
    indexfns :: M.Map VName IndexFn
    -- toplevel :: M.Map E.VName ([E.Pat], IndexFn)
  }


-- The IndexFn monad keeps a source of fresh names and writes indexfns.
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

-- This is required by MonadSoP.
instance Nameable Symbol where
  mkName (VNameSource i) = (Var $ E.VName "x" i, VNameSource $ i + 1)

instance MonadSoP Symbol E.Exp IndexFnM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

runIndexFnM :: IndexFnM a -> VNameSource -> (a, M.Map VName IndexFn)
runIndexFnM (IndexFnM m) vns = getRes $ runRWS m () s
  where
    getRes (x, env, _) = (x, indexfns env)
    s = VEnv vns mempty mempty

insertIndexFn :: E.VName -> IndexFn -> IndexFnM ()
insertIndexFn x v =
  modify $ \env -> env {indexfns = M.insert x v $ indexfns env}

-- insertTopLevel :: E.VName -> ([E.Pat], IndexFn) -> IndexFnM ()
-- insertTopLevel vn (args, ixfn) =
--   modify $
--     \env -> env {toplevel = M.insert vn (args, ixfn) $ toplevel env}

-- clearAlgEnv :: IndexFnM ()
-- clearAlgEnv =
--   modify $ \env -> env {algenv = mempty}




-- NOTE don't think this should be re-implemented. Instead use
-- unification on index functions.
--
-- instance Eq Domain where
--   -- TODO need to rename.
--   -- Since the whole domain must be covered by an index function,
--   -- it is sufficient to check that starts and ends are equal.
--   u == v =
--     start u == start v && end u == end v
--     where
--       start :: Domain -> SoP Symbol
--       start (Iota _) = int2SoP 0
--       start (Cat k _ b) = rep (M.singleton k $ int2SoP 0) b

--       end (Iota n) = n .-. int2SoP 1
--       end (Cat k m b) = rep (M.singleton k m) b .-. int2SoP 1

-- instance Eq Iterator where
--   (Forall _ u@(Cat k _ _)) == (Forall _ v@(Cat k' _ _)) = u == v && k == k'
--   (Forall _ u) == (Forall _ v) = u == v
--   Empty == Empty = True
--   _ == _ = False
