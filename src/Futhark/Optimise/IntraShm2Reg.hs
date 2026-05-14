-- | This module tries to switch the default allocation
--   of fast arrays from shared memory to registers.

module Futhark.Optimise.IntraShm2Reg
  ( applyShm2Reg ) where

--import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad (forM)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Maybe
import Futhark.Builder
import Futhark.IR.SOACS
-- import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))
import Debug.Trace


----------------------------------------
--- SoP
----------------------------------------

data Shm2RegEnv = Shm2RegEnv
  { vNameSource :: VNameSource,
    algEnv :: AlgEnv VName (PrimExp VName)
  }

newtype Shm2RegM a = Shm2RegM (ReaderT (Scope GPU) (State Shm2RegEnv) a)
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadState Shm2RegEnv,
      HasScope GPU,
      LocalScope GPU
    )

instance MonadFreshNames Shm2RegM where
  getNameSource = gets vNameSource
  putNameSource source = modify (\env -> env {vNameSource = source})

instance MonadSoP VName (PrimExp VName) Shm2RegM where
  getUntrans = gets (untrans . algEnv)
  getRanges = gets (ranges . algEnv)
  getEquivs = gets (equivs . algEnv)
  modifyEnv f = modify $ \env -> env {algEnv = f $ algEnv env}

runShm2RegM :: (MonadFreshNames m) => Scope GPU -> Shm2RegEnv -> Shm2RegM a -> m a
runShm2RegM scope env (Shm2RegM a) = modifyNameSource $ \src ->
  let state = runReaderT a scope
      (x, newEnv) = runState state (env {vNameSource = src})
   in (x, vNameSource newEnv)

----------------------------------------
--- Environments
----------------------------------------

type Env = (TopEnv, BotEnv)

-----------------------------------------
--- The Definition of the Pass
----------------------------------------

runShm2Reg :: Env -> Scope GPU -> Stms GPU -> PassM (Stms GPU)
runShm2Reg env scope stms =
  runShm2RegM
    scope
    (Shm2RegEnv { vNameSource = mempty, algEnv = mempty })
    (applySchedOnStms env stms)

applyShm2RegOnFun :: Stms GPU -> FunDef GPU -> PassM (FunDef GPU)
applyShm2RegOnFun consts fun = do
  let fun_bdy = funDefBody fun
      topenv  = freshTopEnv { bdy_res = map resSubExp $ bodyResult fun_bdy }
      scope   = scopeOf fun <> scopeOf consts
  fun_stms' <- runShm2Reg (topenv, freshBotEnv) scope $ bodyStms fun_bdy
  pure fun { funDefBody = (funDefBody fun) { bodyStms = fun_stms' } }

-- | The pass definition.
{-# NOINLINE applySchedules #-}
applyShm2Reg :: Pass GPU GPU
applyShm2Reg =
  Pass
    { passName = "Enabling Register Mapping of Arrays in IntraGroup Kernels"
    , passDescription = "Attempts to change the default allocation of fast arrays in intra-group kernels from shared memory to registers."
    , passFunction = \ prog -> do
        trace ("\nProg before pass: "++prettyString prog++"\n") $
          intraproceduralTransformationWithConsts pure applyShm2RegOnFun prog
    }

-----------------------------------------
--- Analysis
----------------------------------------
