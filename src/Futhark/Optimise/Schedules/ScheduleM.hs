module Futhark.Optimise.Schedules.ScheduleM
  ( ScheduleEnv(..)
  , ScheduleM
  , runScheduleM
  , rollbackAlgEnv
  ) where

import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))

----------------------------------------
--- SoP
----------------------------------------

data ScheduleEnv = ScheduleEnv
  { vNameSource :: VNameSource,
    algEnv :: AlgEnv VName (PrimExp VName)
  }

newtype ScheduleM a = ScheduleM (ReaderT (Scope SOACS) (State ScheduleEnv) a)
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadState ScheduleEnv,
      HasScope SOACS,
      LocalScope SOACS
    )

instance MonadFreshNames ScheduleM where
  getNameSource = gets vNameSource
  putNameSource source = modify (\env -> env {vNameSource = source})

instance MonadSoP VName (PrimExp VName) ScheduleM where
  getUntrans = gets (untrans . algEnv)
  getRanges = gets (ranges . algEnv)
  getEquivs = gets (equivs . algEnv)
  modifyEnv f = modify $ \env -> env {algEnv = f $ algEnv env}

runScheduleM :: (MonadFreshNames m) => Scope SOACS -> ScheduleEnv -> ScheduleM a -> m a
runScheduleM scope env (ScheduleM a) = modifyNameSource $ \src ->
  let state = runReaderT a scope
      (x, newEnv) = runState state (env {vNameSource = src})
   in (x, vNameSource newEnv)

rollbackAlgEnv :: ScheduleM a -> ScheduleM a
rollbackAlgEnv computation = do
  alg <- gets algEnv
  res <- computation
  modify (\env -> env {algEnv = alg})
  pure res

