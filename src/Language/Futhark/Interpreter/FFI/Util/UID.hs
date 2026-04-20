{-# LANGUAGE UndecidableInstances #-}

module Language.Futhark.Interpreter.FFI.Util.UID
  ( -- Unique IDs
    UID (uid),
    -- Unique ID source
    UIDSource,
    nextUID,
    -- Unique ID source monad transformer
    UIDSourceT,
    runUIDSourceT,
    UIDSourceM,
    runUIDSourceM,
    getUID,
    getUIDs,
  )
where

import Control.Monad.RWS (MonadReader (ask, local))
import Control.Monad.State (MonadIO, MonadState (get, put, state), StateT (runStateT))
import Control.Monad.Trans.Class
import Data.Functor.Identity (Identity (runIdentity))

-- External IDs
newtype UID p r = UID {uid :: r}
  deriving (Show, Eq, Ord, Functor)

-- External ID source
newtype UIDSource r = UIDSource r

instance (Ord r) => Semigroup (UIDSource r) where
  UIDSource i1 <> UIDSource i2 = UIDSource $ max i1 i2

instance (Ord r, Bounded r) => Monoid (UIDSource r) where
  mempty = UIDSource minBound

nextUID :: (Enum r) => UIDSource r -> (UID p r, UIDSource r)
nextUID (UIDSource i) = (UID i, UIDSource $ succ i)

-- External ID source monad transformer
newtype UIDSourceT r m a = UIDSourceT (StateT (UIDSource r) m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

type UIDSourceM r = UIDSourceT r Identity

instance (MonadState s m) => MonadState s (UIDSourceT r m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadReader r m) => MonadReader r (UIDSourceT r m) where
  ask = lift ask
  local f (UIDSourceT m) = UIDSourceT (local f m)

runUIDSourceT :: UIDSourceT r m a -> UIDSource r -> m (a, UIDSource r)
runUIDSourceT (UIDSourceT m) = runStateT m

runUIDSourceM :: UIDSourceT r Identity a -> UIDSource r -> (a, UIDSource r)
runUIDSourceM m = runIdentity . runUIDSourceT m

getUID :: (Monad m, Enum r) => UIDSourceT r m (UID p r)
getUID = UIDSourceT $ state nextUID

getUIDs :: (Monad m, Bounded r, Enum r) => r -> UIDSourceT r m [UID p r]
getUIDs n = mapM (const getUID) [minBound .. n]
