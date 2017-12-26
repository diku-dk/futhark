{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Opaque type for an operations log that provides fast O(1)
-- appends.
module Futhark.Util.Log
       ( Log
       , toText
       , ToLog (..)
       , MonadLogger (..)
       )

where

import Control.Monad.Writer
import qualified Control.Monad.RWS.Strict
import qualified Control.Monad.RWS.Lazy
import qualified Data.Text as T
import qualified Data.DList as DL
import qualified Data.Semigroup as Sem

newtype Log = Log { unLog :: DL.DList T.Text }

instance Sem.Semigroup Log where
  Log l1 <> Log l2 = Log $ l1 <> l2

instance Monoid Log where
  mappend = (Sem.<>)
  mempty = Log mempty

-- | Transform a log into text.  Every log entry becomes its own line
-- (or possibly more, in case of multi-line entries).
toText :: Log -> T.Text
toText = T.unlines . DL.toList . unLog

-- | Typeclass for things that can be turned into a single-entry log.
class ToLog a where
  toLog :: a -> Log

instance ToLog String where
  toLog = Log . DL.singleton . T.pack

instance ToLog T.Text where
  toLog = Log . DL.singleton

-- | Typeclass for monads that support logging.
class (Applicative m, Monad m) => MonadLogger m where
  -- | Add one log entry.
  logMsg :: ToLog a => a -> m ()
  logMsg = addLog . toLog

  -- | Append an entire log.
  addLog :: Log -> m ()

instance (Applicative m, Monad m) => MonadLogger (WriterT Log m) where
  addLog = tell

instance (Applicative m, Monad m) => MonadLogger (Control.Monad.RWS.Lazy.RWST r Log s m) where
  addLog = tell

instance (Applicative m, Monad m) => MonadLogger (Control.Monad.RWS.Strict.RWST r Log s m) where
  addLog = tell
