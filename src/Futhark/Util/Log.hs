-- | Opaque type for an operations log that provides fast O(1)
-- appends.
module Futhark.Util.Log
  ( Log,
    toText,
    ToLog (..),
    MonadLogger (..),
  )
where

import Control.Monad.RWS.Lazy qualified
import Control.Monad.RWS.Strict qualified
import Control.Monad.Writer
import Data.DList qualified as DL
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO (stderr)

-- | An efficiently catenable sequence of log entries.
newtype Log = Log {unLog :: DL.DList T.Text}

instance Semigroup Log where
  Log l1 <> Log l2 = Log $ l1 <> l2

instance Monoid Log where
  mempty = Log mempty

-- | Transform a log into pretty.  Every log entry becomes its own line
-- (or possibly more, in case of multi-line entries).
toText :: Log -> T.Text
toText = T.intercalate "\n" . DL.toList . unLog

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
  logMsg :: (ToLog a) => a -> m ()
  logMsg = addLog . toLog

  -- | Append an entire log.
  addLog :: Log -> m ()

instance (Monad m) => MonadLogger (WriterT Log m) where
  addLog = tell

instance (Monad m) => MonadLogger (Control.Monad.RWS.Lazy.RWST r Log s m) where
  addLog = tell

instance (Monad m) => MonadLogger (Control.Monad.RWS.Strict.RWST r Log s m) where
  addLog = tell

instance MonadLogger IO where
  addLog = mapM_ (T.hPutStrLn stderr) . unLog
