module Language.Futhark.Interpreter.FFI.Server
  ( FutharkServer (..),
    startServer,
    FutharkServerM,
    server,
    interface,
    getValueUID,
    getValueUIDs,
    runFutharkServerM,
    typeUIDOf,
    typeLayoutOf,
    getChild,
    putChild
  )
where

import Control.Arrow (Arrow(second))
import Control.Monad.Reader (ReaderT (runReaderT), asks, MonadIO, MonadReader, MonadTrans (lift))
import Control.Monad.RWS (MonadState(put, get), gets)
import Control.Monad.State (StateT (runStateT))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Futhark.Server qualified as S
import Language.Futhark.Interpreter.FFI.Server.Explorer (exploreProgram)
import Language.Futhark.Interpreter.FFI.Server.Interface (ServerInterface (..))
import Language.Futhark.Interpreter.FFI.Server.TypeLayout (TypeLayout (..))
import Language.Futhark.Interpreter.FFI.UIDs (UIDSource, UIDSourceT, ValueUID, TypeUID, runUIDSourceT, getUID, getUIDs)
import Language.Futhark.Interpreter.FFI.Values (Direction)
import Prelude hiding (init)

-- Server and function calling
data FutharkServer = FutharkServer
  { fsInfo :: FutharkServerInfo,
    fsUIDSource :: UIDSource,
    fsState :: FutharkServerState
  }

data FutharkServerInfo = FutharkServerInfo
  { fsiServer :: S.Server,
    fsiInterface :: ServerInterface
  }

data FutharkServerState = FutharkServerState
  { fssValues :: M.Map ValueUID (TypeUID, M.Map Direction ValueUID)
  }

init :: S.Server -> IO FutharkServer
init s = do
  info <- FutharkServerInfo s <$> exploreProgram s
  pure $ FutharkServer info mempty $ FutharkServerState mempty

startServer :: FilePath -> IO FutharkServer
startServer p = S.startServer (S.newServerCfg p []) >>= init

newtype FutharkServerM a = FutharkServerM (ReaderT FutharkServerInfo (StateT FutharkServerState (UIDSourceT IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader FutharkServerInfo, MonadState FutharkServerState)

runFutharkServerM :: FutharkServerM a -> FutharkServer -> IO (a, FutharkServer)
runFutharkServerM (FutharkServerM m) s = do
  ((o, state'), src) <- runUIDSourceT (runStateT (runReaderT m $ fsInfo s) $ fsState s) $ fsUIDSource s
  pure (o, s {
    fsUIDSource = src,
    fsState = state'
    })

server :: FutharkServerM S.Server
server = asks fsiServer

typeUIDOf :: ValueUID -> FutharkServerM (Maybe TypeUID)
typeUIDOf vid = fmap fst . M.lookup vid <$> gets fssValues

typeLayout :: TypeUID -> FutharkServerM (Maybe TypeLayout)
typeLayout t = do
  i <- interface
  pure $ M.lookup t $ siTypeLayout i

typeLayoutOf :: ValueUID -> FutharkServerM (Maybe TypeLayout)
typeLayoutOf vid = typeUIDOf vid >>= maybe (pure Nothing) typeLayout

--typeOf :: ValueUID -> FutharkServerM (Maybe (Type PrimitiveType))
--typeOf =  typeLayoutOf vid >>= maybe (pure Nothing) toType
--  where
--    toType :: TypeLayout -> FutharkServerM (Type PrimitiveType)
--    toType (TLPrimitive t) = pure $ TAtom t
--    toType (TLArray t) = fmap TArray <$> typeOf t
--    toType _ = undefined

getChild :: ValueUID -> Direction -> FutharkServerM (Maybe ValueUID)
getChild vid d = do
  s <- gets fssValues
  pure $ M.lookup vid s >>= M.lookup d . snd

putChild :: ValueUID -> Direction -> ValueUID -> FutharkServerM ()
putChild pvid d cvid = do
  s <- get
  let children = fromMaybe mempty $ snd <$> M.lookup pvid (fssValues s)
  put s { fssValues = M.adjust (second $ const $ M.insert d cvid children) pvid $ fssValues s }

interface :: FutharkServerM ServerInterface
interface = asks fsiInterface

getValueUID :: FutharkServerM ValueUID
getValueUID = FutharkServerM $ lift . lift $ getUID

getValueUIDs :: Word -> FutharkServerM [ValueUID]
getValueUIDs n = FutharkServerM $ lift . lift $ getUIDs n
