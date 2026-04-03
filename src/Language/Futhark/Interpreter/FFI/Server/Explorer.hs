module Language.Futhark.Interpreter.FFI.Server.Explorer
  ( exploreProgram,
  )
where

import Control.Monad (forM)
import Control.Monad.State (MonadIO (liftIO), MonadState, StateT (runStateT), gets, modify)
import Data.Map qualified as M
import Futhark.Server qualified as S
import Language.Futhark.Interpreter.FFI.Util.BiMap qualified as BM
import Language.Futhark.Interpreter.FFI.Server.Interface (Entry (..), ServerInterface (..))
import Language.Futhark.Interpreter.FFI.Server.TypeLayout (TypeLayout (..))
import Language.Futhark.Interpreter.FFI.UIDs
import Language.Futhark.Interpreter.FFI.Values (PrimitiveType (..))

-- The explorer monad
newtype ServerExplorer a = ServerExplorer (UIDSourceT (StateT ServerInterface IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState ServerInterface)

runServerExplorer :: ServerExplorer a -> UIDSource -> IO (ServerInterface, UIDSource)
runServerExplorer (ServerExplorer m) s = do
  ((_, s'), o) <- runStateT (runUIDSourceT m s) mempty
  pure (o, s')

-- Utility functions
lookupTypeName :: S.TypeName -> ServerExplorer (Maybe TypeUID)
lookupTypeName n = ServerExplorer $ gets $ BM.lookupRight n . siType

putEntryPoint :: S.EntryName -> [TypeUID] -> [TypeUID] -> ServerExplorer EntryUID
putEntryPoint n i o = ServerExplorer $ do
  eid <- getUID
  modify
    ( \s ->
        s
          { siEntryPoint = BM.insert n eid $ siEntryPoint s,
            siEntryPointInfo = M.insert eid (Entry i o) $ siEntryPointInfo s
          }
    )
  pure eid

putType :: S.TypeName -> TypeLayout -> ServerExplorer TypeUID
putType n l = ServerExplorer $ do
  tid <- getUID
  modify
    ( \s ->
        s
          { siType = BM.insert n tid $ siType s,
            siTypeLayout = M.insert tid l $ siTypeLayout s
          }
    )
  pure tid

-- Exploration logic
exploreType :: S.Server -> S.TypeName -> ServerExplorer TypeUID
exploreType s n = do
  tid <- lookupTypeName n
  case tid of
    Just tid' -> pure tid'
    Nothing -> do
      k <- liftIO $ S.cmdKind s n
      case k of
        Right S.Primitive -> handlePrimitive
        Right S.Array -> handleArray
        Right S.Record -> handleRecord
        Right S.Sum -> handleSum
        Right S.Opaque -> handleOpaque
        Left _ -> error "TODO (0u2qeiowjdkslm)"
  where
    handlePrimitive = putType n $
      TLPrimitive $
        case n of
          "i8" -> TInt8
          "i16" -> TInt16
          "i32" -> TInt32
          "i64" -> TInt64
          "u8" -> TUInt8
          "u16" -> TUInt16
          "u32" -> TUInt32
          "u64" -> TUInt64
          "f16" -> TFloat16
          "f32" -> TFloat32
          "f64" -> TFloat64
          "bool" -> TBool
          _ -> error "TODO (89urijqowdklmacs)"
    handleArray = do
      e <- liftIO $ S.cmdElemtype s n
      case e of
        Right e' -> exploreType s e' >>= putType n . TLArray
        _ -> error "TODO (u890wqfioajscklm)"
    handleRecord = do
      fs <- liftIO $ S.cmdFields s n
      case fs of
        Right fs' ->
          forM fs' (\f -> (S.fieldName f,) <$> exploreType s (S.fieldType f))
            >>= putType n . TLRecord
        Left _ -> error "TODO (aq0iwpoak)"
    handleSum = do
      vs <- liftIO $ S.cmdVariants s n
      case vs of
        Right vs' ->
          forM vs' (\v -> M.singleton (S.variantName v) <$> mapM (exploreType s) (S.variantTypes v))
            >>= putType n . TLSum . M.unions
        Left _ -> error "TODO (r928quwfijoasckl)"
    handleOpaque = putType n TLOpaque

exploreEntryPoint :: S.Server -> S.EntryName -> ServerExplorer EntryUID
exploreEntryPoint s n = do
  is <- liftIO $ S.cmdInputs s n
  os <- liftIO $ S.cmdOutputs s n
  case (is, os) of
    (Right is', Right os') -> do
      is'' <- forM is' $ exploreType s . S.inputType
      os'' <- forM os' $ exploreType s . S.outputType
      putEntryPoint n is'' os''
    _ -> error "TODO (98urqoijwdlansc)"

exploreProgram :: S.Server -> IO ServerInterface
exploreProgram s = fst <$> runServerExplorer exploreProgram' mempty
  where
    exploreProgram' = do
      es <- liftIO $ S.cmdEntryPoints s
      case es of
        Right es' -> mapM_ (exploreEntryPoint s) es'
        _ -> error "TODO (8u2eiojqdlkm)"
