module Language.Futhark.Interpreter.FFI.ServerM
  ( S.TypeName,
    ValueRef,
    Server,
    startServer,
    ServerM,
    runServerM,
    gc,
    call,
    -- Interrogation
    inputs,
    output,
    kind,
    vtype,
    -- Primitives
    getPrim,
    putPrim,
    -- Arrays
    rank,
    elemType,
    mkArray,
    shape,
    index,
    -- Records
    fields,
    mkRecord,
    project,
    -- Sums
    variants,
    mkSum,
    destruct,
    variant,
    -- Error handling convenience
    throwServerLeft,
    throwServerJust,
    throwLeft,
    throwJust,
    throwNothing,
  )
where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.IORef (IORef, mkWeakIORef, newIORef, readIORef)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Unique (hashUnique, newUnique)
import Data.Vector.Storable qualified as V
import Futhark.Data qualified as D
import Futhark.Server qualified as S
import Futhark.Server.Values qualified as S
import Language.Futhark.Interpreter.FFI.AtomicList as AL
import Language.Futhark.Syntax

-- | Converts a PrimValue to a Data Value
pToD :: PrimValue -> D.Value
pToD (SignedValue (Int8Value i)) = D.putValue1 i
pToD (SignedValue (Int16Value i)) = D.putValue1 i
pToD (SignedValue (Int32Value i)) = D.putValue1 i
pToD (SignedValue (Int64Value i)) = D.putValue1 i
pToD (UnsignedValue (Int8Value i)) = D.putValue1 (fromIntegral i :: Word8)
pToD (UnsignedValue (Int16Value i)) = D.putValue1 (fromIntegral i :: Word16)
pToD (UnsignedValue (Int32Value i)) = D.putValue1 (fromIntegral i :: Word32)
pToD (UnsignedValue (Int64Value i)) = D.putValue1 (fromIntegral i :: Word64)
pToD (FloatValue (Float16Value f)) = D.putValue1 f
pToD (FloatValue (Float32Value f)) = D.putValue1 f
pToD (FloatValue (Float64Value f)) = D.putValue1 f
pToD (BoolValue b) = D.putValue1 b

-- | Converts a Data Value to a PrimValue, assuming that it is a singleton
dToP :: D.Value -> PrimValue
dToP (D.I8Value _ vs) = SignedValue $ Int8Value $ vs V.! 0
dToP (D.I16Value _ vs) = SignedValue $ Int16Value $ vs V.! 0
dToP (D.I32Value _ vs) = SignedValue $ Int32Value $ vs V.! 0
dToP (D.I64Value _ vs) = SignedValue $ Int64Value $ vs V.! 0
dToP (D.U8Value _ vs) = UnsignedValue $ Int8Value $ fromIntegral $ vs V.! 0
dToP (D.U16Value _ vs) = UnsignedValue $ Int16Value $ fromIntegral $ vs V.! 0
dToP (D.U32Value _ vs) = UnsignedValue $ Int32Value $ fromIntegral $ vs V.! 0
dToP (D.U64Value _ vs) = UnsignedValue $ Int64Value $ fromIntegral $ vs V.! 0
dToP (D.F16Value _ vs) = FloatValue $ Float16Value $ vs V.! 0
dToP (D.F32Value _ vs) = FloatValue $ Float32Value $ vs V.! 0
dToP (D.F64Value _ vs) = FloatValue $ Float64Value $ vs V.! 0
dToP (D.BoolValue _ vs) = BoolValue $ vs V.! 0

newtype ValueRef = ValueRef (IORef S.VarName)

mkValueRef :: (MonadIO m, MonadReader Server m) => S.VarName -> m ValueRef
mkValueRef n = do
  r <- liftIO $ newIORef n
  q <- asks queue
  _ <- liftIO $ mkWeakIORef r $ AL.prepend n q
  pure $ ValueRef r

varName :: ValueRef -> IO S.VarName
varName (ValueRef r) = readIORef r

data Server = Server
  { server :: S.Server,
    queue :: AL.AtomicList S.VarName
  }

newtype ServerM a = ServerM (ReaderT Server (ExceptT String IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError String
    )

startServer :: S.ServerCfg -> IO Server
startServer cfg = Server <$> S.startServer cfg <*> AL.new

runServerM :: Server -> ServerM a -> IO (Either String a)
runServerM s (ServerM m) = runExceptT $ runReaderT m s

gc :: ServerM ()
gc = ServerM $ do
  s <- asks server
  vns <- asks queue >>= liftIO . AL.flush
  liftIO (S.cmdFree s vns) >>= throwServerJust ("cmdFree failed on variables " ++ csList (map T.unpack vns) ++ ".")

call :: Name -> [ValueRef] -> ServerM ValueRef
call fn ps = ServerM $ do
  s <- asks server
  nps <- liftIO $ mapM varName ps
  ndst <- liftIO uniqueName
  _ <- liftIO (S.cmdCall s (nameToText fn) ndst nps) >>= throwServerLeft ("cmdCall failed on function " ++ nameToString fn ++ " with parameters " ++ csList (map T.unpack nps) ++ ".")
  mkValueRef ndst

uniqueName :: IO S.VarName
uniqueName = ("v" <>) . T.show . hashUnique <$> newUnique

-- Interrogation
inputs :: Name -> ServerM [S.TypeName]
inputs fn = ServerM $ do
  s <- asks server
  map S.inputType <$> (liftIO (S.cmdInputs s $ nameToText fn) >>= throwServerLeft ("cmdInputs failed on function " ++ nameToString fn ++ "."))

output :: Name -> ServerM S.TypeName
output fn = ServerM $ do
  s <- asks server
  S.outputType <$> (liftIO (S.cmdOutput s $ nameToText fn) >>= throwServerLeft ("cmdOutput failed on function " ++ nameToString fn ++ "."))

kind :: S.TypeName -> ServerM S.Kind
kind tn = ServerM $ do
  s <- asks server
  liftIO (S.cmdKind s tn) >>= throwServerLeft ("cmdKind failed on type " ++ T.unpack tn ++ ".")

vtype :: ValueRef -> ServerM S.TypeName
vtype vr = ServerM $ do
  s <- asks server
  vn <- liftIO $ varName vr
  liftIO (S.cmdType s vn) >>= throwServerLeft ("cmdType failed on variable " ++ T.unpack vn ++ ".")

-- Primitives
getPrim :: ValueRef -> ServerM PrimValue
getPrim vr = ServerM $ do
  s <- asks server
  nsrc <- liftIO $ varName vr
  v <- liftIO (S.getValue s nsrc) >>= throwLeft ("Failed to get primitive variable " ++ T.unpack nsrc ++ ".")
  pure $ dToP v

putPrim :: PrimValue -> ServerM ValueRef
putPrim p = ServerM $ do
  s <- asks server
  ndst <- liftIO uniqueName
  liftIO (S.putValue s ndst $ pToD p) >>= throwServerJust ("Failed to put primitive " ++ show p ++ ".")
  mkValueRef ndst

-- Arrays
rank :: S.TypeName -> ServerM Int
rank tn = ServerM $ do
  s <- asks server
  liftIO (S.cmdRank s tn) >>= throwServerLeft ("cmdRank failed on type " ++ T.unpack tn ++ ".")

elemType :: S.TypeName -> ServerM S.TypeName
elemType tn = ServerM $ do
  s <- asks server
  liftIO (S.cmdElemtype s tn) >>= throwServerLeft ("cmdElemtype failed on type " ++ T.unpack tn ++ ".")

mkArray :: S.TypeName -> [Int64] -> [ValueRef] -> ServerM ValueRef
mkArray tn dims vs = ServerM $ do
  s <- asks server
  vns <- liftIO $ mapM varName vs
  dst <- liftIO uniqueName
  liftIO (S.cmdNewArray s dst tn (map fromIntegral dims) vns) >>= throwServerJust ("cmdNewArray failed on type " ++ T.unpack tn ++ " with variables " ++ csList (map T.unpack vns) ++ ".")
  mkValueRef dst

shape :: ValueRef -> ServerM [Int64]
shape vr = ServerM $ do
  s <- asks server
  vn <- liftIO $ varName vr
  map fromIntegral <$> (liftIO (S.cmdShape s vn) >>= throwServerLeft ("cmdShape failed on variable " ++ T.unpack vn ++ "."))

index :: [Int64] -> ValueRef -> ServerM ValueRef
index is src = ServerM $ do
  s <- asks server
  nsrc <- liftIO $ varName src
  ndst <- liftIO uniqueName
  liftIO (S.cmdIndex s ndst nsrc $ map fromIntegral is) >>= throwServerJust ("cmdIndex failed on source " ++ T.unpack nsrc ++ ", destination " ++ T.unpack ndst ++ ", and index " ++ show is ++ ".")
  mkValueRef ndst

-- Records
fields :: S.TypeName -> ServerM (M.Map Name S.TypeName)
fields tn = ServerM $ do
  s <- asks server
  fs <- liftIO (S.cmdFields s tn) >>= throwServerLeft ("cmdFields failed on type " ++ T.unpack tn ++ ".")
  pure $ M.fromList $ map (\f -> (nameFromText $ S.fieldName f, S.fieldType f)) fs

mkRecord :: S.TypeName -> M.Map Name ValueRef -> ServerM ValueRef
mkRecord tn vrm = ServerM $ do
  s <- asks server
  fns <- map (nameFromText . S.fieldName) <$> (liftIO (S.cmdFields s tn) >>= throwServerLeft ("cmdFields failed on type " ++ T.unpack tn ++ "."))
  vns <-
    mapM
      ( \fn ->
          throwNothing ("Mising field " ++ nameToString fn ++ " when constructing record of type " ++ T.unpack tn ++ ".") (M.lookup fn vrm)
            >>= liftIO . varName
      )
      fns
  dst <- liftIO uniqueName
  liftIO (S.cmdNew s dst tn vns) >>= throwServerJust ("cmdNew failed on type " ++ T.unpack tn ++ " with variables " ++ csList (map T.unpack vns) ++ ".")
  mkValueRef dst

project :: ValueRef -> Name -> ServerM ValueRef
project src fn = ServerM $ do
  s <- asks server
  nsrc <- liftIO $ varName src
  ndst <- liftIO uniqueName
  liftIO (S.cmdProject s ndst nsrc $ nameToText fn) >>= throwServerJust ("cmdKind failed on source " ++ T.unpack nsrc ++ ", destination " ++ T.unpack ndst ++ ", and field " ++ nameToString fn ++ ".")
  mkValueRef ndst

-- Sums
variants :: S.TypeName -> ServerM (M.Map Name [S.TypeName])
variants tn = ServerM $ do
  s <- asks server
  vs <- liftIO (S.cmdVariants s tn) >>= throwServerLeft ("cmdVariants failed on type " ++ T.unpack tn ++ ".")
  pure $ M.fromList $ map (\v -> (nameFromText $ S.variantName v, S.variantTypes v)) vs

mkSum :: S.TypeName -> Name -> [ValueRef] -> ServerM ValueRef
mkSum tn vn vrs = ServerM $ do
  s <- asks server
  vns <- mapM (liftIO . varName) vrs
  dst <- liftIO uniqueName
  liftIO (S.cmdConstruct s dst tn (nameToText vn) vns) >>= throwServerJust ("cmdConstruct failed on type " ++ T.unpack tn ++ ", variant " ++ nameToString vn ++ " with variables " ++ csList (map T.unpack vns) ++ ".")
  mkValueRef dst

destruct :: ValueRef -> ServerM [ValueRef]
destruct src = do
  vn <- variant src
  tn <- vtype src
  vts <- variants tn >>= throwNothing ("Variant " ++ nameToString vn ++ " is not part of its own sum type, " ++ T.unpack tn ++ ". This should be impossible.") . M.lookup vn
  ServerM $ do
    s <- asks server
    nsrc <- liftIO $ varName src
    ndsts <- liftIO $ mapM (const uniqueName) vts
    liftIO (S.cmdDestruct s nsrc ndsts) >>= throwServerJust ("cmdVariants failed on source " ++ T.unpack nsrc ++ ", destinations " ++ csList (map T.unpack ndsts) ++ ".")
    mapM mkValueRef ndsts

variant :: ValueRef -> ServerM Name
variant src = ServerM $ do
  s <- asks server
  nsrc <- liftIO $ varName src
  vn <- liftIO (S.cmdVariant s nsrc) >>= throwServerLeft ("cmdIndex failed on variable " ++ T.unpack nsrc ++ ".")
  pure $ nameFromText vn

-- Error handling convenience
formatServerError :: String -> S.CmdFailure -> String
formatServerError e f | e == mempty = formatServerError "Server error." f
formatServerError e f = T.unpack $ T.unlines $ T.pack e : "Failure message:" : S.failureMsg f

throwServerLeft :: (MonadError String m) => String -> Either S.CmdFailure a -> m a
throwServerLeft e (Left c) = throwError $ formatServerError e c
throwServerLeft _ (Right v) = pure v

throwServerJust :: (MonadError String m) => String -> Maybe S.CmdFailure -> m ()
throwServerJust e c = throwJust $ formatServerError e <$> c

throwLeft :: (MonadError String m) => String -> Either T.Text a -> m a
throwLeft t (Left e) = throwError $ T.unpack $ T.unlines [T.pack t, e]
throwLeft _ (Right v) = pure v

throwJust :: (MonadError String m) => Maybe String -> m ()
throwJust (Just e) = throwError e
throwJust Nothing = pure ()

throwNothing :: (MonadError String m) => String -> Maybe a -> m a
throwNothing _ (Just v) = pure v
throwNothing e Nothing = throwError e

csList :: [String] -> String
csList = intercalate ","
