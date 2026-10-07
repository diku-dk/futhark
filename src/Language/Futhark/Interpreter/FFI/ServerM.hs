module Language.Futhark.Interpreter.FFI.ServerM
  ( FS.TypeName,
    ValueRef,
    Server,
    startServer,
    newServer,
    stopServer,
    ServerM,
    runServerM,
    gc,
    release,
    call,
    -- Interrogation
    inputs,
    output,
    kind,
    vtype,
    -- Primitives
    getPrim,
    putPrim,
    putData,
    getData,
    -- Arrays
    rank,
    elemType,
    mkArray,
    shape,
    index,
    -- Records
    fieldOrder,
    mkRecord,
    project,
    unzipArray,
    -- Sums
    variants,
    mkSum,
    destruct,
    variant,
    -- Error handling convenience
    throwNothing,
  )
where

import Control.Exception (catch)
import Control.Monad (replicateM)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Unique (hashUnique, newUnique)
import Data.Vector.Storable qualified as V
import Futhark.Data qualified as D
import Futhark.Server qualified as FS
import Futhark.Server.Values qualified as FS
import Futhark.Util (mapAccumLM)
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

newtype ValueRef = ValueRef (IORef FS.VarName)

data Server = Server
  { server :: FS.Server,
    -- | Variables whose 'ValueRef' has been garbage collected.
    queue :: AL.AtomicList FS.VarName,
    -- | Variables created by us that have not yet been freed.
    live :: IORef (S.Set FS.VarName)
  }

newtype ServerM a = ServerM (ReaderT Server (ExceptT String IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError String,
      MonadIO
    )

askServer :: ServerM FS.Server
askServer = ServerM $ asks server

askQueue :: ServerM (AL.AtomicList FS.VarName)
askQueue = ServerM $ asks queue

modifyLive :: (S.Set FS.VarName -> S.Set FS.VarName) -> ServerM ()
modifyLive f = do
  r <- ServerM $ asks live
  liftIO $ atomicModifyIORef' r $ (,()) . f

startServer :: FS.ServerCfg -> IO Server
startServer cfg = newServer =<< FS.startServer cfg

-- | Use an already-running server. Shutting it down remains the
-- responsibility of whoever started it.
newServer :: FS.Server -> IO Server
newServer s = Server s <$> AL.new <*> newIORef mempty

-- | Shut down the server. Returns a message on termination failure.
stopServer :: Server -> IO (Maybe T.Text)
stopServer s =
  (Nothing <$ FS.stopServer (server s))
    `catch` \(FS.ServerException e) -> pure $ Just e

runServerM :: Server -> ServerM a -> IO (Either String a)
runServerM s (ServerM m) = runExceptT $ runReaderT m s

varName :: ValueRef -> ServerM FS.VarName
varName (ValueRef r) = liftIO $ readIORef r

uniqueName :: ServerM FS.VarName
uniqueName = ("v" <>) . T.show . hashUnique <$> liftIO newUnique

mkValueRef :: FS.VarName -> ServerM ValueRef
mkValueRef n = do
  modifyLive $ S.insert n
  r <- liftIO $ newIORef n
  q <- askQueue
  _ <- liftIO $ mkWeakIORef r $ AL.prepend n q
  pure $ ValueRef r

gc :: ServerM ()
gc = freeVars =<< liftIO . AL.flush =<< askQueue

freeVars :: [FS.VarName] -> ServerM ()
freeVars vns = do
  s <- askServer
  liftIO (FS.cmdFree s vns)
    >>= throwServerJust ("cmdFree failed on variables " ++ csList (map T.unpack vns) ++ ".")
  modifyLive (`S.difference` S.fromList vns)

-- | End the use of this 'Server'. The variables of the given values are
-- adopted by the caller under the given names, and every other variable we
-- have created is freed, whether or not its 'ValueRef' is still reachable.
-- Neither the 'Server' nor any 'ValueRef' may be used afterwards. A variable
-- may occur more than once, in which case it is adopted under the first of its
-- names. Returns the name of the variable of each value.
release :: [(ValueRef, FS.VarName)] -> ServerM [FS.VarName]
release adopted = do
  s <- askServer
  -- Everything in the queue is also live, so it is freed below.
  _ <- askQueue >>= liftIO . AL.flush
  srcs <- mapM (varName . fst) adopted
  let adopt renamed (src, dst)
        | Just dst' <- M.lookup src renamed = pure (renamed, dst')
        | otherwise = do
            liftIO (FS.cmdRename s src dst)
              >>= throwServerJust ("cmdRename failed on variable " ++ T.unpack src ++ ".")
            pure (M.insert src dst renamed, dst)
  (renamed, dsts) <- mapAccumLM adopt mempty $ zip srcs $ map snd adopted
  modifyLive (`S.difference` M.keysSet renamed)
  freeVars . S.toList =<< liftIO . readIORef =<< ServerM (asks live)
  pure dsts

call :: Name -> [ValueRef] -> ServerM ValueRef
call fn ps = do
  s <- askServer
  nps <- mapM varName ps
  ndst <- uniqueName
  -- A failing call is usually the program itself failing (e.g. OOB), so report
  -- just what the server said.
  _ <-
    liftIO (FS.cmdCall s (nameToText fn) ndst nps)
      >>= either (throwError . T.unpack . T.unlines . FS.failureMsg) pure
  mkValueRef ndst

-- Interrogation
inputs :: Name -> ServerM [FS.TypeName]
inputs fn = do
  s <- askServer
  map FS.inputType <$> (liftIO (FS.cmdInputs s $ nameToText fn) >>= throwServerLeft ("cmdInputs failed on function " ++ nameToString fn ++ "."))

output :: Name -> ServerM FS.TypeName
output fn = do
  s <- askServer
  FS.outputType <$> (liftIO (FS.cmdOutput s $ nameToText fn) >>= throwServerLeft ("cmdOutput failed on function " ++ nameToString fn ++ "."))

kind :: FS.TypeName -> ServerM FS.Kind
kind tn = do
  s <- askServer
  liftIO (FS.cmdKind s tn) >>= throwServerLeft ("cmdKind failed on type " ++ T.unpack tn ++ ".")

vtype :: ValueRef -> ServerM FS.TypeName
vtype vr = do
  s <- askServer
  vn <- varName vr
  liftIO (FS.cmdType s vn) >>= throwServerLeft ("cmdType failed on variable " ++ T.unpack vn ++ ".")

-- Primitives
getPrim :: ValueRef -> ServerM PrimValue
getPrim vr = do
  s <- askServer
  nsrc <- varName vr
  v <- liftIO (FS.getValue s nsrc) >>= throwLeft ("Failed to get primitive variable " ++ T.unpack nsrc ++ ".")
  pure $ dToP v

putPrim :: PrimValue -> ServerM ValueRef
putPrim = putData . pToD

-- | Put an entire value on the server at once. This is only possible for
-- values that can be represented in the Futhark data format (primitives and
-- arrays of primitive).
putData :: D.Value -> ServerM ValueRef
putData v = do
  s <- askServer
  ndst <- uniqueName
  liftIO (FS.putValue s ndst v)
    >>= throwServerJust ("Failed to put value of type " ++ T.unpack (D.valueTypeText (D.valueType v)) ++ ".")
  mkValueRef ndst

-- Arrays
rank :: FS.TypeName -> ServerM Int
rank tn = do
  s <- askServer
  liftIO (FS.cmdRank s tn) >>= throwServerLeft ("cmdRank failed on type " ++ T.unpack tn ++ ".")

elemType :: FS.TypeName -> ServerM FS.TypeName
elemType tn = do
  s <- askServer
  liftIO (FS.cmdElemtype s tn) >>= throwServerLeft ("cmdElemtype failed on type " ++ T.unpack tn ++ ".")

mkArray :: FS.TypeName -> [Int64] -> [ValueRef] -> ServerM ValueRef
mkArray tn dims vs = do
  s <- askServer
  vns <- mapM varName vs
  dst <- uniqueName
  liftIO (FS.cmdNewArray s dst tn (map fromIntegral dims) vns) >>= throwServerJust ("cmdNewArray failed on type " ++ T.unpack tn ++ " with variables " ++ csList (map T.unpack vns) ++ ".")
  mkValueRef dst

shape :: ValueRef -> ServerM [Int64]
shape vr = do
  s <- askServer
  vn <- varName vr
  map fromIntegral <$> (liftIO (FS.cmdShape s vn) >>= throwServerLeft ("cmdShape failed on variable " ++ T.unpack vn ++ "."))

-- | Retrieve an entire value from the server at once. This is only possible for
-- values that can be represented in the Futhark data format (primitives and
-- arrays of primitive).
getData :: ValueRef -> ServerM (Maybe D.Value)
getData vr = do
  s <- askServer
  n <- varName vr
  either (const Nothing) Just <$> liftIO (FS.getValue s n)

index :: [Int64] -> ValueRef -> ServerM ValueRef
index is src = do
  s <- askServer
  nsrc <- varName src
  ndst <- uniqueName
  liftIO (FS.cmdIndex s ndst nsrc $ map fromIntegral is) >>= throwServerJust ("cmdIndex failed on source " ++ T.unpack nsrc ++ ", destination " ++ T.unpack ndst ++ ", and index " ++ show is ++ ".")
  mkValueRef ndst

-- Records

-- | The fields of a record type, in the order the server uses.
fieldOrder :: FS.TypeName -> ServerM [(Name, FS.TypeName)]
fieldOrder tn = do
  s <- askServer
  fs <- liftIO (FS.cmdFields s tn) >>= throwServerLeft ("cmdFields failed on type " ++ T.unpack tn ++ ".")
  pure $ map (\f -> (nameFromText $ FS.fieldName f, FS.fieldType f)) fs

-- | Split an array of records into one array per field, in 'fieldOrder'. The
-- fields of an array cannot be projected one element at a time, and doing so
-- would anyway be impossible for an empty array.
unzipArray :: ValueRef -> Int -> ServerM [ValueRef]
unzipArray src n = do
  s <- askServer
  nsrc <- varName src
  ndsts <- replicateM n uniqueName
  liftIO (FS.cmdUnzip s nsrc ndsts)
    >>= throwServerJust ("cmdUnzip failed on variable " ++ T.unpack nsrc ++ ".")
  mapM mkValueRef ndsts

mkRecord :: FS.TypeName -> M.Map Name ValueRef -> ServerM ValueRef
mkRecord tn vrm = do
  s <- askServer
  fns <- map (nameFromText . FS.fieldName) <$> (liftIO (FS.cmdFields s tn) >>= throwServerLeft ("cmdFields failed on type " ++ T.unpack tn ++ "."))
  vns <-
    mapM
      ( \fn ->
          throwNothing ("Mising field " ++ nameToString fn ++ " when constructing record of type " ++ T.unpack tn ++ ".") (M.lookup fn vrm)
            >>= varName
      )
      fns
  dst <- uniqueName
  liftIO (FS.cmdNew s dst tn vns) >>= throwServerJust ("cmdNew failed on type " ++ T.unpack tn ++ " with variables " ++ csList (map T.unpack vns) ++ ".")
  mkValueRef dst

project :: ValueRef -> Name -> ServerM ValueRef
project src fn = do
  s <- askServer
  nsrc <- varName src
  ndst <- uniqueName
  liftIO (FS.cmdProject s ndst nsrc $ nameToText fn)
    >>= throwServerJust ("cmdProject failed on source " ++ T.unpack nsrc ++ ", destination " ++ T.unpack ndst ++ ", and field " ++ nameToString fn ++ ".")
  mkValueRef ndst

-- Sums
variants :: FS.TypeName -> ServerM (M.Map Name [FS.TypeName])
variants tn = do
  s <- askServer
  vs <- liftIO (FS.cmdVariants s tn) >>= throwServerLeft ("cmdVariants failed on type " ++ T.unpack tn ++ ".")
  pure $ M.fromList $ map (\v -> (nameFromText $ FS.variantName v, FS.variantTypes v)) vs

mkSum :: FS.TypeName -> Name -> [ValueRef] -> ServerM ValueRef
mkSum tn vn vrs = do
  s <- askServer
  vns <- mapM varName vrs
  dst <- uniqueName
  liftIO (FS.cmdConstruct s dst tn (nameToText vn) vns)
    >>= throwServerJust ("cmdConstruct failed on type " ++ T.unpack tn ++ ", variant " ++ nameToString vn ++ " with variables " ++ csList (map T.unpack vns) ++ ".")
  mkValueRef dst

destruct :: ValueRef -> ServerM [ValueRef]
destruct src = do
  vn <- variant src
  tn <- vtype src
  vts <- variants tn >>= throwNothing ("Variant " ++ nameToString vn ++ " is not part of its own sum type, " ++ T.unpack tn ++ ". This should be impossible.") . M.lookup vn
  do
    s <- askServer
    nsrc <- varName src
    ndsts <- mapM (const uniqueName) vts
    liftIO (FS.cmdDestruct s nsrc ndsts)
      >>= throwServerJust ("cmdVariants failed on source " ++ T.unpack nsrc ++ ", destinations " ++ csList (map T.unpack ndsts) ++ ".")
    mapM mkValueRef ndsts

variant :: ValueRef -> ServerM Name
variant src = do
  s <- askServer
  nsrc <- varName src
  vn <-
    liftIO (FS.cmdVariant s nsrc)
      >>= throwServerLeft ("cmdIndex failed on variable " ++ T.unpack nsrc ++ ".")
  pure $ nameFromText vn

-- Error handling convenience
formatServerError :: String -> FS.CmdFailure -> String
formatServerError e f | e == mempty = formatServerError "Server error." f
formatServerError e f = T.unpack $ T.unlines $ T.pack e : "Failure message:" : FS.failureMsg f

throwServerLeft :: (MonadError String m) => String -> Either FS.CmdFailure a -> m a
throwServerLeft e (Left c) = throwError $ formatServerError e c
throwServerLeft _ (Right v) = pure v

throwServerJust :: (MonadError String m) => String -> Maybe FS.CmdFailure -> m ()
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
