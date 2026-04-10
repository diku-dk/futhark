module Language.Futhark.Interpreter.FFI.Server.Packer
  ( call,
    realize,
    realize',
  )
where

import Control.Arrow (Arrow (second))
import Control.Monad (forM, forM_, replicateM, void, zipWithM, zipWithM_)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (MonadTrans (lift), StateT (runStateT), gets, modify)
import Data.Binary qualified as B
import Data.Binary.Get qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Futhark.Server qualified as S
import Futhark.Test.Values qualified as V
import GHC.IO.Handle (hClose)
import Language.Futhark.Interpreter.FFI (ExValue, ExValueAtom)
import Language.Futhark.Interpreter.FFI.Server (FutharkServerM)
import Language.Futhark.Interpreter.FFI.Server qualified as FS
import Language.Futhark.Interpreter.FFI.Server.Interface (Entry (Entry), ServerInterface (..))
import Language.Futhark.Interpreter.FFI.Server.TypeLayout (TypeLayout (..))
import Language.Futhark.Interpreter.FFI.UIDs
import Language.Futhark.Interpreter.FFI.Util.BiMap qualified as BM
import Language.Futhark.Interpreter.FFI.Util.NDArray qualified as ND
import Language.Futhark.Interpreter.FFI.Values
import System.IO.Temp (withSystemTempFile)
import Prelude hiding (init)

varName :: ValueUID -> S.VarName
varName v = "v" <> T.show (uid v)

getType :: ValueUID -> FutharkServerM TypeUID
getType v = do
  s <- FS.server
  t <- either (error "TODO (rqwy8dauisoj)") id <$> liftIO (S.cmdType s $ varName v)
  si <- FS.interface
  case BM.lookupRight t $ siType si of
    Just tid -> pure tid
    Nothing -> error "TODO (ru938wojisdlcmkzx)"

realize' :: Location -> FutharkServerM ExValue
realize' (Location vid ds) = do
  o <- realize $ Location vid $ reverse ds
  s <- FS.server
  _ <- liftIO $ S.cmdType s (varName o)
  t <- getType o
  si <- FS.interface
  (ovs, oids) <- runPackerT (pack fEx t $ Atom o) si
  ovs''' <- mapM (mapM (pure . ooga2)) [ovs]
  head <$> unload si oids (zip [t] ovs''')

-- | Fully unpacks a single primitive value
realize :: Location -> FutharkServerM ValueUID
realize (Location vid []) = pure vid
realize (Location vid (d : ds)) = do
  c <- FS.getChild vid d
  cvid <- maybe (unpack' d) pure c
  realize $ Location cvid ds
  where
    unpack' :: Direction -> FutharkServerM ValueUID
    unpack' (Index is) = do
      s <- FS.server
      vid' <- FS.getValueUID
      void $ liftIO $ S.cmdIndex s (varName vid') (varName vid) is
      FS.putChild vid d vid'
      pure vid'
    unpack' (Field f) = do
      s <- FS.server
      vid' <- FS.getValueUID
      void $ liftIO $ S.cmdProject s (varName vid') (varName vid) f
      FS.putChild vid d vid'
      pure vid'
    unpack' (VariantValue v i) = do
      s <- FS.server
      t <- FS.typeLayoutOf vid
      let ts = case t of
            Just (TLSum m) -> m M.! v
            _ -> error "TODO (98uroijwdl)"
      vid's <- FS.getValueUIDs $ fromIntegral $ length ts
      void $ liftIO $ S.cmdDestruct s v (map varName vid's)
      zipWithM_ (FS.putChild vid . VariantValue v) [0 ..] vid's
      pure $ vid's !! i

newtype PackerT v m a = PackerT (ReaderT ServerInterface (StateT [v] m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (PackerT v) where
  lift = PackerT . lift . lift

type PackerM v = PackerT v Identity

runPackerT :: (Monad m) => PackerT v m a -> ServerInterface -> m (a, [v])
runPackerT (PackerT m) i = second reverse <$> runStateT (runReaderT m i) mempty

runPackerM :: PackerM v a -> ServerInterface -> (a, [v])
runPackerM m = runIdentity . runPackerT m

addValue :: (Monad m) => v -> PackerT v m Int
addValue v = PackerT $ do
  modify (v :)
  gets $ (+ (-1)) . length

interface :: (Monad m) => PackerT v m ServerInterface
interface = PackerT ask

pack :: (Monad m) => (TypeLayout -> a -> PackerT v m (Value b)) -> TypeUID -> Value a -> PackerT v m (Value b)
pack f tid v = do
  i <- interface
  case M.lookup tid $ siTypeLayout i of
    Just l -> pack' l v
    Nothing -> error "TODO (ru98qwojialskcm)"
  where
    pack' l (Atom a) = f l a
    pack' (TLArray t) (Array a) = Array . ND.fromList (ND.shape a) <$> mapM (pack f t) (ND.elems a)
    pack' (TLRecord fs) (Record m) = do
      ms <- mapM (\(n, t) -> pack f t $ fromMaybe (error "TODO (r983uwiofhjklna,)") $ M.lookup n m) fs
      let m' = M.fromList $ zip (map fst fs) ms
      pure $ Record m'
    pack' (TLSum m) (Sum svn svs) = do
      let ts = fromMaybe (error "TODO (r893uqoijwdln)") $ M.lookup svn m
      svs' <- zipWithM (pack f) ts svs
      pure $ Sum svn svs'
    pack' _ _ = error "TODO: (9r8uqowfijlas)"

fIn :: TypeLayout -> ExValueAtom -> PackerM PrimitiveValue (Value (Either Location Int))
fIn (TLPrimitive _) (Right p) = Atom . Right <$> addValue p
fIn TLOpaque (Left l) = pure $ Atom $ Left l
fIn _ _ = error "TODO (u8roqjiwlfa)"

fEx :: TypeLayout -> ValueUID -> PackerT (ValueUID, PrimitiveType) FutharkServerM (Value (Either ValueUID Int))
fEx (TLPrimitive t) l = Atom . Right <$> addValue (l, t)
fEx TLOpaque l = pure $ Atom $ Left l
fEx (TLArray t) vid = pure $ Atom $ Left vid -- do
-- s <- lift $ FS.server
-- shape <- either (error "TODO (98ueiwe)") id <$> liftIO (S.cmdShape s $ varName vid)
-- ids <- mapM (const <$> lift $ FS.getValueUID) [1..foldl (*) 1 shape]
-- let nd = ND.fromList shape ids
-- ND.mapMWithIndex_ (\i v -> liftIO $ S.cmdIndex s (varName v) (varName vid) i) nd
-- Array <$> mapM ((pack fEx t) . Atom) nd
fEx (TLRecord f) vid = do
  s <- lift FS.server
  vids <- forM f $ \(n, _) -> do
    fvid <- lift FS.getValueUID
    void $ liftIO $ S.cmdProject s (varName fvid) (varName vid) n
    pure fvid
  Record . M.fromList . zip (map fst f) <$> zipWithM (pack fEx) (map snd f) (map Atom vids)
fEx (TLSum m) vid = do
  s <- lift FS.server
  void $ liftIO $ S.cmdType s $ varName vid
  vn <- either (error . ("TODO (uojdqlamk) " ++) . show) id <$> liftIO (S.cmdVariant s (varName vid))
  let ts = fromJust $ M.lookup vn m
  vids <- forM ts $ const $ lift FS.getValueUID
  void $ liftIO $ S.cmdDestruct s (varName vid) $ map varName vids
  Sum vn <$> zipWithM (pack fEx) ts (map Atom vids)

packAll :: (Monad m) => (TypeLayout -> a -> PackerT v m (Value b)) -> [(TypeUID, Value a)] -> PackerT v m [Value b]
packAll f vs = forM vs $ uncurry $ pack f

load :: (S.Server, ServerInterface) -> [PrimitiveValue] -> [(TypeUID, Value (Either ValueUID Int))] -> FutharkServerM [ValueUID]
load (s, i) ps vs = do
  vids <- replicateM (length ps) FS.getValueUID
  liftIO $ withSystemTempFile "futhark-call-load" $ \tmpf tmpf_h -> do
    forM_ ps $ BL.hPutStr tmpf_h . encodePrimitive
    hClose tmpf_h
    void $ S.cmdRestore s tmpf $ zip (map varName vids) (map (T.pack . primitiveTypeName . primitiveType) ps)
  forM (map (\(tid, v) -> (tid, look tid, v)) vs) (load' vids)
  where
    encodePrimitive :: PrimitiveValue -> BL.ByteString
    encodePrimitive (Int8 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Int16 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Int32 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Int64 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (UInt8 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (UInt16 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (UInt32 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (UInt64 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Float16 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Float32 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Float64 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Bool v) = B.encode $ fromJust $ V.putValue v

    load' :: [ValueUID] -> (TypeUID, TypeLayout, Value (Either ValueUID Int)) -> FutharkServerM ValueUID
    load' _ (_, _, Atom (Left vid)) = pure vid
    load' vids (_, _, Atom (Right idx)) = pure $ vids !! idx
    load' vids (tid, TLArray t, Array a) = do
      values <- mapM (load' vids . (t,look t,)) $ ND.elems a
      o <- FS.getValueUID
      void $ liftIO $ S.cmdNewArray s (varName o) (fromJust $ BM.lookupLeft tid $ siType i) (ND.shape a) $ map varName values
      pure o
    load' vids (tid, TLRecord r, Record m) = do
      k <- forM r $ load' vids . \(n, t) -> (t, look t, fromJust $ M.lookup n m)
      o <- FS.getValueUID
      void $ liftIO $ S.cmdNew s (varName o) (fromJust $ BM.lookupLeft tid $ siType i) $ map varName k
      pure o
    load' vids (tid, TLSum m, Sum vn vvs) = do
      k <- forM (zip (fromJust $ M.lookup vn m) vvs) $ load' vids . \(t, v) -> (t, look t, v)
      o <- FS.getValueUID
      void $ liftIO $ S.cmdConstruct s (varName o) (fromJust $ BM.lookupLeft tid $ siType i) vn $ map varName k
      pure o
    load' _ _ = error "TODO (y8euiqdhjkanx)"

    look :: TypeUID -> TypeLayout
    look tid = fromJust $ M.lookup tid $ siTypeLayout i

unload :: ServerInterface -> [(ValueUID, PrimitiveType)] -> [(TypeUID, Value (Either Location Int))] -> FutharkServerM [ExValue]
unload i vs k = do
  s <- FS.server
  liftIO $ withSystemTempFile "futhark-call-unload" $ \tmpf tmpf_h -> do
    hClose tmpf_h
    void $ S.cmdStore s tmpf $ map (varName . fst) vs
    bs <- BL.readFile tmpf
    let vs' = case B.runGetOrFail (mapM (getPrimitive . snd) vs) bs of
          Left v -> error $ "TODO (u89riqojkms) " ++ show v
          Right (_, _, v) -> v
    pure $ map (\(tid, v) -> unload' vs' (tid, look tid, v)) k
  where
    getPrimitive :: PrimitiveType -> B.Get PrimitiveValue
    getPrimitive TInt8 = Int8 . fromJust . V.getValue <$> B.get
    getPrimitive TInt16 = Int16 . fromJust . V.getValue <$> B.get
    getPrimitive TInt32 = Int32 . fromJust . V.getValue <$> B.get
    getPrimitive TInt64 = Int64 . fromJust . V.getValue <$> B.get
    getPrimitive TUInt8 = UInt8 . fromJust . V.getValue <$> B.get
    getPrimitive TUInt16 = UInt16 . fromJust . V.getValue <$> B.get
    getPrimitive TUInt32 = UInt32 . fromJust . V.getValue <$> B.get
    getPrimitive TUInt64 = UInt64 . fromJust . V.getValue <$> B.get
    getPrimitive TFloat16 = Float16 . fromJust . V.getValue <$> B.get
    getPrimitive TFloat32 = Float32 . fromJust . V.getValue <$> B.get
    getPrimitive TFloat64 = Float64 . fromJust . V.getValue <$> B.get
    getPrimitive TBool = Bool . fromJust . V.getValue <$> B.get

    look :: TypeUID -> TypeLayout
    look tid = fromJust $ M.lookup tid $ siTypeLayout i

    unload' :: [PrimitiveValue] -> (TypeUID, TypeLayout, Value (Either Location Int)) -> ExValue
    unload' pvs (_, TLPrimitive _, Atom (Right idx)) = Atom $ Right $ pvs !! idx
    unload' _ (_, _, Atom (Left vid)) = Atom $ Left vid
    unload' pvs (_, TLArray t, Array nd) = Array $ fmap (unload' pvs . (t,look t,)) nd
    unload' pvs (_, TLRecord f, Record m) =
      Record $ M.fromList $ zip (map fst f) $ map (\(n, t) -> unload' pvs (t, look t, fromJust $ M.lookup n m)) f
    unload' pvs (_, TLSum m, Sum vn vvs) =
      Sum vn $ zipWith (\t v -> unload' pvs (t, look t, v)) (fromJust $ M.lookup vn m) vvs
    unload' _ _ = error "TODO (u8rqowijdalkcm)"

ooga :: Either Location Int -> FutharkServerM (Either ValueUID Int)
ooga (Left l) = Left <$> realize l
ooga (Right i) = pure $ Right i

ooga2 :: Either ValueUID Int -> Either Location Int
ooga2 (Left l) = Left $ Location l []
ooga2 (Right i) = Right i

call :: S.EntryName -> [ExValue] -> FutharkServerM ExValue
call n vs = do
  s <- FS.server
  si <- FS.interface

  -- Get entry info
  (Entry i o) <- getEntryPointID n >>= getEntryPoint

  -- Send inputs
  let (ivs, ps) = runPackerM (packAll fIn $ zip i vs) si
  ivs''' <- mapM (mapM ooga) ivs
  ivs' <- load (s, si) ps $ zip i ivs'''

  -- Call
  o' <- replicateM (length o) FS.getValueUID
  let o'' = map varName o'
  void $ liftIO $ S.cmdCall s n o'' $ map varName ivs'

  -- Get outputs
  (ovs, oids) <- runPackerT (packAll fEx $ zip o $ map Atom o') si
  ovs''' <- mapM (mapM (pure . ooga2)) ovs
  tuple' <$> unload si oids (zip o ovs''')
  where
    getEntryPointID n' = do
      si <- FS.interface
      case BM.lookupRight n' $ siEntryPoint si of
        Just eid -> pure eid
        Nothing -> error $ "Entry point \"" ++ T.unpack n' ++ "\" not found"

    getEntryPoint eid = do
      si <- FS.interface
      case M.lookup eid $ siEntryPointInfo si of
        Just e -> pure e
        Nothing -> error "Impossible (3urq8wfijoalskm)" -- TODO
    tuple' :: [ExValue] -> ExValue
    tuple' [v] = v
    tuple' vs'' = toTuple vs''

-- TODO
-- realize :: Location -> FutharkServerM ExValue
-- realize vid = do
--  st <- FS.state
--  si <- FS.interface
--  let t = st M.! vid
--  (ov, oids) <- runPackerT (pack fEx t $ Atom vid) si
--  head <$> unload si oids [(t, ov)]
