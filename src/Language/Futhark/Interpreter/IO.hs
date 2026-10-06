-- | IO operations exposed through the interpreter.
module Language.Futhark.Interpreter.IO
  ( IOOp (..),
    determineIO,
    doIOOp,
    ioRelativeTo,
  )
where

import Codec.BMP qualified as BMP
import Control.Exception (SomeException, displayException, try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T
import Data.Vector.Storable qualified as SVec
import Data.Vector.Storable.ByteString qualified as SVec
import Futhark.Test.Values qualified as V
import Futhark.Util (runProgramWithExitCode)
import Language.Futhark
  ( FloatType (..),
    IntType (..),
    PrimType (..),
    RetTypeBase (..),
    ScalarTypeBase (..),
    Shape (..),
    TypeBase (..),
    ValueType,
    isTupleRecord,
    prettyString,
    toStruct,
  )
import Language.Futhark.Interpreter.FFI.ServerM qualified as FFI
import Language.Futhark.Interpreter.Values
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)

-- | The IO operation to perform. The idea of splitting it out into a type like
-- this is to enforce some kind of auditing.
data IOOp
  = LoadBytes FilePath
  | LoadImg FilePath
  | LoadAudio FilePath
  | -- | One value of each type. More than one value is represented as a tuple.
    LoadValue [V.ValueType] FilePath

load ::
  (m ValueType -> FilePath -> m IOOp) ->
  Maybe (m ValueType -> Value m -> m IOOp)
load c = Just $ \t v -> case asByteString v of
  Nothing -> error "loadbytes: not a string"
  Just v' -> c t $ T.unpack $ T.decodeUtf8 v'

primTypeToValueType :: PrimType -> V.PrimType
primTypeToValueType (Signed Int8) = V.I8
primTypeToValueType (Signed Int16) = V.I16
primTypeToValueType (Signed Int32) = V.I32
primTypeToValueType (Signed Int64) = V.I64
primTypeToValueType (Unsigned Int8) = V.U8
primTypeToValueType (Unsigned Int16) = V.U16
primTypeToValueType (Unsigned Int32) = V.U32
primTypeToValueType (Unsigned Int64) = V.U64
primTypeToValueType (FloatType Float16) = V.F16
primTypeToValueType (FloatType Float32) = V.F32
primTypeToValueType (FloatType Float64) = V.F64
primTypeToValueType Bool = V.Bool

-- | The types of the values in a data file that can be loaded as a value of
-- this type. A tuple corresponds to one value per element.
typeToValueTypes :: ValueType -> Maybe [V.ValueType]
typeToValueTypes t = mapM onValue $ fromMaybe [t] $ isTupleRecord t
  where
    onValue (Scalar (Prim pt)) =
      Just $ V.ValueType [] $ primTypeToValueType pt
    onValue (Array _ (Shape ds) (Prim pt)) =
      Just $ V.ValueType (map fromIntegral ds) $ primTypeToValueType pt
    onValue _ = Nothing

loadResType :: ValueType -> [V.ValueType]
loadResType (Scalar (Arrow _ _ _ _ (RetType _ rt)))
  | Just rt' <- typeToValueTypes $ toStruct rt =
      rt'
loadResType t =
  error $ "loadResType: invalid type " <> prettyString t

-- | Determine which IO operation this is.
--
-- If you want to add a new one, then remember to also add it as an intrinsic in
-- the type checker, and probably also to the prelude.
determineIO :: (Monad m) => T.Text -> Maybe (m ValueType -> Value m -> m IOOp)
determineIO "io_loadbytes" = load $ const $ pure . LoadBytes
determineIO "io_loadimg" = load $ const $ pure . LoadImg
determineIO "io_loadaudio" = load $ const $ pure . LoadAudio
determineIO "io_loadvalue" = load $ \t fname -> do
  t' <- t
  pure $ LoadValue (loadResType t') fname
determineIO _ = Nothing
{-# NOINLINE determineIO #-}

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "futhark"

system ::
  FilePath ->
  [String] ->
  T.Text ->
  IO T.Text
system prog options input = do
  res <- runProgramWithExitCode prog options $ T.encodeUtf8 input
  case res of
    Left err ->
      fail $ prog' <> " failed: " <> show err
    Right (ExitSuccess, stdout_t, _) ->
      pure $ T.pack stdout_t
    Right (ExitFailure code', _, stderr_t) ->
      fail $
        prog'
          <> " failed with exit code "
          <> show code'
          <> " and stderr:\n"
          <> stderr_t
  where
    prog' = "\"" <> prog <> "\""

loadBMP :: FilePath -> IO V.Value
loadBMP bmpfile = do
  res <- BMP.readBMP bmpfile
  case res of
    Left err ->
      fail $ "Failed to read BMP:\n" <> show err
    Right bmp -> do
      let bmp_bs = BMP.unpackBMPToRGBA32 bmp
          (w, h) = BMP.bmpDimensions bmp
          shape = SVec.fromList [fromIntegral h, fromIntegral w]
          pix l =
            let (i, j) = l `divMod` w
                l' = (h - 1 - i) * w + j
                r = fromIntegral $ bmp_bs `BS.index` (l' * 4)
                g = fromIntegral $ bmp_bs `BS.index` (l' * 4 + 1)
                b = fromIntegral $ bmp_bs `BS.index` (l' * 4 + 2)
                a = fromIntegral $ bmp_bs `BS.index` (l' * 4 + 3)
             in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
      pure $ V.U32Value shape $ SVec.generate (w * h) pix

loadImage :: FilePath -> IO V.Value
loadImage imgfile =
  withTempDir $ \dir -> do
    let bmpfile = dir </> takeBaseName imgfile `replaceExtension` "bmp"
    void $ system "convert" [imgfile, "-type", "TrueColorAlpha", bmpfile] mempty
    loadBMP bmpfile

loadPCM :: Int -> FilePath -> IO V.Value
loadPCM num_channels pcmfile = do
  contents <- LBS.readFile pcmfile
  let v = SVec.byteStringToVector $ LBS.toStrict contents
      channel_length = SVec.length v `div` num_channels
      shape =
        SVec.fromList
          [ fromIntegral num_channels,
            fromIntegral channel_length
          ]
      -- ffmpeg outputs audio data in column-major format. `backPermuter` computes the
      -- tranposed indexes for a backpermutation.
      backPermuter i = (i `mod` channel_length) * num_channels + i `div` channel_length
      perm = SVec.generate (SVec.length v) backPermuter
  pure $ V.F64Value shape $ SVec.backpermute v perm

loadAudio :: FilePath -> IO V.Value
loadAudio audiofile = do
  s <- system "ffprobe" [audiofile, "-show_entries", "stream=channels", "-select_streams", "a", "-of", "compact=p=0:nk=1", "-v", "0"] mempty
  case T.decimal s of
    Right (num_channels, _) -> do
      withTempDir $ \dir -> do
        let pcmfile = dir </> takeBaseName audiofile `replaceExtension` "pcm"
        void $ system "ffmpeg" ["-i", audiofile, "-c:a", "pcm_f64le", "-map", "0", "-f", "data", pcmfile] mempty
        loadPCM num_channels pcmfile
    _ -> fail "$loadImg failed to detect the number of channels in the audio input"

tryIO :: (MonadIO m) => IO a -> m (Either T.Text a)
tryIO =
  either
    ( pure
        . Left
        . T.pack
        . (displayException :: SomeException -> String)
    )
    (pure . Right)
    <=< liftIO . try

loadValues :: FilePath -> IO [V.Value]
loadValues datafile = do
  contents <- liftIO $ LBS.readFile datafile
  maybe (fail $ "Failed to read data file: " <> datafile) pure $
    V.readValues contents

-- | Resolve relative file paths in the operation relative to the given
-- directory, rather than the current working directory.
ioRelativeTo :: FilePath -> IOOp -> IOOp
ioRelativeTo dir (LoadBytes f) = LoadBytes $ dir </> f
ioRelativeTo dir (LoadImg f) = LoadImg $ dir </> f
ioRelativeTo dir (LoadAudio f) = LoadAudio $ dir </> f
ioRelativeTo dir (LoadValue t f) = LoadValue t $ dir </> f

-- | Turn a loaded value into an interpreter value. Arrays are put on the server,
-- if there is one, as they are very expensive to represent in the interpreter,
-- and are often just passed on to entry points.
fromData :: Maybe FFI.Server -> V.Value -> IO (Value m)
fromData (Just s) v
  | dims@(_ : _) <- V.valueShape v = do
      let shape = foldr (ShapeDim . fromIntegral) ShapeLeaf dims
      either fail (\ref -> pure $ ValueLazyFFI shape ref [])
        =<< FFI.runServerM s (FFI.putData v)
fromData _ v = pure $ fromDataValue v

-- | Run an IO operation. Arrays are put on the server, if one is given.
doIOOp :: Maybe FFI.Server -> IOOp -> IO (Either T.Text (Value m))
doIOOp s (LoadBytes fname) =
  tryIO $ fromData s . V.putValue1 =<< BS.readFile fname
doIOOp s (LoadImg fname) =
  tryIO $ fromData s =<< loadImage fname
doIOOp s (LoadAudio fname) =
  tryIO $ fromData s =<< loadAudio fname
doIOOp s (LoadValue ts fname) = tryIO $ do
  vs <- loadValues fname
  let vs_ts = map V.valueType vs
  when (vs_ts /= ts) . fail $
    "Expected file \""
      <> fname
      <> "\" to contain data of types "
      <> unwords (map prettyString ts)
      <> " but found data of types "
      <> unwords (map prettyString vs_ts)
  asValue <$> mapM (fromData s) vs
  where
    asValue [v] = v
    asValue vs = toTuple vs
{-# NOINLINE doIOOp #-}
