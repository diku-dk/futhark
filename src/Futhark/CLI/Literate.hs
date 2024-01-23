-- | @futhark literate@
--
-- Also contains various utility definitions used by "Futhark.CLI.Script".
module Futhark.CLI.Literate
  ( main,
    Options (..),
    initialOptions,
    scriptCommandLineOptions,
    prepareServer,
  )
where

import Codec.BMP qualified as BMP
import Control.Monad
import Control.Monad.Except
import Control.Monad.State hiding (State)
import Data.Bifunctor (first, second)
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (foldl', transpose)
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Vector.Storable qualified as SVec
import Data.Vector.Storable.ByteString qualified as SVec
import Data.Void
import Data.Word (Word32, Word8)
import Futhark.Data
import Futhark.Script
import Futhark.Server
import Futhark.Test
import Futhark.Test.Values
import Futhark.Util
  ( directoryContents,
    fancyTerminal,
    hashText,
    nubOrd,
    runProgramWithExitCode,
    showText,
  )
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText, prettyTextOneLine)
import Futhark.Util.Pretty qualified as PP
import Futhark.Util.ProgressBar
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    getCurrentDirectory,
    removePathForcibly,
    setCurrentDirectory,
  )
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Text.Megaparsec hiding (State, failure, token)
import Text.Megaparsec.Char
import Text.Printf

newtype ImgParams = ImgParams
  { imgFile :: Maybe FilePath
  }
  deriving (Show)

defaultImgParams :: ImgParams
defaultImgParams =
  ImgParams {imgFile = Nothing}

data VideoParams = VideoParams
  { videoFPS :: Maybe Int,
    videoLoop :: Maybe Bool,
    videoAutoplay :: Maybe Bool,
    videoFormat :: Maybe T.Text,
    videoFile :: Maybe FilePath
  }
  deriving (Show)

defaultVideoParams :: VideoParams
defaultVideoParams =
  VideoParams
    { videoFPS = Nothing,
      videoLoop = Nothing,
      videoAutoplay = Nothing,
      videoFormat = Nothing,
      videoFile = Nothing
    }

data AudioParams = AudioParams
  { audioSamplingFrequency :: Maybe Int,
    audioCodec :: Maybe T.Text
  }
  deriving (Show)

defaultAudioParams :: AudioParams
defaultAudioParams =
  AudioParams
    { audioSamplingFrequency = Nothing,
      audioCodec = Nothing
    }

data Directive
  = DirectiveRes Exp
  | DirectiveBrief Directive
  | DirectiveCovert Directive
  | DirectiveImg Exp ImgParams
  | DirectivePlot Exp (Maybe (Int, Int))
  | DirectiveGnuplot Exp T.Text
  | DirectiveVideo Exp VideoParams
  | DirectiveAudio Exp AudioParams
  deriving (Show)

varsInDirective :: Directive -> S.Set EntryName
varsInDirective (DirectiveRes e) = varsInExp e
varsInDirective (DirectiveBrief d) = varsInDirective d
varsInDirective (DirectiveCovert d) = varsInDirective d
varsInDirective (DirectiveImg e _) = varsInExp e
varsInDirective (DirectivePlot e _) = varsInExp e
varsInDirective (DirectiveGnuplot e _) = varsInExp e
varsInDirective (DirectiveVideo e _) = varsInExp e
varsInDirective (DirectiveAudio e _) = varsInExp e

pprDirective :: Bool -> Directive -> PP.Doc a
pprDirective _ (DirectiveRes e) =
  "> " <> PP.align (PP.pretty e)
pprDirective _ (DirectiveBrief f) =
  pprDirective False f
pprDirective _ (DirectiveCovert f) =
  pprDirective False f
pprDirective _ (DirectiveImg e params) =
  ("> :img " <> PP.align (PP.pretty e))
    <> if null params' then mempty else ";" <> PP.hardline <> PP.stack params'
  where
    params' = catMaybes [p "file" imgFile PP.pretty]
    p s f pretty = do
      x <- f params
      Just $ s <> ": " <> pretty x
pprDirective True (DirectivePlot e (Just (h, w))) =
  PP.stack
    [ "> :plot2d " <> PP.pretty e <> ";",
      "size: (" <> PP.pretty w <> "," <> PP.pretty h <> ")"
    ]
pprDirective _ (DirectivePlot e _) =
  "> :plot2d " <> PP.align (PP.pretty e)
pprDirective True (DirectiveGnuplot e script) =
  PP.stack $
    "> :gnuplot " <> PP.align (PP.pretty e) <> ";"
      : map PP.pretty (T.lines script)
pprDirective False (DirectiveGnuplot e _) =
  "> :gnuplot " <> PP.align (PP.pretty e)
pprDirective False (DirectiveVideo e _) =
  "> :video " <> PP.align (PP.pretty e)
pprDirective True (DirectiveVideo e params) =
  ("> :video " <> PP.pretty e)
    <> if null params' then mempty else ";" <> PP.hardline <> PP.stack params'
  where
    params' =
      catMaybes
        [ p "fps" videoFPS PP.pretty,
          p "loop" videoLoop ppBool,
          p "autoplay" videoAutoplay ppBool,
          p "format" videoFormat PP.pretty,
          p "file" videoFile PP.pretty
        ]
    ppBool b = if b then "true" else "false"
    p s f pretty = do
      x <- f params
      Just $ s <> ": " <> pretty x
pprDirective _ (DirectiveAudio e params) =
  ("> :audio " <> PP.pretty e)
    <> if null params' then mempty else ";" <> PP.hardline <> PP.stack params'
  where
    params' =
      catMaybes
        [ p "sampling_frequency" audioSamplingFrequency PP.pretty,
          p "codec" audioCodec PP.pretty
        ]
    p s f pretty = do
      x <- f params
      Just $ s <> ": " <> pretty x

instance PP.Pretty Directive where
  pretty = pprDirective True

data Block
  = BlockCode T.Text
  | BlockComment T.Text
  | BlockDirective Directive T.Text
  deriving (Show)

varsInScripts :: [Block] -> S.Set EntryName
varsInScripts = foldMap varsInBlock
  where
    varsInBlock (BlockDirective d _) = varsInDirective d
    varsInBlock BlockCode {} = mempty
    varsInBlock BlockComment {} = mempty

type Parser = Parsec Void T.Text

postlexeme :: Parser ()
postlexeme = void $ hspace *> optional (try $ eol *> "--" *> postlexeme)

lexeme :: Parser a -> Parser a
lexeme p = p <* postlexeme

token :: T.Text -> Parser ()
token = void . try . lexeme . string

parseInt :: Parser Int
parseInt = lexeme $ read <$> some (satisfy isDigit)

restOfLine :: Parser T.Text
restOfLine = takeWhileP Nothing (/= '\n') <* (void eol <|> eof)

parseBlockComment :: Parser T.Text
parseBlockComment = T.unlines <$> some line
  where
    line = "--" *> optional " " *> restOfLine

parseTestBlock :: Parser T.Text
parseTestBlock =
  T.unlines <$> ((:) <$> header <*> remainder)
  where
    header = "-- ==" <* eol
    remainder = map ("-- " <>) . T.lines <$> parseBlockComment

parseBlockCode :: Parser T.Text
parseBlockCode = T.unlines . noblanks <$> some line
  where
    noblanks = reverse . dropWhile T.null . reverse . dropWhile T.null
    line = try (notFollowedBy "--") *> notFollowedBy eof *> restOfLine

parsePlotParams :: Parser (Maybe (Int, Int))
parsePlotParams =
  optional $
    ";"
      *> hspace
      *> eol
      *> token "-- size:"
      *> token "("
      *> ((,) <$> parseInt <* token "," <*> parseInt)
      <* token ")"

withPredicate :: (a -> Bool) -> String -> Parser a -> Parser a
withPredicate f msg p = do
  r <- lookAhead p
  if f r then p else fail msg

parseFilePath :: Parser FilePath
parseFilePath =
  withPredicate ok "filename must not have directory component" p
  where
    p = T.unpack <$> lexeme (takeWhileP Nothing (not . isSpace))
    ok f = takeFileName f == f

parseImgParams :: Parser ImgParams
parseImgParams =
  fmap (fromMaybe defaultImgParams) $
    optional $
      ";" *> hspace *> eol *> "-- " *> parseParams defaultImgParams
  where
    parseParams params =
      choice
        [ choice
            [pFile params]
            >>= parseParams,
          pure params
        ]
    pFile params = do
      token "file:"
      b <- parseFilePath
      pure params {imgFile = Just b}

parseVideoParams :: Parser VideoParams
parseVideoParams =
  fmap (fromMaybe defaultVideoParams) $
    optional $
      ";" *> hspace *> eol *> "-- " *> parseParams defaultVideoParams
  where
    parseParams params =
      choice
        [ choice
            [pLoop params, pFPS params, pAutoplay params, pFormat params]
            >>= parseParams,
          pure params
        ]
    parseBool = token "true" $> True <|> token "false" $> False
    pLoop params = do
      token "loop:"
      b <- parseBool
      pure params {videoLoop = Just b}
    pFPS params = do
      token "fps:"
      fps <- parseInt
      pure params {videoFPS = Just fps}
    pAutoplay params = do
      token "autoplay:"
      b <- parseBool
      pure params {videoAutoplay = Just b}
    pFormat params = do
      token "format:"
      s <- lexeme $ takeWhileP Nothing (not . isSpace)
      pure params {videoFormat = Just s}

parseAudioParams :: Parser AudioParams
parseAudioParams =
  fmap (fromMaybe defaultAudioParams) $
    optional $
      ";" *> hspace *> eol *> "-- " *> parseParams defaultAudioParams
  where
    parseParams params =
      choice
        [ choice
            [pSamplingFrequency params, pCodec params]
            >>= parseParams,
          pure params
        ]
    pSamplingFrequency params = do
      token "sampling_frequency:"
      hz <- parseInt
      pure params {audioSamplingFrequency = Just hz}
    pCodec params = do
      token "codec:"
      s <- lexeme $ takeWhileP Nothing (not . isSpace)
      pure params {audioCodec = Just s}

atStartOfLine :: Parser ()
atStartOfLine = do
  col <- sourceColumn <$> getSourcePos
  when (col /= pos1) empty

afterExp :: Parser ()
afterExp = choice [atStartOfLine, choice [void eol, eof]]

withParsedSource :: Parser a -> (a -> T.Text -> b) -> Parser b
withParsedSource p f = do
  s <- getInput
  bef <- getOffset
  x <- p
  aft <- getOffset
  pure $ f x $ T.take (aft - bef) s

stripCommentPrefix :: T.Text -> T.Text
stripCommentPrefix = T.unlines . map onLine . T.lines
  where
    onLine s
      | "-- " `T.isPrefixOf` s = T.drop 3 s
      | otherwise = T.drop 2 s

parseBlock :: Parser Block
parseBlock =
  choice
    [ withParsedSource (token "-- >" *> parseDirective) $ \d s ->
        BlockDirective d $ stripCommentPrefix s,
      BlockCode <$> parseTestBlock,
      BlockCode <$> parseBlockCode,
      BlockComment <$> parseBlockComment
    ]
  where
    parseDirective =
      choice
        [ DirectiveRes <$> parseExp postlexeme <* afterExp,
          directiveName "covert"
            $> DirectiveCovert
            <*> parseDirective,
          directiveName "brief"
            $> DirectiveBrief
            <*> parseDirective,
          directiveName "img"
            $> DirectiveImg
            <*> parseExp postlexeme
            <*> parseImgParams
            <* choice [void eol, eof],
          directiveName "plot2d"
            $> DirectivePlot
            <*> parseExp postlexeme
            <*> parsePlotParams
            <* choice [void eol, eof],
          directiveName "gnuplot"
            $> DirectiveGnuplot
            <*> parseExp postlexeme
            <*> (";" *> hspace *> eol *> parseBlockComment),
          (directiveName "video" <|> directiveName "video")
            $> DirectiveVideo
            <*> parseExp postlexeme
            <*> parseVideoParams
            <* eol,
          directiveName "audio"
            $> DirectiveAudio
            <*> parseExp postlexeme
            <*> parseAudioParams
            <* choice [void eol, eof]
        ]
    directiveName s = try $ token (":" <> s)

parseProg :: FilePath -> T.Text -> Either T.Text [Block]
parseProg fname s =
  either (Left . T.pack . errorBundlePretty) Right $
    parse (many parseBlock <* eof) fname s

parseProgFile :: FilePath -> IO [Block]
parseProgFile prog = do
  pres <- parseProg prog <$> T.readFile prog
  case pres of
    Left err -> do
      T.hPutStr stderr err
      exitFailure
    Right script ->
      pure script

-- | The collection of file paths (all inside the image directory)
-- produced during directive execution.
type Files = S.Set FilePath

newtype State = State {stateFiles :: Files}

newtype ScriptM a = ScriptM (ExceptT T.Text (StateT State IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError T.Text,
      MonadFail,
      MonadIO,
      MonadState State
    )

runScriptM :: ScriptM a -> IO (Either T.Text a, Files)
runScriptM (ScriptM m) = second stateFiles <$> runStateT (runExceptT m) s
  where
    s = State mempty

withTempFile :: (FilePath -> ScriptM a) -> ScriptM a
withTempFile f =
  join . liftIO . withSystemTempFile "futhark-literate" $ \tmpf tmpf_h -> do
    hClose tmpf_h
    (res, files) <- runScriptM (f tmpf)
    pure $ do
      modify $ \s -> s {stateFiles = files <> stateFiles s}
      either throwError pure res

withTempDir :: (FilePath -> ScriptM a) -> ScriptM a
withTempDir f =
  join . liftIO . withSystemTempDirectory "futhark-literate" $ \dir -> do
    (res, files) <- runScriptM (f dir)
    pure $ do
      modify $ \s -> s {stateFiles = files <> stateFiles s}
      either throwError pure res

greyFloatToImg ::
  (RealFrac a, SVec.Storable a) =>
  SVec.Vector a ->
  SVec.Vector Word32
greyFloatToImg = SVec.map grey
  where
    grey i =
      let i' = round (i * 255) .&. 0xFF
       in (i' `shiftL` 16) .|. (i' `shiftL` 8) .|. i'

greyByteToImg ::
  (Integral a, SVec.Storable a) =>
  SVec.Vector a ->
  SVec.Vector Word32
greyByteToImg = SVec.map grey
  where
    grey i =
      (fromIntegral i `shiftL` 16) .|. (fromIntegral i `shiftL` 8) .|. fromIntegral i

-- BMPs are RGBA and bottom-up where we assumes images are top-down
-- and ARGB.  We fix this up before encoding the BMP.  This is
-- probably a little slower than it has to be.
vecToBMP :: Int -> Int -> SVec.Vector Word32 -> LBS.ByteString
vecToBMP h w = BMP.renderBMP . BMP.packRGBA32ToBMP24 w h . SVec.vectorToByteString . frobVec
  where
    frobVec vec = SVec.generate (h * w * 4) (pix vec)
    pix vec l =
      let (i, j) = (l `div` 4) `divMod` w
          argb = vec SVec.! ((h - 1 - i) * w + j)
          c = (argb `shiftR` (24 - ((l + 1) `mod` 4) * 8)) .&. 0xFF
       in fromIntegral c :: Word8

valueToBMP :: Value -> Maybe LBS.ByteString
valueToBMP v@(U32Value _ bytes)
  | [h, w] <- valueShape v =
      Just $ vecToBMP h w bytes
valueToBMP v@(I32Value _ bytes)
  | [h, w] <- valueShape v =
      Just $ vecToBMP h w $ SVec.map fromIntegral bytes
valueToBMP v@(F32Value _ bytes)
  | [h, w] <- valueShape v =
      Just $ vecToBMP h w $ greyFloatToImg bytes
valueToBMP v@(U8Value _ bytes)
  | [h, w] <- valueShape v =
      Just $ vecToBMP h w $ greyByteToImg bytes
valueToBMP v@(F64Value _ bytes)
  | [h, w] <- valueShape v =
      Just $ vecToBMP h w $ greyFloatToImg bytes
valueToBMP v@(BoolValue _ bytes)
  | [h, w] <- valueShape v =
      Just $ vecToBMP h w $ greyByteToImg $ SVec.map ((*) 255 . fromEnum) bytes
valueToBMP _ = Nothing

valueToBMPs :: Value -> Maybe [LBS.ByteString]
valueToBMPs = mapM valueToBMP . valueElems

system ::
  (MonadIO m, MonadError T.Text m) =>
  FilePath ->
  [String] ->
  T.Text ->
  m T.Text
system prog options input = do
  res <- liftIO $ runProgramWithExitCode prog options $ T.encodeUtf8 input
  case res of
    Left err ->
      throwError $ prog' <> " failed: " <> showText err
    Right (ExitSuccess, stdout_t, _) ->
      pure $ T.pack stdout_t
    Right (ExitFailure code', _, stderr_t) ->
      throwError $
        prog'
          <> " failed with exit code "
          <> showText code'
          <> " and stderr:\n"
          <> T.pack stderr_t
  where
    prog' = "'" <> T.pack prog <> "'"

formatDataForGnuplot :: [Value] -> T.Text
formatDataForGnuplot = T.unlines . map line . transpose . map valueElems
  where
    line = T.unwords . map prettyText

imgBlock :: FilePath -> T.Text
imgBlock f = "![](" <> T.pack f <> ")\n"

videoBlock :: VideoParams -> FilePath -> T.Text
videoBlock opts f = "![](" <> T.pack f <> ")" <> opts' <> "\n"
  where
    opts'
      | all T.null [loop, autoplay] =
          mempty
      | otherwise =
          "{" <> T.unwords [loop, autoplay] <> "}"
    boolOpt s prop
      | Just b <- prop opts =
          if b then s <> "=\"true\"" else s <> "=\"false\""
      | otherwise =
          mempty
    loop = boolOpt "loop" videoLoop
    autoplay = boolOpt "autoplay" videoAutoplay

plottable :: CompoundValue -> Maybe [Value]
plottable (ValueTuple vs) = do
  (vs', ns') <- mapAndUnzipM inspect vs
  guard $ length (nubOrd ns') == 1
  Just vs'
  where
    inspect (ValueAtom v)
      | [n] <- valueShape v = Just (v, n)
    inspect _ = Nothing
plottable _ = Nothing

withGnuplotData ::
  [(T.Text, T.Text)] ->
  [(T.Text, [Value])] ->
  ([T.Text] -> [T.Text] -> ScriptM a) ->
  ScriptM a
withGnuplotData sets [] cont = uncurry cont $ unzip $ reverse sets
withGnuplotData sets ((f, vs) : xys) cont =
  withTempFile $ \fname -> do
    liftIO $ T.writeFile fname $ formatDataForGnuplot vs
    withGnuplotData ((f, f <> "='" <> T.pack fname <> "'") : sets) xys cont

loadBMP :: FilePath -> ScriptM (Compound Value)
loadBMP bmpfile = do
  res <- liftIO $ BMP.readBMP bmpfile
  case res of
    Left err ->
      throwError $ "Failed to read BMP:\n" <> showText err
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
      pure $ ValueAtom $ U32Value shape $ SVec.generate (w * h) pix

loadImage :: FilePath -> ScriptM (Compound Value)
loadImage imgfile =
  withTempDir $ \dir -> do
    let bmpfile = dir </> takeBaseName imgfile `replaceExtension` "bmp"
    void $ system "convert" [imgfile, "-type", "TrueColorAlpha", bmpfile] mempty
    loadBMP bmpfile

loadPCM :: Int -> FilePath -> ScriptM (Compound Value)
loadPCM num_channels pcmfile = do
  contents <- liftIO $ LBS.readFile pcmfile
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
  pure $ ValueAtom $ F64Value shape $ SVec.backpermute v perm

loadAudio :: FilePath -> ScriptM (Compound Value)
loadAudio audiofile = do
  s <- system "ffprobe" [audiofile, "-show_entries", "stream=channels", "-select_streams", "a", "-of", "compact=p=0:nk=1", "-v", "0"] mempty
  case T.decimal s of
    Right (num_channels, _) -> do
      withTempDir $ \dir -> do
        let pcmfile = dir </> takeBaseName audiofile `replaceExtension` "pcm"
        void $ system "ffmpeg" ["-i", audiofile, "-c:a", "pcm_f64le", "-map", "0", "-f", "data", pcmfile] mempty
        loadPCM num_channels pcmfile
    _ -> throwError "$loadImg failed to detect the number of channels in the audio input"

literateBuiltin :: EvalBuiltin ScriptM
literateBuiltin "loadimg" vs =
  case vs of
    [ValueAtom v]
      | Just path <- getValue v -> do
          let path' = map (chr . fromIntegral) (path :: [Word8])
          loadImage path'
    _ ->
      throwError $
        "$loadimg does not accept arguments of types: "
          <> T.intercalate ", " (map (prettyText . fmap valueType) vs)
literateBuiltin "loadaudio" vs =
  case vs of
    [ValueAtom v]
      | Just path <- getValue v -> do
          let path' = map (chr . fromIntegral) (path :: [Word8])
          loadAudio path'
    _ ->
      throwError $
        "$loadaudio does not accept arguments of types: "
          <> T.intercalate ", " (map (prettyText . fmap valueType) vs)
literateBuiltin f vs =
  scriptBuiltin "." f vs

-- | Some of these only make sense for @futhark literate@, but enough
-- are also sensible for @futhark script@ that we can share them.
data Options = Options
  { scriptBackend :: String,
    scriptFuthark :: Maybe FilePath,
    scriptExtraOptions :: [String],
    scriptCompilerOptions :: [String],
    scriptSkipCompilation :: Bool,
    scriptOutput :: Maybe FilePath,
    scriptVerbose :: Int,
    scriptStopOnError :: Bool
  }

initialOptions :: Options
initialOptions =
  Options
    { scriptBackend = "c",
      scriptFuthark = Nothing,
      scriptExtraOptions = [],
      scriptCompilerOptions = [],
      scriptSkipCompilation = False,
      scriptOutput = Nothing,
      scriptVerbose = 0,
      scriptStopOnError = False
    }

data Env = Env
  { envImgDir :: FilePath,
    envOpts :: Options,
    envServer :: ScriptServer,
    envHash :: T.Text
  }

newFile :: Env -> (Maybe FilePath, FilePath) -> (FilePath -> ScriptM ()) -> ScriptM FilePath
newFile env (fname_desired, template) m = do
  let fname_base = fromMaybe (T.unpack (envHash env) <> "-" <> template) fname_desired
      fname = envImgDir env </> fname_base
  exists <- liftIO $ doesFileExist fname
  liftIO $ createDirectoryIfMissing True $ envImgDir env
  when (exists && scriptVerbose (envOpts env) > 0) $
    liftIO . T.hPutStrLn stderr $
      "Using existing file: " <> T.pack fname
  unless exists $ do
    when (scriptVerbose (envOpts env) > 0) $
      liftIO . T.hPutStrLn stderr $
        "Generating new file: " <> T.pack fname
    m fname
  modify $ \s -> s {stateFiles = S.insert fname $ stateFiles s}
  pure fname

newFileContents :: Env -> (Maybe FilePath, FilePath) -> (FilePath -> ScriptM ()) -> ScriptM T.Text
newFileContents env f m =
  liftIO . T.readFile =<< newFile env f m

processDirective :: Env -> Directive -> ScriptM T.Text
processDirective env (DirectiveBrief d) =
  processDirective env d
processDirective env (DirectiveCovert d) =
  processDirective env d
processDirective env (DirectiveRes e) = do
  result <-
    newFileContents env (Nothing, "eval.txt") $ \resultf -> do
      v <- either nope pure =<< evalExpToGround literateBuiltin (envServer env) e
      liftIO $ T.writeFile resultf $ prettyText v
  pure $ T.unlines ["```", result, "```"]
  where
    nope t =
      throwError $ "Cannot show value of type " <> prettyText t
--
processDirective env (DirectiveImg e params) = do
  fmap imgBlock . newFile env (imgFile params, "img.png") $ \pngfile -> do
    maybe_v <- evalExpToGround literateBuiltin (envServer env) e
    case maybe_v of
      Right (ValueAtom v)
        | Just bmp <- valueToBMP v -> do
            withTempDir $ \dir -> do
              let bmpfile = dir </> "img.bmp"
              liftIO $ LBS.writeFile bmpfile bmp
              void $ system "convert" [bmpfile, pngfile] mempty
      Right v ->
        nope $ fmap valueType v
      Left t ->
        nope t
  where
    nope t =
      throwError $
        "Cannot create image from value of type " <> prettyText t
--
processDirective env (DirectivePlot e size) = do
  fmap imgBlock . newFile env (Nothing, "plot.png") $ \pngfile -> do
    maybe_v <- evalExpToGround literateBuiltin (envServer env) e
    case maybe_v of
      Right v
        | Just vs <- plottable2d v ->
            plotWith [(Nothing, vs)] pngfile
      Right (ValueRecord m)
        | Just m' <- traverse plottable2d m -> do
            plotWith (map (first Just) $ M.toList m') pngfile
      Right v ->
        throwError $ "Cannot plot value of type " <> prettyText (fmap valueType v)
      Left t ->
        throwError $ "Cannot plot opaque value of type " <> prettyText t
  where
    plottable2d v = do
      [x, y] <- plottable v
      Just [x, y]

    tag (Nothing, xys) j = ("data" <> showText (j :: Int), xys)
    tag (Just f, xys) _ = (f, xys)

    plotWith xys pngfile =
      withGnuplotData [] (zipWith tag xys [0 ..]) $ \fs sets -> do
        let size' = T.pack $
              case size of
                Nothing -> "500,500"
                Just (w, h) -> show w ++ "," ++ show h
            plotCmd f title =
              let title' = case title of
                    Nothing -> "notitle"
                    Just x -> "title '" <> x <> "'"
               in f <> " " <> title' <> " with lines"
            cmds = T.intercalate ", " (zipWith plotCmd fs (map fst xys))
            script =
              T.unlines
                [ "set terminal png size " <> size' <> " enhanced",
                  "set output '" <> T.pack pngfile <> "'",
                  "set key outside",
                  T.unlines sets,
                  "plot " <> cmds
                ]
        void $ system "gnuplot" [] script
--
processDirective env (DirectiveGnuplot e script) = do
  fmap imgBlock . newFile env (Nothing, "plot.png") $ \pngfile -> do
    maybe_v <- evalExpToGround literateBuiltin (envServer env) e
    case maybe_v of
      Right (ValueRecord m)
        | Just m' <- traverse plottable m ->
            plotWith (M.toList m') pngfile
      Right v ->
        throwError $ "Cannot plot value of type " <> prettyText (fmap valueType v)
      Left t ->
        throwError $ "Cannot plot opaque value of type " <> prettyText t
  where
    plotWith xys pngfile = withGnuplotData [] xys $ \_ sets -> do
      let script' =
            T.unlines
              [ "set terminal png enhanced",
                "set output '" <> T.pack pngfile <> "'",
                T.unlines sets,
                script
              ]
      void $ system "gnuplot" [] script'
--
processDirective env (DirectiveVideo e params) = do
  unless (format `elem` ["webm", "gif"]) $
    throwError $
      "Unknown video format: " <> format

  let file = (videoFile params, "video" <.> T.unpack format)
  fmap (videoBlock params) . newFile env file $ \videofile -> do
    v <- evalExp literateBuiltin (envServer env) e
    let nope =
          throwError $
            "Cannot produce video from value of type " <> prettyText (fmap scriptValueType v)
    case v of
      ValueAtom SValue {} -> do
        ValueAtom arr <- getExpValue (envServer env) v
        case valueToBMPs arr of
          Nothing -> nope
          Just bmps ->
            withTempDir $ \dir -> do
              zipWithM_ (writeBMPFile dir) [0 ..] bmps
              onWebM videofile =<< bmpsToVideo dir
      ValueTuple [stepfun, initial, num_frames]
        | ValueAtom (SFun stepfun' _ [_, _] closure) <- stepfun,
          ValueAtom (SValue "i64" _) <- num_frames -> do
            Just (ValueAtom num_frames') <-
              mapM getValue <$> getExpValue (envServer env) num_frames
            withTempDir $ \dir -> do
              let num_frames_int = fromIntegral (num_frames' :: Int64)
              renderFrames dir (stepfun', map ValueAtom closure) initial num_frames_int
              onWebM videofile =<< bmpsToVideo dir
      _ ->
        nope
  where
    framerate = fromMaybe 30 $ videoFPS params
    format = fromMaybe "webm" $ videoFormat params
    bmpfile dir j = dir </> printf "frame%010d.bmp" (j :: Int)

    (progressStep, progressDone)
      | fancyTerminal,
        scriptVerbose (envOpts env) > 0 =
          ( \j num_frames -> liftIO $ do
              T.putStr $
                "\r"
                  <> progressBar
                    (ProgressBar 40 (fromIntegral num_frames - 1) (fromIntegral j))
                  <> "generating frame "
                  <> prettyText (j + 1)
                  <> "/"
                  <> prettyText num_frames
                  <> " "
              hFlush stdout,
            liftIO $ T.putStrLn ""
          )
      | otherwise =
          (\_ _ -> pure (), pure ())

    renderFrames dir (stepfun, closure) initial num_frames = do
      foldM_ frame initial [0 .. num_frames - 1]
      progressDone
      where
        frame old_state j = do
          progressStep j num_frames
          v <-
            evalExp literateBuiltin (envServer env)
              . Call (FuncFut stepfun)
              . map valueToExp
              $ closure ++ [old_state]
          freeValue (envServer env) old_state

          let nope =
                throwError $
                  "Cannot handle step function return type: "
                    <> prettyText (fmap scriptValueType v)

          case v of
            ValueTuple [arr_v@(ValueAtom SValue {}), new_state] -> do
              ValueAtom arr <- getExpValue (envServer env) arr_v
              freeValue (envServer env) arr_v
              case valueToBMP arr of
                Nothing -> nope
                Just bmp -> do
                  writeBMPFile dir j bmp
                  pure new_state
            _ -> nope

    writeBMPFile dir j bmp =
      liftIO $ LBS.writeFile (bmpfile dir j) bmp

    bmpsToVideo dir = do
      void $
        system
          "ffmpeg"
          [ "-y",
            "-r",
            show framerate,
            "-i",
            dir </> "frame%010d.bmp",
            "-c:v",
            "libvpx-vp9",
            "-pix_fmt",
            "yuv420p",
            "-b:v",
            "2M",
            dir </> "video.webm"
          ]
          mempty
      pure $ dir </> "video.webm"

    onWebM videofile webmfile
      | format == "gif" =
          void $ system "ffmpeg" ["-i", webmfile, videofile] mempty
      | otherwise =
          liftIO $ copyFile webmfile videofile

--
processDirective env (DirectiveAudio e params) = do
  fmap imgBlock . newFile env (Nothing, "output." <> T.unpack output_format) $
    \audiofile -> do
      withTempDir $ \dir -> do
        maybe_v <- evalExpToGround literateBuiltin (envServer env) e
        maybe_raw_files <- toRawFiles dir maybe_v
        case maybe_raw_files of
          (input_format, raw_files) -> do
            void $
              system
                "ffmpeg"
                ( concatMap
                    ( \raw_file ->
                        [ "-f",
                          input_format,
                          "-ar",
                          show sampling_frequency,
                          "-i",
                          raw_file
                        ]
                    )
                    raw_files
                    ++ [ "-f",
                         T.unpack output_format,
                         "-filter_complex",
                         concatMap
                           (\i -> "[" <> show i <> ":a]")
                           [0 .. length raw_files - 1]
                           <> "amerge=inputs="
                           <> show (length raw_files)
                           <> "[a]",
                         "-map",
                         "[a]",
                         audiofile
                       ]
                )
                mempty
  where
    writeRaw dir name v = do
      let rawfile = dir </> name
      let Just bytes = toBytes v
      liftIO $ LBS.writeFile rawfile $ LBS.fromStrict bytes

    toRawFiles dir (Right (ValueAtom v))
      | length (valueShape v) == 1,
        Just input_format <- toFfmpegFormat v = do
          writeRaw dir "raw.pcm" v
          pure (input_format, [dir </> "raw.pcm"])
      | length (valueShape v) == 2,
        Just input_format <- toFfmpegFormat v = do
          (input_format,)
            <$> zipWithM
              ( \v' i -> do
                  let file_name = "raw-" <> show i <> ".pcm"
                  writeRaw dir file_name v'
                  pure $ dir </> file_name
              )
              (valueElems v)
              [0 :: Int ..]
    toRawFiles _ v = nope $ fmap (fmap valueType) v

    toFfmpegFormat I8Value {} = Just "s8"
    toFfmpegFormat U8Value {} = Just "u8"
    toFfmpegFormat I16Value {} = Just "s16le"
    toFfmpegFormat U16Value {} = Just "u16le"
    toFfmpegFormat I32Value {} = Just "s32le"
    toFfmpegFormat U32Value {} = Just "u32le"
    toFfmpegFormat F32Value {} = Just "f32le"
    toFfmpegFormat F64Value {} = Just "f64le"
    toFfmpegFormat _ = Nothing

    toBytes (I8Value _ bytes) = Just $ SVec.vectorToByteString bytes
    toBytes (U8Value _ bytes) = Just $ SVec.vectorToByteString bytes
    toBytes (I16Value _ bytes) = Just $ SVec.vectorToByteString bytes
    toBytes (U16Value _ bytes) = Just $ SVec.vectorToByteString bytes
    toBytes (I32Value _ bytes) = Just $ SVec.vectorToByteString bytes
    toBytes (U32Value _ bytes) = Just $ SVec.vectorToByteString bytes
    toBytes (F32Value _ bytes) = Just $ SVec.vectorToByteString bytes
    toBytes (F64Value _ bytes) = Just $ SVec.vectorToByteString bytes
    toBytes _ = Nothing

    output_format = fromMaybe "wav" $ audioCodec params
    sampling_frequency = fromMaybe 44100 $ audioSamplingFrequency params
    nope _ = throwError "Cannot create audio from value"

-- Did this script block succeed or fail?
data Failure = Failure | Success
  deriving (Eq, Ord, Show)

processBlock :: Env -> Block -> IO (Failure, T.Text, Files)
processBlock _ (BlockCode code)
  | T.null code = pure (Success, mempty, mempty)
  | otherwise = pure (Success, "```futhark\n" <> code <> "```\n", mempty)
processBlock _ (BlockComment pretty) =
  pure (Success, pretty, mempty)
processBlock env (BlockDirective directive text) = do
  when (scriptVerbose (envOpts env) > 0) $
    T.hPutStrLn stderr . PP.docText $
      "Processing " <> PP.align (PP.pretty directive) <> "..."
  let prompt = case directive of
        DirectiveCovert _ -> mempty
        DirectiveBrief _ ->
          "```\n" <> PP.docText (pprDirective False directive) <> "\n```\n"
        _ ->
          "```\n" <> text <> "```\n"
      env' = env {envHash = hashText (envHash env <> prettyText directive)}
  (r, files) <- runScriptM $ processDirective env' directive
  case r of
    Left err -> failed prompt err files
    Right t -> pure (Success, prompt <> "\n" <> t, files)
  where
    failed prompt err files = do
      let message = prettyTextOneLine directive <> " failed:\n" <> err <> "\n"
      liftIO $ T.hPutStr stderr message
      when (scriptStopOnError (envOpts env)) exitFailure
      pure
        ( Failure,
          T.unlines [prompt, "**FAILED**", "```", err, "```"],
          files
        )

-- Delete all files in the given directory that are not contained in
-- 'files'.
cleanupImgDir :: Env -> Files -> IO ()
cleanupImgDir env keep_files =
  mapM_ toRemove . filter (not . (`S.member` keep_files))
    =<< (directoryContents (envImgDir env) `catchError` onError)
  where
    onError e
      | isDoesNotExistError e = pure []
      | otherwise = throwError e
    toRemove f = do
      when (scriptVerbose (envOpts env) > 0) $
        T.hPutStrLn stderr $
          "Deleting unused file: " <> T.pack f
      removePathForcibly f

processScript :: Env -> [Block] -> IO (Failure, T.Text)
processScript env script = do
  (failures, outputs, files) <-
    unzip3 <$> mapM (processBlock env) script
  cleanupImgDir env $ mconcat files
  pure (foldl' min Success failures, T.intercalate "\n" outputs)

scriptCommandLineOptions :: [FunOptDescr Options]
scriptCommandLineOptions =
  [ Option
      []
      ["backend"]
      ( ReqArg
          (\backend -> Right $ \config -> config {scriptBackend = backend})
          "PROGRAM"
      )
      "The compiler used (defaults to 'c').",
    Option
      []
      ["futhark"]
      ( ReqArg
          (\prog -> Right $ \config -> config {scriptFuthark = Just prog})
          "PROGRAM"
      )
      "The binary used for operations (defaults to same binary as 'futhark script').",
    Option
      "p"
      ["pass-option"]
      ( ReqArg
          ( \opt ->
              Right $ \config ->
                config {scriptExtraOptions = opt : scriptExtraOptions config}
          )
          "OPT"
      )
      "Pass this option to programs being run.",
    Option
      []
      ["pass-compiler-option"]
      ( ReqArg
          ( \opt ->
              Right $ \config ->
                config {scriptCompilerOptions = opt : scriptCompilerOptions config}
          )
          "OPT"
      )
      "Pass this option to the compiler.",
    Option
      []
      ["skip-compilation"]
      (NoArg $ Right $ \config -> config {scriptSkipCompilation = True})
      "Use already compiled program.",
    Option
      "v"
      ["verbose"]
      (NoArg $ Right $ \config -> config {scriptVerbose = scriptVerbose config + 1})
      "Enable logging. Pass multiple times for more."
  ]

commandLineOptions :: [FunOptDescr Options]
commandLineOptions =
  scriptCommandLineOptions
    <> [ Option
           "o"
           ["output"]
           (ReqArg (\opt -> Right $ \config -> config {scriptOutput = Just opt}) "FILE")
           "Override output file. Image directory is set to basename appended with -img/.",
         Option
           []
           ["stop-on-error"]
           (NoArg $ Right $ \config -> config {scriptStopOnError = True})
           "Stop and do not produce output file if any directive fails."
       ]

prepareServer :: FilePath -> Options -> (ScriptServer -> IO a) -> IO a
prepareServer prog opts f = do
  futhark <- maybe getExecutablePath pure $ scriptFuthark opts

  let is_fut = takeExtension prog == ".fut"

  unless (scriptSkipCompilation opts || not is_fut) $ do
    let compile_options = "--server" : scriptCompilerOptions opts
    when (scriptVerbose opts > 0) $
      T.hPutStrLn stderr $
        "Compiling " <> T.pack prog <> "..."
    when (scriptVerbose opts > 1) $
      T.hPutStrLn stderr $
        T.pack $
          unwords compile_options

    let onError err = do
          mapM_ (T.hPutStrLn stderr) err
          exitFailure

    void $
      either onError pure <=< runExceptT $
        compileProgram compile_options (FutharkExe futhark) (scriptBackend opts) prog

  let run_options = scriptExtraOptions opts
      onLine "call" l = T.putStrLn l
      onLine _ _ = pure ()
      prog' = if is_fut then dropExtension prog else prog
      cfg =
        (futharkServerCfg ("." </> prog') run_options)
          { cfgOnLine =
              if scriptVerbose opts > 0
                then onLine
                else const . const $ pure ()
          }

  withScriptServer cfg f

-- | Run @futhark literate@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialOptions commandLineOptions "program" $ \args opts ->
  case args of
    [prog] -> Just $ do
      futhark <- maybe getExecutablePath pure $ scriptFuthark opts
      let onError err = do
            T.hPutStrLn stderr err
            exitFailure
      proghash <-
        either onError pure <=< runExceptT $
          system futhark ["hash", prog] mempty
      script <- parseProgFile prog

      orig_dir <- getCurrentDirectory
      let entryOpt v = "--entry-point=" ++ T.unpack v
          opts' =
            opts
              { scriptCompilerOptions =
                  map entryOpt (S.toList (varsInScripts script))
                    <> scriptCompilerOptions opts
              }
      prepareServer prog opts' $ \server -> do
        let mdfile = fromMaybe (prog `replaceExtension` "md") $ scriptOutput opts
            prog_dir = takeDirectory prog
            imgdir = dropExtension (takeFileName mdfile) <> "-img"
            env =
              Env
                { envServer = server,
                  envOpts = opts,
                  envHash = proghash,
                  envImgDir = imgdir
                }

        when (scriptVerbose opts > 0) $ do
          T.hPutStrLn stderr $ "Executing from " <> T.pack prog_dir
        setCurrentDirectory prog_dir

        (failure, md) <- processScript env script
        T.writeFile (orig_dir </> mdfile) md
        when (failure == Failure) exitFailure
    _ -> Nothing
