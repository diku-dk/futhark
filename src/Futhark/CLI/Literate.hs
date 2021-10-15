{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @futhark literate@
module Futhark.CLI.Literate (main) where

import qualified Codec.BMP as BMP
import Control.Monad.Except
import Control.Monad.State hiding (State)
import Data.Bifunctor (first, second)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Functor
import Data.Int (Int64)
import Data.List (foldl', transpose)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as SVec
import qualified Data.Vector.Storable.ByteString as SVec
import Data.Void
import Data.Word (Word32, Word8)
import Futhark.Data
import Futhark.Script
import Futhark.Server
import Futhark.Test
import Futhark.Test.Values
import Futhark.Util
  ( directoryContents,
    hashText,
    nubOrd,
    runProgramWithExitCode,
  )
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText, prettyTextOneLine)
import qualified Futhark.Util.Pretty as PP
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    removePathForcibly,
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

data VideoParams = VideoParams
  { videoFPS :: Maybe Int,
    videoLoop :: Maybe Bool,
    videoAutoplay :: Maybe Bool,
    videoFormat :: Maybe T.Text
  }
  deriving (Show)

defaultVideoParams :: VideoParams
defaultVideoParams =
  VideoParams
    { videoFPS = Nothing,
      videoLoop = Nothing,
      videoAutoplay = Nothing,
      videoFormat = Nothing
    }

data Directive
  = DirectiveRes Exp
  | DirectiveBrief Directive
  | DirectiveCovert Directive
  | DirectiveImg Exp
  | DirectivePlot Exp (Maybe (Int, Int))
  | DirectiveGnuplot Exp T.Text
  | DirectiveVideo Exp VideoParams
  deriving (Show)

varsInDirective :: Directive -> S.Set EntryName
varsInDirective (DirectiveRes e) = varsInExp e
varsInDirective (DirectiveBrief d) = varsInDirective d
varsInDirective (DirectiveCovert d) = varsInDirective d
varsInDirective (DirectiveImg e) = varsInExp e
varsInDirective (DirectivePlot e _) = varsInExp e
varsInDirective (DirectiveGnuplot e _) = varsInExp e
varsInDirective (DirectiveVideo e _) = varsInExp e

pprDirective :: Bool -> Directive -> PP.Doc
pprDirective _ (DirectiveRes e) =
  "> " <> PP.align (PP.ppr e)
pprDirective _ (DirectiveBrief f) =
  pprDirective False f
pprDirective _ (DirectiveCovert f) =
  pprDirective False f
pprDirective _ (DirectiveImg e) =
  "> :img " <> PP.align (PP.ppr e)
pprDirective True (DirectivePlot e (Just (h, w))) =
  PP.stack
    [ "> :plot2d " <> PP.ppr e <> ";",
      "size: (" <> PP.ppr w <> "," <> PP.ppr h <> ")"
    ]
pprDirective _ (DirectivePlot e _) =
  "> :plot2d " <> PP.align (PP.ppr e)
pprDirective True (DirectiveGnuplot e script) =
  PP.stack $
    "> :gnuplot " <> PP.align (PP.ppr e) <> ";" :
    map PP.strictText (T.lines script)
pprDirective False (DirectiveGnuplot e _) =
  "> :gnuplot " <> PP.align (PP.ppr e)
pprDirective False (DirectiveVideo e _) =
  "> :video " <> PP.align (PP.ppr e)
pprDirective True (DirectiveVideo e params) =
  "> :video " <> PP.ppr e
    <> if null params' then mempty else PP.stack $ ";" : params'
  where
    params' =
      catMaybes
        [ p "fps" videoFPS PP.ppr,
          p "loop" videoLoop ppBool,
          p "autoplay" videoAutoplay ppBool,
          p "format" videoFormat PP.strictText
        ]
    ppBool b = if b then "true" else "false"
    p s f ppr = do
      x <- f params
      Just $ s <> ": " <> ppr x

instance PP.Pretty Directive where
  ppr = pprDirective True

data Block
  = BlockCode T.Text
  | BlockComment T.Text
  | BlockDirective Directive
  deriving (Show)

varsInScripts :: [Block] -> S.Set EntryName
varsInScripts = foldMap varsInBlock
  where
    varsInBlock (BlockDirective d) = varsInDirective d
    varsInBlock BlockCode {} = mempty
    varsInBlock BlockComment {} = mempty

type Parser = Parsec Void T.Text

postlexeme :: Parser ()
postlexeme = void $ hspace *> optional (try $ eol *> "-- " *> postlexeme)

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
    ";" *> hspace *> eol *> token "-- size:"
      *> token "("
      *> ((,) <$> parseInt <* token "," <*> parseInt) <* token ")"

parseVideoParams :: Parser VideoParams
parseVideoParams =
  fmap (fromMaybe defaultVideoParams) $
    optional $ ";" *> hspace *> eol *> "-- " *> parseParams defaultVideoParams
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

atStartOfLine :: Parser ()
atStartOfLine = do
  col <- sourceColumn <$> getSourcePos
  when (col /= pos1) empty

afterExp :: Parser ()
afterExp = choice [atStartOfLine, void eol]

parseBlock :: Parser Block
parseBlock =
  choice
    [ token "-- >" $> BlockDirective <*> parseDirective,
      BlockCode <$> parseTestBlock,
      BlockCode <$> parseBlockCode,
      BlockComment <$> parseBlockComment
    ]
  where
    parseDirective =
      choice
        [ DirectiveRes <$> parseExp postlexeme <* afterExp,
          directiveName "covert" $> DirectiveCovert
            <*> parseDirective,
          directiveName "brief" $> DirectiveBrief
            <*> parseDirective,
          directiveName "img" $> DirectiveImg
            <*> parseExp postlexeme <* eol,
          directiveName "plot2d" $> DirectivePlot
            <*> parseExp postlexeme
            <*> parsePlotParams <* eol,
          directiveName "gnuplot" $> DirectiveGnuplot
            <*> parseExp postlexeme
            <*> (";" *> hspace *> eol *> parseBlockComment),
          (directiveName "video" <|> directiveName "video") $> DirectiveVideo
            <*> parseExp postlexeme
            <*> parseVideoParams <* eol
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
          argb = vec SVec.! ((h -1 - i) * w + j)
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
      throwError $ prog' <> " failed: " <> T.pack (show err)
    Right (ExitSuccess, stdout_t, _) ->
      pure $ T.pack stdout_t
    Right (ExitFailure code', _, stderr_t) ->
      throwError $
        prog' <> " failed with exit code "
          <> T.pack (show code')
          <> " and stderr:\n"
          <> T.pack stderr_t
  where
    prog' = "'" <> T.pack prog <> "'"

formatDataForGnuplot :: [Value] -> T.Text
formatDataForGnuplot = T.unlines . map line . transpose . map valueElems
  where
    line = T.unwords . map prettyText

imgBlock :: FilePath -> T.Text
imgBlock f = "\n\n![](" <> T.pack f <> ")\n\n"

videoBlock :: VideoParams -> FilePath -> T.Text
videoBlock opts f = "\n\n![](" <> T.pack f <> ")" <> opts' <> "\n\n"
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
  (vs', ns') <- unzip <$> mapM inspect vs
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
      throwError $ "Failed to read BMP:\n" <> T.pack (show err)
    Right bmp -> do
      let bmp_bs = BMP.unpackBMPToRGBA32 bmp
          (w, h) = BMP.bmpDimensions bmp
          shape = SVec.fromList [fromIntegral h, fromIntegral w]
          pix l =
            let (i, j) = l `divMod` w
                l' = (h -1 - i) * w + j
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

literateBuiltin :: EvalBuiltin ScriptM
literateBuiltin "loadimg" vs =
  case vs of
    [ValueAtom v]
      | Just path <- getValue v -> do
        let path' = map (chr . fromIntegral) (path :: [Word8])
        loadImage path'
    _ ->
      throwError $
        "$imgfile does not accept arguments of types: "
          <> T.intercalate ", " (map (prettyText . fmap valueType) vs)
literateBuiltin f _ =
  throwError $ "Unknown builtin function $" <> prettyText f

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
    -- | Image dir relative to program.
    envRelImgDir :: FilePath,
    envOpts :: Options,
    envServer :: ScriptServer,
    envHash :: T.Text
  }

newFileWorker :: Env -> FilePath -> (FilePath -> ScriptM ()) -> ScriptM (FilePath, FilePath)
newFileWorker env template m = do
  let fname_base = T.unpack (envHash env) <> "-" <> template
      fname = envImgDir env </> fname_base
      fname_rel = envRelImgDir env </> fname_base
  exists <- liftIO $ doesFileExist fname
  liftIO $ createDirectoryIfMissing True $ envImgDir env
  when (exists && scriptVerbose (envOpts env) > 0) $
    liftIO $ T.hPutStrLn stderr $ "Using existing file: " <> T.pack fname
  unless exists $ do
    when (scriptVerbose (envOpts env) > 0) $
      liftIO $ T.hPutStrLn stderr $ "Generating new file: " <> T.pack fname
    m fname
  modify $ \s -> s {stateFiles = S.insert fname $ stateFiles s}
  pure (fname, fname_rel)

newFile :: Env -> FilePath -> (FilePath -> ScriptM ()) -> ScriptM FilePath
newFile env template m = snd <$> newFileWorker env template m

newFileContents :: Env -> FilePath -> (FilePath -> ScriptM ()) -> ScriptM T.Text
newFileContents env template m =
  liftIO . T.readFile . fst =<< newFileWorker env template m

processDirective :: Env -> Directive -> ScriptM T.Text
processDirective env (DirectiveBrief d) =
  processDirective env d
processDirective env (DirectiveCovert d) =
  processDirective env d
processDirective env (DirectiveRes e) = do
  result <-
    newFileContents env "eval.txt" $ \resultf -> do
      v <- either nope pure =<< evalExpToGround literateBuiltin (envServer env) e
      liftIO $ T.writeFile resultf $ prettyText v
  pure $
    T.unlines
      [ "",
        "```",
        result,
        "```",
        ""
      ]
  where
    nope t =
      throwError $ "Cannot show value of type " <> prettyText t
--
processDirective env (DirectiveImg e) = do
  fmap imgBlock . newFile env "img.png" $ \pngfile -> do
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
  fmap imgBlock . newFile env "plot.png" $ \pngfile -> do
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

    tag (Nothing, xys) j = ("data" <> T.pack (show (j :: Int)), xys)
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
  fmap imgBlock . newFile env "plot.png" $ \pngfile -> do
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
  when (format `notElem` ["webm", "gif"]) $
    throwError $ "Unknown video format: " <> format

  fmap (videoBlock params) . newFile env ("video" <.> T.unpack format) $ \videofile -> do
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
          ValueAtom (SValue _ _) <- initial,
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

    renderFrames dir (stepfun, closure) initial num_frames =
      foldM_ frame initial [0 .. num_frames -1]
      where
        frame old_state j = do
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

-- Did this script block succeed or fail?
data Failure = Failure | Success
  deriving (Eq, Ord, Show)

processBlock :: Env -> Block -> IO (Failure, T.Text, Files)
processBlock _ (BlockCode code)
  | T.null code = pure (Success, "\n", mempty)
  | otherwise = pure (Success, "\n```futhark\n" <> code <> "```\n\n", mempty)
processBlock _ (BlockComment text) =
  pure (Success, text, mempty)
processBlock env (BlockDirective directive) = do
  when (scriptVerbose (envOpts env) > 0) $
    T.hPutStrLn stderr . prettyText $
      "Processing " <> PP.align (PP.ppr directive) <> "..."
  let prompt = case directive of
        DirectiveCovert _ -> mempty
        DirectiveBrief _ ->
          "```\n" <> prettyText (pprDirective False directive) <> "\n```\n"
        _ ->
          "```\n" <> prettyText (pprDirective True directive) <> "\n```\n"
      env' = env {envHash = hashText (envHash env <> prettyText directive)}
  (r, files) <- runScriptM $ processDirective env' directive
  case r of
    Left err -> failed prompt err files
    Right t -> pure (Success, prompt <> t, files)
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
        T.hPutStrLn stderr $ "Deleting unused file: " <> T.pack f
      removePathForcibly f

processScript :: Env -> [Block] -> IO (Failure, T.Text)
processScript env script = do
  (failures, outputs, files) <-
    unzip3 <$> mapM (processBlock env) script
  cleanupImgDir env $ mconcat files
  pure (foldl' min Success failures, mconcat outputs)

commandLineOptions :: [FunOptDescr Options]
commandLineOptions =
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
      "Enable logging.  Pass multiple times for more.",
    Option
      "o"
      ["output"]
      (ReqArg (\opt -> Right $ \config -> config {scriptOutput = Just opt}) "FILE")
      "Enable logging.  Pass multiple times for more.",
    Option
      []
      ["stop-on-error"]
      (NoArg $ Right $ \config -> config {scriptStopOnError = True})
      "Stop and do not produce output file if any directive fails."
  ]

-- | Run @futhark literate@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialOptions commandLineOptions "program" $ \args opts ->
  case args of
    [prog] -> Just $ do
      futhark <- maybe getExecutablePath return $ scriptFuthark opts

      script <- parseProgFile prog

      unless (scriptSkipCompilation opts) $ do
        let entryOpt v = "--entry-point=" ++ T.unpack v
            compile_options =
              "--server" :
              map entryOpt (S.toList (varsInScripts script))
                ++ scriptCompilerOptions opts
        when (scriptVerbose opts > 0) $
          T.hPutStrLn stderr $ "Compiling " <> T.pack prog <> "..."
        when (scriptVerbose opts > 1) $
          T.hPutStrLn stderr $ T.pack $ unwords compile_options

        let onError err = do
              mapM_ (T.hPutStrLn stderr) err
              exitFailure
        void $
          either onError pure <=< runExceptT $
            compileProgram compile_options (FutharkExe futhark) (scriptBackend opts) prog

      let onError err = do
            T.hPutStrLn stderr err
            exitFailure
      proghash <-
        either onError pure <=< runExceptT $
          system futhark ["hash", prog] mempty

      let mdfile = fromMaybe (prog `replaceExtension` "md") $ scriptOutput opts
          imgdir_rel = dropExtension (takeFileName mdfile) <> "-img"
          imgdir = takeDirectory mdfile </> imgdir_rel
          run_options = scriptExtraOptions opts
          cfg = futharkServerCfg ("." </> dropExtension prog) run_options

      withScriptServer cfg $ \server -> do
        let env =
              Env
                { envServer = server,
                  envOpts = opts,
                  envHash = proghash,
                  envImgDir = imgdir,
                  envRelImgDir = imgdir_rel
                }
        (failure, md) <- processScript env script
        T.writeFile mdfile md
        when (failure == Failure) exitFailure
    _ -> Nothing
