{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @futhark literate@
module Futhark.CLI.Literate (main) where

import qualified Codec.BMP as BMP
import Control.Monad.Except
import Data.Bifunctor (bimap, first, second)
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
import Futhark.Script
import Futhark.Server
import Futhark.Test
import Futhark.Test.Values
import Futhark.Util (nubOrd, runProgramWithExitCode)
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText, prettyTextOneLine)
import qualified Futhark.Util.Pretty as PP
import System.Directory
  ( createDirectoryIfMissing,
    removeFile,
    removePathForcibly,
  )
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Text.Megaparsec hiding (failure, token)
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
restOfLine = takeWhileP Nothing (/= '\n') <* eol

parseBlockComment :: Parser T.Text
parseBlockComment = T.unlines <$> some line
  where
    line = ("-- " *> restOfLine) <|> ("--" *> eol $> "")

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
    line = try (notFollowedBy "--") *> restOfLine

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

parseBlock :: Parser Block
parseBlock =
  choice
    [ token "-- >" $> BlockDirective <*> parseDirective <* void eol,
      BlockCode <$> parseTestBlock,
      BlockCode <$> parseBlockCode,
      BlockComment <$> parseBlockComment
    ]
  where
    parseDirective =
      choice
        [ DirectiveRes <$> parseExp postlexeme,
          directiveName "covert" $> DirectiveCovert
            <*> parseDirective,
          directiveName "brief" $> DirectiveBrief
            <*> parseDirective,
          directiveName "img" $> DirectiveImg
            <*> parseExp postlexeme,
          directiveName "plot2d" $> DirectivePlot
            <*> parseExp postlexeme
            <*> parsePlotParams,
          directiveName "gnuplot" $> DirectiveGnuplot
            <*> parseExp postlexeme
            <*> (";" *> hspace *> eol *> parseBlockComment),
          (directiveName "video" <|> directiveName "video") $> DirectiveVideo
            <*> parseExp postlexeme
            <*> parseVideoParams
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

type ScriptM = ExceptT T.Text IO

withTempFile :: (FilePath -> ScriptM a) -> ScriptM a
withTempFile f =
  join . liftIO . withSystemTempFile "futhark-literate" $ \tmpf tmpf_h -> do
    hClose tmpf_h
    either throwError pure <$> runExceptT (f tmpf)

withTempDir :: (FilePath -> ScriptM a) -> ScriptM a
withTempDir f =
  join . liftIO . withSystemTempDirectory "futhark-literate" $ \dir ->
    either throwError pure <$> runExceptT (f dir)

greyFloatToImg ::
  (RealFrac a, SVec.Storable a) =>
  SVec.Vector a ->
  SVec.Vector Word32
greyFloatToImg = SVec.map grey
  where
    grey i =
      let i' = round (i * 255) .&. 0xFF
       in (i' `shiftL` 16) .|. (i' `shiftL` 8) .|. i'

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
valueToBMP v@(Word32Value _ bytes)
  | [h, w] <- valueShape v =
    Just $ vecToBMP h w bytes
valueToBMP v@(Int32Value _ bytes)
  | [h, w] <- valueShape v =
    Just $ vecToBMP h w $ SVec.map fromIntegral bytes
valueToBMP v@(Float32Value _ bytes)
  | [h, w] <- valueShape v =
    Just $ vecToBMP h w $ greyFloatToImg bytes
valueToBMP v@(Float64Value _ bytes)
  | [h, w] <- valueShape v =
    Just $ vecToBMP h w $ greyFloatToImg bytes
valueToBMP _ = Nothing

valueToBMPs :: Value -> Maybe [LBS.ByteString]
valueToBMPs = mapM valueToBMP . valueElems

system :: FilePath -> [String] -> T.Text -> ScriptM T.Text
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

bmpToPNG :: FilePath -> ScriptM FilePath
bmpToPNG bmp = do
  void $ system "convert" [bmp, png] mempty
  pure png
  where
    png = bmp `replaceExtension` "png"

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
      pure $ ValueAtom $ Word32Value shape $ SVec.generate (w * h) pix

loadImage :: FilePath -> ScriptM (Compound Value)
loadImage imgfile =
  withTempDir $ \dir -> do
    let bmpfile = dir </> imgfile `replaceExtension` "bmp"
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

processDirective :: FilePath -> ScriptServer -> Int -> Directive -> ScriptM T.Text
processDirective imgdir server i (DirectiveBrief d) =
  processDirective imgdir server i d
processDirective imgdir server i (DirectiveCovert d) =
  processDirective imgdir server i d
processDirective _ server _ (DirectiveRes e) = do
  vs <- evalExpToGround literateBuiltin server e
  pure $
    T.unlines
      [ "",
        "```",
        prettyText vs,
        "```",
        ""
      ]
--
processDirective imgdir server i (DirectiveImg e) = do
  vs <- evalExpToGround literateBuiltin server e
  case vs of
    ValueAtom v
      | Just bmp <- valueToBMP v -> do
        let bmpfile = imgdir </> "img" <> show i <.> ".bmp"
        liftIO $ createDirectoryIfMissing True imgdir
        liftIO $ LBS.writeFile bmpfile bmp
        pngfile <- bmpToPNG bmpfile
        liftIO $ removeFile bmpfile
        pure $ imgBlock pngfile
    _ ->
      throwError $
        "Cannot create image from value of type "
          <> prettyText (fmap valueType vs)
--
processDirective imgdir server i (DirectivePlot e size) = do
  v <- evalExpToGround literateBuiltin server e
  case v of
    _
      | Just vs <- plottable2d v ->
        plotWith [(Nothing, vs)]
    ValueRecord m
      | Just m' <- traverse plottable2d m ->
        plotWith $ map (first Just) $ M.toList m'
    _ ->
      throwError $
        "Cannot plot value of type " <> prettyText (fmap valueType v)
  where
    plottable2d v = do
      [x, y] <- plottable v
      Just [x, y]

    pngfile = imgdir </> "plot" <> show i <.> ".png"

    tag (Nothing, xys) j = ("data" <> T.pack (show (j :: Int)), xys)
    tag (Just f, xys) _ = (f, xys)

    plotWith xys = withGnuplotData [] (zipWith tag xys [0 ..]) $ \fs sets -> do
      liftIO $ createDirectoryIfMissing True imgdir
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
      pure $ imgBlock pngfile
--
processDirective imgdir server i (DirectiveGnuplot e script) = do
  vs <- evalExpToGround literateBuiltin server e
  case vs of
    ValueRecord m
      | Just m' <- traverse plottable m ->
        plotWith $ M.toList m'
    _ ->
      throwError $
        "Cannot plot value of type " <> prettyText (fmap valueType vs)
  where
    pngfile = imgdir </> "plot" <> show i <.> ".png"

    plotWith xys = withGnuplotData [] xys $ \_ sets -> do
      liftIO $ createDirectoryIfMissing True imgdir
      let script' =
            T.unlines
              [ "set terminal png enhanced",
                "set output '" <> T.pack pngfile <> "'",
                T.unlines sets,
                script
              ]
      void $ system "gnuplot" [] script'
      pure $ imgBlock pngfile
--
processDirective imgdir server i (DirectiveVideo e params) = do
  when (format `notElem` ["webm", "gif"]) $
    throwError $ "Unknown video format: " <> format

  v <- evalExp literateBuiltin server e
  let nope =
        throwError $
          "Cannot produce video from value of type " <> prettyText (fmap scriptValueType v)
  case v of
    ValueAtom SValue {} -> do
      ValueAtom arr <- getExpValue server v
      case valueToBMPs arr of
        Nothing -> nope
        Just bmps ->
          withTempDir $ \dir -> do
            zipWithM_ (writeBMPFile dir) [0 ..] bmps
            bmpsToVideo dir
    ValueTuple [stepfun, initial, num_frames]
      | ValueAtom (SFun stepfun' _ [_, _] closure) <- stepfun,
        ValueAtom (SValue _ _) <- initial,
        ValueAtom (SValue "i64" _) <- num_frames -> do
        Just (ValueAtom num_frames') <-
          mapM getValue <$> getExpValue server num_frames
        withTempDir $ \dir -> do
          let num_frames_int = fromIntegral (num_frames' :: Int64)
          renderFrames dir (stepfun', map ValueAtom closure) initial num_frames_int
          bmpsToVideo dir
    _ ->
      nope

  when (videoFormat params == Just "gif") $ do
    void $ system "ffmpeg" ["-i", webmfile, giffile] mempty
    liftIO $ removeFile webmfile

  pure $ videoBlock params videofile
  where
    framerate = fromMaybe 30 $ videoFPS params
    format = fromMaybe "webm" $ videoFormat params
    webmfile = imgdir </> "video" <> show i <.> "webm"
    giffile = imgdir </> "video" <> show i <.> "gif"
    bmpfile dir j = dir </> printf "frame%010d.bmp" (j :: Int)
    videofile = imgdir </> "video" <> show i <.> T.unpack format

    renderFrames dir (stepfun, closure) initial num_frames =
      foldM_ frame initial [0 .. num_frames -1]
      where
        frame old_state j = do
          v <-
            evalExp literateBuiltin server
              . Call (FuncFut stepfun)
              . map valueToExp
              $ closure ++ [old_state]
          freeValue server old_state

          let nope =
                throwError $
                  "Cannot handle step function return type: "
                    <> prettyText (fmap scriptValueType v)

          case v of
            ValueTuple [arr_v@(ValueAtom SValue {}), new_state] -> do
              ValueAtom arr <- getExpValue server arr_v
              freeValue server arr_v
              case valueToBMP arr of
                Nothing -> nope
                Just bmp -> do
                  writeBMPFile dir j bmp
                  pure new_state
            _ -> nope

    bmpsToVideo dir = do
      liftIO $ createDirectoryIfMissing True imgdir
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
            webmfile
          ]
          mempty

    writeBMPFile dir j bmp =
      liftIO $ LBS.writeFile (bmpfile dir j) bmp

-- Did this script block succeed or fail?
data Failure = Failure | Success
  deriving (Eq, Ord, Show)

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

processBlock :: Options -> FilePath -> ScriptServer -> Int -> Block -> IO (Failure, T.Text)
processBlock _ _ _ _ (BlockCode code)
  | T.null code = pure (Success, "\n")
  | otherwise = pure (Success, "\n```futhark\n" <> code <> "```\n\n")
processBlock _ _ _ _ (BlockComment text) =
  pure (Success, text)
processBlock opts imgdir server i (BlockDirective directive) = do
  when (scriptVerbose opts > 0) $
    T.hPutStrLn stderr . prettyText $
      "Processing " <> PP.align (PP.ppr directive) <> "..."
  let prompt = case directive of
        DirectiveCovert _ -> mempty
        DirectiveBrief _ ->
          "```\n" <> prettyText (pprDirective False directive) <> "\n```\n"
        _ ->
          "```\n" <> prettyText (pprDirective True directive) <> "\n```\n"
  r <- runExceptT $ processDirective imgdir server i directive
  second (prompt <>) <$> case r of
    Left err -> failed err
    Right t -> pure (Success, t)
  where
    failed err = do
      let message = prettyTextOneLine directive <> " failed:\n" <> err <> "\n"
      liftIO $ T.hPutStr stderr message
      when (scriptStopOnError opts) exitFailure
      pure
        ( Failure,
          T.unlines ["**FAILED**", "```", err, "```"]
        )

processScript :: Options -> FilePath -> ScriptServer -> [Block] -> IO (Failure, T.Text)
processScript opts imgdir server script =
  bimap (foldl' min Success) mconcat . unzip
    <$> zipWithM (processBlock opts imgdir server) [0 ..] script

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

-- | Run @futhark script@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialOptions commandLineOptions "program" $ \args opts ->
  case args of
    [prog] -> Just $ do
      futhark <- maybe getExecutablePath return $ scriptFuthark opts

      script <- parseProgFile prog

      unless (scriptSkipCompilation opts) $ do
        let entryOpt v = "--entry=" ++ T.unpack v
            compile_options =
              "--server" :
              map entryOpt (S.toList (varsInScripts script))
                ++ scriptCompilerOptions opts
        when (scriptVerbose opts > 0) $
          T.hPutStrLn stderr $ "Compiling " <> T.pack prog <> "..."
        cres <-
          runExceptT $
            compileProgram compile_options (FutharkExe futhark) (scriptBackend opts) prog
        case cres of
          Left err -> do
            mapM_ (T.hPutStrLn stderr) err
            exitFailure
          Right _ ->
            pure ()

      let mdfile = fromMaybe (prog `replaceExtension` "md") $ scriptOutput opts
          imgdir = dropExtension mdfile <> "-img"
          run_options = scriptExtraOptions opts

      removePathForcibly imgdir

      withScriptServer ("." </> dropExtension prog) run_options $ \server -> do
        (failure, md) <- processScript opts imgdir server script
        when (failure == Failure) exitFailure
        T.writeFile mdfile md
    _ -> Nothing
