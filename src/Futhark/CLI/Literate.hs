-- | @futhark literate@
module Futhark.CLI.Literate (main) where

import Codec.BMP qualified as BMP
import Control.Monad
import Control.Monad.Except
import Control.Monad.Free.Church (F, runF)
import Control.Monad.State hiding (State)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Data.Array qualified as A
import Data.Bifunctor (first, second)
import Data.Bits
import Data.ByteString.Lazy qualified as LBS
import Data.Char
import Data.Functor (($>))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Storable qualified as SVec
import Data.Vector.Storable.ByteString qualified as SVec
import Data.Void
import Data.Word (Word32, Word8)
import Futhark.Compiler (readProgramFilesExceptKnown)
import Futhark.Data
import Futhark.Error (prettyCompilerError)
import Futhark.Eval
  ( EvalConfig (..),
    evalConfig,
    forceValue,
    initialiseInterpreter,
    runFFI,
  )
import Futhark.FreshNames (VNameSource)
import Futhark.Server
import Futhark.Test
import Futhark.Util
  ( directoryContents,
    ensureCacheDirectory,
    fancyTerminal,
    hashText,
    nubOrd,
    runProgramWithExitCode,
    showText,
  )
import Futhark.Util.Loc qualified as Loc
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText, prettyTextOneLine)
import Futhark.Util.Pretty qualified as PP
import Futhark.Util.ProgressBar
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Interpreter.FFI.ServerM qualified as FFI
import Language.Futhark.Interpreter.Values qualified as IV
import Language.Futhark.Parser (SyntaxError (..), parseExpAt)
import Language.Futhark.Pretty ()
import Language.Futhark.Primitive qualified as P
import Language.Futhark.Prop (UncheckedExp, typeOf)
import Language.Futhark.Semantic qualified as T
import Language.Futhark.Syntax qualified as F
import Language.Futhark.Tuple (areTupleFields)
import Language.Futhark.TypeChecker qualified as T
import System.Directory
  ( copyFile,
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
  = DirectiveRes UncheckedExp
  | DirectiveBrief Directive
  | DirectiveCovert Directive
  | DirectiveImg UncheckedExp ImgParams
  | DirectivePlot UncheckedExp (Maybe (Int, Int))
  | DirectiveGnuplot UncheckedExp T.Text
  | DirectiveVideo UncheckedExp VideoParams
  | DirectiveAudio UncheckedExp AudioParams
  deriving (Show)

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

-- | The current position, in the form used by the Futhark parser.
sourcePos :: Parser Loc.Pos
sourcePos = do
  p <- getSourcePos
  Loc.Pos (sourceName p) (unPos (sourceLine p)) (unPos (sourceColumn p)) <$> getOffset

-- | Replace the comment marker on every line but the first with spaces.
blankCommentPrefix :: T.Text -> T.Text
blankCommentPrefix s =
  case T.lines s of
    [] -> s
    l : ls -> T.intercalate "\n" $ l : map onLine ls
  where
    onLine l = maybe l ("  " <>) $ T.stripPrefix "--" l

-- | A directive expression extends to the end of the enclosing comment block,
-- or to the ';' that introduces directive parameters. We slice out that text
-- and hand it to the Futhark parser. This is somewhat clumsy because the
-- Futhark parser is not written with parser combinators.
parseDirectiveExp :: Parser UncheckedExp
parseDirectiveExp = do
  pos <- sourcePos
  s <- getInput
  bef <- getOffset
  expText
  aft <- getOffset
  -- To get the right source positions, we replace comment prefixes with spaces.
  case parseExpAt pos $ blankCommentPrefix $ T.take (aft - bef) s of
    Left (SyntaxError loc msg) -> do
      case loc of
        Loc.Loc start _ -> setOffset $ Loc.posCoff start
        Loc.NoLoc -> pure ()
      fail $ T.unpack $ T.strip msg
    Right e -> pure e
  where
    expText = do
      more <- expLine
      -- A line that starts a new directive does not continue this one. The line
      -- break is consumed only if the expression does continue, as the
      -- directive parsers expect to find it.
      cont <- optional $ try $ eol *> notFollowedBy "-- >" *> lookAhead "--"
      when (more && isJust cont) expText

    -- Consume the expression text on this line (but not the line
    -- break), returning whether it may continue on the next one.
    expLine = do
      l <- lookAhead $ takeWhileP Nothing (/= '\n')
      case paramsIn l of
        Just n -> False <$ takeP Nothing n
        Nothing -> True <$ takeP Nothing (T.length l)

    -- Parameters are introduced by a ';' at the end of a line.  If
    -- this line has one, how much of it belongs to the expression?
    paramsIn l =
      case T.breakOnEnd ";" l of
        (before, after)
          | not $ T.null before,
            T.all isSpace after ->
              Just $ T.length before - 1
        _ -> Nothing

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
        [ directiveName "covert"
            $> DirectiveCovert
            <*> parseDirective,
          directiveName "brief"
            $> DirectiveBrief
            <*> parseDirective,
          directiveName "img"
            $> DirectiveImg
            <*> parseDirectiveExp
            <*> parseImgParams
            <* choice [void eol, eof],
          directiveName "plot2d"
            $> DirectivePlot
            <*> parseDirectiveExp
            <*> parsePlotParams
            <* choice [void eol, eof],
          directiveName "gnuplot"
            $> DirectiveGnuplot
            <*> parseDirectiveExp
            <*> (";" *> hspace *> eol *> parseBlockComment),
          (directiveName "video" <|> directiveName "video")
            $> DirectiveVideo
            <*> parseDirectiveExp
            <*> parseVideoParams
            <* eol,
          directiveName "audio"
            $> DirectiveAudio
            <*> parseDirectiveExp
            <*> parseAudioParams
            <* choice [void eol, eof],
          DirectiveRes <$> parseDirectiveExp <* choice [void eol, eof]
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

-- | The monad in which 'futhark literate' runs. Just does error propagation and
-- state management on top of IO.
newtype LiterateM a = LiterateM (ExceptT T.Text (StateT State IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError T.Text,
      MonadFail,
      MonadIO,
      MonadState State
    )

runLiterateM :: LiterateM a -> IO (Either T.Text a, Files)
runLiterateM (LiterateM m) = second stateFiles <$> runStateT (runExceptT m) s
  where
    s = State mempty

withTempFile :: (FilePath -> LiterateM a) -> LiterateM a
withTempFile f =
  join . liftIO . withSystemTempFile "futhark-literate" $ \tmpf tmpf_h -> do
    hClose tmpf_h
    (res, files) <- runLiterateM (f tmpf)
    pure $ do
      modify $ \s -> s {stateFiles = files <> stateFiles s}
      either throwError pure res

withTempDir :: (FilePath -> LiterateM a) -> LiterateM a
withTempDir f =
  join . liftIO . withSystemTempDirectory "futhark-literate" $ \dir -> do
    (res, files) <- runLiterateM (f dir)
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
formatDataForGnuplot = T.unlines . map line . L.transpose . map valueElems
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

-- | A tuple of one-dimensional arrays of the same length, which is what the
-- plotting directives expect.
plottable :: Env -> I.Value -> LiterateM (Maybe [Value])
plottable env (IV.ValueRecord fs) = runMaybeT $ do
  vs <- hoistMaybe $ areTupleFields fs
  (vs', ns') <- mapAndUnzipM inspect vs
  guard $ length (nubOrd ns') == 1
  pure vs'
  where
    inspect v = do
      v' <- MaybeT $ dataValue env v
      case valueShape v' of
        [n] -> pure (v', n)
        _ -> hoistMaybe Nothing
plottable _ _ = pure Nothing

-- | As 'plottable', but for exactly two arrays, interpreted as x and y values.
plottable2d :: Env -> I.Value -> LiterateM (Maybe [Value])
plottable2d env v = do
  vs <- plottable env v
  pure $ case vs of
    Just [x, y] -> Just [x, y]
    _ -> Nothing

-- | The fields of a record, as expected by the plotting directives. Note that a
-- tuple is also a record, so this must be tried only after 'plottable'.
plottableFields ::
  (I.Value -> LiterateM (Maybe [Value])) ->
  I.Value ->
  LiterateM (Maybe [(T.Text, [Value])])
plottableFields f (IV.ValueRecord fs)
  | Nothing <- areTupleFields fs =
      runMaybeT $ mapM onField $ M.toList fs
  where
    onField (k, v) = (F.nameToText k,) <$> MaybeT (f v)
plottableFields _ _ = pure Nothing

withGnuplotData ::
  [(T.Text, T.Text)] ->
  [(T.Text, [Value])] ->
  ([T.Text] -> [T.Text] -> LiterateM a) ->
  LiterateM a
withGnuplotData sets [] cont = uncurry cont $ unzip $ reverse sets
withGnuplotData sets ((f, vs) : xys) cont =
  withTempFile $ \fname -> do
    liftIO $ T.writeFile fname $ formatDataForGnuplot vs
    withGnuplotData ((f, f <> "='" <> T.pack fname <> "'") : sets) xys cont

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
    -- | Entry points are not interpreted, but run on this server.
    envServer :: FFI.Server,
    envSrc :: VNameSource,
    envTypeEnv :: T.Env,
    envCtx :: I.Ctx,
    envHash :: T.Text
  }

-- | Run an interpreter action. Traces are shown when verbose, external calls
-- are dispatched to the server, and breakpoints are ignored.
runInterpreter :: Env -> F I.ExtOp a -> LiterateM a
runInterpreter env m = runF m pure intOp
  where
    intOp (I.ExtOpError err) =
      throwError $ PP.docText $ I.prettyInterpreterError err
    intOp (I.ExtOpTrace w v c) = do
      when (scriptVerbose (envOpts env) > 0) $
        liftIO . T.putStrLn . PP.docText $
          PP.pretty w <> ":" PP.<+> PP.align v
      c
    intOp (I.ExtOpBreak _ _ _ c) = c
    intOp (I.ExtOpFFI sm c) =
      either (throwError . PP.docText . I.prettyInterpreterError) c
        =<< liftIO (runFFI (Just (envServer env)) sm)

-- | Type check and evaluate an expression, returning its type (which is useful
-- for error messages) and the value in full.
evalExp :: Env -> UncheckedExp -> LiterateM (F.StructType, I.Value)
evalExp env e = do
  fexp <- case T.checkExp [] (envSrc env) (envTypeEnv env) e of
    (_, Left terr) -> throwError $ PP.docText $ T.prettyTypeError terr
    (_, Right ([], fexp)) -> pure fexp
    (_, Right (_, fexp)) ->
      throwError $ "Ambiguous type of expression: " <> prettyText (typeOf fexp)
  (typeOf fexp,) <$> runInterpreter env (I.interpretExp (envCtx env) fexp)

-- | As 'evalExp', but also fetch any parts of the value that reside on
-- the server.
evalExpForced :: Env -> UncheckedExp -> LiterateM (F.StructType, I.Value)
evalExpForced env e = do
  (t, v) <- evalExp env e
  (t,) <$> force env v

-- | Fetch a value that resides on the server. Anything we do with a
-- value, except passing it back to the server, needs it in full.
force :: Env -> I.Value -> LiterateM I.Value
force env v = do
  v' <- liftIO $ forceValue (Just (envServer env)) v
  either (throwError . PP.docText . I.prettyInterpreterError) pure v'

-- | As 'evalExp', but convert the value to the flat representation expected by
-- the external programs we use. The description is used in the error message if
-- the value has no such representation.
evalExpToData :: Env -> T.Text -> UncheckedExp -> LiterateM Value
evalExpToData env what e = do
  (t, v) <- evalExp env e
  let nope = "Cannot " <> what <> " value of type " <> prettyText t
  maybe (throwError nope) pure =<< dataValue env v

-- | As 'dataValue', but for a value that is already present in full. Only
-- primitives and arrays of primitives have a flat representation, and not even
-- all of those: the element type of an empty array cannot be recovered from the
-- value alone.
localDataValue :: I.Value -> Maybe Value
localDataValue (IV.ValuePrim v) = primsToValue mempty [v]
localDataValue v@IV.ValueArray {} =
  primsToValue (SVec.fromList (map fromIntegral (dims (IV.valueShape v))))
    =<< prims v
  where
    prims (IV.ValueArray _ arr) = concat <$> mapM prims (A.elems arr)
    prims (IV.ValuePrim x) = Just [x]
    prims _ = Nothing
    dims (IV.ShapeDim n shape) = n : dims shape
    dims _ = []
localDataValue _ = Nothing

-- | Convert an interpreter value to the flat representation of the Futhark data
-- format, if it has one. A value that resides on the server is retrieved in its
-- entirety, which is far faster than forcing it, as that fetches an array one
-- element at a time.
dataValue :: Env -> I.Value -> LiterateM (Maybe Value)
dataValue env v@(IV.ValueLazyFFI _ vr []) = do
  r <- liftIO $ FFI.runServerM (envServer env) $ FFI.getData vr
  case r of
    Right (Just v') -> pure $ Just v'
    -- The value is opaque, or something went wrong; fall back to
    -- fetching it piecemeal, which may still work.
    _ -> dataValue env =<< force env v
dataValue env v@IV.ValueLazyFFI {} =
  -- A partially indexed array cannot be retrieved directly.
  dataValue env =<< force env v
dataValue _ v = pure $ localDataValue v

-- | The elements must all be of the same type, which is that of the
-- first one.
primsToValue :: SVec.Vector Int -> [F.PrimValue] -> Maybe Value
primsToValue shape vs =
  case vs of
    [] -> Nothing
    F.SignedValue (P.Int8Value _) : _ -> I8Value shape <$> vec asI8
    F.SignedValue (P.Int16Value _) : _ -> I16Value shape <$> vec asI16
    F.SignedValue (P.Int32Value _) : _ -> I32Value shape <$> vec asI32
    F.SignedValue (P.Int64Value _) : _ -> I64Value shape <$> vec asI64
    F.UnsignedValue (P.Int8Value _) : _ -> U8Value shape <$> vec (fmap fromIntegral . asI8)
    F.UnsignedValue (P.Int16Value _) : _ -> U16Value shape <$> vec (fmap fromIntegral . asI16)
    F.UnsignedValue (P.Int32Value _) : _ -> U32Value shape <$> vec (fmap fromIntegral . asI32)
    F.UnsignedValue (P.Int64Value _) : _ -> U64Value shape <$> vec (fmap fromIntegral . asI64)
    F.FloatValue (P.Float16Value _) : _ -> F16Value shape <$> vec asF16
    F.FloatValue (P.Float32Value _) : _ -> F32Value shape <$> vec asF32
    F.FloatValue (P.Float64Value _) : _ -> F64Value shape <$> vec asF64
    F.BoolValue _ : _ -> BoolValue shape <$> vec asBool
  where
    vec :: (SVec.Storable a) => (F.PrimValue -> Maybe a) -> Maybe (SVec.Vector a)
    vec f = SVec.fromList <$> mapM f vs
    asI8 (F.SignedValue (P.Int8Value x)) = Just x
    asI8 (F.UnsignedValue (P.Int8Value x)) = Just x
    asI8 _ = Nothing
    asI16 (F.SignedValue (P.Int16Value x)) = Just x
    asI16 (F.UnsignedValue (P.Int16Value x)) = Just x
    asI16 _ = Nothing
    asI32 (F.SignedValue (P.Int32Value x)) = Just x
    asI32 (F.UnsignedValue (P.Int32Value x)) = Just x
    asI32 _ = Nothing
    asI64 (F.SignedValue (P.Int64Value x)) = Just x
    asI64 (F.UnsignedValue (P.Int64Value x)) = Just x
    asI64 _ = Nothing
    asF16 (F.FloatValue (P.Float16Value x)) = Just x
    asF16 _ = Nothing
    asF32 (F.FloatValue (P.Float32Value x)) = Just x
    asF32 _ = Nothing
    asF64 (F.FloatValue (P.Float64Value x)) = Just x
    asF64 _ = Nothing
    asBool (F.BoolValue x) = Just x
    asBool _ = Nothing

newFile :: Env -> (Maybe FilePath, FilePath) -> (FilePath -> LiterateM ()) -> LiterateM FilePath
newFile env (fname_desired, template) m = do
  let fname_base = fromMaybe (T.unpack (envHash env) <> "-" <> template) fname_desired
      fname = envImgDir env </> fname_base
  exists <- liftIO $ doesFileExist fname
  liftIO $ ensureCacheDirectory $ envImgDir env
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

newFileContents :: Env -> (Maybe FilePath, FilePath) -> (FilePath -> LiterateM ()) -> LiterateM T.Text
newFileContents env f m =
  liftIO . T.readFile =<< newFile env f m

processDirective :: Env -> Directive -> LiterateM T.Text
processDirective env (DirectiveBrief d) =
  processDirective env d
processDirective env (DirectiveCovert d) =
  processDirective env d
processDirective env (DirectiveRes e) = do
  result <-
    newFileContents env (Nothing, "eval.txt") $ \resultf -> do
      v <- snd <$> evalExpForced env e
      liftIO $ T.writeFile resultf $ PP.docText $ I.prettyValue v
  pure $ T.unlines ["```", result, "```"]
--
processDirective env (DirectiveImg e params) = do
  fmap imgBlock . newFile env (imgFile params, "img.png") $ \pngfile -> do
    (t, v) <- evalExp env e
    bmp <- (valueToBMP =<<) <$> dataValue env v
    case bmp of
      Just bmp' ->
        withTempDir $ \dir -> do
          let bmpfile = dir </> "img.bmp"
          liftIO $ LBS.writeFile bmpfile bmp'
          void $ system "convert" [bmpfile, pngfile] mempty
      Nothing ->
        throwError $
          "Cannot create image from value of type " <> prettyText t
--
processDirective env (DirectivePlot e size) = do
  fmap imgBlock . newFile env (Nothing, "plot.png") $ \pngfile -> do
    (t, v) <- evalExp env e
    one <- plottable2d env v
    fields <- plottableFields (plottable2d env) v
    case (one, fields) of
      (Just vs, _) ->
        plotWith [(Nothing, vs)] pngfile
      (_, Just fs) ->
        plotWith (map (first Just) fs) pngfile
      _ ->
        throwError $ "Cannot plot value of type " <> prettyText t
  where
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
    (t, v) <- evalExp env e
    fields <- plottableFields (plottable env) v
    case fields of
      Just fs ->
        plotWith fs pngfile
      Nothing ->
        throwError $ "Cannot plot value of type " <> prettyText t
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
    (t, v) <- evalExp env e
    let nope =
          throwError $
            "Cannot produce video from value of type " <> prettyText t
    case v of
      IV.ValueRecord fs
        | Just [stepfun@IV.ValueFun {}, initial, num_frames] <- areTupleFields fs,
          IV.ValuePrim (F.SignedValue (P.Int64Value num_frames')) <- num_frames ->
            withTempDir $ \dir -> do
              renderFrames dir stepfun initial $ fromIntegral num_frames'
              onWebM videofile =<< bmpsToVideo dir
      _ -> do
        bmps <- (valueToBMPs =<<) <$> dataValue env v
        case bmps of
          Just bmps' ->
            withTempDir $ \dir -> do
              zipWithM_ (writeBMPFile dir) [0 ..] bmps'
              onWebM videofile =<< bmpsToVideo dir
          Nothing -> nope
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

    -- Only the image of each frame is fetched from the server; the
    -- state is passed straight back to the step function, so all the
    -- frames need not exist at once.
    renderFrames dir stepfun initial num_frames = do
      foldM_ frame initial [0 .. num_frames - 1]
      progressDone
      where
        frame old_state j = do
          progressStep j num_frames
          v <- runInterpreter env $ I.interpretApply (envCtx env) stepfun old_state
          case v of
            IV.ValueRecord fs
              | Just [arr, new_state] <- areTupleFields fs -> do
                  bmp <-
                    maybe badFrame pure . (valueToBMP =<<) =<< dataValue env arr
                  writeBMPFile dir j bmp
                  pure new_state
            _ -> badFrame
        badFrame =
          throwError "Cannot handle step function return value."

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
        v <- evalExpToData env "create audio from" e
        maybe_raw_files <- toRawFiles dir v
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

    toRawFiles dir v
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
    toRawFiles _ v = nope $ valueTypeText $ valueType v

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
    nope t = throwError $ "Cannot create audio from value of type " <> t

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
  (r, files) <- runLiterateM $ processDirective env' directive
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
  cleanupImgDir env $
    (envImgDir env </> "CACHEDIR.TAG") `S.insert` mconcat files
  pure (L.foldl' min Success failures, T.intercalate "\n" outputs)

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
      "The binary used for operations (defaults to same binary as 'futhark literate').",
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

-- Start up (and eventually shut down) a Futhark server corresponding
-- to the provided program. If the program has a @.fut@ extension, it
-- will be compiled automatically.
prepareServer :: FilePath -> Options -> (Server -> IO a) -> IO a
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
          T.hPutStrLn stderr err
          exitFailure

    void $
      either onError pure <=< runExceptT $
        compileProgram compile_options (FutharkExe futhark) (scriptBackend opts) prog

  let run_options = scriptExtraOptions opts
      onLine "call" l = T.putStrLn l
      onLine "startup" l = T.putStrLn l
      onLine _ _ = pure ()
      prog' = if is_fut then dropExtension prog else prog
      cfg =
        (futharkServerCfg ("." </> prog') run_options)
          { cfgOnLine =
              if scriptVerbose opts > 0
                then onLine
                else const . const $ pure ()
          }

  withServer cfg f

-- | Run @futhark literate@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialOptions commandLineOptions "program" $ \args opts ->
  case args of
    [prog] -> Just $ do
      futhark <- maybe getExecutablePath pure $ scriptFuthark opts
      let onError err = do
            T.hPutStrLn stderr err
            exitFailure
          onDocError err = do
            PP.hPutDocLn stderr err
            exitFailure
      proghash <-
        either onError pure <=< runExceptT $
          system futhark ["hash", prog] mempty
      script <- parseProgFile prog

      orig_dir <- getCurrentDirectory
      -- Every directive is interpreted, and may call any entry point of
      -- the program, so we cannot compile just a subset of them.
      prepareServer prog opts $ \server -> do
        -- The interpreter uses the server for entry point calls, and
        -- does not shut it down.
        ffi_server <- FFI.newServer server
        (_, imports, src) <-
          either (onDocError . prettyCompilerError) pure
            =<< runExceptT (readProgramFilesExceptKnown [] mempty [prog])
        let eval_cfg =
              evalConfig
                { evalPrintWarnings = False,
                  evalFile = Just prog,
                  evalBackend = Just $ scriptBackend opts
                }
        (_, tenv, ictx) <-
          either onDocError pure
            =<< initialiseInterpreter eval_cfg (Just prog) (Just ffi_server) imports

        let mdfile = fromMaybe (prog `replaceExtension` "md") $ scriptOutput opts
            prog_dir = takeDirectory prog
            imgdir = dropExtension (takeFileName mdfile) <> "-img"
            env =
              Env
                { envServer = ffi_server,
                  envSrc = src,
                  envTypeEnv = tenv,
                  envCtx = ictx,
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
