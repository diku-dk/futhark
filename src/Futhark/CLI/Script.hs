{-# LANGUAGE OverloadedStrings #-}

module Futhark.CLI.Script (main) where

import Control.Monad.Except
import Data.Bifunctor (bimap, first, second)
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Functor
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as SVec
import Data.Void
import Futhark.Script
import Futhark.Server
import Futhark.Test
import qualified Futhark.Test.Values as V
import Futhark.Util (runProgramWithExitCode)
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText, prettyTextOneLine)
import qualified Futhark.Util.Pretty as PP
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempFile)
import Text.Megaparsec hiding (failure, token)
import Text.Megaparsec.Char

data Directive
  = DirectiveRes Exp
  | DirectiveImg Exp
  | DirectivePlot (Maybe (Int, Int)) Exp
  | DirectiveGnuplot Exp T.Text
  deriving (Show)

instance PP.Pretty Directive where
  ppr (DirectiveRes e) =
    "> " <> PP.align (PP.ppr e)
  ppr (DirectiveImg e) =
    "> :img " <> PP.align (PP.ppr e)
  ppr (DirectivePlot Nothing e) =
    ">:plot " <> PP.align (PP.ppr e)
  ppr (DirectivePlot (Just (w, h)) e) =
    "> :plot<" <> PP.ppr w <> "," <> PP.ppr h <> "> " <> PP.align (PP.ppr e)
  ppr (DirectiveGnuplot e script) =
    PP.stack
      [ "> :gnuplot " <> PP.align (PP.ppr e) <> ";",
        PP.strictText script
      ]

data ScriptBlock
  = BlockCode T.Text
  | BlockComment T.Text
  | BlockDirective Directive
  deriving (Show)

type Parser = Parsec Void T.Text

postlexeme :: Parser ()
postlexeme = void $ hspace *> optional (try $ eol *> "-- " *> postlexeme)

lexeme :: Parser a -> Parser a
lexeme p = p <* postlexeme

token :: T.Text -> Parser ()
token = void . try . lexeme . string

parseInt :: Parser Int
parseInt = read <$> some (satisfy isDigit)

parsePlotParams :: Parser (Maybe (Int, Int))
parsePlotParams =
  optional . between (token "<") (token ">") $
    (,) <$> parseInt <* token "," <*> parseInt

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

parseScriptBlock :: Parser ScriptBlock
parseScriptBlock =
  choice
    [ token "-- >" *> (BlockDirective <$> parseDirective),
      BlockCode <$> parseTestBlock,
      BlockCode <$> parseBlockCode,
      BlockComment <$> parseBlockComment
    ]
  where
    parseDirective =
      choice
        [ DirectiveRes <$> parseExp postlexeme,
          directiveName "img"
            *> (DirectiveImg <$> parseExp postlexeme),
          directiveName "plot2d"
            *> (DirectivePlot <$> parsePlotParams <*> parseExp postlexeme),
          directiveName "gnuplot"
            *> ( DirectiveGnuplot <$> parseExp postlexeme
                   <*> (";" *> hspace *> eol *> parseBlockComment)
               )
        ]
        <* (void eol <|> eof)
    directiveName s = try $ token (":" <> s)

parseScript :: FilePath -> T.Text -> Either T.Text [ScriptBlock]
parseScript fname s =
  either (Left . T.pack . errorBundlePretty) Right $
    parse (many parseScriptBlock <* eof) fname s

parseScriptFile :: FilePath -> IO [ScriptBlock]
parseScriptFile prog = do
  pres <- parseScript prog <$> T.readFile prog
  case pres of
    Left err -> do
      T.hPutStr stderr err
      exitFailure
    Right script ->
      pure script

type ScriptM = ExceptT T.Text IO

withTempFile :: (FilePath -> ScriptM a) -> ScriptM a
withTempFile f =
  join . liftIO . withSystemTempFile "futhark-script" $ \tmpf tmpf_h -> do
    hClose tmpf_h
    either throwError pure <$> runExceptT (f tmpf)

ppmHeader :: Int -> Int -> BS.ByteString
ppmHeader h w =
  "P6\n" <> BS.pack (show w) <> " " <> BS.pack (show h) <> "\n255\n"

rgbIntToImg ::
  (Integral a, Bits a, SVec.Storable a) =>
  Int ->
  Int ->
  SVec.Vector a ->
  BS.ByteString
rgbIntToImg h w bytes =
  ppmHeader h w <> fst (BS.unfoldrN (h * w * 3) byte 0)
  where
    getChan word chan =
      (word `shiftR` (chan * 8)) .&. 0xFF
    byte i =
      Just
        ( chr . fromIntegral $ getChan (bytes SVec.! (i `div` 3)) (2 - (i `mod` 3)),
          i + 1
        )

greyFloatToImg ::
  (RealFrac a, SVec.Storable a) =>
  Int ->
  Int ->
  SVec.Vector a ->
  BS.ByteString
greyFloatToImg h w bytes =
  ppmHeader h w <> fst (BS.unfoldrN (h * w * 3) byte 0)
  where
    byte i =
      Just (chr $ round (bytes SVec.! (i `div` 3)) * 255, i + 1)

valueToPPM :: V.Value -> Maybe BS.ByteString
valueToPPM v@(V.Word32Value _ bytes)
  | [h, w] <- V.valueShape v =
    Just $ rgbIntToImg h w bytes
valueToPPM v@(V.Int32Value _ bytes)
  | [h, w] <- V.valueShape v =
    Just $ rgbIntToImg h w bytes
valueToPPM v@(V.Float32Value _ bytes)
  | [h, w] <- V.valueShape v =
    Just $ greyFloatToImg h w bytes
valueToPPM v@(V.Float64Value _ bytes)
  | [h, w] <- V.valueShape v =
    Just $ greyFloatToImg h w bytes
valueToPPM _ = Nothing

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

ppmToPNG :: FilePath -> ScriptM FilePath
ppmToPNG ppm = do
  void $ system "convert" [ppm, png] mempty
  pure png
  where
    png = ppm `replaceExtension` "png"

formatDataForGnuplot :: V.Value -> V.Value -> T.Text
formatDataForGnuplot xs ys =
  T.unlines $ zipWith line (V.valueElems xs) (V.valueElems ys)
  where
    line x y = prettyText x <> " " <> prettyText y

imgBlock :: FilePath -> T.Text
imgBlock f = "\n\n![](" <> T.pack f <> ")\n\n"

plottable :: V.CompoundValue -> Maybe (V.Value, V.Value)
plottable (V.ValueTuple [V.ValueAtom xs, V.ValueAtom ys])
  | [n_xs] <- V.valueShape xs,
    [n_ys] <- V.valueShape ys,
    n_xs == n_ys =
    Just (xs, ys)
plottable _ = Nothing

withGnuplotData ::
  [(T.Text, T.Text)] ->
  [(T.Text, (Value, Value))] ->
  ([T.Text] -> [T.Text] -> ScriptM a) ->
  ScriptM a
withGnuplotData sets [] cont = uncurry cont $ unzip sets
withGnuplotData sets ((f, (xs, ys)) : xys) cont =
  withTempFile $ \fname -> do
    liftIO $ T.writeFile fname $ formatDataForGnuplot xs ys
    withGnuplotData ((f, f <> "='" <> T.pack fname <> "'") : sets) xys cont

processDirective :: FilePath -> Server -> Int -> Directive -> ScriptM T.Text
processDirective _ server _ (DirectiveRes e) = do
  vs <- evalExp server e
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
  vs <- evalExp server e
  case vs of
    V.ValueAtom v
      | Just ppm <- valueToPPM v -> do
        let ppmfile = imgdir </> "img" <> show i <.> ".ppm"
        liftIO $ createDirectoryIfMissing True imgdir
        liftIO $ BS.writeFile ppmfile ppm
        pngfile <- ppmToPNG ppmfile
        liftIO $ removeFile ppmfile
        pure $ imgBlock pngfile
    _ ->
      throwError $
        "Cannot create image from value of type "
          <> prettyText (fmap V.valueType vs)
--
processDirective imgdir server i (DirectivePlot size e) = do
  v <- evalExp server e
  case v of
    _
      | Just (xs, ys) <- plottable v ->
        plotWith [(Nothing, (xs, ys))]
    V.ValueRecord m
      | Just m' <- traverse plottable m ->
        plotWith $ map (first Just) $ M.toList m'
    _ ->
      throwError $
        "Cannot plot value of type " <> prettyText (fmap V.valueType v)
  where
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
  vs <- evalExp server e
  case vs of
    V.ValueRecord m
      | Just m' <- traverse plottable m ->
        plotWith $ M.toList m'
    _ ->
      throwError $
        "Cannot plot value of type " <> prettyText (fmap V.valueType vs)
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

-- Did this script block succeed or fail?
data Failure = Failure | Success
  deriving (Eq, Ord, Show)

data ScriptOptions = ScriptOptions
  { scriptBackend :: String,
    scriptFuthark :: Maybe FilePath,
    scriptExtraOptions :: [String],
    scriptCompilerOptions :: [String],
    scriptSkipCompilation :: Bool,
    scriptOutput :: Maybe FilePath,
    scriptVerbose :: Int,
    scriptStopOnError :: Bool
  }

initialScriptOptions :: ScriptOptions
initialScriptOptions =
  ScriptOptions
    { scriptBackend = "c",
      scriptFuthark = Nothing,
      scriptExtraOptions = [],
      scriptCompilerOptions = [],
      scriptSkipCompilation = False,
      scriptOutput = Nothing,
      scriptVerbose = 0,
      scriptStopOnError = False
    }

processScriptBlock :: ScriptOptions -> FilePath -> Server -> Int -> ScriptBlock -> IO (Failure, T.Text)
processScriptBlock _ _ _ _ (BlockCode code)
  | T.null code = pure (Success, mempty)
  | otherwise = pure (Success, "\n```\n" <> code <> "```\n\n")
processScriptBlock _ _ _ _ (BlockComment text) =
  pure (Success, text)
processScriptBlock opts server imgdir i (BlockDirective directive) = do
  when (scriptVerbose opts > 0) $
    T.hPutStrLn stderr . prettyText $
      "Processing " <> PP.align (PP.ppr directive) <> "..."
  let prompt = "```\n" <> prettyText directive <> "\n```\n"
  r <- runExceptT $ processDirective server imgdir i directive
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

processScript :: ScriptOptions -> FilePath -> Server -> [ScriptBlock] -> IO (Failure, T.Text)
processScript opts imgdir server script =
  bimap (foldl' min Success) mconcat . unzip
    <$> zipWithM (processScriptBlock opts imgdir server) [0 ..] script

commandLineOptions :: [FunOptDescr ScriptOptions]
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
main = mainWithOptions initialScriptOptions commandLineOptions "program" $ \args opts ->
  case args of
    [prog] -> Just $ do
      futhark <- maybe getExecutablePath return $ scriptFuthark opts
      let compile_options = "--server" : scriptCompilerOptions opts
          run_options = scriptExtraOptions opts

      script <- parseScriptFile prog

      unless (scriptSkipCompilation opts) $ do
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

      withServer ("." </> dropExtension prog) run_options $ \server -> do
        (failure, md) <- processScript opts imgdir server script
        when (failure == Failure) exitFailure
        T.writeFile mdfile md
    _ -> Nothing
