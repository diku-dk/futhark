{-# LANGUAGE OverloadedStrings #-}

module Futhark.CLI.Script (main) where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Functor
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
import Futhark.Util
import Futhark.Util.Options
import Futhark.Util.Pretty hiding (float, line, string, text, (</>), (<|>))
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath
import System.IO
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char

data ScriptOptions = ScriptOptions
  { scriptBackend :: String,
    scriptFuthark :: Maybe FilePath,
    scriptExtraOptions :: [String],
    scriptCompilerOptions :: [String],
    scriptSkipCompilation :: Bool,
    scriptOutput :: Maybe FilePath,
    scriptVerbose :: Int
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
      scriptVerbose = 0
    }

data Directive
  = DirectiveRes Exp
  | DirectiveImg Exp
  | DirectivePlot (Maybe (Int, Int)) Exp
  deriving (Show)

instance Pretty Directive where
  ppr (DirectiveRes e) =
    ":res " <> ppr e
  ppr (DirectiveImg e) =
    ":img " <> ppr e
  ppr (DirectivePlot Nothing e) =
    ":plot " <> ppr e
  ppr (DirectivePlot (Just (w, h)) e) =
    ":plot<" <> ppr w <> "," <> ppr h <> "> " <> ppr e

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

parseBlockCode :: Parser T.Text
parseBlockCode = T.unlines . noblanks <$> some line
  where
    noblanks = reverse . dropWhile T.null . reverse . dropWhile T.null
    line = try (notFollowedBy "--") *> restOfLine

parseScriptBlock :: Parser ScriptBlock
parseScriptBlock =
  choice
    [ BlockDirective <$> parseDirective,
      BlockCode <$> parseBlockCode,
      BlockComment <$> parseBlockComment
    ]
  where
    parseDirective =
      choice
        [ directiveName "img"
            *> (DirectiveImg <$> parseExp postlexeme),
          directiveName "res"
            *> (DirectiveRes <$> parseExp postlexeme),
          directiveName "plot2d"
            *> (DirectivePlot <$> parsePlotParams <*> parseExp postlexeme)
        ]
        <* eol
    directiveName s = try $ token "--" *> token (":" <> s)

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

argbIntToImg ::
  (Integral a, Bits a, SVec.Storable a) =>
  Int ->
  Int ->
  SVec.Vector a ->
  BS.ByteString
argbIntToImg h w bytes =
  "P6\n" <> BS.pack (show w) <> " " <> BS.pack (show h) <> "\n255\n"
    <> fst (BS.unfoldrN (h * w * 3) byte 0)
  where
    getChan word chan =
      (word `shiftR` (chan * 8)) .&. 0xFF
    byte i =
      Just
        ( chr . fromIntegral $ getChan (bytes SVec.! (i `div` 3)) (i `mod` 3),
          i + 1
        )

valueToPPM :: V.Value -> Maybe BS.ByteString
valueToPPM v@(V.Word32Value _ bytes)
  | [h, w] <- V.valueShape v =
    Just $ argbIntToImg h w bytes
valueToPPM v@(V.Int32Value _ bytes)
  | [h, w] <- V.valueShape v =
    Just $ argbIntToImg h w bytes
valueToPPM _ = Nothing

ppmToPNG :: FilePath -> ScriptM FilePath
ppmToPNG ppm = do
  res <- liftIO $ runProgramWithExitCode "convert" [ppm, png] mempty
  case res of
    Left err -> throwError $ T.pack $ show err
    Right _ -> pure png
  where
    png = ppm `replaceExtension` "png"

formatDataForGnuplot :: V.Value -> V.Value -> T.Text
formatDataForGnuplot xs ys =
  T.unlines $ zipWith line (V.valueElems xs) (V.valueElems ys)
  where
    line x y = prettyText x <> " " <> prettyText y

promptLine :: Exp -> T.Text
promptLine e = "> " <> prettyTextOneLine e

imgRes :: Exp -> FilePath -> T.Text
imgRes e f =
  T.unlines
    [ "```",
      promptLine e,
      "```",
      "![](" <> T.pack f <> ")\n"
    ]

processDirective :: FilePath -> Server -> Int -> Directive -> ScriptM T.Text
processDirective _ server _ (DirectiveRes e) = do
  vs <- evalExp server e
  pure $
    T.unlines
      [ "",
        "```",
        promptLine e,
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
        pure $ imgRes e pngfile
    _ ->
      throwError $
        "Cannot create image from values of types "
          <> prettyText (fmap V.valueType vs)
--
processDirective imgdir server i (DirectivePlot size e) = do
  vs <- evalExp server e
  case vs of
    V.ValueTuple [v]
      | Just (xs, ys) <- plottable v ->
        plotWith [(Nothing, (xs, ys))]
    V.ValueRecord m
      | Just m' <- traverse plottable m ->
        plotWith $ map (first Just) $ M.toList m'
    _ ->
      throwError $
        "Cannot plot values of types " <> prettyText (fmap V.valueType vs)
  where
    plottable (V.ValueTuple [V.ValueAtom xs, V.ValueAtom ys])
      | [n_xs] <- V.valueShape xs,
        [n_ys] <- V.valueShape ys,
        n_xs == n_ys =
        Just (xs, ys)
    plottable _ = Nothing

    plotWith :: [(Maybe T.Text, (V.Value, V.Value))] -> ScriptM T.Text
    plotWith xys = do
      let pngfile = imgdir </> "plot" <> show i <.> ".png"
          size' = BS.pack $
            case size of
              Nothing -> "500,500"
              Just (w, h) -> show w ++ "," ++ show h
      (datafiles, cmds) <-
        fmap unzip $
          forM (zip xys [0 ..]) $ \((title, (xs, ys)), j) -> do
            let datafile =
                  imgdir
                    </> ("plot" <> show i <> "_" <> show (j :: Int))
                    <.> "data"
                title' = case title of
                  Nothing -> "notitle"
                  Just x -> "title '" <> T.encodeUtf8 x <> "' with lines"
            liftIO $ T.writeFile datafile $ formatDataForGnuplot xs ys
            pure
              ( datafile,
                "'" <> BS.pack datafile <> "' " <> title'
              )
      let script =
            BS.unlines
              [ "set key outside",
                "set terminal png size " <> size' <> " enhanced",
                "set output '" <> BS.pack pngfile <> "'",
                "plot " <> BS.intercalate ", " cmds
              ]
      res <- liftIO $ runProgramWithExitCode "gnuplot" [] script
      case res of
        Left err -> throwError $ T.pack $ show err
        Right _ -> do
          liftIO $ mapM_ removeFile datafiles
          pure $ imgRes e pngfile

processScriptBlock :: ScriptOptions -> FilePath -> Server -> Int -> ScriptBlock -> ScriptM T.Text
processScriptBlock _ _ _ _ (BlockCode code)
  | T.null code = pure mempty
  | otherwise = pure $ "\n```\n" <> code <> "```\n\n"
processScriptBlock _ _ _ _ (BlockComment text) =
  pure text
processScriptBlock opts server imgdir i (BlockDirective directive) = do
  when (scriptVerbose opts > 0) $
    liftIO $ T.hPutStrLn stderr $ "Processing " <> prettyText directive <> "..."
  processDirective server imgdir i directive `catchError` failed
  where
    failed err = do
      let message = prettyTextOneLine directive <> " failed:\n" <> err <> "\n"
      liftIO $ T.hPutStr stderr message
      pure $ T.unlines ["```", message, "```"]

processScript :: ScriptOptions -> FilePath -> Server -> [ScriptBlock] -> ScriptM T.Text
processScript opts imgdir server script =
  mconcat <$> zipWithM (processScriptBlock opts imgdir server) [0 ..] script

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
      "Enable logging.  Pass multiple times for more."
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
            compileProgram compile_options (FutharkExe futhark) "c" prog
        case cres of
          Left err -> do
            mapM_ (T.hPutStrLn stderr) err
            exitFailure
          Right _ ->
            pure ()

      let mdfile = fromMaybe (prog `replaceExtension` "md") $ scriptOutput opts
          imgdir = dropExtension mdfile <> "-img"

      withServer ("." </> dropExtension prog) run_options $ \server -> do
        res <- runExceptT $ processScript opts imgdir server script
        case res of
          Right md -> T.writeFile mdfile md
          Left err -> do
            T.hPutStrLn stderr err
            exitFailure
    _ -> Nothing
