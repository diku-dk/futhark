{-# LANGUAGE OverloadedStrings #-}

module Futhark.CLI.Script (main) where

import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Data.Functor
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Futhark.Server
import Futhark.Test
import qualified Futhark.Test.Values as V
import Futhark.Util.Options
import Futhark.Util.Pretty hiding (float, line, string, text, (</>), (<|>))
import Language.Futhark.Prop (primValueType)
import Language.Futhark.Syntax
  ( FloatValue (..),
    IntValue (..),
    PrimValue (..),
  )
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char

data Exp
  = Call EntryName [Exp]
  | Const PrimValue
  deriving (Show)

instance Pretty Exp where
  ppr = pprPrec 0
  pprPrec _ (Const v) = ppr v
  pprPrec i (Call v args) =
    parensIf (i > 0) $ ppr v <+> spread (map (pprPrec 1) args)

data ScriptBlock
  = CodeBlock T.Text
  | CommentBlock T.Text
  | ImgBlock Exp
  | ResBlock Exp
  deriving (Show)

type Parser = Parsec Void T.Text

postlexeme :: Parser ()
postlexeme = void $ hspace *> optional (try $ eol *> string "-- " *> postlexeme)

lexeme :: Parser a -> Parser a
lexeme p = p <* postlexeme

token :: T.Text -> Parser ()
token = void . try . lexeme . string

parseEntryName :: Parser EntryName
parseEntryName =
  fmap T.pack $ lexeme $ (:) <$> satisfy isAlpha <*> many (satisfy constituent)
  where
    constituent c = isAlphaNum c || c == '_'

parseDouble :: Parser Double
parseDouble = do
  x <- many $ satisfy isDigit
  ( do
      void $ string "."
      y <- many $ satisfy isDigit
      pure $ read $ x ++ "." ++ y
    )
    <|> pure (read x)

parseInteger :: Parser Integer
parseInteger = read <$> some (satisfy isDigit)

parseIntConst :: Parser PrimValue
parseIntConst = do
  x <- parseInteger
  choice
    [ signed Int8Value x "i8",
      signed Int16Value x "i16",
      signed Int32Value x "i32",
      signed Int64Value x "i64"
    ]
  where
    signed mk x suffix =
      string suffix $> SignedValue (mk (fromInteger x))

parseFloatConst :: Parser PrimValue
parseFloatConst = do
  x <- parseDouble
  choice
    [ float Float32Value x "f32",
      float Float64Value x "f64"
    ]
  where
    float mk x suffix =
      string suffix $> FloatValue (mk (realToFrac x))

parsePrimValue :: Parser PrimValue
parsePrimValue =
  lexeme $
    choice
      [ try parseIntConst,
        parseFloatConst,
        string "true" $> BoolValue True,
        string "false" $> BoolValue False
      ]

parseExp :: Parser Exp
parseExp =
  between (token "(") (token ")") parseExp
    <|> Call <$> parseEntryName <*> many parseExp
    <|> Const <$> parsePrimValue

parseDirective :: T.Text -> Parser ()
parseDirective s = try $ token "--" *> token (":" <> s)

restOfLine :: Parser T.Text
restOfLine = takeWhileP Nothing (/= '\n') <* eol

parseCommentBlock :: Parser T.Text
parseCommentBlock = T.unlines <$> some line
  where
    line = (string "-- " *> restOfLine) <|> (string "--" *> eol $> "")

parseCodeBlock :: Parser T.Text
parseCodeBlock = T.unlines . noblanks <$> some line
  where
    noblanks = reverse . dropWhile T.null . reverse . dropWhile T.null
    line = try (notFollowedBy $ string "--") *> restOfLine

parseScriptBlock :: Parser ScriptBlock
parseScriptBlock =
  choice
    [ parseDirective "img" *> (ImgBlock <$> parseExp <* eol),
      parseDirective "res" *> (ResBlock <$> parseExp <* eol),
      CodeBlock <$> parseCodeBlock,
      CommentBlock <$> parseCommentBlock
    ]

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

cmdFailure :: CmdFailure -> IO a
cmdFailure (CmdFailure bef aft) = do
  T.hPutStrLn stderr "Server failure:"
  mapM_ (T.hPutStrLn stderr) $ bef ++ aft
  exitFailure

cmdMaybe :: IO (Maybe CmdFailure) -> IO ()
cmdMaybe m = maybe (pure ()) cmdFailure =<< m

cmdEither :: IO (Either CmdFailure a) -> IO a
cmdEither m = either cmdFailure pure =<< m

readVar :: Server -> VarName -> IO V.Value
readVar server v =
  withSystemTempFile "futhark-server-read" $ \tmpf tmpf_h -> do
    hClose tmpf_h
    cmdMaybe $ cmdStore server tmpf [v]
    s <- LBS.readFile tmpf
    case V.readValues s of
      Just [val] -> pure val
      _ -> error "Invalid data file produced by Futhark server."

writeVar :: Server -> VarName -> PrimValue -> IO ()
writeVar server v val =
  withSystemTempFile "futhark-server-write" $ \tmpf tmpf_h -> do
    T.hPutStr tmpf_h $ prettyText val
    hClose tmpf_h
    let t = prettyText $ primValueType val
    cmdMaybe $ cmdRestore server tmpf [(v, t)]

evalExp :: Server -> Exp -> IO [V.Value]
evalExp server top_level_e = do
  counter <- newIORef (0 :: Int)
  let newVar :: IO VarName
      newVar = do
        x <- readIORef counter
        modifyIORef counter (+ 1)
        pure $ "v" <> prettyText x

      evalExpToVar :: Exp -> IO VarName
      evalExpToVar e = do
        vs <- evalExpToVars e
        case vs of
          [v] -> pure v
          _ -> do
            T.hPutStrLn stderr "Expression produced more than one value."
            exitFailure

      evalExpToVars :: Exp -> IO [VarName]
      evalExpToVars (Call name es) = do
        ins <- mapM evalExpToVar es
        out_types <- cmdEither $ cmdOutputs server name
        outs <- replicateM (length out_types) newVar
        void $ cmdEither $ cmdCall server name outs ins
        pure outs
      evalExpToVars (Const val) = do
        v <- newVar
        writeVar server v val
        pure [v]

  mapM (readVar server) =<< evalExpToVars top_level_e

processScriptBlock :: Server -> ScriptBlock -> IO T.Text
processScriptBlock _ (CodeBlock code)
  | T.null code = pure mempty
  | otherwise = pure $ "\n```\n" <> code <> "```\n\n"
processScriptBlock _ (CommentBlock text) =
  pure text
processScriptBlock server (ResBlock e) = do
  v <- evalExp server e
  pure $
    T.unlines $
      "" :
      "```" :
      ("> " <> prettyText e) :
      map prettyText v
        ++ ["```", ""]

processScript :: Server -> [ScriptBlock] -> IO T.Text
processScript server script =
  mconcat <$> mapM (processScriptBlock server) script

-- | Run @futhark script@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [prog] -> Just $ do
      futhark <- getExecutablePath
      let compile_options = ["--server"]
          run_options = []

      script <- parseScriptFile prog

      cres <-
        runExceptT $
          compileProgram compile_options (FutharkExe futhark) "c" prog
      case cres of
        Left err -> do
          mapM_ (T.hPutStrLn stderr) err
          exitFailure
        Right _ ->
          withServer ("." </> dropExtension prog) run_options $ \server -> do
            md <- processScript server script
            T.writeFile (prog `replaceExtension` "md") md
    _ -> Nothing
