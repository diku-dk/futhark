{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | FutharkScript is a (tiny) subset of Futhark used to write small
-- expressions that are evaluated by server executables.  The @futhark
-- script@ command is the main user.
module Futhark.Script
  ( Exp (..),
    parseExp,
    evalExp,
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Data.Functor
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Futhark.Server
import qualified Futhark.Test.Values as V
import Futhark.Util.Pretty hiding (float, line, sep, string, text, (</>), (<|>))
import Language.Futhark.Prop (primValueType)
import Language.Futhark.Syntax
  ( FloatValue (..),
    IntValue (..),
    PrimValue (..),
  )
import System.IO
import System.IO.Temp
import Text.Megaparsec
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

type Parser = Parsec Void T.Text

parseEntryName :: Parser EntryName
parseEntryName =
  fmap T.pack $ (:) <$> satisfy isAlpha <*> many (satisfy constituent)
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
  choice
    [ try parseIntConst,
      parseFloatConst,
      string "true" $> BoolValue True,
      string "false" $> BoolValue False
    ]

lexeme :: Parser () -> Parser a -> Parser a
lexeme sep p = p <* sep

parseExp :: Parser () -> Parser Exp
parseExp sep =
  between (lexeme sep (string "(")) (lexeme sep (string ")")) (parseExp sep)
    <|> Call <$> lexeme sep parseEntryName <*> many (parseExp sep)
    <|> Const <$> lexeme sep parsePrimValue

prettyFailure :: CmdFailure -> T.Text
prettyFailure (CmdFailure bef aft) =
  T.unlines $ bef ++ aft

cmdMaybe :: (MonadError T.Text m, MonadIO m) => IO (Maybe CmdFailure) -> m ()
cmdMaybe m = maybe (pure ()) (throwError . prettyFailure) =<< liftIO m

cmdEither :: (MonadError T.Text m, MonadIO m) => IO (Either CmdFailure a) -> m a
cmdEither m = either (throwError . prettyFailure) pure =<< liftIO m

readVar :: (MonadError T.Text m, MonadIO m) => Server -> VarName -> m V.Value
readVar server v =
  either throwError pure <=< liftIO $
    withSystemTempFile "futhark-server-read" $ \tmpf tmpf_h -> do
      hClose tmpf_h
      store_res <- cmdStore server tmpf [v]
      case store_res of
        Just err -> pure $ Left $ prettyFailure err
        Nothing -> do
          s <- LBS.readFile tmpf
          case V.readValues s of
            Just [val] -> pure $ Right val
            _ -> pure $ Left "Invalid data file produced by Futhark server."

writeVar :: (MonadError T.Text m, MonadIO m) => Server -> VarName -> PrimValue -> m ()
writeVar server v val =
  cmdMaybe . liftIO . withSystemTempFile "futhark-server-write" $ \tmpf tmpf_h -> do
    T.hPutStr tmpf_h $ prettyText val
    hClose tmpf_h
    let t = prettyText $ primValueType val
    cmdRestore server tmpf [(v, t)]

evalExp :: (MonadError T.Text m, MonadIO m) => Server -> Exp -> m [V.Value]
evalExp server top_level_e = do
  counter <- liftIO $ newIORef (0 :: Int)
  let newVar base = liftIO $ do
        x <- readIORef counter
        modifyIORef counter (+ 1)
        pure $ base <> prettyText x

      evalExpToVar e = do
        vs <- evalExpToVars e
        case vs of
          [v] -> pure v
          _ -> throwError "Expression produced more than one value."

      evalExpToVars (Call name es) = do
        ins <- mapM evalExpToVar es
        out_types <- cmdEither $ cmdOutputs server name
        outs <- replicateM (length out_types) $ newVar "out"
        void $ cmdEither $ cmdCall server name outs ins
        pure outs
      evalExpToVars (Const val) = do
        v <- newVar "const"
        writeVar server v val
        pure [v]

  cmdMaybe $ cmdClear server
  mapM (readVar server) =<< evalExpToVars top_level_e
