{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec-based parser for primitive 'Value's.  The difference
-- between this and the parser defined in "Futhark.Test.Values" is
-- that we don't try to handle both the textual and binary format -
-- only the former.  On the other hand, this parser has (much) better
-- error messages and can be easily used by other parsers (like the
-- ones for FutharkScript or test blocks).
module Futhark.Test.Values.Parser
  ( parsePrimType,
    parseType,
    parsePrimValue,
    parseValue,
  )
where

import Control.Monad.Except
import Data.Functor
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Storable as SVec
import Data.Void
import Futhark.Test.Values
import qualified Language.Futhark.Syntax as F
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
  ( binary,
    decimal,
    float,
    hexadecimal,
    signed,
  )

type Parser = Parsec Void T.Text

-- | Parse the name of a primitive type.  Does *not* consume any
-- trailing whitespace, nor does it permit any internal whitespace.
parsePrimType :: Parser F.PrimType
parsePrimType =
  choice
    [ "i8" $> F.Signed F.Int8,
      "i16" $> F.Signed F.Int16,
      "i32" $> F.Signed F.Int32,
      "i64" $> F.Signed F.Int64,
      "u8" $> F.Unsigned F.Int8,
      "u16" $> F.Unsigned F.Int16,
      "u32" $> F.Unsigned F.Int32,
      "u64" $> F.Unsigned F.Int64,
      "f32" $> F.FloatType F.Float32,
      "f64" $> F.FloatType F.Float64,
      "bool" $> F.Bool
    ]

parseInteger :: Parser Integer
parseInteger =
  signed (pure ()) $
    choice
      [ "0b" *> binary,
        "0x" *> hexadecimal,
        decimal
      ]

parseIntConst :: Parser F.PrimValue
parseIntConst = do
  x <- parseInteger
  notFollowedBy $ "f32" <|> "f64" <|> "." <|> "e"
  choice
    [ signedV F.Int8Value x "i8",
      signedV F.Int16Value x "i16",
      signedV F.Int32Value x "i32",
      signedV F.Int64Value x "i64",
      unsignedV F.Int8Value x "u8",
      unsignedV F.Int16Value x "u16",
      unsignedV F.Int32Value x "u32",
      unsignedV F.Int64Value x "u64",
      signedV F.Int32Value x ""
    ]
  where
    signedV mk x suffix =
      suffix $> F.SignedValue (mk (fromInteger x))
    unsignedV mk x suffix =
      suffix $> F.UnsignedValue (mk (fromInteger x))

parseFloatConst :: Parser F.PrimValue
parseFloatConst =
  choice
    [ "f32.nan" $> F.FloatValue (F.Float32Value (0 / 0)),
      "f64.nan" $> F.FloatValue (F.Float64Value (0 / 0)),
      "f32.inf" $> F.FloatValue (F.Float32Value (1 / 0)),
      "f64.inf" $> F.FloatValue (F.Float64Value (1 / 0)),
      "-f32.inf" $> F.FloatValue (F.Float32Value (-1 / 0)),
      "-f64.inf" $> F.FloatValue (F.Float64Value (-1 / 0)),
      numeric
    ]
  where
    numeric = do
      x <-
        signed (pure ()) $ choice [try float, fromInteger <$> decimal]
      choice
        [ floatV F.Float32Value x "f32",
          floatV F.Float64Value x "f64",
          floatV F.Float64Value x ""
        ]

    floatV mk x suffix =
      suffix $> F.FloatValue (mk (realToFrac (x :: Double)))

-- | Parse a primitive value.  Does *not* consume any trailing
-- whitespace, nor does it permit any internal whitespace.
parsePrimValue :: Parser F.PrimValue
parsePrimValue =
  choice
    [ try parseIntConst,
      parseFloatConst,
      "true" $> F.BoolValue True,
      "false" $> F.BoolValue False
    ]

lexeme :: Parser () -> Parser a -> Parser a
lexeme sep p = p <* sep

inBrackets :: Parser () -> Parser a -> Parser a
inBrackets sep = between (lexeme sep "[") (lexeme sep "]")

-- | Parse a type.  Does *not* consume any trailing whitespace, nor
-- does it permit any internal whitespace.
parseType :: Parser ValueType
parseType = ValueType <$> many parseDim <*> parsePrimType
  where
    parseDim = fromInteger <$> ("[" *> parseInteger <* "]")

parseEmpty :: Parser Value
parseEmpty = do
  ValueType dims t <- parseType
  unless (product dims == 0) $ fail "Expected at least one empty dimension"
  pure $ case t of
    F.Signed F.Int8 -> Int8Value (SVec.fromList dims) mempty
    F.Signed F.Int16 -> Int16Value (SVec.fromList dims) mempty
    F.Signed F.Int32 -> Int32Value (SVec.fromList dims) mempty
    F.Signed F.Int64 -> Int64Value (SVec.fromList dims) mempty
    F.Unsigned F.Int8 -> Word8Value (SVec.fromList dims) mempty
    F.Unsigned F.Int16 -> Word16Value (SVec.fromList dims) mempty
    F.Unsigned F.Int32 -> Word32Value (SVec.fromList dims) mempty
    F.Unsigned F.Int64 -> Word64Value (SVec.fromList dims) mempty
    F.FloatType F.Float32 -> Float32Value (SVec.fromList dims) mempty
    F.FloatType F.Float64 -> Float64Value (SVec.fromList dims) mempty
    F.Bool -> BoolValue (SVec.fromList dims) mempty

-- | Parse a value, given a post-lexeme parser for whitespace.
parseValue :: Parser () -> Parser Value
parseValue sep =
  choice
    [ putValue' $ lexeme sep parsePrimValue,
      putValue' $ inBrackets sep (parseValue sep `sepBy` lexeme sep ","),
      lexeme sep $ "empty(" *> parseEmpty <* ")"
    ]
  where
    putValue' :: PutValue v => Parser v -> Parser Value
    putValue' p = do
      o <- getOffset
      x <- p
      case putValue x of
        Nothing ->
          parseError . FancyError o . S.singleton $
            ErrorFail "array is irregular or has elements of multiple types."
        Just v ->
          pure v
