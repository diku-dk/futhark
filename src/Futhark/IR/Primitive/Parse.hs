{-# LANGUAGE OverloadedStrings #-}

module Futhark.IR.Primitive.Parse
  ( pPrimValue,
    pPrimType,
    pFloatType,
    pIntType,

    -- * Building blocks
    constituent,
    lexeme,
    keyword,
    whitespace,
  )
where

import Data.Char (isAlphaNum)
import Data.Functor
import qualified Data.Text as T
import Data.Void
import Futhark.IR.Primitive
import Futhark.Util.Pretty hiding (empty)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

constituent :: Char -> Bool
constituent c = isAlphaNum c || (c `elem` ("_/'+-=!&^.<>*|" :: String))

whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = try . L.lexeme whitespace

keyword :: T.Text -> Parser ()
keyword s = lexeme $ chunk s *> notFollowedBy (satisfy constituent)

pIntValue :: Parser IntValue
pIntValue = try $ do
  x <- L.signed (pure ()) L.decimal
  t <- pIntType
  pure $ intValue t (x :: Integer)

pFloatValue :: Parser FloatValue
pFloatValue =
  choice
    [ pNum,
      keyword "f16.nan" $> Float16Value (0 / 0),
      keyword "f16.inf" $> Float16Value (1 / 0),
      keyword "-f16.inf" $> Float16Value (-1 / 0),
      keyword "f32.nan" $> Float32Value (0 / 0),
      keyword "f32.inf" $> Float32Value (1 / 0),
      keyword "-f32.inf" $> Float32Value (-1 / 0),
      keyword "f64.nan" $> Float64Value (0 / 0),
      keyword "f64.inf" $> Float64Value (1 / 0),
      keyword "-f64.inf" $> Float64Value (-1 / 0)
    ]
  where
    pNum = try $ do
      x <- L.signed (pure ()) L.float
      t <- pFloatType
      pure $ floatValue t (x :: Double)

pBoolValue :: Parser Bool
pBoolValue =
  choice
    [ keyword "true" $> True,
      keyword "false" $> False
    ]

-- | Defined in this module for convenience.
pPrimValue :: Parser PrimValue
pPrimValue =
  choice
    [ FloatValue <$> pFloatValue,
      IntValue <$> pIntValue,
      BoolValue <$> pBoolValue,
      UnitValue <$ "()"
    ]
    <?> "primitive value"

pFloatType :: Parser FloatType
pFloatType = choice $ map p allFloatTypes
  where
    p t = keyword (prettyText t) $> t

pIntType :: Parser IntType
pIntType = choice $ map p allIntTypes
  where
    p t = keyword (prettyText t) $> t

pPrimType :: Parser PrimType
pPrimType =
  choice [p Bool, p Unit, FloatType <$> pFloatType, IntType <$> pIntType]
  where
    p t = keyword (prettyText t) $> t
