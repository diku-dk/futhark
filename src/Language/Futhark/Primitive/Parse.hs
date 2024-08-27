-- | Parsers for primitive values and types.
module Language.Futhark.Primitive.Parse
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
import Data.Text qualified as T
import Data.Void
import Futhark.Util.Pretty
import Language.Futhark.Primitive
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | Is this character a valid member of an identifier?
constituent :: Char -> Bool
constituent c = isAlphaNum c || (c `elem` ("_/'+-=!&^.<>*|%" :: String))

-- | Consume whitespace (including skipping line comments).
whitespace :: Parsec Void T.Text ()
whitespace = L.space space1 (L.skipLineComment "--") empty

-- | Consume whitespace after the provided parser, if it succeeds.
lexeme :: Parsec Void T.Text a -> Parsec Void T.Text a
lexeme = try . L.lexeme whitespace

-- | @keyword k@ parses @k@, which must not be immediately followed by
-- a 'constituent' character.  This ensures that @iff@ is not seen as
-- the @if@ keyword followed by @f@.  Sometimes called the "maximum
-- munch" rule.
keyword :: T.Text -> Parsec Void T.Text ()
keyword s = lexeme $ chunk s *> notFollowedBy (satisfy constituent)

-- | Parse an integer value.
pIntValue :: Parsec Void T.Text IntValue
pIntValue = try $ do
  x <- L.signed (pure ()) L.decimal
  t <- pIntType
  pure $ intValue t (x :: Integer)

-- | Parse a floating-point value.
pFloatValue :: Parsec Void T.Text FloatValue
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

-- | Parse a boolean value.
pBoolValue :: Parsec Void T.Text Bool
pBoolValue =
  choice
    [ keyword "true" $> True,
      keyword "false" $> False
    ]

-- | Defined in this module for convenience.
pPrimValue :: Parsec Void T.Text PrimValue
pPrimValue =
  choice
    [ FloatValue <$> pFloatValue,
      IntValue <$> pIntValue,
      BoolValue <$> pBoolValue,
      UnitValue <$ try (lexeme "(" *> lexeme ")")
    ]
    <?> "primitive value"

-- | Parse a floating-point type.
pFloatType :: Parsec Void T.Text FloatType
pFloatType = choice $ map p allFloatTypes
  where
    p t = keyword (prettyText t) $> t

-- | Parse an integer type.
pIntType :: Parsec Void T.Text IntType
pIntType = choice $ map p allIntTypes
  where
    p t = keyword (prettyText t) $> t

-- | Parse a primitive type.
pPrimType :: Parsec Void T.Text PrimType
pPrimType =
  choice [p Bool, p Unit, FloatType <$> pFloatType, IntType <$> pIntType]
  where
    p t = keyword (prettyText t) $> t
