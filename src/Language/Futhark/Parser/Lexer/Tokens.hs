{-# LANGUAGE Strict #-}

-- | Definition of the tokens used in the lexer.
--
-- Also defines other useful building blocks for constructing tokens.
module Language.Futhark.Parser.Lexer.Tokens
  ( Token (..),
    fromRoman,
    symbol,
    mkQualId,
    tokenC,
    tokenS,
    suffZero,
    tryRead,
    decToken,
    binToken,
    hexToken,
    romToken,
    advance,
    readHexRealLit,
  )
where

import Data.ByteString.Lazy qualified as BS
import Data.Char (ord)
import Data.Either
import Data.List (find)
import Data.Loc (Pos (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T
import Language.Futhark.Core
  ( Int16,
    Int32,
    Int64,
    Int8,
    Name,
    Word16,
    Word32,
    Word64,
    Word8,
  )
import Language.Futhark.Prop (leadingOperator)
import Language.Futhark.Syntax (BinOp, nameFromText, nameToText)
import Numeric.Half
import Prelude hiding (exponent)

-- | A lexical token.  It does not itself contain position
-- information, so in practice the parser will consume tokens tagged
-- with a source position.
data Token
  = ID Name
  | COMMENT T.Text
  | INDEXING -- A left bracket immediately following an identifier.
  | SYMBOL BinOp [Name] Name
  | CONSTRUCTOR Name
  | NATLIT Name Integer
  | INTLIT Integer
  | STRINGLIT T.Text
  | I8LIT Int8
  | I16LIT Int16
  | I32LIT Int32
  | I64LIT Int64
  | U8LIT Word8
  | U16LIT Word16
  | U32LIT Word32
  | U64LIT Word64
  | FLOATLIT Double
  | F16LIT Half
  | F32LIT Float
  | F64LIT Double
  | CHARLIT Char
  | COLON
  | COLON_GT
  | BACKSLASH
  | APOSTROPHE
  | APOSTROPHE_THEN_HAT
  | APOSTROPHE_THEN_TILDE
  | BACKTICK
  | HASH_LBRACKET
  | DOT
  | TWO_DOTS
  | TWO_DOTS_LT
  | TWO_DOTS_GT
  | THREE_DOTS
  | LPAR
  | RPAR
  | LBRACKET
  | RBRACKET
  | LCURLY
  | RCURLY
  | COMMA
  | UNDERSCORE
  | RIGHT_ARROW
  | QUESTION_MARK
  | EQU
  | ASTERISK
  | NEGATE
  | BANG
  | DOLLAR
  | LTH
  | HAT
  | TILDE
  | PIPE
  | IF
  | THEN
  | ELSE
  | DEF
  | LET
  | LOOP
  | IN
  | FOR
  | DO
  | WITH
  | ASSERT
  | TRUE
  | FALSE
  | WHILE
  | INCLUDE
  | IMPORT
  | ENTRY
  | TYPE
  | MODULE
  | VAL
  | OPEN
  | LOCAL
  | MATCH
  | CASE
  | DOC T.Text
  | EOF
  | HOLE
  | ERROR T.Text
  deriving (Show, Eq, Ord)

mkQualId :: T.Text -> ([Name], Name)
mkQualId s = case reverse $ T.splitOn "." s of
  [] -> error "mkQualId: no components"
  k : qs -> (map nameFromText (reverse qs), nameFromText k)

-- | Suffix a zero if the last character is dot.
suffZero :: T.Text -> T.Text
suffZero s = if T.last s == '.' then s <> "0" else s

tryRead :: (Read a) => String -> T.Text -> a
tryRead desc s = case reads s' of
  [(x, "")] -> x
  _ -> error $ "Invalid " ++ desc ++ " literal: `" ++ T.unpack s ++ "'."
  where
    s' = T.unpack s

{-# INLINE tokenC #-}
tokenC :: a -> BS.ByteString -> a
tokenC v _ = v

{-# INLINE decToken #-}
decToken :: (Integral a) => (a -> Token) -> BS.ByteString -> Token
decToken f = f . BS.foldl' digit 0
  where
    digit x c =
      if c >= 48 && c <= 57
        then x * 10 + fromIntegral (c - 48)
        else x

{-# INLINE binToken #-}
binToken :: (Integral a) => (a -> Token) -> BS.ByteString -> Token
binToken f = f . BS.foldl' digit 0
  where
    digit x c =
      if c >= 48 && c <= 49
        then x * 2 + fromIntegral (c - 48)
        else x

{-# INLINE hexToken #-}
hexToken :: (Integral a) => (a -> Token) -> BS.ByteString -> Token
hexToken f = f . BS.foldl' digit 0
  where
    digit x c
      | c >= 48 && c <= 57 =
          x * 16 + fromIntegral (c - 48)
      | c >= 65 && c <= 70 =
          x * 16 + fromIntegral (10 + c - 65)
      | c >= 97 && c <= 102 =
          x * 16 + fromIntegral (10 + c - 97)
      | otherwise =
          x

{-# INLINE romToken #-}
romToken :: (Integral a) => (a -> Token) -> BS.ByteString -> Token
romToken f = tokenS $ f . fromRoman

{-# INLINE tokenS #-}
tokenS :: (T.Text -> a) -> BS.ByteString -> a
tokenS f = f . T.decodeUtf8 . BS.toStrict

advance :: Pos -> BS.ByteString -> Pos
advance orig_pos = BS.foldl' advance' orig_pos . BS.init
  where
    advance' (Pos f !line !col !addr) c
      | c == nl = Pos f (line + 1) 1 (addr + 1)
      | otherwise = Pos f line (col + 1) (addr + 1)
    nl = fromIntegral $ ord '\n'

symbol :: [Name] -> Name -> Token
symbol [] q
  | nameToText q == "*" = ASTERISK
  | nameToText q == "-" = NEGATE
  | nameToText q == "<" = LTH
  | nameToText q == "^" = HAT
  | nameToText q == "|" = PIPE
  | otherwise = SYMBOL (leadingOperator q) [] q
symbol qs q = SYMBOL (leadingOperator q) qs q

romanNumerals :: (Integral a) => [(T.Text, a)]
romanNumerals =
  reverse
    [ ("I", 1),
      ("IV", 4),
      ("V", 5),
      ("IX", 9),
      ("X", 10),
      ("XL", 40),
      ("L", 50),
      ("XC", 90),
      ("C", 100),
      ("CD", 400),
      ("D", 500),
      ("CM", 900),
      ("M", 1000)
    ]

fromRoman :: (Integral a) => T.Text -> a
fromRoman s =
  case find ((`T.isPrefixOf` s) . fst) romanNumerals of
    Nothing -> 0
    Just (d, n) -> n + fromRoman (T.drop (T.length d) s)

readHexRealLit :: (RealFloat a) => T.Text -> a
readHexRealLit s =
  let num = T.drop 2 s
   in -- extract number into integer, fractional and (optional) exponent
      let comps = T.split (`elem` ['.', 'p', 'P']) num
       in case comps of
            [i, f, p] ->
              let runTextReader r = fromInteger . fst . fromRight (error "internal error") . r
                  intPart = runTextReader T.hexadecimal i
                  fracPart = runTextReader T.hexadecimal f
                  exponent = runTextReader (T.signed T.decimal) p

                  fracLen = fromIntegral $ T.length f
                  fracVal = fracPart / (16.0 ** fracLen)
                  totalVal = (intPart + fracVal) * (2.0 ** exponent)
               in totalVal
            _ -> error "bad hex real literal"
