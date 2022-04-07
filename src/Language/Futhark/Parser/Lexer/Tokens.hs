{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

-- | Definition of the tokens used in the lexer.
--
-- Also defines other useful building blocks for constructing tokens.
module Language.Futhark.Parser.Lexer.Tokens
  ( Token (..),
    Lexeme,
    fromRoman,
    symbol,
    mkQualId,
    tokenPosM,
    tokenM,
    tokenC,
    keyword,
    tokenS,
    indexing,
    suffZero,
    tryRead,
    readIntegral,
    readHexRealLit,
  )
where

import qualified Data.ByteString.Lazy as BS
import Data.Char (digitToInt, ord)
import Data.Either
import Data.List (find, foldl')
import Data.Loc (Loc (..), Pos (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
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
import Language.Futhark.Parser.Lexer.Wrapper
import Language.Futhark.Prop (leadingOperator)
import Language.Futhark.Syntax (BinOp, nameFromText, nameToText)
import Numeric.Half
import Prelude hiding (exponent)

-- | A lexical token.  It does not itself contain position
-- information, so in practice the parser will consume tokens tagged
-- with a source position.
data Token
  = ID Name
  | INDEXING Name
  | QUALINDEXING [Name] Name
  | QUALPAREN [Name] Name
  | SYMBOL BinOp [Name] Name
  | CONSTRUCTOR Name
  | PROJ_INTFIELD Name
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
  | RPAR_THEN_LBRACKET
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
  | DOC String
  | EOF
  | HOLE
  deriving (Show, Eq, Ord)

keyword :: T.Text -> Token
keyword s =
  case s of
    "true" -> TRUE
    "false" -> FALSE
    "if" -> IF
    "then" -> THEN
    "else" -> ELSE
    "def" -> DEF
    "let" -> LET
    "loop" -> LOOP
    "in" -> IN
    "val" -> VAL
    "for" -> FOR
    "do" -> DO
    "with" -> WITH
    "local" -> LOCAL
    "open" -> OPEN
    "include" -> INCLUDE
    "import" -> IMPORT
    "type" -> TYPE
    "entry" -> ENTRY
    "module" -> MODULE
    "while" -> WHILE
    "assert" -> ASSERT
    "match" -> MATCH
    "case" -> CASE
    _ -> ID $ nameFromText s

indexing :: (Loc, T.Text) -> Alex Name
indexing (loc, s) = case keyword s of
  ID v -> pure v
  _ -> alexError loc $ "Cannot index keyword '" ++ T.unpack s ++ "'."

mkQualId :: T.Text -> Alex ([Name], Name)
mkQualId s = case reverse $ T.splitOn "." s of
  [] -> error "mkQualId: no components"
  k : qs -> pure (map nameFromText (reverse qs), nameFromText k)

-- | Suffix a zero if the last character is dot.
suffZero :: T.Text -> T.Text
suffZero s = if T.last s == '.' then s <> "0" else s

tryRead :: Read a => String -> T.Text -> Alex a
tryRead desc s = case reads s' of
  [(x, "")] -> pure x
  _ -> error $ "Invalid " ++ desc ++ " literal: `" ++ T.unpack s ++ "'."
  where
    s' = T.unpack s

readIntegral :: Integral a => T.Text -> a
readIntegral s
  | "0x" `T.isPrefixOf` s || "0X" `T.isPrefixOf` s = parseBase 16 (T.drop 2 s)
  | "0b" `T.isPrefixOf` s || "0B" `T.isPrefixOf` s = parseBase 2 (T.drop 2 s)
  | "0r" `T.isPrefixOf` s || "0R" `T.isPrefixOf` s = fromRoman (T.drop 2 s)
  | otherwise = parseBase 10 s
  where
    parseBase base = T.foldl (\acc c -> acc * base + fromIntegral (digitToInt c)) 0

tokenC :: a -> (Pos, Char, BS.ByteString, Int64) -> Int64 -> Alex (Lexeme a)
tokenC v = tokenS $ const v

tokenS :: (T.Text -> a) -> (Pos, Char, BS.ByteString, Int64) -> Int64 -> Alex (Lexeme a)
tokenS f = tokenM $ pure . f

type Lexeme a = (Pos, Pos, a)

tokenM ::
  (T.Text -> Alex a) ->
  (Pos, Char, BS.ByteString, Int64) ->
  Int64 ->
  Alex (Lexeme a)
tokenM f = tokenPosM (f . snd)

tokenPosM ::
  ((Loc, T.Text) -> Alex a) ->
  (Pos, Char, BS.ByteString, Int64) ->
  Int64 ->
  Alex (Lexeme a)
tokenPosM f (pos, _, s, _) len = do
  x <- f (Loc pos pos', T.decodeUtf8 $ BS.toStrict s')
  pure (pos, pos', x)
  where
    pos' = advance pos s'
    s' = BS.take len s

advance :: Pos -> BS.ByteString -> Pos
advance orig_pos = foldl' advance' orig_pos . init . BS.unpack
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

romanNumerals :: Integral a => [(T.Text, a)]
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

fromRoman :: Integral a => T.Text -> a
fromRoman s =
  case find ((`T.isPrefixOf` s) . fst) romanNumerals of
    Nothing -> 0
    Just (d, n) -> n + fromRoman (T.drop (T.length d) s)

readHexRealLit :: RealFloat a => T.Text -> Alex a
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
               in pure totalVal
            _ -> error "bad hex real literal"
