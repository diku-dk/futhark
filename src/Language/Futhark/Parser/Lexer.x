{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}
-- | The Futhark lexer.  Takes a string, produces a list of tokens with position information.
module Language.Futhark.Parser.Lexer
  ( Token(..)
  , L(..)
  , scanTokens
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Char (ord, toLower)
import Data.Loc hiding (L)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8)
import Data.Bits
import Data.Function (fix)
import Data.List
import Data.Monoid

import Language.Futhark.Core (Int8, Int16, Int32, Int64, Name, nameFromText, nameToText)
import Language.Futhark.Attributes (leadingOperator)
import Language.Futhark.Syntax (BinOp(..))

}

%wrapper "monad-bytestring"

@charlit = ($printable#['\\]|\\($printable|[0-9]+))
@stringcharlit = ($printable#[\"\\]|\\($printable|[0-9]+)|\n)
@hexlit = 0[xX][0-9a-fA-F]+
@declit = [0-9]+
@binlit = 0[bB][01]+
@intlit = @hexlit|@binlit|@declit
@reallit = (([0-9]+("."[0-9]*)?))([eE][\+\-]?[0-9]+)?

@field = [a-zA-Z0-9] [a-zA-Z0-9_]*

@identifier = [a-zA-Z] [a-zA-Z0-9_']* | "_" [a-zA-Z0-9] [a-zA-Z0-9_']*
@qualidentifier = (@identifier ".")+ @identifier

@unop = ("!"|"~")
@qualunop = (@identifier ".")+ @unop

@symbols = ("+"|"-"|"*"|"/"|"%"|"="|"!"|">"|"<"|"|"|"&"|"^")
@binop = @symbols+
@qualbinop = (@identifier ".")+ @binop

tokens :-

  $white+                               ;
  "--"[^\n]*                            ;
  "="                      { tokenC EQU }
  "("                      { tokenC LPAR }
  ")"                      { tokenC RPAR }
  ")["                     { tokenC RPAR_THEN_LBRACKET }
  "["                      { tokenC LBRACKET }
  "]"                      { tokenC RBRACKET }
  "{"                      { tokenC LCURLY }
  "}"                      { tokenC RCURLY }
  ","                      { tokenC COMMA }
  "_"                      { tokenC UNDERSCORE }
  "->"                     { tokenC RIGHT_ARROW }
  "<-"                     { tokenC LEFT_ARROW }
  ":"                      { tokenC COLON }
  "@"                      { tokenC AT }
  "\"                      { tokenC BACKSLASH }
  "#"                      { tokenC HASH }

  @declit                  { tokenM $ return . DECLIT . readIntegral }

  @intlit i8               { tokenM $ return . I8LIT . readIntegral . T.takeWhile (/='i') }
  @intlit i16              { tokenM $ return . I16LIT . readIntegral . T.takeWhile (/='i') }
  @intlit i32              { tokenM $ return . I32LIT . readIntegral . T.takeWhile (/='i') }
  @intlit i64              { tokenM $ return . I64LIT . readIntegral . T.takeWhile (/='i') }
  @intlit u8               { tokenM $ return . U8LIT . readIntegral . T.takeWhile (/='u') }
  @intlit u16              { tokenM $ return . U16LIT . readIntegral . T.takeWhile (/='u') }
  @intlit u32              { tokenM $ return . U32LIT . readIntegral . T.takeWhile (/='u') }
  @intlit u64              { tokenM $ return . U64LIT . readIntegral . T.takeWhile (/='u') }
  @intlit                  { tokenM $ return . INTLIT . readIntegral }
  @reallit f32             { tokenM $ fmap F32LIT . tryRead "f32" . suffZero . T.takeWhile (/='f') }
  @reallit f64             { tokenM $ fmap F64LIT . tryRead "f64" . suffZero . T.takeWhile (/='f') }
  @reallit                 { tokenM $ fmap REALLIT . tryRead "f64" . suffZero }
  "'" @charlit "'"         { tokenM $ fmap CHARLIT . tryRead "char" }
  \" @stringcharlit* \"    { tokenM $ fmap STRINGLIT . tryRead "string"  }

  @identifier              { tokenS keyword }
  @identifier "["          { tokenM $ fmap INDEXING . indexing . T.takeWhile (/='[') }
  @qualidentifier          { tokenM $ fmap (uncurry QUALID) . mkQualId }
  @qualidentifier "["      { tokenM $ fmap (uncurry QUALINDEXING) . mkQualId . T.takeWhile (/='[') }

  @unop                    { tokenS $ UNOP . nameFromText }
  @qualunop                { tokenM $ fmap (uncurry QUALUNOP) . mkQualId }

  @binop                   { tokenM $ return . symbol [] . nameFromText }
  @qualbinop               { tokenM $ \s -> do (qs,k) <- mkQualId s; return (symbol qs k) }
{

keyword :: T.Text -> Token
keyword s =
  case s of
    "true"         -> TRUE
    "false"        -> FALSE
    "if"           -> IF
    "then"         -> THEN
    "else"         -> ELSE
    "let"          -> LET
    "loop"         -> LOOP
    "in"           -> IN
    "default"      -> DEFAULT
    "fun"          -> FUN
    "for"          -> FOR
    "do"           -> DO
    "with"         -> WITH

    "iota"         -> IOTA
    "shape"        -> SHAPE
    "replicate"    -> REPLICATE
    "reshape"      -> RESHAPE
    "rearrange"    -> REARRANGE
    "transpose"    -> TRANSPOSE
    "rotate"       -> ROTATE
    "map"          -> MAP
    "reduce"       -> REDUCE
    "reduceComm"   -> REDUCECOMM
    "zip"          -> ZIP
    "zipWith"      -> ZIPWITH
    "unzip"        -> UNZIP
    "unsafe"       -> UNSAFE
    "scan"         -> SCAN
    "split"        -> SPLIT
    "concat"       -> CONCAT
    "filter"       -> FILTER
    "partition"    -> PARTITION
    "empty"        -> EMPTY
    "copy"         -> COPY
    "while"        -> WHILE
    "streamMap"    -> STREAM_MAP
    "streamMapPer" -> STREAM_MAPPER
    "streamRed"    -> STREAM_RED
    "streamRedPer" -> STREAM_REDPER
    "streamSeq"    -> STREAM_SEQ
    "write"        -> WRITE
    "include"      -> INCLUDE
    "import"       -> IMPORT
    "type"         -> TYPE
    "entry"        -> ENTRY
    "module"       -> MODULE
    "val"          -> VAL
    "open"         -> OPEN
    _              -> ID $ nameFromText s

indexing :: T.Text -> Alex Name
indexing s = case keyword s of
  ID v -> return v
  _    -> fail $ "Cannot index keyword '" ++ T.unpack s ++ "'."

mkQualId :: T.Text -> Alex ([Name], Name)
mkQualId s = case reverse $ T.splitOn "." s of
  []   -> fail "mkQualId: no components"
  k:qs -> return (map nameFromText (reverse qs), nameFromText k)

-- | Suffix a zero if the last character is dot.
suffZero :: T.Text -> T.Text
suffZero s = if T.last s == '.' then s <> "0" else s

tryRead :: Read a => String -> T.Text -> Alex a
tryRead desc s = case reads s' of
  [(x, "")] -> return x
  _         -> fail $ "Invalid " ++ desc ++ " literal: " ++ T.unpack s
  where s' = T.unpack s

readIntegral :: Integral a => T.Text -> a
readIntegral s
  | "0x" `T.isPrefixOf` s || "0X" `T.isPrefixOf` s =
      T.foldl (another hex_digits) 0 (T.drop 2 s)
  | "0b" `T.isPrefixOf` s || "0b" `T.isPrefixOf` s =
      T.foldl (another binary_digits) 0 (T.drop 2 s)
  | otherwise =
      T.foldl (another decimal_digits) 0 s
      where another digits acc c = acc * base + maybe 0 fromIntegral (elemIndex (toLower c) digits)
              where base = genericLength digits

            binary_digits = ['0', '1']
            decimal_digits = ['0'..'9']
            hex_digits = decimal_digits ++ ['a'..'f']

tokenC v  = tokenS $ const v

tokenS f = tokenM $ return . f

tokenM :: (T.Text -> Alex a)
       -> (AlexPosn, b, ByteString.ByteString, c)
       -> Int64
       -> Alex ((Int, Int, Int), (Int, Int, Int), a)
tokenM f (AlexPn addr line col, _, s, _) len = do
  x <- f $ T.decodeUtf8 $ BS.toStrict $ BS.take len s
  return (pos, pos, x)
  where pos = (line, col, addr)

symbol :: [Name] -> Name -> Token
symbol [] q
  | nameToText q == "*" = ASTERISK
  | nameToText q == "-" = NEGATE
  | nameToText q == "<" = LTH
  | nameToText q == ">" = GTH
  | nameToText q == "<=" = LEQ
  | nameToText q == ">=" = GEQ
  | otherwise = SYMBOL (leadingOperator q) [] q
symbol qs q = SYMBOL (leadingOperator q) qs q

alexEOF = return ((0,0,0), (0,0,0), EOF)

-- | A value tagged with a source location.
data L a = L SrcLoc a deriving (Show)

instance Eq a => Eq (L a) where
  L _ x == L _ y = x == y

instance Located (L a) where
  locOf (L (SrcLoc loc) _) = loc

-- | A lexical token.  It does not itself contain position
-- information, so in practice the parser will consume tokens tagged
-- with a source position.
data Token = ID Name
           | INDEXING Name
           | QUALID [Name] Name
           | QUALINDEXING [Name] Name
           | UNOP Name
           | QUALUNOP [Name] Name
           | SYMBOL BinOp [Name] Name

           | DECLIT Integer
           | STRINGLIT String
           | INTLIT Int64
           | I8LIT Int8
           | I16LIT Int16
           | I32LIT Int32
           | I64LIT Int64
           | U8LIT Int8
           | U16LIT Int16
           | U32LIT Int32
           | U64LIT Int64
           | REALLIT Double
           | F32LIT Float
           | F64LIT Double
           | CHARLIT Char

           | COLON
           | AT
           | BACKSLASH
           | HASH
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
           | LEFT_ARROW

           | EQU
           | ASTERISK
           | NEGATE
           | LTH
           | GTH
           | LEQ
           | GEQ

           | DEFAULT
           | IF
           | THEN
           | ELSE
           | LET
           | LOOP
           | IN
           | FUN
           | FOR
           | DO
           | WITH
           | SHAPE
           | IOTA
           | REPLICATE
           | MAP
           | REDUCE
           | REDUCECOMM
           | RESHAPE
           | REARRANGE
           | TRANSPOSE
           | ROTATE
           | ZIPWITH
           | ZIP
           | UNZIP
           | UNSAFE
           | SCAN
           | SPLIT
           | CONCAT
           | FILTER
           | PARTITION
           | TRUE
           | FALSE
           | EMPTY
           | COPY
           | WHILE
           | STREAM_MAP
           | STREAM_MAPPER
           | STREAM_RED
           | STREAM_REDPER
           | STREAM_SEQ
           | WRITE
           | INCLUDE
           | IMPORT
           | ENTRY
           | TYPE
           | MODULE
           | VAL
           | OPEN

           | EOF

             deriving (Show, Eq)

-- The Alex wrapper only works on ByteStrings, so we have to encode
-- the Text as UTF-8.  Ugh.
scanTokens :: FilePath -> T.Text -> Either String [L Token]
scanTokens file str = runAlex (BS.fromStrict $ T.encodeUtf8 str) $ do
  fix $ \loop -> do
    tok <- alexMonadScan
    case tok of
      (start, end, EOF) ->
        return []
      (start, end, t) -> do
        rest <- loop
        return $ L (pos start end) t : rest
  where pos start end = SrcLoc $ Loc (posnToPos start) (posnToPos end)
        posnToPos (line, col, addr) = Pos file line col addr
}
