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
import Data.Char (ord)
import Data.Loc hiding (L)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8)
import Data.Bits
import Data.Function (fix)

import Language.Futhark.Core (Int8, Int16, Int32, Int64, Name, nameFromText)

}

%wrapper "monad-bytestring"

@charlit = ($printable#['\\]|\\($printable|[0-9]+))
@stringcharlit = ($printable#[\"\\]|\\($printable|[0-9]+)|\n)
@hexlit = 0[xX][0-9a-fA-F]+
@declit = [0-9]+
@intlit = @hexlit|@declit
@reallit = (([0-9]+("."[0-9]+)?))([eE][\+\-]?[0-9]+)?

tokens :-

  $white+                               ;
  "--"[^\n]*                            ;
  "&&"                     { tokenC AND }
  "||"                     { tokenC OR }
  ">>"                     { tokenC SHIFTR }
  ">>>"                    { tokenC ZSHIFTR }
  "<<"                     { tokenC SHIFTL }
  "=>"                     { tokenC ARROW }
  "<-"                     { tokenC SETTO }
  "<="                     { tokenC LEQ }
  ">="                     { tokenC GEQ }
  "+"                      { tokenC PLUS }
  "-"                      { tokenC MINUS }
  "~"                      { tokenC TILDE }
  "*"                      { tokenC TIMES }
  "**"                     { tokenC POW }
  "/"                      { tokenC DIVIDE }
  "%"                      { tokenC MOD }
  "//"                     { tokenC QUOT }
  "%%"                     { tokenC REM }
  "="                      { tokenC EQU }
  "=="                     { tokenC EQU2 }
  "!="                     { tokenC NEQU }
  "<"                      { tokenC LTH }
  ">"                      { tokenC GTH }
  "&"                      { tokenC BAND }
  "|"                      { tokenC BOR }
  "^"                      { tokenC XOR }
  "("                      { tokenC LPAR }
  ")"                      { tokenC RPAR }
  "["                      { tokenC LBRACKET }
  "]"                      { tokenC RBRACKET }
  "{"                      { tokenC LCURLY }
  "}"                      { tokenC RCURLY }
  ","                      { tokenC COMMA }
  "_"                      { tokenC UNDERSCORE }
  "!"                      { tokenC BANG }
  "."                      { tokenC DOT }
  @intlit i8               { tokenS $ I8LIT . readInt8 . T.takeWhile (/='i') }
  @intlit i16              { tokenS $ I16LIT . readInt16 . T.takeWhile (/='i') }
  @intlit i32              { tokenS $ I32LIT . readInt32 . T.takeWhile (/='i') }
  @intlit i64              { tokenS $ I64LIT . readInt64 . T.takeWhile (/='i') }
  @intlit u8               { tokenS $ U8LIT . readInt8 . T.takeWhile (/='u') }
  @intlit u16              { tokenS $ U16LIT . readInt16 . T.takeWhile (/='u') }
  @intlit u32              { tokenS $ U32LIT . readInt32 . T.takeWhile (/='u') }
  @intlit u64              { tokenS $ U64LIT . readInt64 . T.takeWhile (/='u') }
  @intlit                  { tokenS $ INTLIT . readInt64 }
  @reallit f32             { tokenS $ F32LIT . readFloat32 . T.takeWhile (/='f') }
  @reallit f64             { tokenS $ F64LIT . readFloat64 . T.takeWhile (/='f') }
  @reallit                 { tokenS $ REALLIT . readReal }
  [a-zA-Z] [a-zA-Z0-9_']*  { tokenS keyword }
  "'" @charlit "'"         { tokenS $ CHARLIT . read . T.unpack }
  \" @stringcharlit* \"    { tokenS $ STRINGLIT . read . T.unpack }

{

keyword :: T.Text -> Token
keyword s =
  case s of
    "if"           -> IF
    "then"         -> THEN
    "else"         -> ELSE
    "let"          -> LET
    "loop"         -> LOOP
    "in"           -> IN
    "with"         -> WITH
    "default"      -> DEFAULT
    "int"          -> INT
    "float"        -> FLOAT
    "i8"           -> I8
    "i16"          -> I16
    "i32"          -> I32
    "i64"          -> I64
    "u8"           -> U8
    "u16"          -> U16
    "u32"          -> U32
    "u64"          -> U64
    "f32"          -> F32
    "f64"          -> F64
    "bool"         -> BOOL
    "fun"          -> FUN
    "fn"           -> FN
    "for"          -> FOR
    "do"           -> DO
    "True"         -> TRUE
    "False"        -> FALSE
    "abs"          -> ABS
    "signum"       -> SIGNUM

    "iota"         -> IOTA
    "size"         -> SIZE
    "replicate"    -> REPLICATE
    "reshape"      -> RESHAPE
    "rearrange"    -> REARRANGE
    "transpose"    -> TRANSPOSE
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
    "entry"        -> ENTRY
    _              -> ID $ nameFromText s

readReal :: T.Text -> Double
readReal = read . T.unpack

readInt8 :: T.Text -> Int8
readInt8 = read . T.unpack

readInt16 :: T.Text -> Int16
readInt16 = read . T.unpack

readInt32 :: T.Text -> Int32
readInt32 = read . T.unpack

readInt64 :: T.Text -> Int64
readInt64 = read . T.unpack

readFloat32 :: T.Text -> Float
readFloat32 = read . T.unpack

readFloat64 :: T.Text -> Double
readFloat64 = read . T.unpack

-- | A value tagged with a source location.
data L a = L SrcLoc a

instance Eq a => Eq (L a) where
  L _ x == L _ y = x == y

instance Located (L a) where
  locOf (L (SrcLoc loc) _) = loc

-- | A lexical token.  It does not itself contain position
-- information, so in practice the parser will consume tokens tagged
-- with a source position.
data Token = IF
           | THEN
           | ELSE
           | LET
           | LOOP
           | IN
           | INT
           | I8
           | I16
           | I32
           | I64
           | U8
           | U16
           | U32
           | U64
           | BOOL
           | CHAR
           | FLOAT
           | F32
           | F64
           | ID Name
           | STRINGLIT String
           | DEFAULT
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
           | PLUS
           | MINUS
           | TIMES
           | DIVIDE
           | MOD
           | QUOT
           | REM
           | EQU
           | EQU2
           | NEQU
           | LTH
           | GTH
           | LEQ
           | GEQ
           | POW
           | SHIFTL
           | SHIFTR
           | ZSHIFTR
           | BOR
           | BAND
           | XOR
           | LPAR
           | RPAR
           | LBRACKET
           | RBRACKET
           | LCURLY
           | RCURLY
           | COMMA
           | UNDERSCORE
           | FUN
           | FN
           | ARROW
           | SETTO
           | FOR
           | DO
           | WITH
           | SIZE
           | IOTA
           | REPLICATE
           | MAP
           | REDUCE
           | REDUCECOMM
           | RESHAPE
           | REARRANGE
           | TRANSPOSE
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
           | TILDE
           | AND
           | OR
           | EMPTY
           | COPY
           | WHILE
           | STREAM_MAP
           | STREAM_MAPPER
           | STREAM_RED
           | STREAM_REDPER
           | STREAM_SEQ
           | BANG
           | DOT
           | ABS
           | SIGNUM
           | WRITE
           | INCLUDE
           | ENTRY

           | EOF
             deriving (Show, Eq)

tokenC v (AlexPn addr line col, _, _, _) _ =
  return (pos, pos, v)
  where pos = (line, col, addr)

tokenS f (AlexPn addr line col, _, s, _) _ =
  return (pos, pos, f $ T.decodeUtf8 $ BS.toStrict s)
  where pos = (line, col, addr)

alexEOF = return ((0,0,0), (0,0,0), EOF)

-- The Alex wrapper only works on ByteStrings, so we have to encode
-- the Text as UTF-8.  Ugh.
scanTokens :: FilePath -> T.Text -> Either String [L Token]
scanTokens file str = runAlex (BS.fromStrict $ T.encodeUtf8 str) $ do
  fix $ \loop -> do
    tok <- alexMonadScan
    case tok of
      (start, end, EOF) ->
        return [L (pos start end) EOF]
      (start, end, t) -> do
        rest <- loop
        return $ L (pos start end) t : rest
  where pos start end = SrcLoc $ Loc (posnToPos start) (posnToPos end)
        posnToPos (line, col, addr) = Pos file line col addr
}
