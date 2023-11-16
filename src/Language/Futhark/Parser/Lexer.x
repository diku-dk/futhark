{
{-# OPTIONS_GHC -w #-}
-- | The Futhark lexer.  Takes a string, produces a list of tokens with position information.
module Language.Futhark.Parser.Lexer
  ( Token(..)
  , getToken
  , scanTokensText
  ) where

import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Char (chr, ord, toLower)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8)
import Data.Loc (Loc (..), L(..), Pos(..))
import Data.Function (fix)

import Language.Futhark.Core (Int8, Int16, Int32, Int64,
                              Word8, Word16, Word32, Word64,
                              Name, nameFromText, nameToText, nameFromString)
import Language.Futhark.Prop (leadingOperator)
import Language.Futhark.Syntax (BinOp(..))
import Language.Futhark.Parser.Lexer.Wrapper
import Language.Futhark.Parser.Lexer.Tokens

}

@charlit = ($printable#['\\]|\\($printable|[0-9]+))
@stringcharlit = ($printable#[\"\\]|\\($printable|[0-9]+))
@hexlit = 0[xX][0-9a-fA-F][0-9a-fA-F_]*
@declit = [0-9][0-9_]*
@binlit = 0[bB][01][01_]*
@romlit = 0[rR][IVXLCDM][IVXLCDM_]*
@reallit = (([0-9][0-9_]*("."[0-9][0-9_]*)?))([eE][\+\-]?[0-9]+)?
@hexreallit = 0[xX][0-9a-fA-F][0-9a-fA-F_]*"."[0-9a-fA-F][0-9a-fA-F_]*([pP][\+\-]?[0-9_]+)

@field = [a-zA-Z0-9] [a-zA-Z0-9_]*

@constituent = [a-zA-Z0-9_']
@identifier = [a-zA-Z] @constituent* | "_" [a-zA-Z0-9] @constituent*
@qualidentifier = (@identifier ".")+ @identifier

$opchar = [\+\-\*\/\%\=\!\>\<\|\&\^\.]
@binop = ($opchar # \.) $opchar*
@qualbinop = (@identifier ".")+ @binop

@space = [\ \t\f\v]
@doc = "-- |".*(\n@space*"--".*)*

tokens :-

  $white+                               ;
  @doc                     { tokenS $ DOC . T.intercalate "\n" .
                                      map (T.drop 3 . T.stripStart) .
                                           T.split (== '\n') . ("--"<>) .
                                           T.drop 4 }
  "--".*                   { tokenS COMMENT }
  "="                      { tokenC EQU }
  "("                      { tokenC LPAR }
  ")"                      { tokenC RPAR }
  [a-zA-Z0-9_'] ^ "["      { tokenC INDEXING }
  \)            ^ "["      { tokenC INDEXING }
  \]            ^ "["      { tokenC INDEXING }
  "["                      { tokenC LBRACKET }
  "]"                      { tokenC RBRACKET }
  "{"                      { tokenC LCURLY }
  "}"                      { tokenC RCURLY }
  ","                      { tokenC COMMA }
  "_"                      { tokenC UNDERSCORE }
  "?"                      { tokenC QUESTION_MARK }
  "->"                     { tokenC RIGHT_ARROW }
  ":"                      { tokenC COLON }
  ":>"                     { tokenC COLON_GT }
  "\"                      { tokenC BACKSLASH }
  "~"                      { tokenC TILDE }
  "'"                      { tokenC APOSTROPHE }
  "'^"                     { tokenC APOSTROPHE_THEN_HAT }
  "'~"                     { tokenC APOSTROPHE_THEN_TILDE }
  "`"                      { tokenC BACKTICK }
  "#["                     { tokenC HASH_LBRACKET }
  "..<"                    { tokenC TWO_DOTS_LT }
  "..>"                    { tokenC TWO_DOTS_GT }
  "..."                    { tokenC THREE_DOTS }
  ".."                     { tokenC TWO_DOTS }
  "."                      { tokenC DOT }
  "!"                      { tokenC BANG }
  "$"                      { tokenC DOLLAR }
  "???"                    { tokenC HOLE }

  @declit i8               { decToken I8LIT . BS.dropEnd 2 }
  @binlit i8               { binToken I8LIT . BS.drop 2 . BS.dropEnd 2 }
  @hexlit i8               { hexToken I8LIT . BS.drop 2 . BS.dropEnd 2 }
  @romlit i8               { romToken I8LIT . BS.drop 2 . BS.dropEnd 2 }

  @declit i16              { decToken I16LIT . BS.dropEnd 3 }
  @binlit i16              { binToken I16LIT . BS.drop 2 . BS.dropEnd 3 }
  @hexlit i16              { hexToken I16LIT . BS.drop 2 . BS.dropEnd 3 }
  @romlit i16              { romToken I16LIT . BS.drop 2 . BS.dropEnd 3 }

  @declit i32              { decToken I32LIT . BS.dropEnd 3 }
  @binlit i32              { binToken I32LIT . BS.drop 2 . BS.dropEnd 3 }
  @hexlit i32              { hexToken I32LIT . BS.drop 2 . BS.dropEnd 3 }
  @romlit i32              { romToken I32LIT . BS.drop 2 . BS.dropEnd 3 }

  @declit i64              { decToken I64LIT . BS.dropEnd 3 }
  @binlit i64              { binToken I64LIT . BS.drop 2 . BS.dropEnd 3 }
  @hexlit i64              { hexToken I64LIT . BS.drop 2 . BS.dropEnd 3 }
  @romlit i64              { romToken I64LIT . BS.drop 2 . BS.dropEnd 3 }

  @declit u8               { decToken U8LIT . BS.dropEnd 2 }
  @binlit u8               { binToken U8LIT . BS.drop 2 . BS.dropEnd 2 }
  @hexlit u8               { hexToken U8LIT . BS.drop 2 . BS.dropEnd 2 }
  @romlit u8               { romToken U8LIT . BS.drop 2 . BS.dropEnd 2 }

  @declit u16              { decToken U16LIT . BS.dropEnd 3 }
  @binlit u16              { binToken U16LIT . BS.drop 2 . BS.dropEnd 3 }
  @hexlit u16              { hexToken U16LIT . BS.drop 2 . BS.dropEnd 3 }
  @romlit u16              { romToken U16LIT . BS.drop 2 . BS.dropEnd 3 }

  @declit u32              { decToken U32LIT . BS.dropEnd 3 }
  @binlit u32              { binToken U32LIT . BS.drop 2 . BS.dropEnd 3 }
  @hexlit u32              { hexToken U32LIT . BS.drop 2 . BS.dropEnd 3 }
  @romlit u32              { romToken U32LIT . BS.drop 2 . BS.dropEnd 3 }

  @declit u64              { decToken U64LIT . BS.dropEnd 3 }
  @binlit u64              { binToken U64LIT . BS.drop 2 . BS.dropEnd 3 }
  @hexlit u64              { hexToken U64LIT . BS.drop 2 . BS.dropEnd 3 }
  @romlit u64              { romToken U64LIT . BS.drop 2 . BS.dropEnd 3 }

  @declit                  { \s -> decToken (NATLIT (nameFromBS s)) s }
  @binlit                  { binToken INTLIT . BS.drop 2 }
  @hexlit                  { hexToken INTLIT . BS.drop 2 }
  @romlit                  { romToken INTLIT . BS.drop 2 }

  [\n[^\.]] ^ @reallit f16 { tokenS $ F16LIT . tryRead "f16" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  [\n[^\.]] ^ @reallit f32 { tokenS $ F32LIT . tryRead "f32" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  [\n[^\.]] ^ @reallit f64 { tokenS $ F64LIT . tryRead "f64" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  [\n[^\.]] ^ @reallit     { tokenS $ FLOATLIT . tryRead "f64" . suffZero . T.filter (/= '_') }
  @hexreallit f16          { tokenS $ F16LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit f32          { tokenS $ F32LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit f64          { tokenS $ F64LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit              { tokenS $ FLOATLIT . readHexRealLit . T.filter (/= '_') }
  "'" @charlit "'"         { tokenS $ CHARLIT . tryRead "char" }
  \" @stringcharlit* \"    { tokenS $ STRINGLIT . T.pack . tryRead "string"  }

  "true"                   { tokenC TRUE }
  "false"                  { tokenC FALSE }
  "if"                     { tokenC IF }
  "then"                   { tokenC THEN }
  "else"                   { tokenC ELSE }
  "def"                    { tokenC DEF }
  "let"                    { tokenC LET }
  "loop"                   { tokenC LOOP }
  "in"                     { tokenC IN }
  "val"                    { tokenC VAL }
  "for"                    { tokenC FOR }
  "do"                     { tokenC DO }
  "with"                   { tokenC WITH }
  "local"                  { tokenC LOCAL }
  "open"                   { tokenC OPEN }
  "include"                { tokenC INCLUDE }
  "import"                 { tokenC IMPORT }
  "type"                   { tokenC TYPE }
  "entry"                  { tokenC ENTRY }
  "module"                 { tokenC MODULE }
  "while"                  { tokenC WHILE }
  "assert"                 { tokenC ASSERT }
  "match"                  { tokenC MATCH }
  "case"                   { tokenC CASE }

  @identifier              { tokenS $ ID . nameFromText }

  "#" @identifier          { tokenS $ CONSTRUCTOR . nameFromText . T.drop 1 }

  @binop                   { tokenS $ symbol [] . nameFromText }
  @qualbinop               { tokenS $ uncurry symbol . mkQualId }

  .                        { tokenS ERROR }
{

nameFromBS :: BS.ByteString -> Name
nameFromBS = nameFromString . map (chr . fromIntegral) . BS.unpack

getToken :: AlexInput -> Either LexerError (AlexInput, (Pos, Pos, Token))
getToken state@(pos,c,s,n) =
  case alexScan state 0 of
    AlexEOF -> Right (state, (pos, pos, EOF))
    AlexError (pos,_,_,_) ->
      Left $ LexerError (Loc pos pos) "Invalid lexical syntax."
    AlexSkip state' _len ->
      getToken state'
    AlexToken state'@(pos',_,_,n') _ action -> do
      let x = action (BS.take (n'-n) s)
      x `seq` Right (state', (pos, pos', x))

scanTokens :: Pos -> BS.ByteString -> Either LexerError ([L Token], Pos)
scanTokens pos str = loop $ initialLexerState pos str
  where
   loop s = do
     (s', tok) <- getToken s
     case tok of
       (start, end, EOF) ->
         pure ([], end)
       (start, end, t) -> do
         (rest, endpos) <- loop s'
         pure (L (Loc start end) t : rest, endpos)

-- | Given a starting position, produce tokens from the given text (or
-- a lexer error).  Returns the final position.
scanTokensText :: Pos -> T.Text -> Either LexerError ([L Token], Pos)
scanTokensText pos = scanTokens pos . BS.fromStrict . T.encodeUtf8
}
