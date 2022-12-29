{
{-# OPTIONS_GHC -w #-}
-- | The Futhark lexer.  Takes a string, produces a list of tokens with position information.
module Language.Futhark.Parser.Lexer
  ( Token(..)
  , scanTokens
  , scanTokensText
  ) where

import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Char (ord, toLower, digitToInt)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8)
import Data.Loc (Loc (..), L(..), Pos(..))
import Data.Function (fix)

import Language.Futhark.Core (Int8, Int16, Int32, Int64,
                              Word8, Word16, Word32, Word64,
                              Name, nameFromText, nameToText)
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
@intlit = @hexlit|@binlit|@declit|@romlit
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
  @doc                     { tokenM $ pure . DOC . T.unpack . T.unlines .
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

  @declit                  { tokenS $ \s -> NATLIT (nameFromText s) (readIntegral s) }
  @intlit i8               { tokenM $ pure . I8LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i16              { tokenM $ pure . I16LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i32              { tokenM $ pure . I32LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i64              { tokenM $ pure . I64LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit u8               { tokenM $ pure . U8LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u16              { tokenM $ pure . U16LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u32              { tokenM $ pure . U32LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u64              { tokenM $ pure . U64LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit                  { tokenM $ pure . INTLIT . readIntegral . T.filter (/= '_') }

  [\n[^\.]] ^ @reallit f16     { tokenM $ fmap F16LIT . tryRead "f16" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  [\n[^\.]] ^ @reallit f32     { tokenM $ fmap F32LIT . tryRead "f32" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  [\n[^\.]] ^ @reallit f64     { tokenM $ fmap F64LIT . tryRead "f64" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  [\n[^\.]] ^ @reallit         { tokenM $ fmap FLOATLIT . tryRead "f64" . suffZero . T.filter (/= '_') }
  @hexreallit f16          { tokenM $ fmap F16LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit f32          { tokenM $ fmap F32LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit f64          { tokenM $ fmap F64LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit              { tokenM $ fmap FLOATLIT . readHexRealLit . T.filter (/= '_') }
  "'" @charlit "'"         { tokenM $ fmap CHARLIT . tryRead "char" }
  \" @stringcharlit* \"    { tokenM $ fmap (STRINGLIT . T.pack) . tryRead "string"  }

  @identifier              { tokenS keyword }
  "#" @identifier          { tokenS $ CONSTRUCTOR . nameFromText . T.drop 1 }

  @binop                   { tokenM $ pure . symbol [] . nameFromText }
  @qualbinop               { tokenM $ \s -> do (qs,k) <- mkQualId s; pure (symbol qs k) }
{

getToken :: Alex (Lexeme Token)
getToken = do
  inp@(_,_,_,n) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> do pos <- alexGetPos
                  pure (pos, pos, EOF)
    AlexError (pos,_,_,_) ->
      alexError (Loc pos pos) "Invalid lexical syntax."
    AlexSkip  inp' _len -> do
      alexSetInput inp'
      getToken
    AlexToken inp'@(_,_,_,n') _ action -> let len = n'-n in do
      alexSetInput inp'
      action inp len

scanTokens :: Pos -> BS.ByteString -> Either LexerError ([L Token], Pos)
scanTokens pos str =
  runAlex pos str $ do
  fix $ \loop -> do
    tok <- getToken
    case tok of
      (start, end, EOF) ->
        pure ([], end)
      (start, end, t) -> do
        (rest, endpos) <- loop
        pure (L (Loc start end) t : rest, endpos)

-- | Given a starting position, produce tokens from the given text (or
-- a lexer error).  Returns the final position.
scanTokensText :: Pos -> T.Text -> Either LexerError ([L Token], Pos)
scanTokensText pos = scanTokens pos . BS.fromStrict . T.encodeUtf8
}
