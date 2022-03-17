{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
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
import Data.Bits
import Data.Function (fix)
import Data.List
import Data.Monoid
import Data.Either
import Numeric.Half

import Language.Futhark.Core (Int8, Int16, Int32, Int64,
                              Word8, Word16, Word32, Word64,
                              Name, nameFromText, nameToText)
import Language.Futhark.Prop (leadingOperator)
import Language.Futhark.Syntax (BinOp(..))
import Futhark.Util.Loc hiding (L)
import Language.Futhark.Parser.Lexer.Wrapper
import qualified Data.ByteString.Internal as ByteString (w2c)
import qualified Data.ByteString.Lazy as ByteString

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

@identifier = [a-zA-Z] [a-zA-Z0-9_']* | "_" [a-zA-Z0-9] [a-zA-Z0-9_']*
@qualidentifier = (@identifier ".")+ @identifier

$opchar = [\+\-\*\/\%\=\!\>\<\|\&\^\.]
@binop = ($opchar # \.) $opchar*
@qualbinop = (@identifier ".")+ @binop

@space = [\ \t\f\v]
@doc = "-- |".*(\n@space*"--".*)*

tokens :-

  $white+                               ;
  @doc                     { tokenM $ return . DOC . T.unpack . T.unlines .
                                      map (T.drop 3 . T.stripStart) .
                                           T.split (== '\n') . ("--"<>) .
                                           T.drop 4 }
  "--".*                            ;
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

  @intlit i8               { tokenM $ return . I8LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i16              { tokenM $ return . I16LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i32              { tokenM $ return . I32LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i64              { tokenM $ return . I64LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit u8               { tokenM $ return . U8LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u16              { tokenM $ return . U16LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u32              { tokenM $ return . U32LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u64              { tokenM $ return . U64LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit                  { tokenM $ return . INTLIT . readIntegral . T.filter (/= '_') }

  @reallit f16             { tokenM $ fmap F16LIT . tryRead "f16" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  @reallit f32             { tokenM $ fmap F32LIT . tryRead "f32" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  @reallit f64             { tokenM $ fmap F64LIT . tryRead "f64" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  @reallit                 { tokenM $ fmap FLOATLIT . tryRead "f64" . suffZero . T.filter (/= '_') }
  @hexreallit f16          { tokenM $ fmap F16LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit f32          { tokenM $ fmap F32LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit f64          { tokenM $ fmap F64LIT . readHexRealLit . T.filter (/= '_') . T.dropEnd 3 }
  @hexreallit              { tokenM $ fmap FLOATLIT . readHexRealLit . T.filter (/= '_') }
  "'" @charlit "'"         { tokenM $ fmap CHARLIT . tryRead "char" }
  \" @stringcharlit* \"    { tokenM $ fmap (STRINGLIT . T.pack) . tryRead "string"  }

  @identifier              { tokenS keyword }
  @identifier "["          { tokenPosM $ fmap INDEXING . indexing . second (T.takeWhile (/='[')) }
  @qualidentifier "["      { tokenM $ fmap (uncurry QUALINDEXING) . mkQualId . T.takeWhile (/='[') }
  @identifier "." "("      { tokenPosM $ fmap (QUALPAREN []) . indexing . second (T.init . T.takeWhile (/='(')) }
  @qualidentifier "." "("  { tokenM $ fmap (uncurry QUALPAREN) . mkQualId . T.init . T.takeWhile (/='(') }
  "#" @identifier          { tokenS $ CONSTRUCTOR . nameFromText . T.drop 1 }

  @binop                   { tokenM $ return . symbol [] . nameFromText }
  @qualbinop               { tokenM $ \s -> do (qs,k) <- mkQualId s; return (symbol qs k) }

  "." [0-9]+               { tokenS $ PROJ_INTFIELD . nameFromText . T.drop 1 }
{

keyword :: T.Text -> Token
keyword s =
  case s of
    "true"         -> TRUE
    "false"        -> FALSE
    "if"           -> IF
    "then"         -> THEN
    "else"         -> ELSE
    "def"          -> DEF
    "let"          -> LET
    "loop"         -> LOOP
    "in"           -> IN
    "val"          -> VAL
    "for"          -> FOR
    "do"           -> DO
    "with"         -> WITH
    "local"        -> LOCAL
    "open"         -> OPEN
    "include"      -> INCLUDE
    "import"       -> IMPORT
    "type"         -> TYPE
    "entry"        -> ENTRY
    "module"       -> MODULE
    "while"        -> WHILE
    "assert"       -> ASSERT
    "match"        -> MATCH
    "case"         -> CASE

    _              -> ID $ nameFromText s

indexing :: (Loc, T.Text) -> Alex Name
indexing (loc, s) = case keyword s of
  ID v -> return v
  _    -> alexError loc $ "Cannot index keyword '" ++ T.unpack s ++ "'."

mkQualId :: T.Text -> Alex ([Name], Name)
mkQualId s = case reverse $ T.splitOn "." s of
  []   -> error "mkQualId: no components"
  k:qs -> return (map nameFromText (reverse qs), nameFromText k)

-- | Suffix a zero if the last character is dot.
suffZero :: T.Text -> T.Text
suffZero s = if T.last s == '.' then s <> "0" else s

tryRead :: Read a => String -> T.Text -> Alex a
tryRead desc s = case reads s' of
  [(x, "")] -> return x
  _         -> error $ "Invalid " ++ desc ++ " literal: `" ++ T.unpack s ++ "'."
  where s' = T.unpack s

readIntegral :: Integral a => T.Text -> a
readIntegral s
  | "0x" `T.isPrefixOf` s || "0X" `T.isPrefixOf` s = parseBase 16 (T.drop 2 s)
  | "0b" `T.isPrefixOf` s || "0B" `T.isPrefixOf` s = parseBase 2 (T.drop 2 s)
  | "0r" `T.isPrefixOf` s || "0R" `T.isPrefixOf` s = fromRoman (T.drop 2 s)
  | otherwise = parseBase 10 s
      where parseBase base = T.foldl (\acc c -> acc * base + fromIntegral (digitToInt c)) 0

tokenC v  = tokenS $ const v

tokenS f = tokenM $ return . f

type Lexeme a = (Pos, Pos, a)

tokenM :: (T.Text -> Alex a)
       -> (Pos, Char, ByteString.ByteString, Int64)
       -> Int64
       -> Alex (Lexeme a)
tokenM f = tokenPosM (f . snd)

tokenPosM :: ((Loc, T.Text) -> Alex a)
          -> (Pos, Char, ByteString.ByteString, Int64)
          -> Int64
          -> Alex (Lexeme a)
tokenPosM f (pos, _, s, _) len = do
  x <- f (Loc pos pos', T.decodeUtf8 $ BS.toStrict s')
  return (pos, pos', x)
  where pos' = advance pos s'
        s' = BS.take len s

advance :: Pos -> ByteString.ByteString -> Pos
advance orig_pos = foldl' advance' orig_pos . init . ByteString.unpack
  where advance' (Pos f !line !col !addr) c
          | c == nl   = Pos f (line + 1) 1 (addr + 1)
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


romanNumerals :: Integral a => [(T.Text,a)]
romanNumerals = reverse
                [ ("I",     1)
                , ("IV",    4)
                , ("V",     5)
                , ("IX",    9)
                , ("X",    10)
                , ("XL",   40)
                , ("L",    50)
                , ("XC",   90)
                , ("C",   100)
                , ("CD",  400)
                , ("D",   500)
                , ("CM",  900)
                , ("M",  1000)
                ]

fromRoman :: Integral a => T.Text -> a
fromRoman s =
  case find ((`T.isPrefixOf` s) . fst) romanNumerals of
    Nothing -> 0
    Just (d,n) -> n+fromRoman (T.drop (T.length d) s)

readHexRealLit :: RealFloat a => T.Text -> Alex a
readHexRealLit s =
  let num =  (T.drop 2 s) in
  -- extract number into integer, fractional and (optional) exponent
  let comps = T.split (`elem` ['.','p','P']) num in
  case comps of
    [i, f, p] ->
        let runTextReader r = fromIntegral . fst . fromRight (error "internal error") . r
            intPart = runTextReader T.hexadecimal i
            fracPart = runTextReader T.hexadecimal f
            exponent = runTextReader (T.signed T.decimal) p

            fracLen = fromIntegral $ T.length f
            fracVal = fracPart / (16.0 ** fracLen)
            totalVal = (intPart + fracVal) * (2.0 ** exponent) in
        return totalVal
    _ -> error "bad hex real literal"

alexGetPos :: Alex Pos
alexGetPos = Alex $ \s -> Right (s, alex_pos s)

-- | A lexical token.  It does not itself contain position
-- information, so in practice the parser will consume tokens tagged
-- with a source position.
data Token = ID Name
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

             deriving (Show, Eq, Ord)

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

-- | Given a starting position, produce tokens from the given text (or
-- a lexer error).  Returns the final position.
scanTokensText :: Pos -> T.Text -> Either LexerError ([L Token], Pos)
scanTokensText pos = scanTokens pos . BS.fromStrict . T.encodeUtf8

scanTokens :: Pos -> BS.ByteString -> Either LexerError ([L Token], Pos)
scanTokens pos str =
  runAlex' pos str $ do
  fix $ \loop -> do
    tok <- getToken
    case tok of
      (start, end, EOF) ->
        pure ([], end)
      (start, end, t) -> do
        (rest, endpos) <- loop
        pure (L (Loc start end) t : rest, endpos)
}
