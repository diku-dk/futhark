{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -w #-}
-- | The Futhark lexer.  Takes a string, produces a list of tokens with position information.
module Language.Futhark.Parser.Lexer
  ( Token(..)
  , L(..)
  , scanTokens
  , scanTokensText
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

import Language.Futhark.Core (Int8, Int16, Int32, Int64,
                              Word8, Word16, Word32, Word64,
                              Name, nameFromText, nameToText)
import Language.Futhark.Attributes (leadingOperator)
import Language.Futhark.Syntax (BinOp(..))

}

%wrapper "monad-bytestring"

@charlit = ($printable#['\\]|\\($printable|[0-9]+))
@stringcharlit = ($printable#[\"\\]|\\($printable|[0-9]+)|\n)
@hexlit = 0[xX][0-9a-fA-F][0-9a-fA-F_]*
@declit = [0-9][0-9_]*
@binlit = 0[bB][01][01_]*
@romlit = 0[rR][IVXLCDM][IVXLCDM_]*
@intlit = @hexlit|@binlit|@declit|@romlit
@reallit = (([0-9][0-9_]*("."[0-9][0-9_]*)?))([eE][\+\-]?[0-9]+)?
@hexreallit = 0[xX][0-9a-fA-F][0-9a-fA-F_]*"."[0-9a-fA-F][0-9a-fA-F_]*([pP][\+\-]?[0-9]+)

@field = [a-zA-Z0-9] [a-zA-Z0-9_]*

@identifier = [a-zA-Z] [a-zA-Z0-9_']* | "_" [a-zA-Z0-9] [a-zA-Z0-9_']*
@qualidentifier = (@identifier ".")+ @identifier

@unop = "!"
@qualunop = (@identifier ".")+ @unop

@opchar = ("+"|"-"|"*"|"/"|"%"|"="|"!"|">"|"<"|"|"|"&"|"^"|".")
@binop = ("+"|"-"|"*"|"/"|"%"|"="|"!"|">"|"<"|"|"|"&"|"^") @opchar*
@qualbinop = (@identifier ".")+ @binop

@space = [\ \t\f\v]
@doc = "-- |"[^\n]*(\n@space*"--"[^\n]*)*

tokens :-

  $white+                               ;
  @doc                     { tokenM $ return . DOC . T.unpack . T.unlines .
                                      map (T.drop 3 . T.stripStart) .
                                           T.split (== '\n') . ("--"<>) .
                                           T.drop 4 }
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
  ":"                      { tokenC COLON }
  "\"                      { tokenC BACKSLASH }
  "'"                      { tokenC APOSTROPHE }
  "'^"                     { tokenC APOSTROPHE_THEN_HAT }
  "`"                      { tokenC BACKTICK }
  "..<"                    { tokenC TWO_DOTS_LT }
  "..>"                    { tokenC TWO_DOTS_GT }
  "..."                    { tokenC THREE_DOTS }
  ".."                     { tokenC TWO_DOTS }

  @intlit i8               { tokenM $ return . I8LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i16              { tokenM $ return . I16LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i32              { tokenM $ return . I32LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit i64              { tokenM $ return . I64LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='i') }
  @intlit u8               { tokenM $ return . U8LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u16              { tokenM $ return . U16LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u32              { tokenM $ return . U32LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit u64              { tokenM $ return . U64LIT . readIntegral . T.filter (/= '_') . T.takeWhile (/='u') }
  @intlit                  { tokenM $ return . INTLIT . readIntegral . T.filter (/= '_') }

  @reallit f32             { tokenM $ fmap F32LIT . tryRead "f32" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  @reallit f64             { tokenM $ fmap F64LIT . tryRead "f64" . suffZero . T.filter (/= '_') . T.takeWhile (/='f') }
  @reallit                 { tokenM $ fmap FLOATLIT . tryRead "f64" . suffZero . T.filter (/= '_') }
  @hexreallit f32          { tokenM $ fmap F32LIT . readHexRealLit "f32" . suffZero . T.filter (/= '_') . fst . T.breakOn (T.pack "f32") }
  @hexreallit f64          { tokenM $ fmap F64LIT . readHexRealLit "f64" . suffZero . T.filter (/= '_') . fst . T.breakOn (T.pack "f64") }
  @hexreallit              { tokenM $ fmap FLOATLIT . readHexRealLit "f64" . suffZero . T.filter (/= '_') . fst . T.breakOn (T.pack "f64") }
  "'" @charlit "'"         { tokenM $ fmap CHARLIT . tryRead "char" }
  \" @stringcharlit* \"    { tokenM $ fmap STRINGLIT . tryRead "string"  }

  @identifier              { tokenS keyword }
  @identifier "["          { tokenM $ fmap INDEXING . indexing . T.takeWhile (/='[') }
  @qualidentifier "["      { tokenM $ fmap (uncurry QUALINDEXING) . mkQualId . T.takeWhile (/='[') }
  @identifier "." "("      { tokenM $ fmap (QUALPAREN []) . indexing . T.init . T.takeWhile (/='(') }
  @qualidentifier "." "("  { tokenM $ fmap (uncurry QUALPAREN) . mkQualId . T.init . T.takeWhile (/='(') }
  "#" @identifier          { tokenS $ CONSTRUCTOR . nameFromText . T.drop 1 }

  @unop                    { tokenS $ UNOP . nameFromText }
  @qualunop                { tokenM $ fmap (uncurry QUALUNOP) . mkQualId }

  @binop                   { tokenM $ return . symbol [] . nameFromText }
  @qualbinop               { tokenM $ \s -> do (qs,k) <- mkQualId s; return (symbol qs k) }

  "." (@identifier|[0-9]+) { tokenM $ return . PROJ_FIELD . nameFromText . T.drop 1 }
  "." "["                  { tokenC PROJ_INDEX }
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
    "unsafe"       -> UNSAFE
    "assert"       -> ASSERT
    "match"        -> MATCH
    "case"         -> CASE

    _              -> ID $ nameFromText s

indexing :: T.Text -> Alex Name
indexing s = case keyword s of
  ID v -> return v
  _    -> alexError $ "Cannot index keyword '" ++ T.unpack s ++ "'."

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
  | "0x" `T.isPrefixOf` s || "0X" `T.isPrefixOf` s =
      T.foldl (another hex_digits) 0 (T.drop 2 s)
  | "0b" `T.isPrefixOf` s || "0b" `T.isPrefixOf` s =
      T.foldl (another binary_digits) 0 (T.drop 2 s)
  | "0r" `T.isPrefixOf` s =
       fromRoman (T.drop 2 s)
  | otherwise =
      T.foldl (another decimal_digits) 0 s
      where another digits acc c = acc * base + maybe 0 fromIntegral (elemIndex (toLower c) digits)
              where base = fromIntegral $ length digits

            binary_digits = ['0', '1']
            decimal_digits = ['0'..'9']
            hex_digits = decimal_digits ++ ['a'..'f']

tokenC v  = tokenS $ const v

tokenS f = tokenM $ return . f

type Lexeme a = ((Int, Int, Int), (Int, Int, Int), a)

tokenM :: (T.Text -> Alex a)
       -> (AlexPosn, Char, ByteString.ByteString, Int64)
       -> Int64
       -> Alex (Lexeme a)
tokenM f (AlexPn addr line col, _, s, _) len = do
  x <- f $ T.decodeUtf8 $ BS.toStrict s'
  return (pos, advance pos s', x)
  where pos = (line, col, addr)
        s' = BS.take len s

advance :: (Int, Int, Int) -> ByteString.ByteString -> (Int, Int, Int)
advance orig_pos = foldl' advance' orig_pos . init . ByteString.unpack
  where advance' (!line, !col, !addr) c
          | c == nl   = (line + 1, 1, addr + 1)
          | otherwise = (line, col + 1, addr + 1)
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

fromHexRealLit :: RealFloat a => T.Text -> Maybe a
fromHexRealLit s =
  let num =  (T.drop 2 s) in
  -- extract number into integer, fractional and (optional) exponent
  let comps = (T.split (\x -> x == '.' || x == 'p' || x == 'P') num) in
  case comps of
    [i, f, p] ->
        let int_part = readIntegral (T.pack ("0x" ++ (T.unpack i)))
            frac_part = readIntegral (T.pack ("0x" ++ (T.unpack f)))
            exponent = if ((T.pack "-") `T.isPrefixOf` p)
                       then -1 * (readIntegral p)
                       else readIntegral p

            frac_len = T.length f
            frac_val = (fromIntegral frac_part) / (16.0 ** (fromIntegral frac_len))
            total_val = ((fromIntegral int_part) + frac_val) * (2.0 ** (fromIntegral exponent)) in
        Just (total_val)
    _ -> Nothing

readHexRealLit :: RealFloat a => String -> T.Text -> Alex a
readHexRealLit desc s =
  case fromHexRealLit s of
    Just (n) -> return n
    Nothing -> error $ "Invalid " ++ desc ++ " literal: " ++ T.unpack s

alexGetPosn :: Alex (Int, Int, Int)
alexGetPosn = Alex $ \s ->
  let (AlexPn off line col) = alex_pos s
  in Right (s, (line, col, off))

alexEOF = do
  posn <- alexGetPosn
  return (posn, posn, EOF)

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
           | QUALINDEXING [Name] Name
           | QUALPAREN [Name] Name
           | UNOP Name
           | QUALUNOP [Name] Name
           | SYMBOL BinOp [Name] Name
           | CONSTRUCTOR Name
           | PROJ_FIELD Name
           | PROJ_INDEX

           | INTLIT Integer
           | STRINGLIT String
           | I8LIT Int8
           | I16LIT Int16
           | I32LIT Int32
           | I64LIT Int64
           | U8LIT Word8
           | U16LIT Word16
           | U32LIT Word32
           | U64LIT Word64
           | FLOATLIT Double
           | F32LIT Float
           | F64LIT Double
           | CHARLIT Char

           | COLON
           | BACKSLASH
           | APOSTROPHE
           | APOSTROPHE_THEN_HAT
           | BACKTICK
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

           | EQU
           | ASTERISK
           | NEGATE
           | LTH
           | HAT
           | PIPE

           | IF
           | THEN
           | ELSE
           | LET
           | LOOP
           | IN
           | FOR
           | DO
           | WITH
           | UNSAFE
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

runAlex' :: AlexPosn -> ByteString.ByteString -> Alex a -> Either String a
runAlex' start_pos input__ (Alex f) =
  case f (AlexState { alex_pos = start_pos
                    , alex_bpos = 0
                    , alex_inp = input__
                    , alex_chr = '\n'
                    , alex_scd = 0}) of Left msg -> Left msg
                                        Right ( _, a ) -> Right a

scanTokensText :: Pos -> T.Text -> Either String ([L Token], Pos)
scanTokensText pos = scanTokens pos . BS.fromStrict . T.encodeUtf8

scanTokens :: Pos -> BS.ByteString -> Either String ([L Token], Pos)
scanTokens (Pos file start_line start_col start_off) str =
  runAlex' (AlexPn start_off start_line start_col) str $ do
  fix $ \loop -> do
    tok <- alexMonadScan
    case tok of
      (start, end, EOF) ->
        return ([], posnToPos end)
      (start, end, t) -> do
        (rest, endpos) <- loop
        return (L (pos start end) t : rest, endpos)
  where pos start end = SrcLoc $ Loc (posnToPos start) (posnToPos end)
        posnToPos (line, col, off) = Pos file line col off
}
