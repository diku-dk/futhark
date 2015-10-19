{
{-# OPTIONS_GHC -w #-}
-- | The Futhark lexer.  Takes a string, produces a list of tokens with position information.
module Language.Futhark.Parser.Lexer
  ( Token(..)
  , alexScanTokens
  , L(..)
  ) where

import Data.Loc hiding (L)
import Data.Int (Int32)

import Language.Futhark.Core (nameFromString)
import Language.Futhark.Parser.Tokens

import Data.Word (Word8)

import Data.Bits

}

@charlit = ($printable#['\\]|\\($printable|[0-9]+))
@stringcharlit = ($printable#[\"\\]|\\($printable|[0-9]+)|\n)

tokens :-

  $white+                               ;
  "--"[^\n]*                            ;
  "&&"                     { const AND }
  "||"                     { const OR }
  ">>"                     { const SHIFTR }
  "<<"                     { const SHIFTL }
  "=>"                     { const ARROW }
  "<-"                     { const SETTO }
  "<="                     { const LEQ }
  ">="                     { const GEQ }
  "+"                      { const PLUS }
  "-"                      { const MINUS }
  "~"                      { const TILDE }
  "*"                      { const TIMES }
  "/"                      { const DIVIDE }
  "%"                      { const MOD }
  "//"                     { const QUOT }
  "%%"                     { const REM }
  "="                      { const EQU }
  "=="                     { const EQU2 }
  "<"                      { const LTH }
  ">"                      { const GTH }
  "&"                      { const BAND }
  "|"                      { const BOR }
  "^"                      { const XOR }
  "("                      { const LPAR }
  ")"                      { const RPAR }
  "["                      { const LBRACKET }
  "]"                      { const RBRACKET }
  "{"                      { const LCURLY }
  "}"                      { const RCURLY }
  ","                      { const COMMA }
  "_"                      { const UNDERSCORE }
  "!"                      { const BANG }
  0[xX][0-9a-fA-F]*        { INTLIT . readInt32 }
  [0-9]+                   { INTLIT . readInt32 }
  (([0-9]+("."[0-9]+)?))
    ([eE][\+\-]?[0-9]+)?   { REALLIT . readReal }
  [a-zA-Z] [a-zA-Z0-9_']* { keyword }
  "'" @charlit "'" { CHARLIT . read }
  \" @stringcharlit* \" { STRINGLIT . read }

{

keyword :: String -> Token
keyword s =
  case s of
    "if"           -> IF
    "then"         -> THEN
    "else"         -> ELSE
    "let"          -> LET
    "loop"         -> LOOP
    "in"           -> IN
    "with"         -> WITH
    "int"          -> INT
    "real"         -> REAL
    "bool"         -> BOOL
    "cert"         -> CERT
    "char"         -> CHAR
    "fun"          -> FUN
    "fn"           -> FN
    "for"          -> FOR
    "do"           -> DO
    "op"           -> OP
    "True"         -> TRUE
    "False"        -> FALSE
    "Checked"      -> CHECKED
    "pow"          -> POW
    "abs"          -> ABS
    "signum"       -> SIGNUM

    "iota"         -> IOTA
    "size"         -> SIZE
    "replicate"    -> REPLICATE
    "reshape"      -> RESHAPE
    "rearrange"    -> REARRANGE
    "stripe"       -> STRIPE
    "unstripe"     -> UNSTRIPE
    "transpose"    -> TRANSPOSE
    "map"          -> MAP
    "reduce"       -> REDUCE
    "zip"          -> ZIP
    "zipWith"      -> ZIPWITH
    "unzip"        -> UNZIP
    "scan"         -> SCAN
    "split"        -> SPLIT
    "concat"       -> CONCAT
    "concatMap"    -> CONCATMAP
    "filter"       -> FILTER
    "partition"    -> PARTITION
    "redomap"      -> REDOMAP
    "empty"        -> EMPTY
    "copy"         -> COPY
    "while"        -> WHILE
    "streamMap"       -> STREAM_MAP
    "streamMapMax"    -> STREAM_MAPMAX
    "streamMapPer"    -> STREAM_MAPPER
    "streamMapPerMax" -> STREAM_MAPPERMAX
    "streamRed"       -> STREAM_RED
    "streamRedMax"    -> STREAM_REDMAX
    "streamRedPer"    -> STREAM_REDPER
    "streamRedPerMax" -> STREAM_REDPERMAX
    "streamSeq"       -> STREAM_SEQ
    "streamSeqMax"    -> STREAM_SEQMAX
    "assert"       -> ASSERT
    _              -> ID $ nameFromString s

type Byte = Word8

type Action result = String -> result

data AlexPosn = AlexPn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- rest of the bytes for the current char
                  String)       -- current input string

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

-- | Given a string, returns either a lexer error, or a finite stream
-- of tokens, each with embedded position information.  The
-- 'FilePath' is used solely for error messages and the position
-- information.
alexScanTokens :: FilePath -> String -> Either String [L Token]
alexScanTokens file str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return []
                AlexError ((AlexPn _ line column),_,_,_) -> Left $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column) ++ "."

                AlexSkip  inp' len     -> go inp'
                AlexToken inp'@(pos',_,_,_) len act -> do
                  let tok = L (loc pos pos') $ act (take len str)
                  toks <- go inp'
                  return $ tok : toks
        loc beg end = SrcLoc $ Loc (posnToPos beg) (posnToPos end)
        posnToPos (AlexPn offset line col) = Pos file line col offset

readReal :: String -> Double
readReal = read

readInt32 :: String -> Int32
readInt32 = read

-- | A value tagged with a source location.
data L a = L SrcLoc a

instance Eq a => Eq (L a) where
  L _ x == L _ y = x == y

instance Located (L a) where
  locOf (L (SrcLoc loc) _) = loc

}
