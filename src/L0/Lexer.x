{
{-# OPTIONS_GHC -w #-}
module L0.Lexer
  ( Token(..)
  , alexScanTokens
  ) where

import L0.AbSyn

}

%wrapper "posn"

@charlit = ($printable#['\\]|"\\"($printable|[0-9]+))

tokens :-

  $white+				;
  "//"[^\n]*				;
  "&&"                     { ign $ AND . toPos }
  "||"                     { ign $ OR . toPos }
  ">>"                     { ign $ SHIFTR . toPos }
  "<<"                     { ign $ SHIFTL . toPos }
  "=>"                     { ign $ ARROW . toPos }
  "<-"                     { ign $ SETTO . toPos }
  "<="                     { ign $ LEQ . toPos }
  "+"                      { ign $ PLUS . toPos }
  "-"                      { ign $ MINUS . toPos }
  "~"                      { ign $ NEGATE . toPos }
  "*"                      { ign $ TIMES . toPos }
  "/"                      { ign $ DIVIDE . toPos }
  "="                      { ign $ EQU . toPos }
  "<"                      { ign $ LTH . toPos }
  "&"                      { ign $ BAND . toPos }
  "|"                      { ign $ BOR . toPos }
  "^"                      { ign $ XOR . toPos }
  "("                      { ign $ LPAR . toPos }
  ")"                      { ign $ RPAR . toPos }
  "["                      { ign $ LBRACKET . toPos }
  "]"                      { ign $ RBRACKET . toPos }
  "{"                      { ign $ LCURLY . toPos }
  "}"                      { ign $ RCURLY . toPos }
  ","                      { ign $ COMMA . toPos }
  [0-9]+                   { \p -> flip INTLIT (toPos p) . read }
  "~"? (([0-9]+("."[0-9]*)?|"."[0-9]+))
    ([eE][\+\~]?[0-9]+)?     { \p -> flip REALLIT (toPos p) . readReal }
  [a-zA-Z] [a-zA-Z0-9_]* { keyword . toPos }
  "'" @charlit "'" { \p -> flip CHARLIT (toPos p) . read }
  \" @charlit* \" { \p -> flip STRINGLIT (toPos p) . read }

{
data Token = IF { tokPos :: Pos }
           | THEN { tokPos :: Pos }
           | ELSE { tokPos :: Pos }
           | LET { tokPos :: Pos }
           | IN { tokPos :: Pos }
           | INT { tokPos :: Pos }
           | BOOL { tokPos :: Pos }
           | CHAR { tokPos :: Pos }
           | REAL { tokPos :: Pos }

           | ID { idName :: String, tokPos :: Pos }

           | STRINGLIT { stringLit :: String, tokPos :: Pos }
           | INTLIT { intLit :: Int, tokPos :: Pos }
           | REALLIT { realLit :: Double, tokPos :: Pos }
           | CHARLIT { charLit :: Char, tokPos :: Pos }

           | PLUS { tokPos :: Pos }
           | MINUS { tokPos :: Pos }
           | TIMES { tokPos :: Pos }
           | DIVIDE { tokPos :: Pos }
           | EQU { tokPos :: Pos }
           | LTH { tokPos :: Pos }
           | LEQ { tokPos :: Pos }
           | POW { tokPos :: Pos }
           | SHIFTL { tokPos :: Pos }
           | SHIFTR { tokPos :: Pos }
           | BOR { tokPos :: Pos }
           | BAND { tokPos :: Pos }
           | XOR { tokPos :: Pos }
           | LPAR { tokPos :: Pos }
           | RPAR { tokPos :: Pos }
           | LBRACKET { tokPos :: Pos }
           | RBRACKET { tokPos :: Pos }
           | LCURLY { tokPos :: Pos }
           | RCURLY { tokPos :: Pos }
           | COMMA { tokPos :: Pos }
           | FUN { tokPos :: Pos }
           | FN { tokPos :: Pos }
           | ARROW { tokPos :: Pos }
           | SETTO { tokPos :: Pos }
           | FOR { tokPos :: Pos }
           | DO { tokPos :: Pos }
           | WITH { tokPos :: Pos }
           | MERGE { tokPos :: Pos }
           | IOTA { tokPos :: Pos }
           | REPLICATE { tokPos :: Pos }
           | MAP { tokPos :: Pos }
           | REDUCE { tokPos :: Pos }
           | RESHAPE { tokPos :: Pos }
           | TRANSPOSE { tokPos :: Pos }
           | READ { tokPos :: Pos }
           | WRITE { tokPos :: Pos }
           | ZIPWITH { tokPos :: Pos }
           | SCAN { tokPos :: Pos }
           | SPLIT { tokPos :: Pos }
           | CONCAT { tokPos :: Pos }
           | FILTER { tokPos :: Pos }
           | MAPALL { tokPos :: Pos }
           | REDOMAP { tokPos :: Pos }
           | TRUE { tokPos :: Pos }
           | FALSE { tokPos :: Pos }
           | NOT { tokPos :: Pos }
           | NEGATE { tokPos :: Pos }
           | AND { tokPos :: Pos }
           | OR { tokPos :: Pos }
           | OP { tokPos :: Pos }
             deriving (Show, Eq)

toPos :: AlexPosn -> Pos
toPos (AlexPn _ line col) = (line, col)

keyword :: Pos -> String -> Token
keyword pos s =
  case s of
    "if"           -> IF    pos
    "then"         -> THEN  pos
    "else"         -> ELSE  pos
    "let"          -> LET   pos
    "in"           -> IN    pos
    "with"         -> WITH  pos
    "merge"        -> MERGE pos
    "int"          -> INT   pos
    "real"         -> REAL  pos
    "bool"         -> BOOL  pos
    "char"         -> CHAR  pos
    "fun"          -> FUN   pos
    "fn"           -> FN    pos
    "for"          -> FOR   pos
    "do"           -> DO    pos
    "op"           -> OP    pos
    "not"          -> NOT   pos
    "True"         -> TRUE  pos
    "False"        -> FALSE pos
    "pow"          -> POW   pos

    "iota"         -> IOTA      pos
    "replicate"    -> REPLICATE pos
    "reshape"      -> RESHAPE   pos
    "transpose"    -> TRANSPOSE pos
    "map"          -> MAP       pos
    "reduce"       -> REDUCE    pos
    "read"         -> READ      pos
    "write"        -> WRITE     pos
    "zipWith"      -> ZIPWITH   pos
    "scan"         -> SCAN      pos
    "split"        -> SPLIT     pos
    "concat"       -> CONCAT    pos
    "mapall"       -> MAPALL    pos
    "filter"       -> FILTER    pos
    "redomap"      -> REDOMAP   pos
    _              -> ID s pos

ign :: (a -> b) -> a -> c -> b
ign g x _ = g x

readReal :: String -> Double
readReal = read . map subst
  where subst '~' = '-'
        subst c   = c
}
