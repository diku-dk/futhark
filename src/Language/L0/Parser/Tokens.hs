module Language.L0.Parser.Tokens
  ( Token(..)
  )
  where

import Language.L0.Syntax (Name)

data Token = IF
           | THEN
           | ELSE
           | LET
           | LOOP
           | IN
           | INT
           | BOOL
           | CHAR
           | REAL
           | ID { idName :: Name }
           | STRINGLIT { stringLit :: String }
           | INTLIT { intLit :: Int }
           | REALLIT { realLit :: Double }
           | CHARLIT { charLit :: Char }
           | PLUS
           | MINUS
           | TIMES
           | DIVIDE
           | MOD
           | EQU
           | LTH
           | LEQ
           | POW
           | SHIFTL
           | SHIFTR
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
           | RESHAPE
           | TRANSPOSE
           | ZIP
           | UNZIP
           | SCAN
           | SPLIT
           | CONCAT
           | FILTER
           | REDOMAP
           | TRUE
           | FALSE
           | NOT
           | NEGATE
           | AND
           | OR
           | OP
           | EMPTY
           | COPY
           | MAP2
           | REDUCE2
           | SCAN2
           | FILTER2
           | MAPALL2
           | REDOMAP2

           | MIN
           | MAX

             deriving (Show, Eq)
