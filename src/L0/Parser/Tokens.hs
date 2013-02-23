module L0.Parser.Tokens
  ( Token(..)
  )
  where

data Token = IF
           | THEN
           | ELSE
           | LET
           | IN
           | INT
           | BOOL
           | CHAR
           | REAL
           | ID { idName :: String }
           | STRINGLIT { stringLit :: String }
           | INTLIT { intLit :: Int }
           | REALLIT { realLit :: Double }
           | CHARLIT { charLit :: Char }
           | PLUS
           | MINUS
           | TIMES
           | DIVIDE
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
           | FUN
           | FN
           | ARROW
           | SETTO
           | FOR
           | DO
           | WITH
           | MERGE
           | SIZE
           | IOTA
           | REPLICATE
           | MAP
           | REDUCE
           | RESHAPE
           | TRANSPOSE
           | READ
           | WRITE
           | ZIP
           | UNZIP
           | SCAN
           | SPLIT
           | CONCAT
           | FILTER
           | MAPALL
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
             deriving (Show, Eq)
