module L0.Parser.Tokens
  ( Token(..)
  )
  where

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
           | ID { idName :: String }
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
